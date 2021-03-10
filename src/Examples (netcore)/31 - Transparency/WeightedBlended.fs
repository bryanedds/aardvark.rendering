﻿namespace Transparency

// TODO: Antialiasing is handled kind of sketchy, isn't it?

module WeightedBlended =

    open Aardvark.Base

    open Aardvark.Rendering
    open Aardvark.SceneGraph
    open FSharp.Data.Adaptive
    open FSharp.Data.Adaptive.Operators

    module private DefaultSemantic =
        let Accum = Symbol.Create "Accum"
        let Revealage = Symbol.Create "Revealage"
        let AccumBuffer = TypedSymbol<ITexture>("AccumBuffer")
        let RevealageBuffer = TypedSymbol<ITexture>("RevealageBuffer")

    module private Shaders =
        open FShade

        type Fragment = {
            [<Color>] color : V4d
            [<FragCoord>] coord : V4d
            [<SampleId>] sample : int
        }

        // Computes the sum of colors as well as the revealage.
        // Accum: (R * A * w, G * A * w, B * A * w, A * w)
        // Revealage: (A)
        // Channels of the accum buffer are summed, alpha values are multiplied to compute revealage.
        // http://casual-effects.blogspot.com/2015/03/implemented-weighted-blended-order.html
        let weightedBlend (f : Fragment) =
            fragment {
                let a = f.color.W * 8.0 + 0.01
                let b = -f.coord.Z * 0.95 + 1.0
                let w = clamp 1e-2 3e2 (a * a * a * 1e8 * b * b * b)

                let alpha = f.color.W
                let color = V4d(f.color.XYZ * alpha, alpha) * w

                return {| Accum = color
                          Revealage = alpha |}
            }

        let private accumSampler =
            sampler2d {
                texture uniform?AccumBuffer
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private revealageSampler =
            sampler2d {
                texture uniform?RevealageBuffer
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private accumSamplerMS =
            sampler2dMS {
                texture uniform?AccumBuffer
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private revealageSamplerMS =
            sampler2dMS {
                texture uniform?RevealageBuffer
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        // Composites the results using the two buffers from the earlier pass.
        let composite (samples : int) (f : Fragment) =
            fragment {
                let mutable accum = V4d.Zero
                let mutable revealage = V4d.Zero

                if samples > 1 then
                    for i in 0 .. samples - 1 do
                        accum <- accum + accumSamplerMS.Read(V2i f.coord.XY, i)
                        revealage <- revealage + revealageSamplerMS.Read(V2i f.coord.XY, i)

                    accum <- accum / float samples
                    revealage <- revealage / float samples
                else
                    accum <- accumSampler.Read(V2i f.coord.XY, 0)
                    revealage <- revealageSampler.Read(V2i f.coord.XY, 0)

                let accum =
                    if isInfinity accum then
                        V4d(accum.W)
                    else
                        accum

                return V4d(accum.XYZ / max accum.W 1e-5, 1.0 - revealage.X)
            }

        // Blit one multisampled texture to another.
        let private diffuseSampler =
            sampler2d {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private diffuseSamplerMS =
            sampler2dMS {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let blit (samples : int) (f : Fragment) =
            fragment {
                if samples > 1 then
                    return diffuseSamplerMS.Read(V2i f.coord.XY, f.sample)
                else
                    return diffuseSampler.Read(V2i f.coord.XY, 0)
            }

    [<AutoOpen>]
    module private Utility =
        let createAttachment (runtime : IRuntime) (format : RenderbufferFormat) (samples : int) (size : aval<V2i>) =
            runtime.CreateTextureAttachment(
                runtime.CreateTexture2D(TextureFormat.ofRenderbufferFormat format, samples, size)
            )

    type Technique(runtime : IRuntime, framebuffer : FramebufferInfo, scene : Scene) =

        let size = framebuffer.size
        let samples = framebuffer.samples

        // We create a separate framebuffer so we have access to the depth buffer.
        // Ideally we'd want to use the regular framebuffer directly...
        let offscreenPass =
            runtime.CreateFramebufferSignature(samples, [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba8
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
            ])

        // Framebuffer for the transparency pass, reusing the depth buffer from
        // the opaque geometry pass.
        let transparentPass =
            runtime.CreateFramebufferSignature(samples, [
                DefaultSemantic.Accum, RenderbufferFormat.Rgba16f
                DefaultSemantic.Revealage, RenderbufferFormat.R32f
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
            ])

        let depthBuffer =
            runtime.CreateRenderbufferAttachment(
                runtime.CreateRenderbuffer(RenderbufferFormat.Depth24Stencil8, samples, size)
            )

        let offscreenFbo =
            runtime.CreateFramebuffer(offscreenPass, Map.ofList [
                DefaultSemantic.Colors, createAttachment runtime RenderbufferFormat.Rgba8 samples size
                DefaultSemantic.Depth, depthBuffer
            ])

        let transparentFbo =
            runtime.CreateFramebuffer(transparentPass, Map.ofList [
                DefaultSemantic.Accum, createAttachment runtime RenderbufferFormat.Rgba16f samples size
                DefaultSemantic.Revealage, createAttachment runtime RenderbufferFormat.R32f samples size
                DefaultSemantic.Depth, depthBuffer
            ])

        // Renders the opaque scene to the regular (offscreen) framebuffer.
        let opaqueTask =
            let sg =
                scene.opaque
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                }
                |> Sg.viewTrafo scene.viewTrafo
                |> Sg.projTrafo scene.projTrafo

            let clear = runtime.CompileClear(offscreenPass, C4f.Black, 1.0)
            let render = runtime.CompileRender(offscreenPass, sg)
            RenderTask.ofList [clear; render]

        // Renders the transparent scene to the dedicated framebuffer, reusing
        // the depth buffer (for testing only) from the opaque geometry pass.
        let transparentTask, colorOutput, alphaOutput =
            let sg =
                scene.transparent
                |> Sg.depthWrite' false
                |> Sg.blendModes' (Map.ofList [
                    DefaultSemantic.Accum, BlendMode.Add
                    DefaultSemantic.Revealage, BlendMode.Multiply
                ])
                |> Sg.viewTrafo scene.viewTrafo
                |> Sg.projTrafo scene.projTrafo
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.sgColor
                    do! Shaders.weightedBlend
                }

            let clearColors =
                Map.ofList [
                    DefaultSemantic.Accum, C4f.Zero
                    DefaultSemantic.Revealage, C4f.White
                ]

            let clear = runtime.CompileClear(transparentPass, clearColors)
            let render = runtime.CompileRender(transparentPass, sg)
            let task = RenderTask.ofList [clear; render]

            let output = task |> RenderTask.renderTo transparentFbo
            task, output.GetOutputTexture DefaultSemantic.Accum, output.GetOutputTexture DefaultSemantic.Revealage

        // Run both passes blending the result in the offscreen framebuffer.
        let compositeTask, compositeOutput =
            let sg =
                Sg.fullScreenQuad
                |> Sg.depthTest' DepthTest.None
                |> Sg.blendMode' BlendMode.Blend
                |> Sg.texture DefaultSemantic.AccumBuffer colorOutput
                |> Sg.texture DefaultSemantic.RevealageBuffer alphaOutput
                |> Sg.shader {
                    do! Shaders.composite samples
                }

            let composite = runtime.CompileRender(offscreenPass, sg)
            let output =
                RenderTask.ofList [opaqueTask; composite]
                |> RenderTask.renderTo offscreenFbo

            composite, output.GetOutputTexture DefaultSemantic.Colors

        // Display the results in the main framebuffer.
        let finalTask =
            let sg =
                Sg.fullScreenQuad
                |> Sg.diffuseTexture compositeOutput
                |> Sg.shader {
                    do! Shaders.blit samples
                }

            runtime.CompileRender(framebuffer.signature, sg)

        member x.Task = finalTask

        member x.Dispose() =
            finalTask.Dispose()
            transparentTask.Dispose()
            compositeTask.Dispose()
            opaqueTask.Dispose()
            runtime.DeleteFramebufferSignature offscreenPass
            runtime.DeleteFramebufferSignature transparentPass

        interface ITechnique with
            member x.Name = "Weighted Blended OIT"
            member x.Task = x.Task
            member x.Dispose() = x.Dispose()