namespace Aardvark.Rendering.Vulkan


open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Vulkan.Raytracing
open Aardvark.Rendering.Vulkan.NVRayTracing

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CameraUniform =
    struct
        new(v, p) = {viewInverse = v; projInverse = p}

        val viewInverse : M44f
        val projInverse : M44f
    end

[<Struct; StructLayout(LayoutKind.Sequential)>]
type RaytracingSettings =
    struct
        new(bounces, min, max) = {maxBounces = bounces; tmin = min; tmax = max}

        val maxBounces : uint32
        val tmin : float32
        val tmax : float32
    end

[<AutoOpen>]
module DummyHelpers =
    open FShade
    open FShade.GLSL
    open FShade.Formats
    open FShade.Imperative

    let backend = Backends.glslVulkan

    let raygenInfo : RayHitInfo =
        {
            neededUniforms = Map.ofList [
                "resultImage", typeof<IntImage2d<rgba8i>>
                "camera", typeof<CameraUniform>
                "raytracingSettings", typeof<RaytracingSettings>
                "scene", typeof<VkAccelerationStructureNV>
            ]
            neededSamplers = Map.empty
            neededBuffers = Map.empty
            payloadInType = typeof<unit>
            payloadOutType = typeof<unit>
        }

    let fixedBindings = LookupTable.lookupTable [
        "resultImage", 0
        "camera", 1
        "raytracingSettings", 2
        "scene", 3
    ]

    let getType t =
        let t = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend t)
        let (final, _, _) = t |> LayoutStd140.layout
        final
        
    let getFields t =
        match (getType t) with
            | Struct(_, fields, _) -> fields |> List.map (fun (name, typ, _) -> name, typ)
            | _ -> []

    let getUniformBuffer set binding name typ =
        let t = getType typ

        let toUniformBufferField (name, typ, offset) = {
            ufName = name
            ufType = typ
            ufOffset = offset
        }

        match t with
            | Struct(_, fields, size) ->
                {
                    ubSet = set
                    ubBinding = binding
                    ubName = name
                    ubFields = fields |> List.map toUniformBufferField
                    ubSize = size
                }
            | _ ->
                failwith "Not a struct"

    let assign (info : RayHitInfo) =

        let mapi f =
            (Map.toList >> List.mapi f >> Map.ofList)

        let uniformBuffers =
            info.neededUniforms 
                |> mapi (fun i (name, uniform) -> ((0, fixedBindings name), (name, getFields uniform)))

        let samplers =
            let offset = uniformBuffers.Count
            info.neededSamplers
                |> mapi (fun i (name, sampler) -> ((0, fixedBindings name), (name, sampler)))

        let buffers =
            let offset = uniformBuffers.Count + samplers.Count
            info.neededBuffers
                |> mapi (fun i (name, (dim, buffer)) -> ((0, fixedBindings name), (name, dim, buffer)))

        {
            uniformBuffers = uniformBuffers
            samplers = samplers
            buffers = buffers
            payloadInLocation = 0
            payloadOutLocation = 0
        }

[<AutoOpen>]
module ``Trace Command Extensions`` =

     type Command with

            static member BindPipeline(pipeline : VkPipeline) =
                { new Command() with
                    member x.Compatible = QueueFlags.Graphics
                    member x.Enqueue cmd =
                        cmd.AppendCommand()
                        VkRaw.vkCmdBindPipeline(cmd.Handle, VkPipelineBindPoint.RayTracingNv, pipeline)
                        Disposable.Empty
                }

            static member BindDescriptorSet(descriptorSet : VkDescriptorSet, pipelineLayout : VkPipelineLayout) =
                { new Command() with
                    member x.Compatible = QueueFlags.Graphics
                    member x.Enqueue cmd =
                        cmd.AppendCommand()
                        pin (fun ptr ->
                            VkRaw.vkCmdBindDescriptorSets(cmd.Handle, VkPipelineBindPoint.RayTracingNv, pipelineLayout,
                                0u, 1u, ptr, 0u, NativePtr.zero)
                        ) descriptorSet
                        Disposable.Empty
                }

            static member TraceRays(raygenShaderBindingTableBuffer : VkBuffer, 
                                    raygenShaderBindingOffset : VkDeviceSize,
                                    missShaderBindingTableBuffer : VkBuffer, 
                                    missShaderBindingOffset : VkDeviceSize, 
                                    missShaderBindingStride : VkDeviceSize,
                                    hitShaderBindingTableBuffer : VkBuffer,
                                    hitShaderBindingOffset : VkDeviceSize,
                                    hitShaderBindingStride : VkDeviceSize,
                                    callableShaderBindingTableBuffer : VkBuffer,
                                    callableShaderBindingOffset : VkDeviceSize,
                                    callableShaderBindingStride : VkDeviceSize,
                                    width : uint32, height : uint32, depth : uint32) =
                { new Command() with
                    member x.Compatible = QueueFlags.Graphics
                    member x.Enqueue cmd =
                        cmd.AppendCommand()
                        VkRaw.vkCmdTraceRaysNV (cmd.Handle, raygenShaderBindingTableBuffer, raygenShaderBindingOffset,
                            missShaderBindingTableBuffer, missShaderBindingOffset, missShaderBindingStride,
                            hitShaderBindingTableBuffer, hitShaderBindingOffset, hitShaderBindingStride,
                            callableShaderBindingTableBuffer, callableShaderBindingOffset, callableShaderBindingStride,
                            width, height, depth)
                        Disposable.Empty
                }

type TraceTask(device : Device, scene : TraceScene) =
    inherit AdaptiveObject()

    let locks = ReferenceCountingSet<ILockedResource>()

    let user =
        { new IResourceUser with
            member x.AddLocked l = lock locks (fun () -> locks.Add l |> ignore)
            member x.RemoveLocked l = lock locks (fun () -> locks.Remove l |> ignore)
        }

    let manager = new ResourceManager(user, device)
    let resources = new ResourceLocationSet(user)

    let raygenInterface = assign raygenInfo

    let preparedScene =
        let prep = manager.PrepareTraceScene(raygenInfo, raygenInterface, scene)
        prep.resources |> List.iter resources.Add

        prep

    member x.Run (token : AdaptiveToken) (size : V3i) =
        let outputImage = preparedScene.original.textures.[Symbol.Create "resultImage"]

        x.EvaluateAlways token (fun t ->
            resources.Update t |> ignore
            preparedScene.Update t
            
            let pipeline = preparedScene.pipeline
            let pipelineLayout = pipeline.Description.layout
            let descriptorSet = preparedScene.descriptorSet.Update t
            let sbt = preparedScene.shaderBindingTable

            let outputImage : Image = unbox <| outputImage.GetValue t

            device.GraphicsFamily.run {
                do! Command.TransformLayout(outputImage, VkImageLayout.General)
                do! Command.BindPipeline pipeline.Handle
                do! Command.BindDescriptorSet(descriptorSet.handle.Handle, pipelineLayout.Handle)
                do! Command.TraceRays(sbt.Handle, sbt.RaygenShaderBindingOffset,
                                        sbt.Handle, sbt.MissShaderBindingOffset, sbt.MissShaderBindingStride,
                                        sbt.Handle, sbt.HitShaderBindingOffset, sbt.HitShaderBindingStride,
                                        sbt.Handle, sbt.CallableShaderBindingOffset, sbt.CallableShaderBindingStride,
                                        uint32 size.X, uint32 size.Y, uint32 size.Z)
                do! Command.TransformLayout(outputImage, VkImageLayout.ShaderReadOnlyOptimal)
            }
        )

    member x.Dispose() =
        preparedScene.Dispose()

    interface ITraceTask with
        member x.Run (token : AdaptiveToken) (size : V3i) =
            x.Run token size

    interface IDisposable with
        member x.Dispose() =
            x.Dispose()