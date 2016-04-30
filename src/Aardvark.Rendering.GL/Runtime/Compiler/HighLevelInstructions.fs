﻿namespace Aardvark.Rendering.GL.Compiler

open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL
open Microsoft.FSharp.NativeInterop

#nowarn "9"

module Instructions =
    open OpenTK.Graphics.OpenGL4

    let setDepthTest (m : IMod<DepthTestMode>) =
        m |> Mod.map (fun dt ->
            if dt <> DepthTestMode.None then
                [ 
                    Instruction.Enable(int OpenGl.Enums.State.DepthTest)
                    Instruction.DepthFunc(Translations.toGLComparison dt) 
                ]
            else
                [ Instruction.Disable(int OpenGl.Enums.State.DepthTest) ]
        )

    let setFillMode (m : IMod<FillMode>) =
        m |> Mod.map (fun fm -> 
            [ Instruction.PolygonMode (int OpenGl.Enums.Face.FrontAndBack) (Translations.toGLPolygonMode fm) ]
        )

    let setCullMode (m : IMod<CullMode>) =
        m |> Mod.map (fun cm -> 
            if cm <> CullMode.None then
                [ 
                    Instruction.Enable(int OpenGl.Enums.State.CullFace)
                    Instruction.CullFace(Translations.toGLFace cm) 
                ]
            else
                [ Instruction.Disable(int OpenGl.Enums.State.CullFace) ]
        )

    let setBlendMode (m : IMod<BlendMode>) =
        m |> Mod.map (fun bm -> 
            //TODO: actually depending on the Framebuffer (premultiplied alpha)
            if bm.Enabled then
                let src = Translations.toGLFactor bm.SourceFactor
                let dst = Translations.toGLFactor bm.DestinationFactor
                let op = Translations.toGLOperation bm.Operation

                let srcA = Translations.toGLFactor bm.SourceAlphaFactor
                let dstA = Translations.toGLFactor bm.DestinationAlphaFactor
                let opA = Translations.toGLOperation bm.AlphaOperation

                [ 
                    Instruction.Enable(int OpenGl.Enums.State.Blend)
                    Instruction.BlendFuncSeparate src dst srcA dstA
                    Instruction.BlendEquationSeparate op opA
                ]
            else
                [ Instruction.Disable(int OpenGl.Enums.State.Blend) ]
        )

    let setStencilMode (m : IMod<StencilMode>) =
        m |> Mod.map (fun sm -> 
            //TODO: actually depending on the Framebuffer (premultiplied alpha)
            if sm.IsEnabled then
                let cmpFront = Translations.toGLFunction sm.CompareFront.Function
                let cmpBack= Translations.toGLFunction sm.CompareBack.Function
                let opFrontSF = Translations.toGLStencilOperation sm.OperationFront.StencilFail
                let opBackSF = Translations.toGLStencilOperation sm.OperationBack.StencilFail
                let opFrontDF = Translations.toGLStencilOperation sm.OperationFront.DepthFail
                let opBackDF = Translations.toGLStencilOperation sm.OperationBack.DepthFail
                let opFrontP = Translations.toGLStencilOperation sm.OperationFront.DepthPass
                let opBackP = Translations.toGLStencilOperation sm.OperationBack.DepthPass

                [ 
                    Instruction.Enable(int OpenGl.Enums.State.StencilTest) 
                    Instruction.StencilFuncSeparate(int OpenGl.Enums.Face.Front) cmpFront sm.CompareFront.Reference (int sm.CompareFront.Mask)
                    Instruction.StencilFuncSeparate(int OpenGl.Enums.Face.Back) cmpBack sm.CompareBack.Reference (int sm.CompareBack.Mask)
                    Instruction.StencilOpSeparate(int OpenGl.Enums.Face.Front) opFrontSF opFrontDF opFrontP
                    Instruction.StencilOpSeparate(int OpenGl.Enums.Face.Back) opBackSF opBackDF opBackP
                ]
            else
                [ Instruction.Disable(int OpenGl.Enums.State.StencilTest) ]
        )

    let bindProgram (p : IResource<Program>) =
        p.Handle |> Mod.map (fun r -> Instruction.BindProgram(r.Handle))

    let bindUniformBuffer (index : int) (u : IResource<UniformBuffer>) =   
        u.Handle |> Mod.map (fun r -> 
            //ExecutionContext.bindUniformBuffer index r
            Instruction.BindBufferRange (int OpenGl.Enums.BufferTarget.UniformBuffer) index r.Handle 0n (nativeint r.Size)
        )

    let bindUniformBufferView (index : int) (u : IResource<UniformBufferView>) =   
        u.Handle |> Mod.bind (fun r ->
            r.Buffer |> Mod.map (fun b ->
                let b = unbox<Buffer> b
                //ExecutionContext.bindUniformBuffer index r
                Instruction.BindBufferRange (int OpenGl.Enums.BufferTarget.UniformBuffer) index b.Handle r.Offset (nativeint r.Size)
            )
        )

    let bindIndirectBuffer (u : IResource<IndirectBuffer>) =   
        u.Handle |> Mod.map (fun r -> 
            //ExecutionContext.bindUniformBuffer index r
            Instruction.BindBuffer (int OpenTK.Graphics.OpenGL4.BufferTarget.DrawIndirectBuffer) r.Buffer.Handle
        )

   

    let setActiveTexture (index : int) =
        Instruction.ActiveTexture ((int OpenGl.Enums.TextureUnit.Texture0) + index)

    let bindSampler (index : int) (sampler : IResource<Sampler>) =
        if ExecutionContext.samplersSupported then
            sampler.Handle |> Mod.map (fun r -> [Instruction.BindSampler index r.Handle])
        else
            let s = sampler.Handle.GetValue().Description
            let target = int OpenGl.Enums.TextureTarget.Texture2D
            let unit = int OpenGl.Enums.TextureUnit.Texture0 + index 
            Mod.constant [
                Instruction.TexParameteri target (int TextureParameterName.TextureWrapS) (SamplerStateHelpers.wrapMode s.AddressU)
                Instruction.TexParameteri target (int TextureParameterName.TextureWrapT) (SamplerStateHelpers.wrapMode s.AddressV)
                Instruction.TexParameteri target (int TextureParameterName.TextureWrapR) (SamplerStateHelpers.wrapMode s.AddressW)
                Instruction.TexParameteri target (int TextureParameterName.TextureMinFilter) (SamplerStateHelpers.minFilter s.Filter.Min s.Filter.Mip)
                Instruction.TexParameteri target (int TextureParameterName.TextureMagFilter) (SamplerStateHelpers.magFilter s.Filter.Mag)
                Instruction.TexParameterf target (int TextureParameterName.TextureLodBias) (s.MipLodBias)
                Instruction.TexParameterf target (int TextureParameterName.TextureMinLod) (s.MinLod)
                Instruction.TexParameterf target (int TextureParameterName.TextureMaxLod) (s.MaxLod)
            ]

    let bindTexture (tex : IResource<Texture>) =
        tex.Handle |> Mod.map(fun r -> 
            let target = Translations.toGLTarget r.Dimension r.IsArray r.Multisamples
            [ Instruction.BindTexture target r.Handle ]
        )

    let bindVertexArray (vao : IResource<VertexArrayObject>) =
        if ExecutionContext.vertexArrayObjectsSupported then
            vao.Handle |> Mod.map (fun r -> 
                fun (ctx : ContextHandle) -> 
                    [Instruction.BindVertexArray(r.Handle)]
            )
        else
            vao.Handle |> Mod.map (fun r -> 
                fun (ctx : ContextHandle) ->
                    [
                        for (i,b) in r.Bindings do
                            yield Instruction.BindBuffer (int OpenTK.Graphics.OpenGL4.BufferTarget.ArrayBuffer) b.Buffer.Handle
                            yield Instruction.EnableVertexAttribArray i
                            if b.BaseType = typeof<C4b> then
                                yield Instruction.VertexAttribPointer i 0x80E1 (int b.VertexAttributeType) b.Normalized b.Stride (nativeint b.Offset)
                            else
                                yield Instruction.VertexAttribPointer i b.Dimension (int b.VertexAttributeType) b.Normalized b.Stride (nativeint b.Offset)

          
                        match r.Index with
                            | Some i -> yield Instruction.BindBuffer (int OpenTK.Graphics.OpenGL4.BufferTarget.ElementArrayBuffer) i.Handle
                            | None -> yield Instruction.BindBuffer (int OpenTK.Graphics.OpenGL4.BufferTarget.ElementArrayBuffer) 0
                    ]
            )
    
    let bindVertexAttribValue (index : int) (value : IMod<Option<V4f>>) =
        value |> Mod.map (fun v ->
            match v with
                | Some v -> [Instruction.VertexAttrib4f index v.X v.Y v.Z v.W]
                | _ -> []
        )

    let drawIndirect (program : Program) (indexArray : IMod<System.Array>) (buffer : IResource<IndirectBuffer>) (mode : IMod<IndexedGeometryMode>) (isActive : IMod<bool>) =
        let hasTess = program.Shaders |> List.exists (fun s -> s.Stage = ShaderStage.TessControl)

        let indexType = 
            if indexArray <> null then
                indexArray |> Mod.map (fun ia -> (ia <> null, if ia <> null then ia.GetType().GetElementType() else typeof<obj>))
            else
                Mod.constant (false, typeof<obj>) 

        let patchSize (mode : IndexedGeometryMode) =
            match mode with
                | IndexedGeometryMode.LineList -> 2
                | IndexedGeometryMode.PointList -> 1
                | IndexedGeometryMode.TriangleList -> 3
                | m -> failwithf "unsupported patch-mode: %A" m

        let instruction  =
            adaptive {
                let! buffer = buffer.Handle
                let! igMode = mode
                let! (indexed, indexType) = indexType
                let count = NativePtr.toNativeInt buffer.Count
                let mode =
                    if hasTess then int OpenGl.Enums.DrawMode.Patches
                    else 
                        let realMode = 
                            match program.SupportedModes with
                                | Some set ->
                                    if Set.contains igMode set then 
                                        igMode
                                    else failwithf "invalid mode for program: %A (should be in: %A)" igMode set
                                | None -> 
                                    igMode

                        Translations.toGLMode realMode
                

                if indexed then

                    let indexType =
                        if indexType = typeof<byte> then int OpenGl.Enums.IndexType.UnsignedByte
                        elif indexType = typeof<uint16> then int OpenGl.Enums.IndexType.UnsignedShort
                        elif indexType = typeof<uint32> then int OpenGl.Enums.IndexType.UnsignedInt
                        elif indexType = typeof<sbyte> then int OpenGl.Enums.IndexType.UnsignedByte
                        elif indexType = typeof<int16> then int OpenGl.Enums.IndexType.UnsignedShort
                        elif indexType = typeof<int32> then int OpenGl.Enums.IndexType.UnsignedInt
                        else failwithf "unsupported index type: %A"  indexType

                    return igMode, Instruction.MultiDrawElementsIndirectPtr mode indexType 0n count 0
                else
                    return igMode, Instruction.MultiDrawArraysIndirectPtr mode 0n count 0
            }


        instruction |> Mod.map (fun (mode,i) ->
            if hasTess then
                let size = patchSize mode
                [ Instruction.PatchParameter (int OpenTK.Graphics.OpenGL4.PatchParameterInt.PatchVertices) size; i]
            else
                [i]
        )

    let draw (program : Program) (indexArray : IMod<System.Array>) (call : IMod<list<DrawCallInfo>>) (mode : IMod<IndexedGeometryMode>) (isActive : IMod<bool>) =
        let hasTess = program.Shaders |> List.exists (fun s -> s.Stage = ShaderStage.TessControl)

        let indexType = 
            if indexArray <> null then
                indexArray |> Mod.map (fun ia -> (ia <> null, if ia <> null then ia.GetType().GetElementType() else typeof<obj>))
            else
                Mod.constant (false, typeof<obj>)

        let patchSize (mode : IndexedGeometryMode) =
            match mode with
                | IndexedGeometryMode.LineList -> 2
                | IndexedGeometryMode.PointList -> 1
                | IndexedGeometryMode.TriangleList -> 3
                | m -> failwithf "unsupported patch-mode: %A" m

        let instruction  =
            adaptive {
                let! igMode = mode
                let! (indexed, indexType) = indexType
                let! (isActive) = isActive

                let! calls = call
                return 
                    igMode,
                    calls |> List.map (fun call ->
                        let faceVertexCount =
                            if isActive then call.FaceVertexCount
                            else 0

                        let mode =
                            if hasTess then int OpenGl.Enums.DrawMode.Patches
                            else 
                                let realMode = 
                                    match program.SupportedModes with
                                        | Some set ->
                                            if Set.contains igMode set then 
                                                igMode
                                            else failwithf "invalid mode for program: %A (should be in: %A)" igMode set
                                        | None -> 
                                            igMode

                                Translations.toGLMode realMode

                        if indexed then
                            let offset = nativeint (call.FirstIndex * indexType.GLSize)

                            let indexType =
                                if indexType = typeof<byte> then int OpenGl.Enums.IndexType.UnsignedByte
                                elif indexType = typeof<uint16> then int OpenGl.Enums.IndexType.UnsignedShort
                                elif indexType = typeof<uint32> then int OpenGl.Enums.IndexType.UnsignedInt
                                elif indexType = typeof<sbyte> then int OpenGl.Enums.IndexType.UnsignedByte
                                elif indexType = typeof<int16> then int OpenGl.Enums.IndexType.UnsignedShort
                                elif indexType = typeof<int32> then int OpenGl.Enums.IndexType.UnsignedInt
                                else failwithf "unsupported index type: %A"  indexType

                            match call.InstanceCount with
                                | 1 -> Instruction.DrawElements mode faceVertexCount indexType offset
                                | n -> Instruction.DrawElementsInstanced mode faceVertexCount indexType offset n
                        else
                            match call.InstanceCount with
                                | 1 -> Instruction.DrawArrays mode call.FirstIndex faceVertexCount
                                | n -> Instruction.DrawArraysInstanced mode call.FirstIndex faceVertexCount n
                    )
            }

        instruction |> Mod.map (fun (mode,i) ->
            if hasTess then
                let size = patchSize mode
                [ Instruction.PatchParameter (int OpenTK.Graphics.OpenGL4.PatchParameterInt.PatchVertices) size ] @ i
            else
                i
        )