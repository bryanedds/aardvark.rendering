namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base

open FShade
open FShade.GLSL

type TraceSceneShaderParams =
    {
        samplers        : Map<Symbol, GLSLSampler>
        storageImages   : Map<Symbol, GLSLImage>
        uniformBuffers  : Map<Symbol, GLSLUniformBuffer>
        storageBuffers  : Map<Symbol, GLSLStorageBuffer>
        scenes          : Map<Symbol, int * int>
    }

[<AutoOpen>]
module private TraceSceneShaderParamsHelpers =
    open FShade.Imperative

    let backend = Backends.glslVulkan

    let getType t =
        let t = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend t)
        let (final, _, size) = t |> LayoutStd140.layout
        final, size

    let getUniformBuffer set binding name typ =
        let typ, size = getType typ
        {
            ubSet = set
            ubBinding = binding
            ubName = name
            ubFields = [{ufName = name; ufType = typ; ufOffset = 0}]
            ubSize = size
        }

    let getImage set binding name format dim isArray isMS valueType typ =
        let imageType = {
            original = typ
            format = ImageFormat.ofFormatType format
            dimension = dim
            isArray = isArray
            isMS = isMS
            valueType = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend valueType)
        }

        {
            imageSet = set
            imageBinding = binding
            imageName = name
            imageType = imageType
        }

    let getSampler set binding name (info : SamplerInfo) =
        let samplerType = {
            original = info.samplerType
            dimension = info.dimension
            isShadow = info.isShadow
            isArray = info.isArray
            isMS = info.isMS
            valueType = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend info.valueType)
        }

        {
            samplerSet = set
            samplerBinding = binding
            samplerName = name
            samplerCount = 1
            samplerTextures = [info.textureName, info.samplerState]
            samplerType = samplerType
        }

    let getBuffer set binding name (info : BufferInfo) =
        {
            ssbSet = set
            ssbBinding = binding
            ssbName = name
            ssbType = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend info.elementType)
        }

    let getScene set binding _ _ =
        set, binding

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceSceneShaderParams =

    let create (bindings : TraceSceneBindings) =

        let buildMaybe (f : int -> int -> string -> 'a -> 'b option) (input : MapExt<string, DescriptorBinding<'a>>) =
            input
            |> Seq.choose (fun (KeyValue(name, binding)) ->
                let value = binding.value
                let set = fst binding.key.Value
                let binding = snd binding.key.Value

                f set binding name value
                    |> Option.map (fun param -> Symbol.Create name, param)
            )
            |> Map.ofSeq

        let build (f : int -> int -> string -> 'a -> 'b) (input : MapExt<string, DescriptorBinding<'a>>) =
            input |> buildMaybe (fun set binding name value ->
                Some (f set binding name value)
            )
            
        let samplers =
            bindings.samplers |> build getSampler

        let images =
            bindings.uniforms |> buildMaybe (fun set binding name ->
                function
                | ImageType (format, dim, isArray, isMS, valueType) as typ ->
                    Some (getImage set binding name format dim isArray isMS valueType typ)
                | _ ->
                    None
            )

        let uniforms =
            bindings.uniforms |> buildMaybe (fun set binding name ->
                function
                | ImageType _ ->
                    None
                | typ ->
                    Some (getUniformBuffer set binding name typ)
            )

        let buffers =
            bindings.buffers |> build getBuffer

        let scenes =
            bindings.scenes |> build getScene

        {
            samplers = samplers
            storageImages = images
            uniformBuffers = uniforms
            storageBuffers = buffers
            scenes = scenes
        }

    let setSamplerCount (name : Symbol) (count : int) (p : TraceSceneShaderParams) =
        match p.samplers |> Map.tryFind name with
        | Some sampler ->
            let samplers = p.samplers |> Map.add name { sampler with samplerCount = count }
            { p with samplers = samplers }
        | None ->
            p