namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

open System
open System.Runtime.CompilerServices

open FShade

// TODO: Remove all this as soon as FShade handles RTX shaders
[<AutoOpen>]
module private DummyHelpers =
    open FShade.GLSL
    open FShade.Imperative
    open System.Collections.Generic

    let backend = Backends.glslVulkan

    let getType t =
        let t = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend t)
        let (final, _, size) = t |> LayoutStd140.layout
        final, size

    let fixedBindings = LookupTable.lookupTable [
         "resultImage", 0
         "camera", 1
         "raytracingSettings", 2
         "scene", 3
         "testBuffer", 4
    ]

    let assignBindings (bindings : TraceSceneBindings) =

        let mutable nextPayloadLocation = 0
        let payloads = Dictionary<Type, int>()

        let assignPayload payload =
            match payloads.TryGetValue payload with
            | true, i -> i
            | false, _ ->
                let i = nextPayloadLocation
                inc &nextPayloadLocation
                payloads.Add(payload, i)
                i

        let assignDescriptor name =
            0, fixedBindings name

        bindings |> TraceSceneBindings.assign assignDescriptor assignPayload
        (*let mapi f =
            (Map.toList >> List.mapi f >> Map.ofList)

        let uniformBuffers =
            data.uniforms
                |> mapi (fun i (name, uniform) -> ((0, fixedBindings name), (name, [name, uniform])))

        let samplers =
            let offset = uniformBuffers.Count
            data.samplers
                |> mapi (fun i (name, sampler) -> ((0, fixedBindings name), (name, sampler)))

        let buffers =
            let offset = uniformBuffers.Count + samplers.Count
            data.buffers
                |> mapi (fun i (name, buffer) -> ((0, fixedBindings name), (name, buffer)))

        {
            uniformBuffers = uniformBuffers
            samplers = samplers
            buffers = buffers
            payloadInLocation = 0
            payloadOutLocation = 0
        }*)
        
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

    let createDescriptorSetLayout (device : Device) (bindings : TraceSceneBindings) =

        let uniformParam set binding name typ =
            match typ with
            | ImageType (format, dim, isArray, isMS, valueType) ->
                VkDescriptorType.StorageImage,
                ShaderUniformParameter.ImageParameter (getImage set binding name format dim isArray isMS valueType typ)
            | _ ->
                VkDescriptorType.UniformBuffer,
                ShaderUniformParameter.UniformBlockParameter (getUniformBuffer set binding name typ)

        let scenes =
            bindings.scenes
            |> Seq.map (fun (KeyValue(name, binding)) ->
                let set = fst binding.key.Value
                let binding = snd binding.key.Value

                VkDescriptorType.AccelerationStructureNv,
                ShaderUniformParameter.AccelerationStructureParameter(name, set, binding)
            )

        let uniforms =
            bindings.uniforms
            |> Seq.map (fun (KeyValue(name, binding)) ->
                let typ = binding.value
                let set = fst binding.key.Value
                let binding = snd binding.key.Value

                uniformParam set binding name typ
            )

        let samplers =
            bindings.samplers
            |> Seq.map (fun (KeyValue(name, binding)) ->
                let info = binding.value
                let set = fst binding.key.Value
                let binding = snd binding.key.Value

                VkDescriptorType.CombinedImageSampler,
                ShaderUniformParameter.SamplerParameter (getSampler set binding name info)
            )

        let buffers =
            bindings.buffers
            |> Seq.map (fun (KeyValue(name, binding)) ->
                let info = binding.value
                let set = fst binding.key.Value
                let binding = snd binding.key.Value

                VkDescriptorType.StorageBuffer,
                ShaderUniformParameter.StorageBufferParameter (getBuffer set binding name info)
            )

        // TODO: Set stage flags properly
        let stageFlags =
            VkShaderStageFlags.RaygenBitNv |||
            VkShaderStageFlags.MissBitNv |||
            VkShaderStageFlags.CallableBitNv |||
            VkShaderStageFlags.AnyHitBitNv |||
            VkShaderStageFlags.ClosestHitBitNv |||
            VkShaderStageFlags.IntersectionBitNv

        let bindings =
            seq {scenes; uniforms; samplers; buffers}
                |> Seq.concat
                |> Seq.map (fun (typ, param) ->
                      DescriptorSetLayoutBinding.create typ stageFlags param device
                )
                |> Seq.sortBy (fun x -> x.Binding)
                |> Seq.toArray

        DescriptorSetLayout.create bindings device
        
    let createPipelineLayout (device : Device) (layout : VkDescriptorSetLayout) =
        let handle =
            native {
                let! pLayout = layout

                let! pInfo =
                    VkPipelineLayoutCreateInfo(
                        VkStructureType.PipelineLayoutCreateInfo, 0n,
                        VkPipelineLayoutCreateFlags.MinValue, 1u, pLayout, 0u, NativePtr.zero
                    )

                let! pHandle = VkPipelineLayout.Null
                VkRaw.vkCreatePipelineLayout(device.Handle, pInfo, NativePtr.zero, pHandle)
                    |> check "failed to create pipeline layout"

                return !!pHandle
            }

        let info =    
            {
                pInputs         = []
                pOutputs        = []
                pUniformBlocks  = []
                pStorageBlocks  = []
                pTextures       = []
                pImages         = []
                pEffectLayout   = None
            }

        new PipelineLayout(device, handle, Array.empty, info, 0, Set.empty)

type PreparedTraceScene =
    {
        device              : Device
        original            : TraceScene
        resources           : IResourceLocation list
        additionalResources : IDisposable list

        pipeline            : IResourceLocation<TracePipeline>
        pipelineLayout      : PipelineLayout
        descriptorSet       : IResourceLocation<DescriptorSet>
        descriptorSetLayout : DescriptorSetLayout
        shaderBindingTable  : IResourceLocation<ShaderBindingTable>
    }

    member x.Dispose() =
        for r in x.resources do r.Release()
        for r in x.additionalResources do r.Dispose()
        PipelineLayout.delete x.pipelineLayout x.device
        DescriptorSetLayout.delete x.descriptorSetLayout x.device

    member x.Update(caller : AdaptiveToken) =
        for r in x.resources do r.Update(caller) |> ignore

    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<AbstractClass; Sealed; Extension>]
type DevicePreparedRenderObjectExtensions private() =

    static let prepareTraceScene (this : ResourceManager) (scene : TraceScene) =

        let resources = System.Collections.Generic.List<IResourceLocation>()

        let indexPool = IndexPool.create scene
        let shaderPool = ShaderPool.create this.Device scene

        // Top-level acceleration structure
        let instanceBuffer = this.CreateInstanceBuffer(indexPool, shaderPool)
        let tlAS = this.CreateAccelerationStructure(instanceBuffer)

        // Compute the combined bindings of all shaders
        // in the scene and assign them
        let sceneBindings =
            TraceSceneBindings.compute [
                yield scene.RaygenShader
                yield! scene.MissShaders
                yield! scene.CallableShaders
                yield! scene.HitGroupShaders
            ]
            |> assignBindings

        // Descriptor sets
        let descriptorSetLayout = createDescriptorSetLayout this.Device sceneBindings
        let pipelineLayout = createPipelineLayout this.Device descriptorSetLayout.Handle

        let descriptorSet =
            let descriptors =
                descriptorSetLayout.Bindings |> Array.map (fun b ->
                    match b.Parameter with
                        | ImageParameter img ->
                            let texture = scene.Textures.[Symbol.Create img.imageName]
                            let image = this.CreateImage(texture) 
                            let view = this.CreateImageView(img.imageType, image)

                            Resources.AdaptiveStorageImage(img.imageBinding, view)

                        | UniformBlockParameter layout ->
                            let buffer = this.CreateUniformBuffer(layout, scene.Globals)
                            resources.Add(buffer)

                            Resources.AdaptiveUniformBuffer(layout.ubBinding, buffer)

                        | AccelerationStructureParameter (_, _, binding) ->
                            Resources.AdaptiveAccelerationStructure(binding, tlAS)

                        | StorageBufferParameter layout ->
                            let buffer = this.CreateStorageBuffer(layout, scene.Buffers)
                            resources.Add(buffer)

                            Resources.AdaptiveStorageBuffer(layout.ssbBinding, buffer)

                        | SamplerParameter sampler ->
                            failwith "Not implemented"
                )

            this.CreateDescriptorSet(descriptorSetLayout, descriptors)

        let descriptorSetBindings = this.CreateDescriptorSetBinding(pipelineLayout, [|descriptorSet|])
        resources.Add(descriptorSetBindings)

        // Pipeline
        let pipeline = this.CreateTracePipeline(pipelineLayout, 0u, shaderPool)

        // Shader binding table
        let shaderBindingTable = this.CreateShaderBindingTable(pipeline)
        resources.Add(shaderBindingTable)

        {
            device              = this.Device
            original            = scene
            resources           = resources |> CSharpList.toList
            additionalResources = [indexPool; shaderPool]

            pipeline            = pipeline
            pipelineLayout      = pipelineLayout
            descriptorSet       = descriptorSet
            descriptorSetLayout = descriptorSetLayout
            shaderBindingTable  = shaderBindingTable
        }

    [<Extension>]
    static member PrepareTraceScene(this : ResourceManager, scene : TraceScene) =
        prepareTraceScene this scene
