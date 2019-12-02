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
         "color", 4
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

        new PipelineLayout(device, handle, Array.empty, Unchecked.defaultof<_>, 0, Set.empty)

type PreparedTraceScene =
    {
        device              : Device
        original            : TraceScene
        manager             : TraceResourceManager

        pipeline            : IResourceLocation<TracePipeline>
        pipelineLayout      : PipelineLayout
        descriptorSet       : IResourceLocation<DescriptorSet>
        descriptorSetLayout : DescriptorSetLayout
        shaderBindingTable  : IResourceLocation<ShaderBindingTable>
    }

    member x.Dispose() =
        x.manager.Dispose()

    member x.Update(caller : AdaptiveToken) =
        x.manager.Update caller

    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<AbstractClass; Sealed; Extension>]
type DevicePreparedRenderObjectExtensions private() =

    static let prepareTraceScene (this : ResourceManager) (scene : TraceScene) =

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

        // Trace resource manager
        let manager = new TraceResourceManager(this, scene, descriptorSetLayout.Bindings, indexPool)
        manager.Add(fun _ -> PipelineLayout.delete pipelineLayout this.Device)
        manager.Add(fun _ -> DescriptorSetLayout.delete descriptorSetLayout this.Device)
        manager.Add(indexPool)
        manager.Add(shaderPool)

        let descriptorSet =
            let descriptors =
                descriptorSetLayout.Bindings |> Array.map (fun b ->
                    match b.Parameter with
                    | ImageParameter img ->
                        Resources.AdaptiveStorageImage(img.imageBinding, manager.Image img.imageName)

                    | UniformBlockParameter layout ->
                        // Uniform buffer handles never actually change, thus the descriptor set
                        // does not create an dependency. For us to see uniform buffer updates, we need
                        // to manually call update
                        let buffer = manager.UniformBuffer layout.ubName
                        manager.Add(buffer)

                        Resources.AdaptiveUniformBuffer(layout.ubBinding, buffer)

                    | AccelerationStructureParameter (_, _, binding) ->
                        Resources.AdaptiveAccelerationStructure(binding, tlAS)

                    | StorageBufferParameter layout ->
                        Resources.AdaptiveStorageBuffer(layout.ssbBinding, manager.StorageBuffer layout.ssbName)

                    | SamplerParameter sampler ->
                        failwith "Not implemented"
                )

            this.CreateDescriptorSet(descriptorSetLayout, descriptors)

        let descriptorSetBindings = this.CreateDescriptorSetBinding(pipelineLayout, [|descriptorSet|])
        manager.Add(descriptorSetBindings)

        // Pipeline
        let pipeline = this.CreateTracePipeline(pipelineLayout, 0u, shaderPool)

        // Shader binding table
        let shaderBindingTable = this.CreateShaderBindingTable(pipeline)
        manager.Add(shaderBindingTable)

        {
            device              = this.Device
            original            = scene
            manager             = manager

            pipeline            = pipeline
            pipelineLayout      = pipelineLayout
            descriptorSet       = descriptorSet
            descriptorSetLayout = descriptorSetLayout
            shaderBindingTable  = shaderBindingTable
        }

    [<Extension>]
    static member PrepareTraceScene(this : ResourceManager, scene : TraceScene) =
        prepareTraceScene this scene
