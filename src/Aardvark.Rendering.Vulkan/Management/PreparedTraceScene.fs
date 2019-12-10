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
    open System.Collections.Generic

    let fixedBindings = LookupTable.lookupTable [
         "resultImage", 0
         "camera", 1
         "raytracingSettings", 2
         "scene", 3
         "color", 4
         "diffuseTexture", 5
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

    let createDescriptorSetLayout (device : Device) (shaderParams : TraceSceneShaderParams) =

        let map f x =
            x |> Seq.map (fun (KeyValue(name, value)) -> f name value)

        let samplers =
            shaderParams.samplers |> map (fun _ sampler ->
                let partiallyBound =
                    if sampler.samplerCount > 1 then
                        VkDescriptorBindingFlagsEXT.PartiallyBoundBit
                    else
                        VkDescriptorBindingFlagsEXT.None

                VkDescriptorType.CombinedImageSampler,
                partiallyBound,
                ShaderUniformParameter.SamplerParameter sampler
            )

        let storageImages =
            shaderParams.storageImages |> map (fun _ image ->
                VkDescriptorType.StorageImage,
                VkDescriptorBindingFlagsEXT.None,
                ShaderUniformParameter.ImageParameter image
            )

        let uniformBuffers =
            shaderParams.uniformBuffers |> map (fun _ buffer ->
                VkDescriptorType.UniformBuffer,
                VkDescriptorBindingFlagsEXT.None,
                ShaderUniformParameter.UniformBlockParameter buffer
            )

        let storageBuffers =
            shaderParams.storageBuffers |> map (fun _ buffer ->
                VkDescriptorType.StorageBuffer,
                VkDescriptorBindingFlagsEXT.None,
                ShaderUniformParameter.StorageBufferParameter buffer
            )

        let scenes =
            shaderParams.scenes |> map (fun name (set, binding) ->
                VkDescriptorType.AccelerationStructureNv,
                VkDescriptorBindingFlagsEXT.None,
                ShaderUniformParameter.AccelerationStructureParameter(string name, set, binding)
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
            seq {samplers; storageImages; uniformBuffers; storageBuffers; scenes}
                |> Seq.concat
                |> Seq.map (fun (typ, flags, param) ->
                      DescriptorSetLayoutBinding.create typ stageFlags param flags device
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

        let shaderParams = TraceSceneShaderParams.create sceneBindings

        // Trace resource manager
        let manager = new TraceResourceManager(this, scene, shaderParams, indexPool)
        manager.Add(indexPool)
        manager.Add(shaderPool)

        // Descriptor sets
        let descriptorSetLayout = createDescriptorSetLayout this.Device manager.ShaderParams
        manager.Add(fun _ -> DescriptorSetLayout.delete descriptorSetLayout this.Device)

        let pipelineLayout = createPipelineLayout this.Device descriptorSetLayout.Handle
        manager.Add(fun _ -> PipelineLayout.delete pipelineLayout this.Device)

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
                        let slots = manager.ImageSampler sampler.samplerName
                        Resources.AdaptiveCombinedImageSampler(sampler.samplerBinding, slots)
                )

            this.CreateDescriptorSet(descriptorSetLayout, descriptors)

        manager.Add(descriptorSet)

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
