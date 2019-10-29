namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

open System
open System.Runtime.CompilerServices

// TODO: Remove all this as soon as FShade handles RTX shaders
[<AutoOpen>]
module DummyHelpers =
    open FShade
    open FShade.GLSL
    open FShade.Imperative

    let backend = Backends.glslVulkan

    let getType t =
        let t = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend t)
        let (final, _, _) = t |> LayoutStd140.layout
        final
        
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

    let dummyDescriptorSetLayout (device : Device) (info : RayHitInfo) (iface : RayHitInterface) =
        let bindings =
            iface.uniformBuffers
                |> Map.map (fun (set, binding) (name, _) ->
                    match info.neededUniforms.[name] with
                        | ImageType (format, dim, isArray, isMS, valueType) as uniform -> 
                            let imageType : GLSLImageType =
                                {
                                    original = uniform
                                    format = ImageFormat.ofFormatType format
                                    dimension = dim
                                    isArray = isArray
                                    isMS = isMS
                                    valueType = GLSLType.ofCType backend.Config.reverseMatrixLogic (CType.ofType backend valueType)
                                }
                        
                            VkDescriptorType.StorageImage,
                            ShaderUniformParameter.ImageParameter
                                {
                                    imageSet = set
                                    imageBinding = binding
                                    imageName = name
                                    imageType = imageType
                                }
                        | t when t = typeof<VkAccelerationStructureNV> ->
                            VkDescriptorType.AccelerationStructureNv,
                            ShaderUniformParameter.AccelerationStructureParameter (name, set, binding)

                        | t when t.IsValueType ->
                            VkDescriptorType.UniformBuffer,
                            ShaderUniformParameter.UniformBlockParameter (getUniformBuffer set binding name t)

                        | _ ->
                            failwith "no"
 
                ) |> Map.values
                  |> Seq.map (fun (t, p) ->
                        DescriptorSetLayoutBinding.create t VkShaderStageFlags.RaygenBitNv p device
                  ) |> Seq.toArray

        DescriptorSetLayout.create bindings device

    let dummyDescriporSet (device : Device) (layout : DescriptorSetLayout) =
        device.CreateDescriptorSet layout
        
    let dummyPipelineLayout (device : Device) (layout : VkDescriptorSetLayout) =
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
        device      : Device
        original    : TraceScene
        resources   : IResourceLocation list

        pipeline            : TracePipeline
        descriptorSet       : IResourceLocation<DescriptorSet>
        descriptorSetLayout : DescriptorSetLayout
        shaderPool          : ShaderPool<byte[], ShaderModule>
        shaderBindingTable  : ShaderBindingTable
    }

    member x.Dispose() =
        for r in x.resources do r.Release()

        ShaderBindingTable.delete x.shaderBindingTable
        PipelineLayout.delete x.pipeline.Description.layout x.device
        TracePipeline.delete x.pipeline
        DescriptorSetLayout.delete x.descriptorSetLayout x.device
        ShaderPool.delete x.device x.shaderPool

    member x.Update(caller : AdaptiveToken) =
        for r in x.resources do r.Update(caller) |> ignore

    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<AbstractClass; Sealed; Extension>]
type DevicePreparedRenderObjectExtensions private() =

    static let prepareTraceScene (this : ResourceManager) (raygenInfo : RayHitInfo)
                                    (raygenInterface : RayHitInterface) (scene : TraceScene) =

        let resources = System.Collections.Generic.List<IResourceLocation>()

        let shaderPool = ShaderPool.create this.Device scene

        // Top-level acceleration structure
        // TODO: Optimize... hard
        let instances =
            scene.objects |> List.mapi (fun i o ->
                adaptive {
                    let! trafo = o.transform
                    return VkGeometryInstance(
                        trafo, i, 0xffuy, shaderPool |> ShaderPool.getHitGroupIndex o,
                        VkGeometryInstanceFlagsNV.VkGeometryInstanceTriangleCullDisableBitNv,
                        unbox o.geometry.Handle
                    )
                }
            ) |> Mod.mapN List.ofSeq

        let instanceBuffer = this.CreateInstanceBuffer(instances)
        let tlAS = this.CreateAccelerationStructure(instanceBuffer)

        // Descriptor sets
        let descriptorSetLayout = dummyDescriptorSetLayout this.Device raygenInfo raygenInterface
        let pipelineLayout = dummyPipelineLayout this.Device descriptorSetLayout.Handle

        let descriptorSet =
            let descriptors =
                descriptorSetLayout.Bindings |> Array.map (fun b ->
                    match b.Parameter with
                        | ImageParameter img ->
                            let texture = scene.textures.[Symbol.Create img.imageName]
                            let image = this.CreateImage(texture) 
                            let view = this.CreateImageView(img.imageType, image)

                            Resources.AdaptiveStorageImage(img.imageBinding, view)

                        | UniformBlockParameter layout ->
                            let buffer = this.CreateUniformBuffer(layout, scene.globals)
                            resources.Add(buffer)

                            Resources.AdaptiveUniformBuffer(layout.ubBinding, buffer)

                        | AccelerationStructureParameter (_, _, binding) ->
                            Resources.AdaptiveAccelerationStructure(binding, tlAS)

                        | _ -> failwith "Not implemented"
                )

            this.CreateDescriptorSet(descriptorSetLayout, Array.toList descriptors)

        let descriptorSetBindings = this.CreateDescriptorSetBinding(pipelineLayout, [descriptorSet])
        resources.Add(descriptorSetBindings)

        // Pipeline
        let pipelineDescription =
            TracePipelineDescription.create pipelineLayout 0u
                |> ShaderPool.addToPipelineDescription shaderPool

        let pipeline = TracePipeline.create this.Device pipelineDescription

        // Shader binding table
        let sbtEntries = TracePipeline.getShaderBindingTableEntries pipeline
        let shaderBindingTable = ShaderBindingTable.create this.Device pipeline.Handle sbtEntries

        {
            device      = this.Device
            original    = scene
            resources   = resources |> CSharpList.toList

            pipeline            = pipeline
            descriptorSet       = descriptorSet
            descriptorSetLayout = descriptorSetLayout
            shaderPool          = shaderPool
            shaderBindingTable  = shaderBindingTable
        }

    [<Extension>]
    static member PrepareTraceScene(this : ResourceManager, raygenInfo : RayHitInfo,
                                    raygenInterface : RayHitInterface, scene : TraceScene) =
        prepareTraceScene this raygenInfo raygenInterface scene
