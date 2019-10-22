namespace Aardvark.Rendering.Vulkan

open System
open Aardvark.Base
open Aardvark.Rendering.Vulkan.Raytracing
open Aardvark.Rendering.Vulkan.NVRayTracing
open System.Runtime.InteropServices
open Aardvark.Base.Rendering

type SamplerInfo =
    {
        samplerType     : Type
        textureName     : string
        samplerState    : FShade.SamplerState
        dimension       : FShade.SamplerDimension
        isArray         : bool
        isShadow        : bool
        isMS            : bool
        valueType       : Type
    }

type RayHitInfo =
    {
        neededUniforms : Map<string, Type>
        neededSamplers : Map<string, SamplerInfo>
        neededBuffers  : Map<string, int * Type>
        payloadInType  : Type
        payloadOutType : Type
    }

type RayHitInterface =
    {
        uniformBuffers      : Map<int * int, string * list<string * FShade.GLSL.GLSLType>>
        samplers            : Map<int * int, string * SamplerInfo>
        buffers             : Map<int * int, string * int * Type>
        payloadInLocation   : int
        payloadOutLocation  : int
    }

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

    let dummyShaderModule (device : Device) (stage : Aardvark.Base.ShaderStage) (code : byte[]) =
        let handle =
            native {
                let! pCode = code
                let! pInfo =
                    VkShaderModuleCreateInfo(
                        VkStructureType.ShaderModuleCreateInfo, 0n, 
                        VkShaderModuleCreateFlags.MinValue, 
                        uint64 code.Length, NativePtr.cast pCode
                    )

                let! pHandle = VkShaderModule.Null
                VkRaw.vkCreateShaderModule(device.Handle, pInfo, NativePtr.zero, pHandle)
                    |> check "failed to create shader module"
                return !!pHandle
            }

        new ShaderModule(device, handle, stage, Map.empty, code)

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

// The shader pool contains all the shaders included in the scene
// Duplicate shaders are removed at two levels: first duplicate code is only compiled to
// a module once, second duplicate hit groups are removed completely
type private ShaderPool<'source, 'compiled when 'source : comparison> = {

    // Dict mapping source to compiled modules
    // Duplicate sources are removed
    modules : Dict<ShaderStage, Dict<'source, 'compiled>>

    // List of shader groups
    // Identical hit groups are removed
    groups : ShaderGroup<'source> list

    // (Local) indices of the hit groups (e.g. the first hit group has index 0
    // even if it is not the first shader group over all. Used in the
    // instanceOffset field of the VkGeometryInstance struct
    hitGroupIndices  : Dict<ShaderGroup<'source>, int>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private ShaderPool =

    let create (device : Device) (scene : TraceScene) =

        let createCache stage shaders =
            shaders |> List.distinct
                    |> List.map (fun source -> source, dummyShaderModule device stage source)
                    |> Dict.ofList

        // Create cache to lookup shader modules based on stage and source
        let cache = Dict.empty

        cache.[ShaderStage.Raygen] <-
            [scene.raygenShader] |> createCache ShaderStage.Raygen

        cache.[ShaderStage.Miss] <-
            scene.missShaders |> createCache ShaderStage.Miss

        cache.[ShaderStage.Callable] <-
            scene.callableShaders |> createCache ShaderStage.Callable

        cache.[ShaderStage.AnyHit] <-
            scene.objects |> List.choose (fun o -> o.anyHitShader)
                          |> createCache ShaderStage.AnyHit
                          
        cache.[ShaderStage.ClosestHit] <-
            scene.objects |> List.choose (fun o -> o.closestHitShader)
                          |> createCache ShaderStage.ClosestHit
                          
        cache.[ShaderStage.Intersection] <-
            scene.objects |> List.choose (fun o -> o.intersectionShader)
                          |> createCache ShaderStage.Intersection        

        // Remove duplicate hit groups
        let hitGroups =
            scene.objects |> List.map ShaderGroup.ofTraceObject
                          |> List.distinct

        // Create shader groups
        let shaderGroups =
            [
                scene.raygenShader |> (Raygen >> List.singleton)
                scene.missShaders |> List.map Miss
                scene.callableShaders |> List.map Callable
                hitGroups
            ] 
            |> List.concat

        {
            groups          = shaderGroups
            modules         = cache
            hitGroupIndices = hitGroups |> List.indexed
                                        |> List.map (fun (i, x) -> x, i)
                                        |> Dict.ofList
        }

    let addToPipelineDescription (pool : ShaderPool<_,_>) (desc : TracePipelineDescription) =

        // Keep track of stage indices
        let indices = Dict.empty
        
        let lookup stage source =
            indices.[(stage, source)]

        // Add stages to description
        let mutable description = desc

        for KeyValue(stage, perStage) in pool.modules do
            for KeyValue(source, shader) in perStage do

                indices.[(stage, source)] <- uint32 description.stages.Length
                description <- TracePipelineDescription.addShaderStage shader description
                
        // Add groups referencing the indices
        for group in pool.groups do
            let group = ShaderGroup.mapWithStage lookup group
            description <- TracePipelineDescription.addShaderGroup group description

        description

    let delete (device : Device) (pool : ShaderPool<_,_>) =
        pool.modules.Values |> Seq.iter (fun dict ->
            dict.Values |> Seq.iter (fun g -> ShaderModule.delete g device)
        ) 

    let getHitGroupIndex (obj : TraceObject) (pool : ShaderPool<_,_>) =
        let g = ShaderGroup.ofTraceObject obj
        pool.hitGroupIndices.[g]


type TraceTask(device : Device, scene : TraceScene) =

    let shaderPool = ShaderPool.create device scene

    // Top-level acceleration structure
    let instances = scene.objects |> List.mapi (fun i o ->
        let mutable inst = VkGeometryInstance()
        inst.transform <- M34f.op_Explicit(o.transform.Forward)
        inst.instanceId <- uint24 <| uint32 i
        inst.mask <- 0xffuy
        inst.instanceOffset <- uint24 <| uint32 (shaderPool |> ShaderPool.getHitGroupIndex o)
        inst.flags <- uint8 VkGeometryInstanceFlagsNV.VkGeometryInstanceTriangleCullDisableBitNv
        inst.accelerationStructureHandle <- unbox o.geometry.Handle

        inst
    )

    let tlAS = AccelerationStructure.createTopLevel device {
        instanceBuffer = InstanceBuffer.create device instances
        instanceCount = uint32 instances.Length
    }

    // Descriptor sets
    let descriptorSetLayout = dummyDescriptorSetLayout device raygenInfo (assign raygenInfo)
    let descriptorSet = device.CreateDescriptorSet descriptorSetLayout

    let uniformBuffers = System.Collections.Generic.List<UniformBuffer>()
    
    do for binding in descriptorSetLayout.Bindings do
        match binding.Parameter with
            | ImageParameter img -> 
                let texture = scene.textures.[Symbol.Create img.imageName]
                let image = device.CreateImage(texture)
                let view = device.CreateOutputImageView(image)

                descriptorSet.Update([|StorageImage (img.imageBinding, view)|])

                device.Delete(view)
                device.Delete(image)

            | UniformBlockParameter layout ->
                let buffer = device.CreateUniformBuffer layout
                let value = scene.globals.[Symbol.Create layout.ubName]
                let typ = raygenInfo.neededUniforms.[layout.ubName]

                let writer = UniformWriters.getWriter 0 (getType typ) typ
                writer.WriteUnsafeValue(value, buffer.Storage.Pointer)
                device.Upload(buffer)

                descriptorSet.Update([|UniformBuffer (layout.ubBinding, buffer)|])
                uniformBuffers.Add(buffer)

            | AccelerationStructureParameter (_, _, binding) ->
                descriptorSet.Update([|Descriptor.AccelerationStructure (binding, tlAS)|])

            | _ -> failwith "blub"

    // Pipeline
    let pipelineLayout = dummyPipelineLayout device descriptorSetLayout.Handle

    let pipelineDescription =
        TracePipelineDescription.create pipelineLayout 0u
            |> ShaderPool.addToPipelineDescription shaderPool

    let pipeline = TracePipeline.create device pipelineDescription

    // Shader binding table
    let sbtEntries = TracePipeline.getShaderBindingTableEntries pipeline
    let sbt = ShaderBindingTable.create device pipeline.Handle sbtEntries

    interface ITraceTask with
        member x.Run (size : V3i) =
            device.GraphicsFamily.run {
                do! Command.BindPipeline pipeline.Handle
                do! Command.BindDescriptorSet(descriptorSet.Handle, pipelineLayout.Handle)
                do! Command.TraceRays(sbt.Handle, sbt.RaygenShaderBindingOffset,
                                      sbt.Handle, sbt.MissShaderBindingOffset, sbt.MissShaderBindingStride,
                                      sbt.Handle, sbt.HitShaderBindingOffset, sbt.HitShaderBindingStride,
                                      sbt.Handle, sbt.CallableShaderBindingOffset, sbt.CallableShaderBindingStride,
                                      uint32 size.X, uint32 size.Y, uint32 size.Z)
            }

    interface IDisposable with
        member x.Dispose() =
            for ub in uniformBuffers do
                device.Delete(ub)

            ShaderBindingTable.delete sbt
            AccelerationStructure.delete tlAS
            InstanceBuffer.delete tlAS.Description.instanceBuffer
            TracePipeline.delete pipeline
            PipelineLayout.delete pipelineLayout device
            DescriptorSetLayout.delete descriptorSetLayout device
            ShaderPool.delete device shaderPool
            

