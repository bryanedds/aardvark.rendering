namespace Aardvark.Rendering.Vulkan

open System
open Aardvark.Base
open Aardvark.Rendering.Vulkan.Raytracing
open Aardvark.Rendering.Vulkan.NVRayTracing
open FShade

type SamplerInfo =
    {
        samplerType     : Type
        textureName     : string
        samplerState    : SamplerState
        dimension       : SamplerDimension
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
        uniformBuffers      : Map<int * int, string * list<string * Type>>
        samplers            : Map<int * int, string * SamplerInfo>
        buffers             : Map<int * int, string * int * Type>
        payloadInLocation   : int
        payloadOutLocation  : int
    }

[<AutoOpen>]
module DummyHelpers =
    open FShade.GLSL
    open FShade.Formats
    open Microsoft.FSharp.Reflection
    open FShade.Imperative

    let backend = Backends.glslVulkan

    let raygenInfo : RayHitInfo =
        {
            neededUniforms = Map.ofList ["resultImage", typeof<IntImage2d<rgba8i>>]
            neededSamplers = Map.empty
            neededBuffers = Map.empty
            payloadInType = typeof<unit>
            payloadOutType = typeof<unit>
        }

    let assign (info : RayHitInfo) =

        let mapi f =
            (Map.toList >> List.mapi f >> Map.ofList)

        let getFields t =
            if FSharpType.IsRecord t then
                FSharpType.GetRecordFields t
                    |> Array.toList
                    |> List.map (fun p -> p.Name, p.Type)
            else
                []

        let uniformBuffers =
            info.neededUniforms 
                |> mapi (fun i (name, uniform) -> ((0, i), (name, getFields uniform)))

        let samplers =
            let offset = uniformBuffers.Count
            info.neededSamplers
                |> mapi (fun i (name, sampler) -> ((0, offset + i), (name, sampler)))

        let buffers =
            let offset = uniformBuffers.Count + samplers.Count
            info.neededBuffers
                |> mapi (fun i (name, (dim, buffer)) -> ((0, offset + i), (name, dim, buffer)))

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
                        | _ ->
                            failwith "no"
 
                ) |> Map.values
                  |> Seq.map (fun (t, p) ->
                        DescriptorSetLayoutBinding.create t VkShaderStageFlags.RaygenBitNv p device
                  ) |> Seq.toArray

        let bindings =
            Array.append bindings [|
                DescriptorSetLayoutBinding.create
                    VkDescriptorType.AccelerationStructureNv
                    VkShaderStageFlags.RaygenBitNv
                    (AccelerationStructureParameter ("scene", 0, 1))
                    device
            |]

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

[<AutoOpen>]
module private ShaderGroups =

    type ShaderGroup<'a> =
        | Raygen of 'a
        | Miss of 'a
        | Callable of 'a
        | HitGroup of anyHit : Option<'a> * closestHit : Option<'a> * intersection : Option<'a>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ShaderGroup =
        
        let compile (device : Device) (group : ShaderGroup<byte[]>) =
            match group with
                | Raygen x -> Raygen (dummyShaderModule device ShaderStage.Raygen x)
                | Miss x -> Miss (dummyShaderModule device ShaderStage.Miss x)
                | Callable x -> Callable (dummyShaderModule device ShaderStage.Callable x)
                | HitGroup (x, y, z) -> HitGroup (
                                            x |> Option.map (dummyShaderModule device ShaderStage.AnyHit),
                                            y |> Option.map (dummyShaderModule device ShaderStage.ClosestHit),
                                            z |> Option.map (dummyShaderModule device ShaderStage.Intersection)
                                        )

        let addToPipelineDescription (group : ShaderGroup<ShaderModule>) (desc : TracePipelineDescription) =
            match group with
                | Raygen x
                | Miss x
                | Callable x ->
                    desc |> TracePipelineDescription.addShaderStage x
                | HitGroup (anyHit, closestHit, intersection) ->
                    desc |> TracePipelineDescription.addHitShaderStage anyHit closestHit intersection

        let iter (f : 'a -> unit) = function
            | Raygen x
            | Miss x
            | Callable x ->
                f x
            | HitGroup (x, y, z) ->
                [x; y; z] |> List.iter (Option.iter f)

        let delete (device : Device) = 
            iter (fun g -> ShaderModule.delete g device)

type TraceTask(device : Device, scene : TraceScene) =

    let shaderGroups = 
        [
            scene.raygenShader |> (Raygen >> List.singleton)
            scene.missShaders |> List.map Miss
            scene.callableShaders |> List.map Callable
            scene.objects |> List.map (fun o -> HitGroup (o.anyHitShader, o.closestHitShader, o.intersectionShader))
        ] 
        |> List.concat
        |> List.distinct

    let shaderModules = shaderGroups |> List.map (ShaderGroup.compile device)

    // Top-level acceleration structure
    let instances = scene.objects |> List.mapi (fun i o ->
        let mutable inst = VkGeometryInstance()
        inst.transform <- M34f(o.transform.Forward.Transposed.Elements |> Seq.map float32 |> Seq.toArray)
        inst.instanceId <- uint24(uint32 i)
        inst.mask <- 0xffuy
        inst.instanceOffset <- uint24 0u
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
    
    do for binding in descriptorSetLayout.Bindings do
        match binding.Parameter with
            | ImageParameter img -> 
                let texture = scene.textures.[Symbol.Create img.imageName]
                let image = device.CreateImage(texture)
                let view = device.CreateOutputImageView(image)

                descriptorSet.Update([|StorageImage (img.imageBinding, view)|])

                device.Delete(view)
                device.Delete(image)

            | AccelerationStructureParameter (_, _, binding) ->
                descriptorSet.Update([|Descriptor.AccelerationStructure (binding, tlAS)|])

            | _ -> failwith "blub"

    // Pipeline
    let pipelineLayout = dummyPipelineLayout device descriptorSetLayout.Handle

    let pipelineDescription =
        shaderModules |> List.fold (fun desc shader ->
            desc |> ShaderGroup.addToPipelineDescription shader
        ) (TracePipelineDescription.create pipelineLayout 0u)

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
            ShaderBindingTable.delete sbt
            AccelerationStructure.delete tlAS
            InstanceBuffer.delete tlAS.Description.instanceBuffer
            TracePipeline.delete pipeline
            PipelineLayout.delete pipelineLayout device
            DescriptorSetLayout.delete descriptorSetLayout device
            shaderModules |> List.iter (ShaderGroup.delete device)
            

