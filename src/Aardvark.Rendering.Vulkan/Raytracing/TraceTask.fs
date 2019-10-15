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

    let raygenInfo : RayHitInfo =
        {
            neededUniforms = Map.ofList ["resultImage", typeof<VkImage>]
            neededSamplers = Map.empty
            neededBuffers = Map.empty
            payloadInType = typeof<unit>
            payloadOutType = typeof<unit>
        }

    let resultImage : GLSLImage =
        let imageType : GLSLImageType =
            {
                original = Unchecked.defaultof<_>
                format = None
                dimension = SamplerDimension.Sampler2d
                isArray = false
                isMS = false
                valueType = GLSLType.Vec(4, GLSLType.Int (false, 8))
            }

        {
            imageSet = 0
            imageBinding = 0
            imageName = "resultImage"
            imageType = imageType
        }

    let dummyShaderModule (device : Device) (code : byte[]) =
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

        new ShaderModule(device, handle, ShaderStage.Raygen, Map.empty, code)

    let dummyDescriptorSetLayout (device : Device) =
        let binding =
            DescriptorSetLayoutBinding.create VkDescriptorType.StorageImage VkShaderStageFlags.RaygenBitNv
                (ShaderUniformParameter.ImageParameter resultImage) device

        DescriptorSetLayout.create [|binding|] device

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

type TraceTask(device : Device, scene : TraceScene) =

    let raygenShaderModule = dummyShaderModule device scene.raygenShader

    // Descriptor sets
    let descriptorSetLayout = dummyDescriptorSetLayout device
    let descriptorSet = device.CreateDescriptorSet descriptorSetLayout
    
    let foo = 
        for binding in descriptorSetLayout.Bindings do
            match binding.Parameter with
                | ImageParameter img -> 
                    let texture = scene.textures.[Symbol.Create img.imageName]
                    let image = device.CreateImage(texture)
                    let view = device.CreateOutputImageView(image)

                    descriptorSet.Update([|StorageImage (img.imageBinding, view)|])

                    device.Delete(view)
                    device.Delete(image)

                | _ -> failwith "blub"

    // Pipeline
    let pipelineLayout = dummyPipelineLayout device descriptorSetLayout.Handle

    let pipelineDescription =
            PipelineDescription.create pipelineLayout 0u
                |> PipelineDescription.addShaderStage raygenShaderModule

    let pipeline = Pipeline.create device pipelineDescription

    // Top-level acceleration structure
    let instances = scene.objects |> List.mapi (fun i o ->
        let mutable inst = VkGeometryInstance()
        inst.transform <- M34f.op_Explicit o.transform
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

    // Shader binding table
    let sbtEntries = Pipeline.getShaderBindingTableEntries pipeline
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
            Pipeline.delete pipeline
            PipelineLayout.delete pipelineLayout device
            DescriptorSetLayout.delete descriptorSetLayout device
            ShaderModule.delete raygenShaderModule device
            

