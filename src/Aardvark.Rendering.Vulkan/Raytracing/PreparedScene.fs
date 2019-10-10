namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Raytracing

type PreparedScene =
    {
        device                  : Device
        objects                 : list<PreparedObject>
        
        resources               : list<IResourceLocation>

        pipeline                : IResourceLocation<Pipeline>
        topLevelAS              : IResourceLocation<AccelerationStructure>
        
        shaderBindingTable      : IResourceLocation<ShaderBindingTable>
    }

    member x.Dispose() =
        for r in x.resources do r.Release()

    member x.Update(caller : AdaptiveToken, token : RenderToken) =
        for r in x.resources do r.Update(caller) |> ignore

    member x.IncrementReferenceCount() =
        for r in x.resources do r.Acquire()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

namespace Aardvark.Rendering.Vulkan.Raytracing.Extensions

open System.IO
open System.Runtime.CompilerServices
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Raytracing
open Aardvark.Rendering.Vulkan.NVRayTracing
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Reactive.Concurrency

[<AbstractClass; Sealed; Extension>]
type ResourceManagerPreparedSceneExtensions private() =

    static let dummyShaderModule (device : Device) (code : byte[]) =
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

    static let dummyDescriptorSetLayout (device : Device) =
        DescriptorSetLayout.create Array.empty device
        
    static let dummyPipelineLayout (device : Device) (layout : VkDescriptorSetLayout) =
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
        

    static let prepareScene (this : ResourceManager) (renderPass : RenderPass) (raygenShader : byte[]) (objects : list<PreparedObject>) =
        let resources = System.Collections.Generic.List<IResourceLocation>()
        for o in objects do
            resources.AddRange o.resources

        let instances = 
            objects |> List.mapi (fun i o ->
                Mod.custom (fun t ->
                    let trafo = o.original.Trafo.GetValue t
                    let blas = o.bottomLevelAS.Update t

                    let handle =
                        temporary (fun pHandle ->
                            VkRaw.vkGetAccelerationStructureHandleNV(this.Device.Handle, blas.handle.Handle,
                                                                 uint64 sizeof<uint64>, NativePtr.toNativeInt pHandle)
                                |> check "failed to get handle of acceleration structure"
                            NativePtr.read pHandle
                        )

                    let mutable inst = VkGeometryInstance()
                    inst.transform <- M34f.Identity
                    inst.instanceId <- uint24(uint32 i)
                    inst.mask <- 0xffuy
                    inst.instanceOffset <- uint24 0u
                    inst.flags <- uint8 VkGeometryInstanceFlagsNV.VkGeometryInstanceTriangleCullDisableBitNv
                    inst.accelerationStructureHandle <- handle

                    inst
                )
            )

        let instanceBuffer =
            instances |> Mod.mapN id
                      |> Mod.map Seq.toList
                      |> this.CreateInstanceBuffer

        resources.Add instanceBuffer

        let raygenShaderModule = dummyShaderModule this.Device raygenShader

        let descriptorSetLayout = dummyDescriptorSetLayout this.Device
        let pipelineLayout = dummyPipelineLayout this.Device descriptorSetLayout.Handle

        let pipelineDescription =
            PipelineDescription.create pipelineLayout 0u
                |> PipelineDescription.addShaderStage raygenShaderModule

        let pipeline = this.CreateRaytracingPipeline(Mod.constant pipelineDescription)
        resources.Add pipeline

        let accelerationStructureDesc =
            Mod.custom (fun t ->
                let buffer = instanceBuffer.Update t
                TopLevelDescription.create (uint32 <| List.length instances) buffer.handle
                    |> TopLevel
            )

        let tlas = this.CreateAccelerationStructure(accelerationStructureDesc)
        resources.Add tlas

        let sbtEntries =
            Mod.custom (fun t ->
                let pipeline = pipeline.Update t
                Pipeline.getShaderBindingTableEntries pipeline.handle
            )

        let sbt = this.CreateShaderBindingTable(pipeline, sbtEntries)
        resources.Add sbt
 
        {
            device = this.Device
            objects = objects
            resources = CSharpList.toList resources
            pipeline = pipeline
            topLevelAS = tlas
            shaderBindingTable = sbt
        }

    [<Extension>]
    static member PrepareRaytracingScene(this : ResourceManager, renderPass : RenderPass,
                                         raygenShader : byte[], objects : list<PreparedObject>) =
        prepareScene this renderPass raygenShader objects