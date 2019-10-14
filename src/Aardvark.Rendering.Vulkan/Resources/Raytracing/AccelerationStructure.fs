namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing
open Microsoft.FSharp.NativeInterop

type BottomLevelDescription = {
    geometries : TraceGeometry list
}

type TopLevelDescription = {
    instanceCount : uint32
    instanceBuffer : InstanceBuffer
}

type AccelerationStructureDescription =
    | BottomLevel of BottomLevelDescription
    | TopLevel of TopLevelDescription

[<AbstractClass>]
type AccelerationStructure =
    class
        inherit Resource<VkAccelerationStructureNV>
        val mutable Description : AccelerationStructureDescription
        val mutable Memory : DevicePtr
        val mutable ScratchBuffer : Buffer option
        val mutable OpaqueHandle : uint64

        new(device : Device, handle : VkAccelerationStructureNV, description : AccelerationStructureDescription) =
            let _handle =
                temporary (fun pHandle ->
                    VkRaw.vkGetAccelerationStructureHandleNV(device.Handle, handle,
                                                             uint64 sizeof<uint64>, NativePtr.toNativeInt pHandle)
                        |> check "failed to get handle of acceleration structure"
                    NativePtr.read pHandle
                )

            { inherit Resource<_>(device, handle)
              Description = description
              Memory = DevicePtr.Null
              ScratchBuffer = None 
              OpaqueHandle = _handle }   
    end

type BottomLevelAccelerationStructure =
    class
        inherit AccelerationStructure
        val mutable Description : BottomLevelDescription

         new(device : Device, handle : VkAccelerationStructureNV, description : BottomLevelDescription) = 
            { inherit AccelerationStructure(device, handle, BottomLevel description)
              Description = description }

        interface IAccelerationStructure with
            member x.Handle = x.OpaqueHandle :> obj
            member x.Geometries = x.Description.geometries
    end

type TopLevelAccelerationStructure =
    class
        inherit AccelerationStructure
        val mutable Description : TopLevelDescription

         new(device : Device, handle : VkAccelerationStructureNV, description : TopLevelDescription) = 
            { inherit AccelerationStructure(device, handle, TopLevel description)
              Description = description }
    end

[<AutoOpen>]
module private AccelerationStructureHelpers =
    open System.Runtime.InteropServices

    let getBufferHandle (buffer : MyBuffer) =
        match buffer.buffer with
            | :? Buffer as b -> b.Handle
            | _ -> VkBuffer.Null

    let getFormat =
        LookupTable.lookupTable [
            typeof<V3f>, VkFormat.R32g32b32Sfloat
            typeof<V2f>, VkFormat.R32g32Sfloat
        ]

    let getIndexType =
        LookupTable.lookupTable [
            typeof<uint16>, VkIndexType.Uint16
            typeof<uint32>, VkIndexType.Uint32
        ]        

    let getStride (t : System.Type) =
        uint64 <| Marshal.SizeOf(t)

    (*
            VkFormat.R16g16b16Sfloat, 6;
            VkFormat.R16g16Sfloat, 4;
            VkFormat.R16g16Snorm, 4;
            VkFormat.R16g16b16Snorm, 6;
     *)

(*[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BottomLevelDescription =

    let empty =
        { geometries = [] }

    let addGeometry geometry info =
        { info with geometries = geometry::info.geometries}

    let addIndexedTriangles vertexBuffer vertexFormat indexCount indexBuffer indexType info =
        info |> addGeometry (
            Indexed(vertexBuffer, vertexFormat, indexCount, indexBuffer, indexType)
                |> Triangles
        )
    
    let indexedTriangles vertexBuffer vertexFormat indexCount indexBuffer indexType =
        empty |> addIndexedTriangles vertexBuffer vertexFormat indexCount indexBuffer indexType*)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TopLevelDescription =

    let create count buffer =
        { instanceCount = count; instanceBuffer = buffer }
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AccelerationStructure =

    let private getMemoryRequirements (s : AccelerationStructure) =
        let get memoryType =
            native {
                let! pInfo = 
                    VkAccelerationStructureMemoryRequirementsInfoNV(
                        VkStructureType.AccelerationStructureMemoryRequirementsInfoNv, 0n,
                        memoryType, s.Handle
                    )

                let! pReqs = VkMemoryRequirements2()
                VkRaw.vkGetAccelerationStructureMemoryRequirementsNV(s.Device.Handle, pInfo, pReqs)

                return pReqs.Value.memoryRequirements
            }

        get VkAccelerationStructureMemoryRequirementsTypeNV.VkAccelerationStructureMemoryRequirementsTypeObjectNv,
        get VkAccelerationStructureMemoryRequirementsTypeNV.VkAccelerationStructureMemoryRequirementsTypeBuildScratchNv

    let private allocateResultMemory (requirements : VkMemoryRequirements) (s : AccelerationStructure) =
        native {
            s.Memory <- s.Device.Alloc(requirements, true)
                
            let! pInfo =
                VkBindAccelerationStructureMemoryInfoNV(
                    VkStructureType.BindAccelerationStructureMemoryInfoNv, 0n,
                    s.Handle, s.Memory.Memory.Handle, uint64 s.Memory.Offset, 0u, NativePtr.zero
                )

            VkRaw.vkBindAccelerationStructureMemoryNV(s.Device.Handle, 1u, pInfo)
                |> check "failed to bind acceleration structure memory"
        }
        s

    let private allocateScratchBuffer (requirements : VkMemoryRequirements) (s : AccelerationStructure) =
        let memory = s.Device.GetMemory(requirements.memoryTypeBits, true)
        s.ScratchBuffer <- Some <| Buffer.create VkBufferUsageFlags.RayTracingBitNv (int64 requirements.size) memory
        s

    let private freeMemory (s : AccelerationStructure) =
        s.Memory.Dispose()
        s.Memory <- DevicePtr.Null

        s.ScratchBuffer |> Option.iter (fun b -> Buffer.delete b s.Device)
        s.ScratchBuffer <- None

        s

    let private allocateMemory (s : AccelerationStructure) =
        let requirements = getMemoryRequirements s

        s |> freeMemory
          |> allocateResultMemory (fst requirements)
          |> allocateScratchBuffer (snd requirements)
          |> ignore

    let private build (info : VkAccelerationStructureInfoNV) (instanceBuffer : VkBuffer) (s : AccelerationStructure) =
        let build = { new Command() with
            member x.Compatible = QueueFlags.Graphics
            member x.Enqueue cmd =
                cmd.AppendCommand()

                native {
                    let! pInfo = info
                    VkRaw.vkCmdBuildAccelerationStructureNV(
                        cmd.Handle, pInfo, instanceBuffer, 0UL,
                        0u, s.Handle,
                        VkAccelerationStructureNV.Null,
                        s.ScratchBuffer.Value.Handle, 0UL
                    )
                }
                    
                Disposable.Empty
        }

        let barrier = { new Command() with
            member x.Compatible = QueueFlags.Graphics
            member x.Enqueue cmd =
                cmd.AppendCommand()

                native {
                    let! pBarrier =
                        VkMemoryBarrier(
                            VkStructureType.MemoryBarrier, 0n,
                            VkAccessFlags.AccelerationStructureReadBitNv ||| VkAccessFlags.AccelerationStructureWriteBitNv,
                            VkAccessFlags.AccelerationStructureReadBitNv ||| VkAccessFlags.AccelerationStructureWriteBitNv
                        )
                            
                    VkRaw.vkCmdPipelineBarrier(
                        cmd.Handle,
                        VkPipelineStageFlags.AccelerationStructureBuildBitNv,
                        VkPipelineStageFlags.AccelerationStructureBuildBitNv ||| VkPipelineStageFlags.RayTracingShaderBitNv,
                        VkDependencyFlags.None, 1u, pBarrier, 0u, NativePtr.zero, 0u, NativePtr.zero
                    )
                }
                    
                Disposable.Empty
        }

        allocateMemory s
        s.Device.GraphicsFamily.run {
            do! build
            do! barrier
        }

    let createBottomLevel (device : Device) (desc : BottomLevelDescription) =

        let createTriangles (vb : MyBuffer) (ib : option<MyBuffer>) =
            let triangles =
                match ib with
                    | None ->
                        VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                            getBufferHandle vb, uint64 vb.offset, uint32 vb.count, getStride vb.format, getFormat vb.format,
                            VkBuffer.Null, 0UL, 0u, VkIndexType.NoneNv, VkBuffer.Null, 0UL)
                    | Some ib ->
                        VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                            getBufferHandle vb, uint64 vb.offset, uint32 vb.count, getStride vb.format, getFormat vb.format,
                            getBufferHandle ib, uint64 ib.offset, uint32 ib.count, getIndexType ib.format,
                            VkBuffer.Null, 0UL)

            VkGeometryNV(
                VkStructureType.GeometryNv, 0n, 
                VkGeometryTypeNV.VkGeometryTypeTrianglesNv,
                VkGeometryDataNV(triangles,
                    VkGeometryAABBNV(VkStructureType.GeometryAabbNv, 0n, VkBuffer.Null, 0u, 0u, 0UL)
                ),
                VkGeometryFlagsNV.VkGeometryOpaqueBitNv
            )
 
        let createAABB (buffer : MyBuffer) =
            VkGeometryNV(
                VkStructureType.GeometryNv, 0n, 
                VkGeometryTypeNV.VkGeometryTypeAabbsNv,
                VkGeometryDataNV(
                    VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                        VkBuffer.Null, 0UL, 0u, 0UL, VkFormat.Undefined,
                        VkBuffer.Null, 0UL, 0u, VkIndexType.NoneNv, 
                        VkBuffer.Null, 0UL),
                    VkGeometryAABBNV(VkStructureType.GeometryAabbNv, 0n,
                        getBufferHandle buffer, uint32 buffer.count, 24u, uint64 buffer.offset)
                ),
                VkGeometryFlagsNV.VkGeometryOpaqueBitNv
            )
        
        let geometries =
            desc.geometries |> List.map (fun g ->
                match g with
                    | Triangles(vb, ib) ->  createTriangles vb ib
                    | AABBs buffer -> createAABB buffer
            ) |> List.toArray

        native {
            let! pGeometries = geometries
        
            let info =
                VkAccelerationStructureInfoNV(
                    VkStructureType.AccelerationStructureInfoNv, 0n,
                    VkAccelerationStructureTypeNV.VkAccelerationStructureTypeBottomLevelNv,
                    VkBuildAccelerationStructureFlagsNV.VkBuildAccelerationStructurePreferFastTraceBitNv,
                    0u, uint32 geometries.Length, pGeometries
                )

            let! pCreateInfo =
                VkAccelerationStructureCreateInfoNV(
                    VkStructureType.AccelerationStructureCreateInfoNv, 0n, 0UL, info
                )

            let! pHandle = VkAccelerationStructureNV.Null
            VkRaw.vkCreateAccelerationStructureNV(device.Handle, pCreateInfo, NativePtr.zero, pHandle)
                |> check "could not create bottom-level acceleration structure"

            let s = new BottomLevelAccelerationStructure(device, !!pHandle, desc)
            s |> build info VkBuffer.Null
            return s
        }

    let createTopLevel (device : Device) (desc : TopLevelDescription) =
        let geometries = 
            VkGeometryNV(
                VkStructureType.GeometryNv, 0n,
                VkGeometryTypeNV.VkGeometryTypeTrianglesNv,
                VkGeometryDataNV(), VkGeometryFlagsNV.None
            )

        let instanceBuffer = desc.instanceBuffer.Handle

        native {
            let! pGeometries = geometries
        
            let info =
                VkAccelerationStructureInfoNV(
                    VkStructureType.AccelerationStructureInfoNv, 0n,
                    VkAccelerationStructureTypeNV.VkAccelerationStructureTypeTopLevelNv,
                    VkBuildAccelerationStructureFlagsNV.VkBuildAccelerationStructurePreferFastTraceBitNv,
                    desc.instanceCount, 0u, pGeometries
                )

            let! pCreateInfo =
                VkAccelerationStructureCreateInfoNV(
                    VkStructureType.AccelerationStructureCreateInfoNv, 0n, 0UL, info
                )

            let! pHandle = VkAccelerationStructureNV.Null
            VkRaw.vkCreateAccelerationStructureNV(device.Handle, pCreateInfo, NativePtr.zero, pHandle)
                |> check "could not create top-level acceleration structure"

            let s = new TopLevelAccelerationStructure(device, !!pHandle, desc)
            s |> build info instanceBuffer
            return s
        }
        
    let create (device : Device) = function
        | TopLevel desc -> createTopLevel device desc :> AccelerationStructure
        | BottomLevel desc -> createBottomLevel device desc :> AccelerationStructure
    
    let delete (s : AccelerationStructure) =
        if s.Handle.IsValid then
            VkRaw.vkDestroyAccelerationStructureNV(s.Device.Handle, s.Handle, NativePtr.zero)
            s.Handle <- VkAccelerationStructureNV.Null
        freeMemory s |> ignore