namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

type TriangleData =
    | Indexed of vertexBuffer : Buffer * vertexFormat : VkFormat * indexCount : uint32 * indexBuffer : Buffer * indexType : VkIndexType
    | Unindexed of vertexCount : uint32 * vertexBuffer : Buffer * vertexFormat : VkFormat

type Geometry =
    | Triangles of TriangleData
    | AABBs of count : uint32 * buffer : Buffer

type BottomLevelDescription = {
    geometries : Geometry list
}

type TopLevelDescription = {
    instanceCount : uint32
    instanceBuffer : InstanceBuffer
}

type AccelerationStructureDescription =
    | BottomLevel of BottomLevelDescription
    | TopLevel of TopLevelDescription

type AccelerationStructure =
    class
        inherit Resource<VkAccelerationStructureNV>
        val mutable Description : AccelerationStructureDescription
        val mutable Memory : DevicePtr
        val mutable ScratchBuffer : Buffer option

        new(device : Device, handle : VkAccelerationStructureNV, description : AccelerationStructureDescription) = 
            { inherit Resource<_>(device, handle)
              Description = description
              Memory = DevicePtr.Null
              ScratchBuffer = None }
    end

[<AutoOpen>]
module private AccelerationStructureHelpers =
    open Aardvark.Base.NiceUtilities

    let getStride =
        LookupTable.lookupTable [
            VkFormat.R32g32b32Sfloat, 12;
            VkFormat.R32g32Sfloat, 8;
            VkFormat.R16g16b16Sfloat, 6;
            VkFormat.R16g16Sfloat, 4;
            VkFormat.R16g16Snorm, 4;
            VkFormat.R16g16b16Snorm, 6;
        ] >> uint64

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
        empty |> addIndexedTriangles vertexBuffer vertexFormat indexCount indexBuffer indexType


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
        s

    let createBottomLevel (device : Device) (desc : BottomLevelDescription) =

        let createTriangles (data : TriangleData) =
            let f = function
                | Indexed(vertexBuffer, vertexFormat, indexCount, indexBuffer, indexType) ->
                    VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                        vertexBuffer.Handle, 0UL, 0u, getStride vertexFormat, vertexFormat,
                        indexBuffer.Handle, 0UL, indexCount, indexType, 
                        VkBuffer.Null, 0UL)
                | Unindexed(vertexCount, vertexBuffer, vertexFormat) ->
                    VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                        vertexBuffer.Handle, 0UL, vertexCount, getStride vertexFormat, vertexFormat,
                        VkBuffer.Null, 0UL, 0u, VkIndexType.NoneNv, 
                        VkBuffer.Null, 0UL)

            VkGeometryNV(
                VkStructureType.GeometryNv, 0n, 
                VkGeometryTypeNV.VkGeometryTypeTrianglesNv,
                VkGeometryDataNV(f data,
                    VkGeometryAABBNV(VkStructureType.GeometryAabbNv, 0n, VkBuffer.Null, 0u, 0u, 0UL)
                ),
                VkGeometryFlagsNV.VkGeometryOpaqueBitNv
            )
 
        let createAABB (count : uint32) (buffer : Buffer) =
            VkGeometryNV(
                VkStructureType.GeometryNv, 0n, 
                VkGeometryTypeNV.VkGeometryTypeAabbsNv,
                VkGeometryDataNV(
                    VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                        VkBuffer.Null, 0UL, 0u, 0UL, VkFormat.Undefined,
                        VkBuffer.Null, 0UL, 0u, VkIndexType.NoneNv, 
                        VkBuffer.Null, 0UL),
                    VkGeometryAABBNV(VkStructureType.GeometryAabbNv, 0n,
                        buffer.Handle, count, 24u, 0UL)
                ),
                VkGeometryFlagsNV.VkGeometryOpaqueBitNv
            )
        
        let geometries =
            desc.geometries |> List.map (fun g ->
                match g with
                    | Triangles data ->  createTriangles data
                    | AABBs(count, buffer) -> createAABB count buffer
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

            let s = new AccelerationStructure(device, !!pHandle, BottomLevel desc)
            return s |> build info VkBuffer.Null
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

            let s = new AccelerationStructure(device, !!pHandle, TopLevel desc)
            return s |> build info instanceBuffer
        }
        
    let create (device : Device) = function
        | TopLevel desc -> createTopLevel device desc
        | BottomLevel desc -> createBottomLevel device desc
    
    let delete (s : AccelerationStructure) =
        if s.Handle.IsValid then
            VkRaw.vkDestroyAccelerationStructureNV(s.Device.Handle, s.Handle, NativePtr.zero)
            s.Handle <- VkAccelerationStructureNV.Null
        freeMemory s |> ignore