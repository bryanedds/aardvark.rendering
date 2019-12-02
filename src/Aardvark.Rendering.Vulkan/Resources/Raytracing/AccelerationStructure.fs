namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing
open Microsoft.FSharp.NativeInterop

[<AbstractClass>]
type AccelerationStructure =
    class
        inherit Resource<VkAccelerationStructureNV>
        val mutable Description : AccelerationStructureDescription
        val mutable Memory : DevicePtr
        val mutable ScratchBuffer : Buffer option
        val mutable OpaqueHandle : uint64

        new(device : Device, handle : VkAccelerationStructureNV, description : AccelerationStructureDescription) =
            { inherit Resource<_>(device, handle)
              Description = description
              Memory = DevicePtr.Null
              ScratchBuffer = None 
              OpaqueHandle = 0UL }   
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

    let getBufferHandle (buffer : IBuffer) =
        match buffer with
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

    // The VkAccelerationStructureInfoNV is a bit finicky
    // since it contains a pointer to an array of VkGeometryNV
    type AccelerationStructureInfo =
        {
            pGeometries : nativeptr<VkGeometryNV>
            data : VkAccelerationStructureInfoNV
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private AccelerationStructureInfo =

    let create (pGeometries : nativeptr<VkGeometryNV>) (geometryCount : int) (instanceCount : int) =
        let kind =
            if geometryCount = 0 then
                VkAccelerationStructureTypeNV.VkAccelerationStructureTypeTopLevelNv
            else
                VkAccelerationStructureTypeNV.VkAccelerationStructureTypeBottomLevelNv

        let info =
            VkAccelerationStructureInfoNV(
                VkStructureType.AccelerationStructureInfoNv, 0n, kind,
                VkBuildAccelerationStructureFlagsNV.VkBuildAccelerationStructurePreferFastTraceBitNv |||
                VkBuildAccelerationStructureFlagsNV.VkBuildAccelerationStructureAllowUpdateBitNv,
                uint32 instanceCount, uint32 geometryCount, pGeometries
            )

        {
            pGeometries = pGeometries
            data = info
        }

    let allocBottomLevel (desc : BottomLevelDescription) =
        let createTriangles (vb : MyBuffer) (ib : option<MyBuffer>) =
            let triangles =
                match ib with
                    | None ->
                        VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                            getBufferHandle vb.buffer, uint64 vb.offset, uint32 vb.count, getStride vb.format, getFormat vb.format,
                            VkBuffer.Null, 0UL, 0u, VkIndexType.NoneNv, VkBuffer.Null, 0UL)
                    | Some ib ->
                        VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                            getBufferHandle vb.buffer, uint64 vb.offset, uint32 vb.count, getStride vb.format, getFormat vb.format,
                            getBufferHandle ib.buffer, uint64 ib.offset, uint32 ib.count, getIndexType ib.format,
                            VkBuffer.Null, 0UL)

            VkGeometryNV(
                VkStructureType.GeometryNv, 0n,
                VkGeometryTypeNV.VkGeometryTypeTrianglesNv,
                VkGeometryDataNV(triangles,
                    VkGeometryAABBNV(VkStructureType.GeometryAabbNv, 0n, VkBuffer.Null, 0u, 0u, 0UL)
                ),
                VkGeometryFlagsNV.VkGeometryOpaqueBitNv
            )

        let createAABB (buffer : IBuffer<'a>) =
            VkGeometryNV(
                VkStructureType.GeometryNv, 0n,
                VkGeometryTypeNV.VkGeometryTypeAabbsNv,
                VkGeometryDataNV(
                    VkGeometryTrianglesNV(VkStructureType.GeometryTrianglesNv, 0n,
                        VkBuffer.Null, 0UL, 0u, 0UL, VkFormat.Undefined,
                        VkBuffer.Null, 0UL, 0u, VkIndexType.NoneNv,
                        VkBuffer.Null, 0UL),
                    VkGeometryAABBNV(VkStructureType.GeometryAabbNv, 0n,
                        getBufferHandle buffer.Buffer, uint32 buffer.Count, uint32 sizeof<'a>, uint64 buffer.Offset)
                ),
                VkGeometryFlagsNV.VkGeometryOpaqueBitNv
            )

        let ptr = NativePtr.alloc<VkGeometryNV> desc.geometries.Length

        desc.geometries |> List.iteri (fun i g ->
            match g with
                | Triangles(vb, ib) -> ptr.[i] <- createTriangles vb ib
                | AABBs buffer -> ptr.[i] <- createAABB buffer
        )

        create ptr desc.geometries.Length 0

    let allocTopLevel (desc : TopLevelDescription) =
        create NativePtr.zero 0 desc.instanceCount

    let alloc = function
        | TopLevel desc -> allocTopLevel desc
        | BottomLevel desc -> allocBottomLevel desc

    let free (info : AccelerationStructureInfo) =
        NativePtr.free info.pGeometries
    
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

        let build, update =
            get VkAccelerationStructureMemoryRequirementsTypeNV.VkAccelerationStructureMemoryRequirementsTypeBuildScratchNv,
            get VkAccelerationStructureMemoryRequirementsTypeNV.VkAccelerationStructureMemoryRequirementsTypeUpdateScratchNv

        // Build and update should require the same type of memory
        // but may differ in size? So we take the larger one as scratch
        // buffer memory
        assert (build.alignment = update.alignment)
        assert (build.memoryTypeBits = update.memoryTypeBits)

        get VkAccelerationStructureMemoryRequirementsTypeNV.VkAccelerationStructureMemoryRequirementsTypeObjectNv,
        if build.size > update.size then build else update

    let private bindResultMemory (s : AccelerationStructure) =
        native {
            let! pInfo =
                VkBindAccelerationStructureMemoryInfoNV(
                    VkStructureType.BindAccelerationStructureMemoryInfoNv, 0n,
                    s.Handle, s.Memory.Memory.Handle, uint64 s.Memory.Offset, 0u, NativePtr.zero
                )

            VkRaw.vkBindAccelerationStructureMemoryNV(s.Device.Handle, 1u, pInfo)
                |> check "failed to bind acceleration structure memory"
        }

    let private allocateResultMemory (requirements : VkMemoryRequirements) (s : AccelerationStructure) =
        s.Memory <- s.Device.Alloc(requirements, true)
        bindResultMemory s
        s

    let private allocateScratchBuffer (requirements : VkMemoryRequirements) (s : AccelerationStructure) =
        let memory = s.Device.GetMemory(requirements.memoryTypeBits, true)
        s.ScratchBuffer <- Some <| Buffer.create VkBufferUsageFlags.RayTracingBitNv (int64 requirements.size) memory
        s

    let private retrieveHandle (s : AccelerationStructure) =
        s.OpaqueHandle <-
                temporary (fun pHandle ->
                    VkRaw.vkGetAccelerationStructureHandleNV(s.Device.Handle, s.Handle,
                                                             uint64 sizeof<uint64>, NativePtr.toNativeInt pHandle)
                        |> check "failed to get handle of acceleration structure"
                    NativePtr.read pHandle
                )
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
          |> retrieveHandle
          |> ignore

    // BUG: Doesn't work for TLAS with no instances
    // https://devtalk.nvidia.com/default/topic/1066165/vulkan/-rtx-building-tlas-with-zero-instances-doesn-t-update-result-memory/
    let private build (info : AccelerationStructureInfo) (updateOnly : bool) (s : AccelerationStructure) =
        let instanceBuffer =
            match s.Description with
                | TopLevel desc -> desc.instances
                | _ -> VkBuffer.Null

        // For updates we use the current handle as source and
        // destination
        let src =
            if updateOnly then
                s.Handle
            else
                VkAccelerationStructureNV.Null

        let build = { new Command() with
            member x.Compatible = QueueFlags.Graphics
            member x.Enqueue cmd =
                cmd.AppendCommand()

                native {
                    let! pInfo = info.data
                    VkRaw.vkCmdBuildAccelerationStructureNV(
                        cmd.Handle, pInfo, instanceBuffer, 0UL,
                        0u, s.Handle, src,
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

        // Allocate memory unless we do an update only
        if not updateOnly then
            allocateMemory s

        // Build and sync
        s.Device.eventually {
            do! build
            do! barrier
        }

    let private createHandle (device : Device) (info : AccelerationStructureInfo) =
        native {
            let! pCreateInfo =
                VkAccelerationStructureCreateInfoNV(
                    VkStructureType.AccelerationStructureCreateInfoNv, 0n, 0UL, info.data
                )

            let! pHandle = VkAccelerationStructureNV.Null
            VkRaw.vkCreateAccelerationStructureNV(device.Handle, pCreateInfo, NativePtr.zero, pHandle)
                |> check "could not create acceleration structure"

            return !!pHandle
        }

    let private updateHandle (info : AccelerationStructureInfo) (s : AccelerationStructure) =
        assert s.Handle.IsValid

        VkRaw.vkDestroyAccelerationStructureNV(s.Device.Handle, s.Handle, NativePtr.zero)
        s.Handle <- createHandle s.Device info
        bindResultMemory s

    let create (device : Device) (desc : AccelerationStructureDescription) =
        let info = AccelerationStructureInfo.alloc desc
        let handle = createHandle device info

        let accel =
            match desc with
                | TopLevel desc ->
                    new TopLevelAccelerationStructure(device, handle, desc)
                        :> AccelerationStructure
                | BottomLevel desc ->
                    new BottomLevelAccelerationStructure(device, handle, desc)
                        :> AccelerationStructure

        accel |> build info false
        AccelerationStructureInfo.free info
        accel

    let createBottomLevel (device : Device) (desc : BottomLevelDescription) : BottomLevelAccelerationStructure =
        create device (BottomLevel desc)
            |> unbox

    let createTopLevel (device : Device) (desc : TopLevelDescription) : TopLevelAccelerationStructure =
        create device (TopLevel desc)
            |> unbox

    let tryUpdate (desc : AccelerationStructureDescription) (s : AccelerationStructure) =

        let isReuseable, recreateHandle =
            match desc, s.Description with
                | TopLevel dst, TopLevel src ->
                    // If we have fewer or the same number of instances we can
                    // reuse the acceleration structure

                    // FIXME: Case for fewer does not work, complains that
                    // instanceData buffer is not valid. Somehow related to the recreation
                    // of the handle
                    dst.instanceCount = src.instanceCount,
                    dst.instanceCount <> src.instanceCount

                | BottomLevel _, BottomLevel _ ->
                    // The Vulkan spec is very cryptic on when a bottom level acceleration
                    // data structure can be reused for an update. For now we just force a full
                    // rebuild.
                    false, false

                | _ ->
                    false, false
        
        if isReuseable then
            let info = AccelerationStructureInfo.alloc desc

            // Even though the memory requirements may be compatible, we may have to
            // recreate the handle since the VkAccelerationStructureInfoNV struct
            // changed
            if recreateHandle then
                updateHandle info s

            s |> build info true
            AccelerationStructureInfo.free info
            s.Description <- desc

            true
        else
            false
    
    let delete (s : AccelerationStructure) =
        if s.Handle.IsValid then
            VkRaw.vkDestroyAccelerationStructureNV(s.Device.Handle, s.Handle, NativePtr.zero)
            s.Handle <- VkAccelerationStructureNV.Null
        freeMemory s |> ignore