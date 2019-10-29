namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

open System.Runtime.InteropServices

[<StructLayout(LayoutKind.Explicit, Size = 3)>]
type uint24 =
    struct
        [<FieldOffset(0)>]
        val mutable public B0 : uint8
        [<FieldOffset(1)>]
        val mutable public B1 : uint8
        [<FieldOffset(2)>]
        val mutable public B2 : uint8

        member x.Value = uint32 (x.B0) ||| uint32(x.B1 <<< 8) ||| uint32 (x.B2 <<< 16)
        new(v : uint32) = { B0 = byte (v &&& 0xFFu); B1 = byte ((v >>> 8) &&& 0xFFu); B2= byte ((v >>> 16) &&& 0xFFu)}
    end

type VkGeometryInstance =
    struct
        val public transform : M34f
        val public instanceId : uint24
        val public mask : uint8
        val public instanceOffset : uint24
        val public flags : uint8
        val public accelerationStructureHandle : uint64

        new (transform : Trafo3d, instanceId : int, mask : uint8,
                instanceOffset : int, flags : VkGeometryInstanceFlagsNV, blAS : uint64) =
            {
                transform = M34f.op_Explicit transform.Forward
                instanceId = uint24 <| uint32 instanceId
                mask = mask
                instanceOffset = uint24 <| uint32 instanceOffset
                flags = uint8 flags
                accelerationStructureHandle = blAS
            }
    end

type InstanceBuffer =
    class
        inherit Buffer
        val mutable public Count : int

        new(device : Device, handle : VkBuffer, ptr : DevicePtr, count : int, flags : VkBufferUsageFlags) =
            { inherit Buffer(device, handle, ptr, int64 (count * sizeof<VkGeometryInstance>), flags)
              Count = count }
    end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InstanceBuffer =
    open Microsoft.FSharp.NativeInterop

    let tryUpdate (instances : VkGeometryInstance list) (buffer : InstanceBuffer) =
        let data = instances |> List.toArray
        let size = data.Length * sizeof<VkGeometryInstance>

        if data.Length = buffer.Count then
            pinned data (fun ptr ->
                buffer.Device.Runtime.Copy(ptr, buffer, 0n, nativeint size)
            )
            
            true
        else
            false

    let create (device : Device) (instances : VkGeometryInstance list) =
        let count = List.length instances
        let size = count * sizeof<VkGeometryInstance>
        let flags = VkBufferUsageFlags.RayTracingBitNv ||| VkBufferUsageFlags.TransferDstBit

        let info =
            VkBufferCreateInfo(
                VkStructureType.BufferCreateInfo, 0n,
                VkBufferCreateFlags.None,
                uint64 size, 
                flags,
                VkSharingMode.Exclusive,
                0u, NativePtr.zero
            )

        let handle =
            info |> pin (fun pInfo ->
                temporary (fun pHandle ->
                    VkRaw.vkCreateBuffer(device.Handle, pInfo, NativePtr.zero, pHandle)
                        |> check "could not create buffer"
                    NativePtr.read pHandle
                )
            )

        let reqs =
            temporary (fun ptr ->   
                VkRaw.vkGetBufferMemoryRequirements(device.Handle, handle, ptr)
                NativePtr.read ptr
            )

        let ptr = device.Alloc(reqs, true)

        VkRaw.vkBindBufferMemory(device.Handle, handle, ptr.Memory.Handle, uint64 ptr.Offset)
            |> check "could not bind buffer-memory"

        let buffer = new InstanceBuffer(device, handle, ptr, count, flags)
        buffer |> tryUpdate instances |> ignore
        
        buffer

    let delete (buffer : InstanceBuffer) =
        Buffer.delete buffer buffer.Device
        