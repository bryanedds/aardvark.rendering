namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

type InstanceBuffer =
    class
        inherit Buffer

        /// The current number of instances in the buffer
        val mutable public Count : int

        /// The capacity (i.e. maximum number of instances) of the buffer
        val mutable public Capacity : int

        new(device : Device, handle : VkBuffer, ptr : DevicePtr, count : int, flags : VkBufferUsageFlags) =
            { inherit Buffer(device, handle, ptr, int64 (count * sizeof<VkGeometryInstance>), flags)
              Count = count 
              Capacity = count }
    end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InstanceBuffer =
    open Microsoft.FSharp.NativeInterop

    let tryUpdate (instances : VkGeometryInstance[]) (buffer : InstanceBuffer) =
        let size = instances.Length * sizeof<VkGeometryInstance>

        if instances.Length = buffer.Capacity then

            if instances.Length > 0 then
                pinned instances (fun ptr ->
                    buffer.Device.Runtime.Copy(ptr, buffer, 0n, nativeint size)
                )

            buffer.Count <- instances.Length
             
            true
        else
            false

    let create (device : Device) (instances : VkGeometryInstance[]) =
        let size = instances.Length * sizeof<VkGeometryInstance>
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

        let buffer = new InstanceBuffer(device, handle, ptr, instances.Length, flags)
        buffer |> tryUpdate instances |> ignore
        
        buffer

    let delete (buffer : InstanceBuffer) =
        Buffer.delete buffer buffer.Device
        