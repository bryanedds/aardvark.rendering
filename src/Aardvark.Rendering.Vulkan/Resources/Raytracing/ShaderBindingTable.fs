namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

type ShaderBindingTableEntryType =
    | Raygen = 0
    | Miss = 1
    | Callable = 2
    | HitGroup = 3

type ShaderBindingTableEntry = {
    kind : ShaderBindingTableEntryType

    // The index of the entry in the pGroups array in                                                                           
    // the VkRayTracingShaderGroupCreateInfoNV struct
    groupIndex : int

    // User data available as a buffer with the shaderRecordNV qualifier
    // in the shaders
    inlineData : uint8[]
}

type ShaderBindingTableEntries(data) =

    let data : Map<ShaderBindingTableEntryType, ShaderBindingTableEntry list> = data

    member x.Values = data |> Map.toSeq

    member x.Count = x.Values |> Seq.sumBy (fun (_, x) -> List.length x)

    member x.Item
        with get(index) = data.[index]

    member x.Add kind values =
        data |> Map.add kind values |> ShaderBindingTableEntries

    //member x.Values = x.sections |> Map.toSeq |> Seq.map (fun (_, x) -> List.rev x)

type ShaderBindingTable =
    class
        inherit Buffer
        val mutable public Pipeline : VkPipeline
        val mutable public Entries : ShaderBindingTableEntries
        val mutable public ShaderGroupHandleSize : uint32
        val mutable public RaygenShaderBindingOffset : VkDeviceSize
        val mutable public MissShaderBindingOffset : VkDeviceSize
        val mutable public MissShaderBindingStride : VkDeviceSize
        val mutable public HitShaderBindingOffset : VkDeviceSize
        val mutable public HitShaderBindingStride : VkDeviceSize
        val mutable public CallableShaderBindingOffset : VkDeviceSize
        val mutable public CallableShaderBindingStride : VkDeviceSize

        new(device : Device, pipeline : VkPipeline, handle : VkBuffer,
            ptr : DevicePtr, size : uint32, flags : VkBufferUsageFlags, entries : ShaderBindingTableEntries,
            handleSize : uint32, raygenOffset : VkDeviceSize,
            missOffset : VkDeviceSize, missStride : VkDeviceSize,
            hitOffset : VkDeviceSize, hitStride : VkDeviceSize,
            callableOffset : VkDeviceSize, callableStride : VkDeviceSize) =
            { inherit Buffer(device, handle, ptr, int64 size, flags)
              Pipeline = pipeline
              Entries = entries
              ShaderGroupHandleSize = handleSize
              RaygenShaderBindingOffset = raygenOffset
              MissShaderBindingOffset = missOffset
              MissShaderBindingStride = missStride
              HitShaderBindingOffset = hitOffset
              HitShaderBindingStride = hitStride
              CallableShaderBindingOffset = callableOffset
              CallableShaderBindingStride = callableStride }
    end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderBindingTableEntryType =

    let ofShaderStage =
        LookupTable.lookupTable [
            ShaderStage.Raygen, ShaderBindingTableEntryType.Raygen
            ShaderStage.AnyHit, ShaderBindingTableEntryType.HitGroup
            ShaderStage.ClosestHit, ShaderBindingTableEntryType.HitGroup
            ShaderStage.Intersection, ShaderBindingTableEntryType.HitGroup
            ShaderStage.Miss, ShaderBindingTableEntryType.Miss
            ShaderStage.Callable, ShaderBindingTableEntryType.Callable
        ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderBindingTableEntries =
    open System

    let count (entries : ShaderBindingTableEntries) =
        entries.Count

    let empty : ShaderBindingTableEntries =
        Enum.GetValues typeof<ShaderBindingTableEntryType>
            |> Seq.cast
            |> Seq.map (fun c -> unbox c, [])
            |> Map.ofSeq
            |> ShaderBindingTableEntries

    let add (kind : ShaderBindingTableEntryType) (data : uint8[]) (entries : ShaderBindingTableEntries) =
        let index = count entries
        let entry = { kind = kind; groupIndex = index; inlineData = data }

        let values =
            List.append entries.[kind] [entry]

        entries.Add kind values

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderBindingTable =
    open Microsoft.FSharp.NativeInterop
    open System.Runtime.InteropServices
    open Aardvark.Base.Prelude.NativePtr.Operators

    // Returns the size of an entry (consists of a program id + inline data)
    // Every entry of a given type must be the same size and aligned to 16 bytes
    let private getEntrySize (handleSize : uint32) (entries : ShaderBindingTableEntry list) =
        let roundUp alignment x =
            (x + alignment - 1u) &&& ~~~(alignment - 1u)

        let maxDefault x =
            if List.isEmpty x then 0u else List.max x 
        
        let maxSize =
            entries |> List.map (fun e -> uint32 e.inlineData.Length)
                    |> maxDefault
            
        (handleSize + maxSize) |> roundUp 16u

    // Returns the total size of the table
    let private getTotalSize (handleSize : uint32) (entries : ShaderBindingTableEntries) =
        entries.Values
            |> Seq.sumBy (fun (_, e) ->
                (uint32 e.Length) * getEntrySize handleSize e
            )
    
    // Returns the base offset for the given entry type
    let private getOffset (kind : ShaderBindingTableEntryType) (handleSize : uint32) (entries : ShaderBindingTableEntries) =
        entries.Values
            |> Seq.takeWhile (fun x -> fst x <> kind)
            |> Seq.sumBy (fun (_, e) ->
                uint64 (getEntrySize handleSize e * uint32 e.Length)
            )

    let private getOffsetsAndStrides (handleSize : uint32) (entries : ShaderBindingTableEntries) =
        getOffset ShaderBindingTableEntryType.Raygen handleSize entries,
        getOffset ShaderBindingTableEntryType.Miss handleSize entries,
        uint64 (getEntrySize handleSize entries.[ShaderBindingTableEntryType.Miss]),
        getOffset ShaderBindingTableEntryType.HitGroup handleSize entries,
        uint64 (getEntrySize handleSize entries.[ShaderBindingTableEntryType.HitGroup]),
        getOffset ShaderBindingTableEntryType.Callable handleSize entries,
        uint64 (getEntrySize handleSize entries.[ShaderBindingTableEntryType.Callable])
       
    let tryUpdate (pipeline : VkPipeline) (entries : ShaderBindingTableEntries) (table : ShaderBindingTable) =
        let totalSize = getTotalSize table.ShaderGroupHandleSize entries

        if table.Size = int64 totalSize then
            let groupCount = entries.Count
            let shaderHandles : uint8[] = Array.zeroCreate <| groupCount * (int table.ShaderGroupHandleSize)

            pinned shaderHandles (fun ptr ->
                VkRaw.vkGetRayTracingShaderGroupHandlesNV(table.Device.Handle, pipeline,
                    0u, uint32 groupCount, uint64 shaderHandles.Length, ptr)
                        |> check "Failed to get shader group handles"
                )

            let output : uint8[] = Array.zeroCreate <| int totalSize

            pinned output (fun ptr ->
                let mutable pOutput : nativeptr<uint8> = NativePtr.ofNativeInt ptr
                let handleSize = int table.ShaderGroupHandleSize
            
                entries.Values |> Seq.iter (fun (_, x) ->
                    let entrySize = int <| getEntrySize table.ShaderGroupHandleSize x

                    x |> List.iter (fun e ->
                        Marshal.Copy(shaderHandles, e.groupIndex * handleSize, NativePtr.toNativeInt pOutput, handleSize)
                        Marshal.Copy(e.inlineData, 0, NativePtr.toNativeInt (pOutput &+ handleSize), e.inlineData.Length)
                        pOutput <- pOutput &+ entrySize
                    )
                )

                table.Device.Runtime.Copy(ptr, table, 0n, nativeint totalSize)
            )

            let (raygenOffset, missOffset, missStride, hitOffset, hitStride, callableOffset, callableStride) =
                getOffsetsAndStrides table.ShaderGroupHandleSize entries

            table.Entries <- entries
            table.Pipeline <- pipeline
            table.RaygenShaderBindingOffset <- raygenOffset
            table.MissShaderBindingOffset <- missOffset
            table.HitShaderBindingOffset <- hitOffset
            table.CallableShaderBindingOffset <- callableOffset
            table.MissShaderBindingStride <- missStride
            table.HitShaderBindingStride <- hitStride
            table.CallableShaderBindingStride <- callableStride
            true
        else
            false

    let create (device : Device) (pipeline : VkPipeline) (entries : ShaderBindingTableEntries) =

        let handleSize = RaytracingProperties.shaderGroupHandleSize device
        let totalSize = getTotalSize handleSize entries
        let flags = VkBufferUsageFlags.RayTracingBitNv ||| VkBufferUsageFlags.TransferDstBit

        let info =
            VkBufferCreateInfo(
                VkStructureType.BufferCreateInfo, 0n,
                VkBufferCreateFlags.None,
                uint64 totalSize, flags,
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

        let (raygenOffset, missOffset, missStride, hitOffset, hitStride, callableOffset, callableStride) =
            getOffsetsAndStrides handleSize entries

        let table = new ShaderBindingTable(device, pipeline, handle, ptr, totalSize, flags, entries, handleSize,
                                           raygenOffset, missOffset, missStride, hitOffset, hitStride, callableOffset,
                                           callableStride)
        table |> tryUpdate pipeline entries |> ignore
        
        table

    let delete (table : ShaderBindingTable) =
        Buffer.delete table table.Device      