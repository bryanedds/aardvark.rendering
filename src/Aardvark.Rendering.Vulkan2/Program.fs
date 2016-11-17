﻿open System
open Aardvark.Base
open Aardvark.Rendering.Vulkan

[<EntryPoint>]
let main args =
    Aardvark.Init()

    Log.start "layers"
    for l in Instance.AvailableLayers do
        Log.start "%s" l.name

        Log.line "description:    %s" l.description
        Log.line "specification:  %A" l.specification 
        Log.line "implementation: %A" l.implementation
        
        match l.extensions with
            | [] -> ()
            | _ -> 
                Log.start "extensions"
                for e in l.extensions do
                    Log.line "%s: %A" e.name e.specification
                Log.stop() 

        Log.stop()
    Log.stop()

    Log.start "extensions"
    for e in Instance.GlobalExtensions do
        Log.line "%s: %A" e.name e.specification
    Log.stop() 



    use instance = new Instance(Version(1,0,2), Set.empty, Set.ofList ["VK_KHR_SURFACE"; "VK_KHR_WIN32_SURFACE"])

    for d in instance.Devices do
        Log.start "device %d" d.Index
        Log.line "vendor:         %s" d.Vendor
        Log.line "name:           %s" d.Name
        Log.line "type:           %A" d.Type
        Log.line "api version:    %A" d.APIVersion
        Log.line "driver version: %A" d.DriverVersion
        Log.line "main queue:     { id = %d; count = %d; flags = %A }" d.MainQueue.index d.MainQueue.count d.MainQueue.flags
        match d.TransferQueue with
            | Some q -> Log.line "transfer queue: { id = %d; count = %d; flags = %A }" q.index q.count q.flags
            | _ -> Log.line "transfer queue: None"

        Log.start "layers"
        for l in d.AvailableLayers do
            Log.start "%s" l.name

            Log.line "description:    %s" l.description
            Log.line "specification:  %A" l.specification 
            Log.line "implementation: %A" l.implementation
        
            match l.extensions with
                | [] -> ()
                | _ -> 
                    Log.start "extensions"
                    for e in l.extensions do
                        Log.line "%s: %A" e.name e.specification
                    Log.stop() 

            Log.stop()
        Log.stop()

        Log.start "extensions"
        for e in d.GlobalExtensions do
            Log.line "%s: %A" e.name e.specification
        Log.stop() 


        Log.start "queues"
        for q in d.QueueFamilies do
            Log.start "queue %d" q.index
            Log.line "flags:          %A" q.flags
            Log.line "count:          %A" q.count
            Log.line "imgGranularity: %A" q.minImgTransferGranularity
            Log.line "timestampBits:  %A" q.timestampBits
            Log.stop()
        Log.stop()
 
        Log.start "memories"
        let t = d.DeviceMemory
        Log.start "device memory"
        Log.line "index:          %d" t.index
        Log.line "heap size:      %A" t.heap.Capacity
        Log.line "flags:          %A" t.flags
        Log.stop()

        let t = d.HostMemory
        Log.start "host memory"
        Log.line "index:          %d" t.index
        Log.line "heap size:      %A" t.heap.Capacity
        Log.line "flags:          %A" t.flags
        Log.stop()

        Log.stop()
  

    

        
        Log.stop()


    let main = instance.Devices.[0]
    use dev = main.CreateDevice(Set.empty, Set.ofList ["VK_KHR_swapchain"; "VK_NV_glsl_shader"], [main.MainQueue, 4])
    let runtime = new Runtime(dev, false, false)

    let size = V2i(1024, 768)
    let color = runtime.CreateTexture(size, TextureFormat.Rgba8, 1, 1, 1)
    let depth = runtime.CreateRenderbuffer(size, RenderbufferFormat.Depth24Stencil8, 1)

    let signature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

    let framebuffer =
        runtime.CreateFramebuffer(
            signature, [
                DefaultSemantic.Colors, { texture = color; level = 0; slice = 0 } :> IFramebufferOutput
                DefaultSemantic.Depth, depth :> IFramebufferOutput
            ]
        )




    let b = dev.CreateBuffer(VkBufferUsageFlags.StorageBufferBit, ArrayBuffer [|1uy; 2uy;|])


    let buffer = dev.CreateBuffer(VkBufferUsageFlags.VertexBufferBit, ArrayBuffer [|1;2;3;4;5|])
    let buffer2 = dev.CreateBuffer(VkBufferUsageFlags.VertexBufferBit, ArrayBuffer [|1;2;3;4;5|])

    let image = dev.CreateImage(@"E:\Development\WorkDirectory\DataSVN\pattern.jpg", { wantCompressed = false; wantSrgb = false; wantMipMaps = true })

    
    Log.warn "allocated: %A" dev.DeviceMemory.Allocated
    dev.Delete b
    dev.Delete buffer
    dev.Delete buffer2
    dev.Delete image

    Log.warn "allocated: %A" dev.DeviceMemory.Allocated


    let mb = dev |> MappedBuffer.create (VkBufferUsageFlags.VertexBufferBit ||| VkBufferUsageFlags.TransferDstBit ||| VkBufferUsageFlags.TransferSrcBit)

    let s = 1L <<< 16

    mb.Resize(s)
    mb.Resize(128L * s)


    let offset = 10L * s - 4L
    let data = Array.init 20 (fun i -> 1 + i)
    mb.Write(offset, data)
    Log.line "size: %A" mb.Size
    
    mb.Resize(256L * s)
    Log.line "size: %A" mb.Size


    let test = mb.Read<int>(offset, data.Length)

    if test <> data then
        Log.warn "input:  %A" data
        Log.warn "output: %A" test
    else
        Log.warn "success"

    mb.Resize(10L * s)

    let test2 = mb.Read<int>(offset, 1)
    if test2.[0] <> data.[0] then
        Log.warn "input:  %A" data.[0]
        Log.warn "output: %A" test2.[0]
    else
        Log.warn "success"

    mb.Resize(s)

    mb.Resize(0L)

    0