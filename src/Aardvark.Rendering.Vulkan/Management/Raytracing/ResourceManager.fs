namespace Aardvark.Rendering.Vulkan.Raytracing

open System
open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan

module Resources =

    [<AbstractClass>]
    type TraceSceneReaderResource<'a>(owner : IResourceCache, key : list<obj>, scene : TraceScene) =
        inherit AbstractResourceLocation<'a>(owner, key)

        let mutable reader = None
        let mutable info = None

        // Compute the deltas from the reader
        let getDeltas (token : AdaptiveToken) (reader : ISetReader<TraceObject>) =
            let deltas = reader.GetOperations token

            let added = List(deltas.Count)
            let removed = List(deltas.Count)

            for d in deltas do
                match d with
                | Add(_, obj) -> obj |> added.Add |> ignore
                | Rem(_, obj) -> obj |> removed.Add |> ignore

            (added :> seq<_>, removed :> seq<_>)

        override x.Create() =
            reader <- Some <| scene.Objects.GetReader()

        override x.Destroy() =
            reader |> Option.iter (fun r -> r.Dispose())
            reader <- None

        abstract member ApplyChanges : AdaptiveToken * TraceObject seq * TraceObject seq -> ResourceInfo<'a>

        override x.GetHandle(token : AdaptiveToken) =
            if x.OutOfDate then
                let (added, removed) = getDeltas token reader.Value
                info <- Some <| x.ApplyChanges(token, added, removed)

            info.Value

    type IndexPoolResource(owner : IResourceCache, key : list<obj>, scene : TraceScene) =
        inherit TraceSceneReaderResource<IndexPool>(owner, key, scene)

        let pool = IndexPool.create()
        let mutable version = 0

        override x.ApplyChanges(_, added : TraceObject seq, removed : TraceObject seq) =
            pool |> IndexPool.applyChanges added removed

            if Seq.length added + Seq.length removed > 0 then
                inc &version

            { handle = pool; version = version }

    type ShaderPoolResource(owner : IResourceCache, key : list<obj>, device : Device, scene : TraceScene) =
        inherit TraceSceneReaderResource<ShaderPool>(owner, key, scene)

        let mutable pool = None
        let mutable version = 0

        override x.Create() =
            base.Create()
            pool <- Some (ShaderPool.create device scene)

        override x.Destroy() =
            pool |> Option.iter ShaderPool.destroy
            base.Destroy()

        override x.ApplyChanges(_, added : TraceObject seq, removed : TraceObject seq) =
            pool.Value |> ShaderPool.applyChanges added removed

            if Seq.length added + Seq.length removed > 0 then
                inc &version

            { handle = pool.Value; version = version }

    type AttributeBufferResource(owner : IResourceCache, key : list<obj>, device : Device, name : Symbol,
                                    scene : TraceScene, indexPool : IResourceLocation<IndexPool>) =
        inherit TraceSceneReaderResource<Buffer>(owner, key, scene)

        let mutable handle = None
        let mutable version = 0

        let mutable array = None

        let usage =
            VkBufferUsageFlags.TransferSrcBit ||| VkBufferUsageFlags.TransferDstBit ||| VkBufferUsageFlags.StorageBufferBit

        let create (data : IBuffer) =
            handle <- Some <| device.CreateBuffer(usage, data)
            inc &version

        let update (data : IBuffer) (buffer : Buffer) =
            let success = Buffer.tryUpdate data buffer

            if not success then
                device.Delete buffer
                create data
            else
                inc &version

        override x.Create() =
            base.Create()
            indexPool.Acquire()
            array <- Some <| AttributeArray.create name

        override x.Destroy() =
            array |> Option.iter AttributeArray.delete
            handle |> Option.iter device.Delete
            indexPool.Release()
            base.Destroy()

        override x.ApplyChanges(token : AdaptiveToken, added : TraceObject seq, removed : TraceObject seq) =
            let indices = (indexPool.Update token).handle

            let data =
                array |> Option.map (fun arr ->
                    arr |> AttributeArray.applyChanges token indices added removed
                    ArrayBuffer(arr.Data)
                ) |> Option.get

            match handle with
            | Some s -> update data s
            | None -> create data

            { handle = handle.Value; version = version }

    type ImageSamplerArrayResource(owner : IResourceCache, key : list<obj>, name : Symbol,
                                    scene : TraceScene, indexPool : IResourceLocation<IndexPool>,
                                    createImageSampler : IMod<ITexture> -> IResourceLocation<Resources.ImageSampler>) =
        inherit TraceSceneReaderResource<Resources.ImageSamplerArray>(owner, key, scene)

        let mutable handle = [||]
        let mutable version = 0

        let mutable array = None

        override x.Create() =
            base.Create()
            indexPool.Acquire()
            array <- Some <| ImageSamplerArray.create createImageSampler name

        override x.Destroy() =
            array |> Option.iter ImageSamplerArray.delete
            indexPool.Release()
            base.Destroy()

        override x.ApplyChanges(token : AdaptiveToken, added : TraceObject seq, removed : TraceObject seq) =
            if Seq.length added + Seq.length removed > 0 then
                let indices = (indexPool.Update token).handle
            
                array |> Option.iter (fun arr ->
                    arr |> ImageSamplerArray.applyChanges indices added removed
                    handle <- arr.Data
                )

                inc &version

            { handle = handle; version = version }

    type InstanceBufferResource(owner : IResourceCache, key : list<obj>, device : Device, scene : TraceScene,
                                indexPool : IResourceLocation<IndexPool>, shaderPool : IResourceLocation<ShaderPool>) =
        inherit TraceSceneReaderResource<InstanceBuffer>(owner, key, scene)

        let mutable handle = None
        let mutable version = 0

        let mutable array = None

        let create instances =
            handle <- Some (InstanceBuffer.create device instances)
            inc &version

        let update instances buffer =
            let success = InstanceBuffer.tryUpdate instances buffer

            if not success then
                InstanceBuffer.delete buffer
                create instances
            else
                inc &version

        override x.Create() =
            base.Create()
            indexPool.Acquire()
            shaderPool.Acquire()
            array <- Some <| InstanceArray.create()

        override x.Destroy() =
            array |> Option.iter InstanceArray.delete
            handle |> Option.iter InstanceBuffer.delete
            shaderPool.Release()
            indexPool.Release()
            base.Destroy()

        override x.ApplyChanges(token : AdaptiveToken, added : TraceObject seq, removed : TraceObject seq) =
            let indices = (indexPool.Update token).handle
            let shaders = (shaderPool.Update token).handle

            let data =
                array |> Option.map (fun arr ->
                    arr |> InstanceArray.applyChanges token indices shaders added removed
                    arr.Data
                ) |> Option.get

            match handle with
            | Some s -> update data s
            | None -> create data

            { handle = handle.Value; version = version }

    type AccelerationStructureResource(owner : IResourceCache, key : list<obj>, 
                                       device : Device, resources : IResourceLocation list,
                                       getDescription : AdaptiveToken -> AccelerationStructureDescription) =
        inherit AbstractResourceLocation<AccelerationStructure>(owner, key)

        let mutable handle = None
        let mutable version = 0

        let create desc =
            handle <- Some (AccelerationStructure.create device desc)
            inc &version

        let update desc s =
            let success = AccelerationStructure.tryUpdate desc s

            if not success then
                AccelerationStructure.delete s
                create desc
            else
                inc &version

        override x.Create() =
            resources |> List.iter (fun r -> r.Acquire())

        override x.Destroy() =
            handle |> Option.iter AccelerationStructure.delete
            resources |> List.iter (fun r -> r.Release())

        override x.GetHandle(token : AdaptiveToken) =
            if x.OutOfDate then
                let desc = getDescription token

                match handle with
                    | Some s -> update desc s
                    | None -> create desc

            { handle = handle.Value; version = version }

    type TracePipelineResource(owner : IResourceCache, key : list<obj>,
                                device : Device, layout : PipelineLayout, maxRecursionDepth : uint32,
                                shaderPool : IResourceLocation<ShaderPool>) =
        inherit AbstractResourceLocation<TracePipeline>(owner, key)

        let mutable handle = None
        let mutable version = 0
        let mutable description = None

        let baseDescription = TracePipelineDescription.create layout maxRecursionDepth

        let destroy() =
            handle |> Option.iter TracePipeline.delete
            handle <- None

        let create desc =
            let basePipeline = handle
            handle <- Some (TracePipeline.create device basePipeline desc)
            description <- Some desc
            basePipeline |> Option.iter TracePipeline.delete
            inc &version

        override x.Create() =
            shaderPool.Acquire()

        override x.Destroy() =
            destroy()
            shaderPool.Release()

        override x.GetHandle(token : AdaptiveToken) =
            if x.OutOfDate then
                let shaders = (shaderPool.Update token).handle
                let desc = shaders.GetPipelineDescription baseDescription

                match handle with
                | Some _ when description |> Option.contains desc -> ()
                | _ -> create desc

            { handle = handle.Value; version = version }

    type ShaderBindingTableResource(owner : IResourceCache, key : list<obj>,
                                        device : Device, pipeline : IResourceLocation<TracePipeline>) =
        inherit AbstractResourceLocation<ShaderBindingTable>(owner, key)

        let mutable handle = None
        let mutable version = 0
        let mutable pipelineVersion = -1

        let create pipeline entries =
            handle <- Some (ShaderBindingTable.create device pipeline entries)
            inc &version

        let update pipeline entries table =
            let success = table |> ShaderBindingTable.tryUpdate pipeline entries

            if not success then
                ShaderBindingTable.delete table
                create pipeline entries
            else
                inc &version

        override x.Create() =
            pipeline.Acquire()

        override x.Destroy() =
            handle |> Option.iter ShaderBindingTable.delete
            pipeline.Release()

        override x.GetHandle(token : AdaptiveToken) =
            if x.OutOfDate then
                let info = pipeline.Update token

                if info.version <> pipelineVersion then
                    let pipeline = info.handle
                    let entries = TracePipeline.getShaderBindingTableEntries pipeline

                    match handle with
                    | Some tbl -> tbl |> update pipeline.Handle entries
                    | None -> create pipeline.Handle entries

                    pipelineVersion <- info.version

            { handle = handle.Value; version = version }

type ResourceManager(user : IResourceUser, device : Device) =
    inherit Aardvark.Rendering.Vulkan.ResourceManager(user, device)

    let indexPoolCache              = ResourceLocationCache<IndexPool>(user)
    let shaderPoolCache             = ResourceLocationCache<ShaderPool>(user)
    let attributeBufferCache        = ResourceLocationCache<Buffer>(user)
    let imageSamplerArrayCache      = ResourceLocationCache<Resources.ImageSamplerArray>(user)
    let instanceBufferCache         = ResourceLocationCache<InstanceBuffer>(user)
    let accelerationStructureCache  = ResourceLocationCache<AccelerationStructure>(user)
    let tracePipelineCache          = ResourceLocationCache<TracePipeline>(user)
    let shaderBindingTableCache     = ResourceLocationCache<ShaderBindingTable>(user)

    member x.Dispose() =
        indexPoolCache.Clear()
        shaderPoolCache.Clear()
        attributeBufferCache.Clear()
        imageSamplerArrayCache.Clear()
        instanceBufferCache.Clear()
        accelerationStructureCache.Clear()
        tracePipelineCache.Clear()
        shaderBindingTableCache.Clear()
        base.Dispose()

    member x.CreateIndexPool(scene : TraceScene) =
        let key = [ scene :> obj ]
        indexPoolCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.IndexPoolResource(cache, key, scene)
        )

    member x.CreateShaderPool(device : Device, scene : TraceScene) =
        let key = [ scene :> obj ]
        shaderPoolCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.ShaderPoolResource(cache, key, device, scene)
        )

    member x.CreateAttributeBuffer(name : Symbol, scene : TraceScene, indexPool : IResourceLocation<IndexPool>) =
        let key = [ name :> obj; scene :> obj; indexPool :> obj]
        attributeBufferCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.AttributeBufferResource(cache, key, device, name, scene, indexPool)
        )

    member x.CreateImageSamplerArray(name : Symbol, scene : TraceScene,
                                        samplerType : FShade.GLSL.GLSLSamplerType,
                                        samplerState : IMod<SamplerStateDescription>,
                                        indexPool : IResourceLocation<IndexPool>) =
        let key = [ name :> obj; scene :> obj; samplerType :> obj; samplerState :> obj; indexPool :> obj]
        imageSamplerArrayCache.GetOrCreate(
            key,
            fun cache key ->
                let createImageSampler texture =
                    x.CreateImageSampler(samplerType, texture, samplerState)

                new Resources.ImageSamplerArrayResource(cache, key, name, scene, indexPool, createImageSampler)
        )

    member x.CreateInstanceBuffer(scene : TraceScene, indexPool : IResourceLocation<IndexPool>, shaderPool : IResourceLocation<ShaderPool>) =
        let key = [ scene :> obj; indexPool :> obj; shaderPool :> obj ]
        instanceBufferCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.InstanceBufferResource(cache, key, device, scene, indexPool, shaderPool)
        )

    member x.CreateAccelerationStructure(instanceBuffer : IResourceLocation<InstanceBuffer>) =
        let getDescription (token : AdaptiveToken) =
            let buffer = (instanceBuffer.Update token).handle

            { instances = buffer.Handle
              instanceCount = buffer.Count }
                |> TopLevel

        let key = [ instanceBuffer :> obj ]
        accelerationStructureCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.AccelerationStructureResource(
                    cache, key, device, [instanceBuffer :> IResourceLocation], getDescription
                )
        )

    member x.CreateAccelerationStructure(desc : IMod<AccelerationStructureDescription>) =
        let getDescription (token : AdaptiveToken) =
            desc.GetValue token

        let key = [ desc :> obj ]
        accelerationStructureCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.AccelerationStructureResource(
                    cache, key, device, [], getDescription
                )
        )

    member x.CreateTracePipeline(layout : PipelineLayout, maxRecursionDepth : uint32, shaderPool : IResourceLocation<ShaderPool>) =
        let key = [ layout :> obj; maxRecursionDepth :> obj; shaderPool :> obj ]
        tracePipelineCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.TracePipelineResource(
                    cache, key, device, layout, maxRecursionDepth, shaderPool
                )
        )

    member x.CreateShaderBindingTable(pipeline : IResourceLocation<TracePipeline>) =
        let key = [ pipeline :> obj ]
        shaderBindingTableCache.GetOrCreate(
            key,
            fun cache key ->
                new Resources.ShaderBindingTableResource(
                    cache, key, device, pipeline
                )
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()