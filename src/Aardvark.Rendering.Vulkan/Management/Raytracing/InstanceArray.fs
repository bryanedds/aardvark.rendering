namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan.NVRayTracing

open System.Collections.Generic

type private InstanceWriter(input : TraceObject, compile : AdaptiveToken -> TraceObject -> VkGeometryInstance) =
    inherit AdaptiveObject()

    member x.Write(token : AdaptiveToken, buffer : VkGeometryInstance[], index : int) =
        x.EvaluateIfNeeded token () (fun token ->
            let value = compile token input
            buffer.[index] <- value
        )

    member x.WriteTrafo(token : AdaptiveToken, buffer : VkGeometryInstance[], index : int) =
        x.EvaluateIfNeeded token () (fun token ->
            let trafo = input.Transform.GetValue token
            buffer.[index].Transform <- M34f.op_Explicit trafo.Forward
        )

    member x.Release() =
        lock x (fun _ ->
            input.Transform.RemoveOutput x
            x.Outputs.Clear()
        )

/// Array that holds VkGeometryInstance structs that adaptively
/// resizes and stays compact
type InstanceArray(indices : IndexPool, shaders : ShaderPool) =
    inherit TraceSceneReader(indices.Scene)

    // CPU buffer
    let mutable data : VkGeometryInstance[] = Array.empty

    // Buffer containing the original instances
    let mutable keys : TraceObject[] = Array.empty

    // Locations of individual instances in the CPU buffer
    let mapping = Dictionary<TraceObject, int>()

    // Writers tracking the trace objects
    let writers = Dictionary<TraceObject, InstanceWriter>()

    // Writes the given instance to an index
    let setIndex (index : int) (key : TraceObject) =
        keys.[index] <- key
        mapping.[key] <- index

    let compileObject (token : AdaptiveToken) (obj : TraceObject) =
        let trafo = obj.Transform.GetValue token
        let index = indices.Get obj
        let hitGroup = shaders.GetHitGroupIndex obj

        VkGeometryInstance(
            trafo, index, 0xffuy, hitGroup,
            VkGeometryInstanceFlagsNV.TriangleCullDisableBit,
            unbox obj.Geometry.Handle
        )

    let applyDeltas (token : AdaptiveToken) (added : TraceObject seq) (removed : TraceObject seq) =
        let free = Queue(removed |> Seq.map (fun k -> mapping.[k]))
        let delta = Seq.length added - Seq.length removed

        let oldCount = data.Length
        let newCount = oldCount + delta
    
        // If we remove more instances than we add, we may end up with
        // holes. Copy elements from the end into these holes.
        for i in 0 .. -(delta + 1) do
            let newIndex = free.Dequeue()

            if newIndex < newCount then
                let oldIndex = newCount + i
                let key = keys.[oldIndex]

                data.[newIndex] <- data.[oldIndex]
                setIndex newIndex key

        // Resize the buffers
        data <- data.Resized newCount
        keys <- keys.Resized newCount

        for i in oldCount .. newCount - 1 do
            free.Enqueue i

        // Remove elements
        for k in removed do
            match writers.TryGetValue k with
            | true, w ->
                w.Release()
                writers.Remove k |> ignore
            | _ -> ()

            mapping.Remove k |> ignore

        // Add elements
        for k in added do
            let i = free.Dequeue()

            let w = InstanceWriter(k, compileObject)
            w.Write(token, data, i)
            writers.[k] <- w

            setIndex i k

        assert(free.IsEmpty())

    // Applies all the pending adds and removes, and compacts the buffer
    override x.ApplyChanges(token, added, removed) =
        indices.Update token
        shaders.Update token

        // Update trafos of existing objects
        for KeyValue(obj, w) in writers do
            w.WriteTrafo(token, data, mapping.[obj])

        // Handle added and removed objects
        if Seq.length added + Seq.length removed > 0 then
            applyDeltas token added removed

    member x.Data =
        data

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InstanceArray =

    let create (indices : IndexPool) (shaders : ShaderPool) =
        new InstanceArray(indices, shaders)

    let delete (array : InstanceArray) =
        array.Dispose()