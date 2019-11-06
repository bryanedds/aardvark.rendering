namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental

open System
open System.Collections.Generic

/// Array that holds VkGeometryInstance structs that adaptively
/// resizes and stays compact
type InstanceArray(input : aset<TraceObject>, compile : AdaptiveToken -> TraceObject -> VkGeometryInstance) =
    inherit AdaptiveObject()

    let reader = input.GetReader()

    // CPU buffer
    let mutable data : VkGeometryInstance[] = Array.empty

    // Buffer containing the original instances
    let mutable keys : TraceObject[] = Array.empty

    // Locations of individual instances in the CPU buffer
    let mapping : Dict<TraceObject, int> = Dict.empty

    // Writes the given instance to an index
    let write (index : int) (key : TraceObject) (value : VkGeometryInstance) =
        data.[index] <- value
        keys.[index] <- key
        mapping.[key] <- index

    // Applies all the pending adds and removes, and compacts the buffer
    let applyDeltas (token : AdaptiveToken) (adds : List<TraceObject>) (rems : List<TraceObject>) =
        
        let free = Queue(rems |> Seq.map (fun k -> mapping.[k]))
        let delta = adds.Count - rems.Count

        let oldCount = data.Length
        let newCount = oldCount + delta
        
        // If we remove more instances than we add, we may end up with
        // holes. Copy elements from the end into these holes.
        for i in 0 .. -(delta + 1) do
            let newIndex = free.Dequeue()

            if newIndex < newCount then
                let oldIndex = newCount + i
                let key = keys.[oldIndex]

                write newIndex key data.[oldIndex]

        // Resize the buffers
        data <- data.Resized newCount
        keys <- keys.Resized newCount

        for i in oldCount .. newCount - 1 do
            free.Enqueue i

        // Remove elements
        for k in rems do
            mapping.Remove k |> ignore

        // Add elements
        for k in adds do
            let v = compile token k
            write (free.Dequeue()) k v

        assert(free.IsEmpty())

    member x.Update (token : AdaptiveToken) =
        x.EvaluateIfNeeded token data (fun token ->
            let deltas = reader.GetOperations token

            if deltas.Count > 0 then
                let adds = List(deltas.Count)
                let rems = List(deltas.Count)

                for d in deltas do
                    match d with
                        | Add(_, obj) -> obj |> adds.Add |> ignore
                        | Rem(_, obj) -> obj |> rems.Add |> ignore

                applyDeltas token adds rems

            for KeyValue(k, i) in mapping do
                if k.Transform.OutOfDate then
                    let trafo = k.Transform.GetValue token
                    data.[i].Transform <- M34f.op_Explicit trafo.Forward
        
            data
        )

    interface IDisposable with
        member x.Dispose() = reader.Dispose()