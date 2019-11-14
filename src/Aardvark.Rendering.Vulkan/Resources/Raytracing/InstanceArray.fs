namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental

open System
open System.Collections.Generic

type private InstanceWriter (input : TraceObject, compile : AdaptiveToken -> TraceObject -> VkGeometryInstance) =
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

/// Array that holds VkGeometryInstance structs that adaptively
/// resizes and stays compact
type InstanceArray(input : aset<TraceObject>) =
    inherit AdaptiveObject()

    let reader = input.GetReader()

    // CPU buffer
    let mutable data : VkGeometryInstance[] = Array.empty

    // Buffer containing the original instances
    let mutable keys : TraceObject[] = Array.empty

    // Locations of individual instances in the CPU buffer
    let mapping : Dict<TraceObject, int> = Dict.empty

    // Writers tracking the trace objects
    let writers : Dict<TraceObject, InstanceWriter> = Dict.empty

    // Writes the given instance to an index
    let setIndex (index : int) (key : TraceObject) =
        keys.[index] <- key
        mapping.[key] <- index

    // Applies all the pending adds and removes, and compacts the buffer
    let applyDeltas (token : AdaptiveToken) (compile : AdaptiveToken -> TraceObject -> VkGeometryInstance)
                        (adds : List<TraceObject>) (rems : List<TraceObject>) =

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

                data.[newIndex] <- data.[oldIndex]
                setIndex newIndex key

        // Resize the buffers
        data <- data.Resized newCount
        keys <- keys.Resized newCount

        for i in oldCount .. newCount - 1 do
            free.Enqueue i

        // Remove elements
        for k in rems do
            writers.Remove k |> ignore
            mapping.Remove k |> ignore

        // Add elements
        for k in adds do
            let i = free.Dequeue()

            let w = InstanceWriter(k, compile)
            w.Write(token, data, i)
            writers.[k] <- w

            setIndex i k

        assert(free.IsEmpty())

    member x.Update (token : AdaptiveToken, compile : AdaptiveToken -> TraceObject -> VkGeometryInstance) =
        x.EvaluateIfNeeded token data (fun token ->
            let deltas = reader.GetOperations token

            if deltas.Count > 0 then
                let adds = List(deltas.Count)
                let rems = List(deltas.Count)

                for d in deltas do
                    match d with
                        | Add(_, obj) -> obj |> adds.Add |> ignore
                        | Rem(_, obj) -> obj |> rems.Add |> ignore

                applyDeltas token compile adds rems

            for KeyValue(obj, w) in writers do
                w.WriteTrafo(token, data, mapping.[obj])
        
            data
        )

    member x.Dispose() =
        reader.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()