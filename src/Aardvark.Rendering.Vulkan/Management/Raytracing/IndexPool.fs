namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open System.Collections.Generic

type IndexPool(scene : TraceScene) =
    inherit TraceSceneReader(scene)

    // Next highest index
    let mutable nextIndex = 0

    // Indices that can be (re-)assigned
    let freeIndices = Queue<int>()

    // Current set of objects and assigned indices
    let indices = Dictionary<TraceObject, int>()

    // Removes objects and assigns indices to newly added ones
    override x.ApplyChanges(_ : AdaptiveToken, added : TraceObject seq, removed : TraceObject seq) : unit =

        for k in removed do
            match indices.TryGetValue k with
            | true, i ->
                freeIndices.Enqueue i
                indices.Remove k |> ignore
            | _ -> ()

        for _ in 1 .. (Seq.length added - freeIndices.Count) do
            let i = nextIndex
            inc &nextIndex
            freeIndices.Enqueue i

        for k in added do
            indices.[k] <- freeIndices.Dequeue()

    /// Returns all objects currently present in the pool
    member x.Objects = indices.Keys :> TraceObject seq

    /// Returns the unique index of the given object
    member x.Get(obj : TraceObject) =
        indices.[obj]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexPool =

    let create (scene : TraceScene) =
        new IndexPool(scene)

    let destroy (pool : IndexPool) =
        pool.Dispose()