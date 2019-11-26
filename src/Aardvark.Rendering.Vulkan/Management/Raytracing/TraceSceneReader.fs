namespace Aardvark.Rendering.Vulkan.Raytracing

open System
open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Incremental

/// Utiltity class that constructs a reader of the objects aset in the given scene
/// and calls ApplyChanges with the newly added and removed objects as parameters.
[<AbstractClass>]
type TraceSceneReader(scene : TraceScene) =
    inherit AdaptiveObject()

    let reader = scene.Objects.GetReader()

    /// Called by the Update function with added and removed objects as parameters.
    abstract member ApplyChanges : AdaptiveToken * TraceObject seq * TraceObject seq -> unit

    member x.Update(token : AdaptiveToken) =
        x.EvaluateIfNeeded token () (fun token ->
            let deltas = reader.GetOperations token

            let added = List(deltas.Count)
            let removed = List(deltas.Count)

            for d in deltas do
                match d with
                | Add(_, obj) -> obj |> added.Add |> ignore
                | Rem(_, obj) -> obj |> removed.Add |> ignore

            x.ApplyChanges(token, added, removed)
        )

    member x.Scene = scene

    member x.Dispose() =
        reader.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()