namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan

open System
open System.Collections.Generic

type ImageSamplerArray(name : Symbol, createImageSampler : IMod<ITexture> -> IResourceLocation<Resources.ImageSampler>) =

    let mutable array = [||]

    // Samplers for each slot in the array
    let slots = Dictionary<int, IResourceLocation<Resources.ImageSampler>>()
    
    // Indices for the currently stored objects
    let mapping = Dictionary<TraceObject, int>()

    // Remove a resource from the given dictionary if it exists
    let remove (obj : TraceObject) =
        match mapping.TryGetValue obj with
        | true, i ->
            let r = slots.[i]
            mapping.Remove obj |> ignore
            slots.Remove i |> ignore
            r.Release()
        | _ -> ()

    // Add a resource to the given dictionary
    let add (index : int) (obj : TraceObject) (r : IResourceLocation<_>) =
        r.Acquire()
        remove obj
        slots.[index] <- r
        mapping.[obj] <- index

    member x.ApplyChanges(indices : IndexPool, added : TraceObject seq, removed : TraceObject seq) =
        for obj in added do
            match obj.Textures.TryGetValue name with
            | true, texture ->
                let s = createImageSampler texture
                add (indices.Get obj) obj s
            | _ ->
                Log.warn "Trace object missing texture '%A'" name

        for obj in removed do
            remove obj

        array <- Dictionary.toArray slots

    member x.Dispose() =
        for (_, s) in array do
            s.Release()

    member x.Data =
        array

    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ImageSamplerArray =

    let create (createImageSampler : IMod<ITexture> -> IResourceLocation<Resources.ImageSampler>) (name : Symbol) =
        new ImageSamplerArray(name, createImageSampler)

    let delete (array : ImageSamplerArray) =
        array.Dispose()

    let applyChanges (indices : IndexPool) (added : TraceObject seq) (removed : TraceObject seq) (array : ImageSamplerArray) =
        array.ApplyChanges(indices, added, removed) 