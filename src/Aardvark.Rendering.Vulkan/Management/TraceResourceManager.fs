namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan

open System
open System.Collections.Generic

type TraceResourceManager(manager : ResourceManager, scene : TraceScene,
                            bindings : DescriptorSetLayoutBinding[], indices : IndexPool) =
    inherit AdaptiveObject()
    
    // Resources that need to be acquired and updated
    let resources = new ResourceLocationSet(manager.ResourceUser)

    // Objects that have to be disposed
    let disposables = List<IDisposable>()

    // Callbacks invoked when Dispose is called
    let disposeCallbacks = List<unit -> unit>()

    let collect f =
        bindings |> Array.choose (fun b -> f b.Parameter)
                 |> SymDict.ofArray

    // Initialize dictionary with images that are required and provided
    // as global resources in the scene, do the same for other resource types as well
    let images =
        collect (function
            | ImageParameter img ->
                let name = Symbol.Create img.imageName
                scene.Textures |> SymDict.tryFind name |> Option.map (fun tex ->
                    let image = manager.CreateImage(tex) 
                    let view = manager.CreateImageView(img.imageType, image)
                    name, view
                )
            | _ -> None
        )

    // Storage buffers are either global, or attribute buffers that obtain their data from
    // the objects in the scene
    let storageBuffers =
        let createAttributeBuffer (name : Symbol) =
            let b = new AttributeArray(name, indices)
            disposables.Add(b)
            b.Data :> IMod

        collect (function
            | StorageBufferParameter layout ->
                let name = Symbol.Create layout.ssbName

                let buffer =
                    scene.Buffers
                    |> SymDict.tryFind name
                    |> Option.map unbox
                    |> Option.defaultWith (fun _ -> createAttributeBuffer name)

                Some (name, manager.CreateStorageBuffer(buffer))
            | _ ->
                None
        )

    let uniformBuffers =
        collect (function
            | UniformBlockParameter layout ->
                let name = Symbol.Create layout.ubName
                scene.Globals |> SymDict.tryFind name |> Option.map (fun value ->
                    name, manager.CreateUniformBuffer(layout, value)
                )
            | _ -> None
        )

    member x.Update(token : AdaptiveToken) =
        x.EvaluateIfNeeded token () (fun token ->
            resources.Update token |> ignore
        )

    member x.Add(r : IResourceLocation) =
        resources.Add(r)

    member x.Add(d : IDisposable) =
        disposables.Add(d)

    member x.Add(f : unit -> unit) =
        disposeCallbacks.Add(f)

    /// Returns the image with the given name
    member x.Image(name : Symbol) =
        images.[name]

    /// Returns the image with the given name
    member x.Image(name : string) =
        x.Image(Symbol.Create name)

    /// Returns the storage buffer with the given name
    member x.StorageBuffer(name : Symbol) =
        storageBuffers.[name]

    /// Returns the storage buffer with the given name
    member x.StorageBuffer(name : string) =
        x.StorageBuffer(Symbol.Create name)

    /// Returns the uniform buffer with the given name
    member x.UniformBuffer(name : Symbol) =
        uniformBuffers.[name]

    /// Returns the uniform buffer with the given name
    member x.UniformBuffer(name : string) =
        x.UniformBuffer(Symbol.Create name)

    member x.Dispose() =
        for x in images do x.Value.ReleaseAll()
        for x in storageBuffers do x.Value.ReleaseAll()
        for x in uniformBuffers do x.Value.ReleaseAll()
        for x in disposables do x.Dispose()
        for f in disposeCallbacks do f()

    interface IDisposable with
        member x.Dispose() = x.Dispose()