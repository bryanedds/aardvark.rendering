namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan

open System
open System.Collections.Generic

type TraceResourceManager(manager : ResourceManager, scene : TraceScene,
                            shaderParams : TraceSceneShaderParams, indices : IndexPool) =
    inherit AdaptiveObject()
    
    // Resources that need to be acquired and updated
    let resources = new ResourceLocationSet(manager.ResourceUser)

    // Objects that have to be disposed
    let disposables = List<IDisposable>()

    // Callbacks invoked when Dispose is called
    let disposeCallbacks = List<unit -> unit>()

    // The shader params are updated for per object resources (e.g. sampler count is adjusted)
    let mutable shaderParams = shaderParams

    let choose f dict =
        dict |> Seq.map (fun (KeyValue(name, value)) -> f name value)
             |> Seq.choose id
             |> SymDict.ofSeq

    // Initialize dictionary with images that are required and provided
    // as global resources in the scene, do the same for other resource types as well
    let images =
        shaderParams.storageImages |> choose (fun name img ->
            scene.Textures |> SymDict.tryFind name |> Option.map (fun tex ->
                let image = manager.CreateImage(tex) 
                let view = manager.CreateImageView(img.imageType, image)
                name, view
            )
        )

    // Storage buffers are either global, or attribute buffers that obtain their data from
    // the objects in the scene
    let storageBuffers =
        let createAttributeBuffer (name : Symbol) =
            let b = new AttributeArray(name, indices)
            disposables.Add(b)
            b.Data :> IMod

        shaderParams.storageBuffers |> choose (fun name layout ->
            let buffer =
                scene.Buffers
                |> SymDict.tryFind name
                |> Option.map unbox
                |> Option.defaultWith (fun _ -> createAttributeBuffer name)

            Some (name, manager.CreateStorageBuffer(buffer))
        )

    let uniformBuffers =
        shaderParams.uniformBuffers |> choose (fun name layout ->
            scene.Globals |> SymDict.tryFind name |> Option.map (fun value ->
                name, manager.CreateUniformBuffer(layout, value)
            )
        )

    let samplers =
        let createGlobalSamplerArray samplerType textures =
            textures
            |> List.choosei (fun i (t, d) ->
                match t with
                | Some t ->
                    Some (i,  manager.CreateImageSampler(samplerType, t, d))
                | _ ->
                    None
            ) |> AMap.ofList

        let createObjectSamplerArray name samplerType samplerState =
            // TODO: Compute the maximum number of descriptors possible accoring to the
            // device limits: https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipelineLayoutCreateInfo.html
            // Or let the user specify the max?
            shaderParams <- shaderParams |> TraceSceneShaderParams.setSamplerCount name 4096
            let array = new ImageSamplerArray(manager, name, indices, samplerType, samplerState)
            disposables.Add array
            array.Data

        shaderParams.samplers |> choose (fun name sampler ->
            match sampler.samplerTextures with
            | [] ->
                Log.warn "could not get sampler information for: %A" sampler
                None

            | textures -> 
                // Retrieve textures from scene textures
                let resolved =
                    textures |> List.map (fun (name, state) ->
                        let r = scene.Textures |> SymDict.tryFind (Symbol.Create name)
                        let s = Mod.constant state.SamplerStateDescription
                        r, s
                    )

                // If we don't find any textures we assume that it's supposed to
                // be a per object texture array
                let hasTexture = fst >> Option.isSome

                let map =
                    if resolved |> List.exists hasTexture then   
                        resolved |> createGlobalSamplerArray sampler.samplerType
                    else
                        let samplerState = resolved |> List.head |> snd
                        createObjectSamplerArray name sampler.samplerType samplerState

                Some (name, map)
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

    /// Returns the image with the given name
    member x.ImageSampler(name : Symbol) =
        samplers.[name]

    /// Returns the image with the given name
    member x.ImageSampler(name : string) =
        x.ImageSampler(Symbol.Create name)

    member x.ShaderParams =
        shaderParams

    member x.Dispose() =
        for x in images do x.Value.ReleaseAll()
        for x in storageBuffers do x.Value.ReleaseAll()
        for x in uniformBuffers do x.Value.ReleaseAll()
        for x in disposables do x.Dispose()
        for f in disposeCallbacks do f()

    interface IDisposable with
        member x.Dispose() = x.Dispose()