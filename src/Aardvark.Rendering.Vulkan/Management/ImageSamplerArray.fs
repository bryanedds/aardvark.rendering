namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.Rendering.Vulkan
open System.Collections.Generic
open Aardvark.Base.Rendering

open FShade
open FShade.GLSL

type ImageSamplerArray(manager : ResourceManager, name : Symbol, indices : IndexPool,
                            samplerType : GLSLSamplerType, samplerState : IMod<SamplerStateDescription>) =
    inherit TraceSceneReader(indices.Scene)

    // Samplers for each slot in the array
    //let samplers = CMap.empty
    let mutable samplers = HMap.empty
    
    // Indices for the currently stored objects
    let mapping = Dictionary<TraceObject, int>()

    let add (obj : TraceObject) (texture : IMod<ITexture>) =
        let i = indices.Get obj
        let s = manager.CreateImageSampler(samplerType, texture, samplerState)
        mapping.[obj] <- i
        samplers <- samplers.Add(i, s)

    let remove (obj : TraceObject) =
        let i = mapping.[obj]
        mapping.Remove obj |> ignore
        samplers <- samplers.Remove i

    override x.ApplyChanges(token, added, removed) =
        indices.Update token

        for obj in added do
            match obj.Textures.TryGetValue name with
            | true, texture ->
                add obj texture
            | _ ->
                Log.warn "Trace object missing texture '%A'" name

        for obj in removed do
            remove obj

    member x.Data =
        Mod.custom (fun token ->
            x.Update token
            samplers
        ) |> AMap.ofMod