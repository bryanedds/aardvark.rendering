namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Base.Monads.State

open System

open FShade

[<AutoOpen>]
module private ShaderPoolHelpers =

    module ModifyState =

        let set (v : bool) : State<bool, unit> =
            State.modify(fun m -> m || v)

    // TODO: Remove this once FShade can handle RTX shaders
    let dummyShaderModule (device : Device) (stage : Aardvark.Base.ShaderStage) (code : byte[]) =
        let handle =
            native {
                let! pCode = code
                let! pInfo =
                    VkShaderModuleCreateInfo(
                        VkStructureType.ShaderModuleCreateInfo, 0n, 
                        VkShaderModuleCreateFlags.MinValue, 
                        uint64 code.Length, NativePtr.cast pCode
                    )

                let! pHandle = VkShaderModule.Null
                VkRaw.vkCreateShaderModule(device.Handle, pInfo, NativePtr.zero, pHandle)
                    |> check "failed to create shader module"
                return !!pHandle
            }

        new ShaderModule(device, handle, stage, Map.empty, code)

type ShaderPool(device : Device, scene : TraceScene) as this =
    inherit AdaptiveObject()

    let reader = scene.Objects.GetReader()

    // Dict mapping source to compiled modules
    // Duplicate sources are removed
    let modules =
        let createCache stage shaders =
            shaders |> List.distinct
                    |> List.map (fun source -> source, dummyShaderModule device stage source)
                    |> Dict.ofList

        let cache = Dict.empty

        cache.[ShaderStage.Raygen] <-
            [scene.RaygenShader.binary] |> createCache ShaderStage.Raygen

        cache.[ShaderStage.Miss] <-
            scene.MissShaders |> List.map TraceShader.binary |> createCache ShaderStage.Miss

        cache.[ShaderStage.Callable] <-
            scene.CallableShaders |> List.map TraceShader.binary |> createCache ShaderStage.Callable

        cache.[ShaderStage.AnyHit] <- Dict.empty
        cache.[ShaderStage.ClosestHit] <- Dict.empty             
        cache.[ShaderStage.Intersection] <- Dict.empty

        cache

    // List of shader groups
    // Identical hit groups are removed
    let groups = HashSet.ofList [
        yield scene.RaygenShader |> TraceShader.binary |> Raygen
        yield! scene.MissShaders |> List.map (TraceShader.binary >> Miss)
        yield! scene.CallableShaders |> List.map (TraceShader.binary >> Callable)
    ]

    // (Local) indices of the hit groups (e.g. the first hit group has index 0
    // even if it is not the first shader group over all. Used in the
    // instanceOffset field of the VkGeometryInstance struct
    let hitGroupIndices = Dict.empty

    let add (obj : TraceObject) =

        let addModule stage source =
            if not (modules.[stage].ContainsKey source) then
                modules.[stage].[source] <- dummyShaderModule device stage source

        // Add modules
        obj.AnyHitShader |> Option.iter (TraceShader.binary >> addModule ShaderStage.AnyHit)
        obj.ClosestHitShader |> Option.iter (TraceShader.binary >> addModule ShaderStage.ClosestHit)
        obj.IntersectionShader |> Option.iter (TraceShader.binary >> addModule ShaderStage.Intersection)

        // Add hit group
        let g = ShaderGroup.ofTraceObject obj
        groups |> HashSet.add g |> ModifyState.set

    let rem (obj : TraceObject) =
        // TODO: Do even want to do something here?
        ModifyState.set false

    let update token =
        this.EvaluateIfNeeded token false (fun token ->
            let deltas = reader.GetOperations token

            let modified =
                state {
                    for d in deltas do
                        match d with
                            | Add(_, obj) -> do! add obj
                            | Rem(_, obj) -> do! rem obj

                } |> State.run false |> fst
            
            // Update hit group indices
            if modified then
                groups |> Seq.filter ShaderGroup.isHitGroup
                       |> Seq.iteri (fun i g -> hitGroupIndices.[g] <- i)

            modified
        )

    let getHitGroupIndex (obj : TraceObject) =
        let g = ShaderGroup.ofTraceObject obj
        hitGroupIndices.[g]

    let getPipelineDescription (desc : TracePipelineDescription) =
        // Keep track of stage indices
        let indices = Dict.empty
        
        let lookup stage source =
            indices.[(stage, source)]

        // Add stages to description
        let mutable description = desc

        for KeyValue(stage, perStage) in modules do
            for KeyValue(source, shader) in perStage do

                indices.[(stage, source)] <- uint32 description.stages.Length
                description <- TracePipelineDescription.addShaderStage shader description
                
        // Add groups referencing the indices
        for group in groups do
            let group = ShaderGroup.mapWithStage lookup group
            description <- TracePipelineDescription.addShaderGroup group description

        description

    /// Updates the shader pool and returns if it has been
    /// modified
    member x.Update (token : AdaptiveToken) =
        update token

    member x.GetHitGroupIndex (obj : TraceObject) =
        getHitGroupIndex obj

    member x.GetPipelineDescription (desc : TracePipelineDescription) =
        getPipelineDescription desc

    member x.Dispose() =
        reader.Dispose()

        modules.Values |> Seq.iter (fun dict ->
            dict.Values |> Seq.iter (fun g -> ShaderModule.delete g device)
        ) 

    interface IDisposable with
        member x.Dispose() = x.Dispose()