namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Rendering.Vulkan

// The shader pool contains all the shaders included in the scene
// Duplicate shaders are removed at two levels: first duplicate code is only compiled to
// a module once, second duplicate hit groups are removed completely
type ShaderPool<'source, 'compiled when 'source : comparison> = {

    // Dict mapping source to compiled modules
    // Duplicate sources are removed
    modules : Dict<ShaderStage, Dict<'source, 'compiled>>

    // List of shader groups
    // Identical hit groups are removed
    groups : ShaderGroup<'source> list

    // (Local) indices of the hit groups (e.g. the first hit group has index 0
    // even if it is not the first shader group over all. Used in the
    // instanceOffset field of the VkGeometryInstance struct
    hitGroupIndices  : Dict<ShaderGroup<'source>, int>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderPool =

    // TODO: Remove this once FShade can handle RTX shaders
    let private dummyShaderModule (device : Device) (stage : Aardvark.Base.ShaderStage) (code : byte[]) =
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

    let create (device : Device) (scene : TraceScene) =

        let createCache stage shaders =
            shaders |> List.distinct
                    |> List.map (fun source -> source, dummyShaderModule device stage source)
                    |> Dict.ofList

        // Create cache to lookup shader modules based on stage and source
        let cache = Dict.empty

        cache.[ShaderStage.Raygen] <-
            [scene.RaygenShader] |> createCache ShaderStage.Raygen

        cache.[ShaderStage.Miss] <-
            scene.MissShaders |> createCache ShaderStage.Miss

        cache.[ShaderStage.Callable] <-
            scene.CallableShaders |> createCache ShaderStage.Callable

        cache.[ShaderStage.AnyHit] <-
            scene.Objects |> List.choose (fun o -> o.AnyHitShader)
                          |> createCache ShaderStage.AnyHit
                          
        cache.[ShaderStage.ClosestHit] <-
            scene.Objects |> List.choose (fun o -> o.ClosestHitShader)
                          |> createCache ShaderStage.ClosestHit
                          
        cache.[ShaderStage.Intersection] <-
            scene.Objects |> List.choose (fun o -> o.IntersectionShader)
                          |> createCache ShaderStage.Intersection        

        // Remove duplicate hit groups
        let hitGroups =
            scene.Objects |> List.map ShaderGroup.ofTraceObject
                          |> List.distinct

        // Create shader groups
        let shaderGroups =
            [
                scene.RaygenShader |> (Raygen >> List.singleton)
                scene.MissShaders |> List.map Miss
                scene.CallableShaders |> List.map Callable
                hitGroups
            ] 
            |> List.concat

        {
            groups          = shaderGroups
            modules         = cache
            hitGroupIndices = hitGroups |> List.indexed
                                        |> List.map (fun (i, x) -> x, i)
                                        |> Dict.ofList
        }

    let addToPipelineDescription (pool : ShaderPool<_,_>) (desc : TracePipelineDescription) =

        // Keep track of stage indices
        let indices = Dict.empty
        
        let lookup stage source =
            indices.[(stage, source)]

        // Add stages to description
        let mutable description = desc

        for KeyValue(stage, perStage) in pool.modules do
            for KeyValue(source, shader) in perStage do

                indices.[(stage, source)] <- uint32 description.stages.Length
                description <- TracePipelineDescription.addShaderStage shader description
                
        // Add groups referencing the indices
        for group in pool.groups do
            let group = ShaderGroup.mapWithStage lookup group
            description <- TracePipelineDescription.addShaderGroup group description

        description

    let delete (device : Device) (pool : ShaderPool<_,_>) =
        pool.modules.Values |> Seq.iter (fun dict ->
            dict.Values |> Seq.iter (fun g -> ShaderModule.delete g device)
        ) 

    let getHitGroupIndex (obj : TraceObject) (pool : ShaderPool<_,_>) =
        let g = ShaderGroup.ofTraceObject obj
        pool.hitGroupIndices.[g]