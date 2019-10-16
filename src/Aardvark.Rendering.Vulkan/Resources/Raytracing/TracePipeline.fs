namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

type ShaderGroupCreateInfo =
    | General of generalShader : uint32
    | HitGroup of anyHitShader : option<uint32> * closestHitShader : option<uint32> * intersectionShader : option<uint32>

type TracePipelineDescription = {
    layout : PipelineLayout
    stages : ShaderModule[]
    groups : ShaderGroupCreateInfo[]
    maxRecursionDepth : uint32
}

type TracePipeline =
    class
        inherit Resource<VkPipeline>
        val mutable Description : TracePipelineDescription

        new(device : Device, handle : VkPipeline, description : TracePipelineDescription) = 
            { inherit Resource<_>(device, handle); Description = description }
    end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderGroupCreateInfo =

    let private VkShaderUnused = ~~~0u

    let createGeneral (generalShader : uint32) =
        General generalShader

    let createHitGroup (anyHitShader : option<uint32>) (closestHitShader : option<uint32>) (intersectionShader : option<uint32>) =
        HitGroup (anyHitShader, closestHitShader, intersectionShader)

    let getType = function
        | General _ ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeGeneralNv
        | HitGroup (_, _, None) ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeTrianglesHitGroupNv
        | HitGroup (_, _, Some _) ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeProceduralHitGroupNv

    let getGeneralShader = function
        | General g -> g
        | _ -> VkShaderUnused

    let getAnyHitShader = function
        | HitGroup (Some anyHit, _, _) -> anyHit
        | _ -> VkShaderUnused

    let getClosestHitShader = function
        | HitGroup (_, Some closestHit, _) -> closestHit
        | _ -> VkShaderUnused

    let getIntersectionShader = function
        | HitGroup (_, _, Some intersection) -> intersection
        | _ -> VkShaderUnused
            

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TracePipelineDescription =

    let empty = {
        layout = Unchecked.defaultof<_>
        stages = [||]
        groups = [||]
        maxRecursionDepth = 0u
    }

    let create (layout : PipelineLayout) (maxRecursionDepth : uint32) =
        { empty with layout = layout
                     maxRecursionDepth = maxRecursionDepth }

    let addShaderStage (shader : ShaderModule) (desc : TracePipelineDescription) =
        let stages = Array.append desc.stages [|shader|]
        let index = uint32 <| stages.Length - 1

        let valid =
            match shader.Stage with
                | ShaderStage.Raygen
                | ShaderStage.Miss
                | ShaderStage.Callable -> true
                | _ -> false

        if not valid then
            Log.error "[Vulkan] %s" "General shader stage not valid"
            failwithf "[Vulkan] %s" "General shader stage not valid"

        let g = ShaderGroupCreateInfo.createGeneral index
        
        { desc with stages = stages
                    groups = Array.append desc.groups [|g|] }

    let addHitShaderStage (anyHit : ShaderModule option)
                          (closestHit : ShaderModule option)
                          (intersection : ShaderModule option) 
                          (desc : TracePipelineDescription) =

        let addToStages (shader : ShaderModule) (desc : TracePipelineDescription) =
            { desc with stages = Array.append desc.stages [|shader|] }

        let addToGroup (index : uint32) (stage : ShaderStage) (desc : TracePipelineDescription) =
            let anyHit, closestHit, intersection =
                match Array.last desc.groups with
                    | General _ -> failwith "cannot add hit group shader to general shader group"
                    | HitGroup (x, y, z) -> x, y, z

            let group =
                match stage with
                    | ShaderStage.AnyHit ->
                        HitGroup (Some index, closestHit, intersection)
                    | ShaderStage.ClosestHit ->
                        HitGroup (anyHit, Some index, intersection)
                    | ShaderStage.Intersection ->
                        HitGroup (closestHit, anyHit, Some index)
                    | _ ->
                        failwithf "cannot add shader of type %A to hit group" stage

            desc.groups.[desc.groups.Length - 1] <- group
            desc

        let flags = List.toArray [ 
            ShaderStage.AnyHit
            ShaderStage.ClosestHit
            ShaderStage.Intersection
        ]

        let input = [anyHit; closestHit; intersection]

        let valid =
            input
                |> List.mapi (fun i x ->
                    x |> Option.map (fun x -> x.Stage = flags.[i])
                      |> Option.defaultValue true
                ) |> List.forall id

        if not valid then
            Log.error "[Vulkan] %s" "Hit shader stage not valid"
            failwithf "[Vulkan] %s" "Hit shader stage not valid"

        let g = ShaderGroupCreateInfo.createHitGroup None None None

        input |> List.fold (fun d s -> 
            match s with
                | None -> d
                | Some s -> 
                    let index = uint32 d.stages.Length
                    d |> addToStages s |> addToGroup index s.Stage
        ) { desc with groups = Array.append desc.groups [|g|] }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TracePipeline =

    let create (device : Device) (desc : TracePipelineDescription) =
    
        let handle =
            native {
                let! pName = "main"
                let! pStages = desc.stages |> Array.map (fun s ->
                    VkPipelineShaderStageCreateInfo(
                        VkStructureType.PipelineShaderStageCreateInfo, 0n,
                        VkPipelineShaderStageCreateFlags.MinValue,
                        VkShaderStageFlags.ofShaderStage s.Stage,
                        s.Handle,
                        pName, NativePtr.zero
                    )
                )

                let! pGroups = desc.groups |> Array.map (fun g ->
                    VkRayTracingShaderGroupCreateInfoNV(
                        VkStructureType.RayTracingShaderGroupCreateInfoNv, 0n,
                        ShaderGroupCreateInfo.getType g,
                        ShaderGroupCreateInfo.getGeneralShader g,
                        ShaderGroupCreateInfo.getClosestHitShader g,
                        ShaderGroupCreateInfo.getAnyHitShader g,
                        ShaderGroupCreateInfo.getIntersectionShader g
                    )
                )
                
                let! pInfo =
                    VkRayTracingPipelineCreateInfoNV(
                        VkStructureType.RayTracingPipelineCreateInfoNv, 0n,
                        VkPipelineCreateFlags.None,
                        uint32 desc.stages.Length, pStages, 
                        uint32 desc.groups.Length, pGroups,
                        desc.maxRecursionDepth, desc.layout.Handle,
                        VkPipeline.Null, 0
                    )

                let! pHandle = VkPipeline.Null
                VkRaw.vkCreateRayTracingPipelinesNV(device.Handle, VkPipelineCache.Null, 1u, pInfo, NativePtr.zero, pHandle)
                    |> check "Failed to create raytraying pipeline"

                return !!pHandle
            }

        new TracePipeline(device, handle, desc)

    let getShaderBindingTableEntries (p : TracePipeline) =
        let entries = ShaderBindingTableEntries.empty
        p.Description.groups |> Array.fold (fun e g ->
            let kind = 
                match g with
                    | HitGroup _ ->
                        ShaderBindingTableEntryType.HitGroup
                    | General index ->
                        ShaderBindingTableEntryType.ofShaderStage p.Description.stages.[int index].Stage
           
            e |> ShaderBindingTableEntries.add kind Array.empty
        ) entries

    let delete (p : TracePipeline) =
        if p.Handle.IsValid then
            VkRaw.vkDestroyPipeline(p.Device.Handle, p.Handle, NativePtr.zero)
            p.Handle <- VkPipeline.Null