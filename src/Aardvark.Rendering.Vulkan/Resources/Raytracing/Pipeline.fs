namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

type ShaderStageCreateInfo = {
    shaderModule : ShaderModule
}

type HitGroupType =
    | Triangles
    | Procedural

type HitGroupData = {
    kind : HitGroupType
    closestHitShader : option<uint32>
    anyHitShader : option<uint32>
    intersectionShader : option<uint32>
}

type ShaderGroupCreateInfo =
    | General of generalShader : uint32
    | HitGroup of data : HitGroupData

type PipelineDescription = {
    layout : PipelineLayout
    stages : ShaderStageCreateInfo[]
    groups : ShaderGroupCreateInfo[]
    maxRecursionDepth : uint32
}

type Pipeline =
    class
        inherit Resource<VkPipeline>
        val mutable Description : PipelineDescription

        new(device : Device, handle : VkPipeline, description : PipelineDescription) = 
            { inherit Resource<_>(device, handle); Description = description }
    end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderStageCreateInfo =

    let create (shaderModule : ShaderModule) =
        { shaderModule = shaderModule }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderGroupCreateInfo =

    let private VkShaderUnused = ~~~0u

    let createGeneral (generalShader : uint32) =
        General generalShader

    let createHitGroup (closestHitShader : option<uint32>) (anyHitShader : option<uint32>) (intersectionShader : option<uint32>) =
        HitGroup {
            kind = Triangles
            closestHitShader = closestHitShader
            anyHitShader = anyHitShader
            intersectionShader = intersectionShader
        }

    let getType = function
        | General _ ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeGeneralNv
        | HitGroup d ->
            match d.kind with
                | Triangles -> VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeTrianglesHitGroupNv
                | Procedural -> VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeProceduralHitGroupNv

    let getGeneralShader = function
        | General g -> g
        | _ -> VkShaderUnused

    let getClosestHitShader = function
        | General _ -> VkShaderUnused
        | HitGroup d -> d.closestHitShader |> Option.defaultValue VkShaderUnused

    let getAnyHitShader = function
        | General _ -> VkShaderUnused
        | HitGroup d -> d.anyHitShader |> Option.defaultValue VkShaderUnused

    let getIntersectionShader = function
        | General _ -> VkShaderUnused
        | HitGroup d -> d.intersectionShader |> Option.defaultValue VkShaderUnused
            

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PipelineDescription =

    let empty = {
        layout = Unchecked.defaultof<_>
        stages = [||]
        groups = [||]
        maxRecursionDepth = 0u
    }

    let create (layout : PipelineLayout) (maxRecursionDepth : uint32) =
        { empty with layout = layout
                     maxRecursionDepth = maxRecursionDepth }

    let addShaderStage (shaderModule : ShaderModule) (desc : PipelineDescription) =
        let stage = ShaderStageCreateInfo.create shaderModule
        let stages = desc.stages |> Array.append [|stage|]
        let index = uint32 <| stages.Length - 1

        let valid =
            match shaderModule.Stage with
                | ShaderStage.Raygen
                | ShaderStage.Miss
                | ShaderStage.Callable -> true
                | _ -> false

        if not valid then
            Log.error "[Vulkan] %s" "General shader stage not valid"
            failwithf "[Vulkan] %s" "General shader stage not valid"

        let g = ShaderGroupCreateInfo.createGeneral index
        
        { desc with stages = stages
                    groups = desc.groups |> Array.append [|g|] }

    let addHitShaderStage (anyHit : ShaderStageCreateInfo option)
                          (closestHit : ShaderStageCreateInfo option)
                          (intersection : ShaderStageCreateInfo option) 
                          (desc : PipelineDescription) =

        let addToStages (stage : ShaderStageCreateInfo) (desc : PipelineDescription) =
            { desc with stages = desc.stages |> Array.append [|stage|] }

        let addToGroup (index : uint32) (stage : ShaderStage) (desc : PipelineDescription) =
            let data =
                match Array.last desc.groups with
                    | General _ -> failwith "cannot add hit group shader to general shader group"
                    | HitGroup data -> data

            let group = 
                match stage with
                    | ShaderStage.AnyHit ->
                        { data with anyHitShader = Some index }
                    | ShaderStage.ClosestHit ->
                        { data with closestHitShader = Some index }
                    | ShaderStage.Intersection ->
                        { data with kind = HitGroupType.Procedural; intersectionShader = Some index }
                    | _ ->
                        failwithf "cannot add shader of type %A to hit group" stage
                |> HitGroup

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
                    x |> Option.map (fun x -> x.shaderModule.Stage = flags.[i])
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
                    d |> addToStages s |> addToGroup index s.shaderModule.Stage
        ) { desc with groups = desc.groups |> Array.append [|g|] }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pipeline =

    let create (device : Device) (desc : PipelineDescription) =

        let handle =
            native {
                let! pName = "main"
                let! pStages = desc.stages |> Array.map (fun s ->
                    VkPipelineShaderStageCreateInfo(
                        VkStructureType.PipelineShaderStageCreateInfo, 0n,
                        VkPipelineShaderStageCreateFlags.MinValue,
                        VkShaderStageFlags.RaygenBitNv, s.shaderModule.Handle,
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

        new Pipeline(device, handle, desc)

    let getShaderBindingTableEntries (p : Pipeline) =
        let entries = ShaderBindingTableEntries.empty
        p.Description.groups |> Array.fold (fun e g ->
            let kind = 
                match g with
                    | HitGroup _ ->
                        ShaderBindingTableEntryType.HitGroup
                    | General index ->
                        ShaderBindingTableEntryType.ofShaderStage p.Description.stages.[int index].shaderModule.Stage
           
            e |> ShaderBindingTableEntries.add kind Array.empty
        ) entries

    let delete (p : Pipeline) =
        if p.Handle.IsValid then
            VkRaw.vkDestroyPipeline(p.Device.Handle, p.Handle, NativePtr.zero)
            p.Handle <- VkPipeline.Null