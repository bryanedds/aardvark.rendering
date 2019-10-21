namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

type TracePipelineDescription = {
    layout : PipelineLayout
    stages : ShaderModule[]
    groups : ShaderGroup<uint32>[]
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

    let getType = function
        | Raygen _ 
        | Miss _
        | Callable _ ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeGeneralNv
        | HitGroup (_, _, None) ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeTrianglesHitGroupNv
        | HitGroup (_, _, Some _) ->
            VkRayTracingShaderGroupTypeNV.VkRayTracingShaderGroupTypeProceduralHitGroupNv

    let getGeneralShader = function
        | Raygen g 
        | Miss g
        | Callable g -> g
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

        let valid =
            match shader.Stage with
                | ShaderStage.Raygen
                | ShaderStage.Miss
                | ShaderStage.Callable
                | ShaderStage.AnyHit
                | ShaderStage.ClosestHit 
                | ShaderStage.Intersection -> true
                | _ -> false

        if not valid then
            Log.error "[Vulkan] %s" "Shader stage not valid"
            failwithf "[Vulkan] %s" "Shader stage not valid"
        
        { desc with stages = stages }

    let addShaderGroup (group : ShaderGroup<uint32>) (desc : TracePipelineDescription) =
        { desc with groups = Array.append desc.groups [|group|] }

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
                    | Raygen _ ->
                        ShaderBindingTableEntryType.Raygen
                    | Miss _ ->
                        ShaderBindingTableEntryType.Miss
                    | Callable _ ->
                        ShaderBindingTableEntryType.Callable
           
            e |> ShaderBindingTableEntries.add kind Array.empty
        ) entries

    let delete (p : TracePipeline) =
        if p.Handle.IsValid then
            VkRaw.vkDestroyPipeline(p.Device.Handle, p.Handle, NativePtr.zero)
            p.Handle <- VkPipeline.Null