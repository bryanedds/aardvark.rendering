namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base

// A shader group is either a single raygen, miss, or callable shader respectively
// or a hit group containing an optional any-hit, closest-hit, and intersection shader
type ShaderGroup<'a> =
    | Raygen of 'a
    | Miss of 'a
    | Callable of 'a
    | HitGroup of anyHit : Option<'a> * closestHit : Option<'a> * intersection : Option<'a>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderGroup =

    let isHitGroup = function
        | HitGroup _ -> true
        | _ -> false

    let ofTraceObject (obj : TraceObject) =
        HitGroup (obj.anyHitShader, obj.closestHitShader, obj.intersectionShader)

    let mapWithStage (f : ShaderStage -> 'a -> 'b) = function
        | Raygen x              -> Raygen (f ShaderStage.Raygen x)
        | Miss x                -> Miss (f ShaderStage.Miss x)
        | Callable x            -> Callable (f ShaderStage.Callable x)
        | HitGroup (x, y, z)    -> HitGroup (
                                        x |> Option.map (f ShaderStage.AnyHit),
                                        y |> Option.map (f ShaderStage.ClosestHit),
                                        z |> Option.map (f ShaderStage.Intersection)
                                    )

    let fold (f : 's -> 'a -> 's) (g : 's -> 'a option -> 'a option -> 'a option -> 's) (s : 's) = function
        | Raygen x
        | Miss x
        | Callable x ->
            f s x
        | HitGroup (x, y, z)  ->
            g s x y z

    let iter (f : 'a -> unit) (g : 'a option -> 'a option -> 'a option -> unit) =
        fold (fun _ x -> f x) (fun _ x y z -> g x y z) ()

    let iter' (f : 'a -> unit) =
        let g x y z =
            [x; y; z] |> List.iter (Option.iter f)

        iter f g