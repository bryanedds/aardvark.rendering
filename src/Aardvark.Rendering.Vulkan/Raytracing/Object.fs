namespace Aardvark.Rendering.Vulkan.Raytracing

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

[<AutoOpen>]
module private ObjectHelpers =
    let newId() =
        (RenderObject.Create()).Id

[<CustomEquality>]
[<CustomComparison>]
type Object =
    {
        Id                          : int
        mutable AttributeScope      : Ag.Scope

        mutable HitGroup            : int
        mutable Indices             : int[]
        mutable Positions           : V3f[]
        mutable VertexAttributes    : Symbol -> System.Array
        mutable Trafo               : IMod<Trafo3d>
        mutable Uniforms            : IUniformProvider
    }

    interface IRenderObject with
        member x.Id = x.Id
        member x.RenderPass = RenderPass.main
        member x.AttributeScope = x.AttributeScope

    static member Create() =
        { Id = newId()
          AttributeScope = Ag.emptyScope
          HitGroup = -1
          Indices = null
          Positions = null
          VertexAttributes = fun _ -> null
          Trafo = null
          Uniforms = null
        }

    static member Clone(org : Object) =
        { org with Id = newId() }

    override x.GetHashCode() = x.Id
    override x.Equals o =
        match o with
            | :? Object as o -> x.Id = o.Id
            | _ -> false

    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? Object as o -> compare x.Id o.Id
                | _ -> failwith "uncomparable"

    
    (*let getPositions (ro : RenderObject) =
        match ro.VertexAttributes.TryGetAttribute DefaultSemantic.Positions with
        | Some bv -> 
            match bv.Buffer.GetValue() with
            | :? ArrayBuffer as b -> b.Data |> unbox<V3f[]> |> Some
            | _ -> None
        | None ->
            None*)