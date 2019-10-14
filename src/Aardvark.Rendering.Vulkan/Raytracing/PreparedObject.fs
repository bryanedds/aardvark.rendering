namespace Aardvark.Rendering.Vulkan.Raytracing

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Raytracing

type PreparedObject =
    {
        device                  : Device
        original                : Object
        
        resources               : list<IResourceLocation>

        indexBuffer             : IResourceLocation<Buffer>
        vertexBuffer            : IResourceLocation<Buffer>
        bottomLevelAS           : IResourceLocation<AccelerationStructure>
    }

    member x.Dispose() =
        for r in x.resources do r.Release()

    member x.Update(caller : AdaptiveToken, token : RenderToken) =
        for r in x.resources do r.Update(caller) |> ignore

    member x.IncrementReferenceCount() =
        for r in x.resources do r.Acquire()

    interface IDisposable with
        member x.Dispose() = x.Dispose()
        
namespace Aardvark.Rendering.Vulkan.Raytracing.Extensions

open System.Runtime.CompilerServices
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Raytracing

[<AbstractClass; Sealed; Extension>]
type ResourceManagerPreparedObjectExtensions private() =

    static let prepareObject (this : ResourceManager) (renderPass : RenderPass) (o : Object) =

        let resources = System.Collections.Generic.List<IResourceLocation>()

        let vbView = BufferView.ofArray o.Positions
        let vb = this.CreateBuffer vbView.Buffer
        resources.Add vb

        let ibView = BufferView.ofArray o.Indices
        let ib = this.CreateIndexBuffer ibView.Buffer
        resources.Add ib

        (*let info =
            Mod.custom (fun t ->
                let vb = (vb.Update t).handle
                let ib = (ib.Update t).handle

                BottomLevelDescription.indexedTriangles vb VkFormat.R32g32b32Sfloat (uint32 o.Indices.Length) ib VkIndexType.Uint32
                    |> BottomLevel
            )
 
        let blas = this.CreateAccelerationStructure(info)
        resources.Add blas*)

        {
            device = this.Device
            original = o
            resources = CSharpList.toList resources
            indexBuffer = ib
            vertexBuffer = vb
            bottomLevelAS = Unchecked.defaultof<_>
        }

    [<Extension>]
    static member PrepareRaytracingObject(this : ResourceManager, renderPass : RenderPass, o : IRenderObject) =
        match o with
            | :? Object as o ->
                prepareObject this renderPass o
            | _ ->
                failf "unsupported TraceObject-type: %A" o