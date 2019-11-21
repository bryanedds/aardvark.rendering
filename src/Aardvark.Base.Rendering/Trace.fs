namespace Aardvark.Base

open System
open Aardvark.Base.Incremental
open FShade

// TODO: What am I supposed to use here?
type MyBuffer = {
    buffer : IBackendBuffer
    count : int
    offset : nativeint
    format : Type
}

/// Geometry for raytracing can be either (indexed) triangles or
/// axis-aligned bounding boxes
type TraceGeometry =
    | Triangles of vertexBuffer: MyBuffer * indexBuffer : option<MyBuffer>
    | AABBs of buffer : IBuffer<Box3f>

/// Geometries prepared in an acceleration structure
type IAccelerationStructure =
    abstract member Handle : obj
    abstract member Geometries : list<TraceGeometry>

type IAccelerationStructureRuntime =
    inherit IBufferRuntime

    abstract member CreateAccelerationStructure : list<TraceGeometry> -> IAccelerationStructure
    abstract member DeleteAccelerationStructure : IAccelerationStructure -> unit

/// An object contained in raytracing scenes
type TraceObject(transform, anyHitShader, closestHitShader, 
                 intersectionShader, geometry, userData) =

    /// The current transformation of the object
    member x.Transform : IMod<Trafo3d> = transform

    /// The any hit shader is called on the first hit
    /// that is encountered (not necessarily the closest)
    member x.AnyHitShader : TraceShader option = anyHitShader

    /// The closest hit shader is called for the closest
    /// ray-object intersection
    member x.ClosestHitShader : TraceShader option = closestHitShader

    /// The intersection shader allows the user to provide logic
    /// for the ray-object intersection (e.g. for a parametric sphere)
    member x.IntersectionShader : TraceShader option = intersectionShader

    /// The actual geometry of the object compiled to an acceleration
    /// structure
    member x.Geometry : IAccelerationStructure = geometry

    /// User data to be accessible in the shaders
    /// TODO: Adaptive
    member x.UserData : SymbolDict<obj> = userData

/// Object describing a scene to be raytraced
type TraceScene(raygenShader, missShaders, callableShaders, hitGroupShaders, objects, globals, buffers, textures) =

    /// Describes how initial rays are generated
    member x.RaygenShader : TraceShader = raygenShader

    /// Describes what happens when a ray does not intersect with
    /// any object in the scene (e.g. return constant color)
    member x.MissShaders : TraceShader list = missShaders

    /// Shaders that can be invoked from other shaders
    member x.CallableShaders : TraceShader list = callableShaders

    /// Shaders to be used in hit groups for objects in the scene have to be supplied
    /// a priori when creating the scene. All the shaders used for a scene consitute an interface
    /// that is used to create the raytracing pipeline. Consequently, the pipeline layout is known when creating the scene
    /// and does not have to be changed dynamically as new objects with new hit groups are added.
    member x.HitGroupShaders : TraceShader list = hitGroupShaders
    
    /// Objects in the scene
    member x.Objects : TraceObject aset = objects

    /// Global uniforms
    member x.Globals : SymbolDict<IMod> = globals

    /// Global buffers
    member x.Buffers : SymbolDict<IMod<IBuffer>> = buffers

    /// Global textures
    member x.Textures : SymbolDict<IMod<ITexture>> = textures

[<RequireQualifiedAccess>]
type TraceCommand =
    | TraceCmd              of size : IMod<V3i>      
    | SyncBufferCmd         of buffer : IMod<IBackendBuffer> * src : ResourceAccess seq * dst : ResourceAccess seq
    | SyncTextureCmd        of texture : IMod<IBackendTexture> * src : ResourceAccess seq * dst : ResourceAccess seq
    | TransformLayoutCmd    of texture : IMod<IBackendTexture> * layout : TextureLayout

    static member Trace(size : IMod<int>) =
        size |> Mod.map (fun x -> V3i(x, 1, 1)) |> TraceCommand.TraceCmd

    static member Trace(size : IMod<V2i>) =
        size |> Mod.map (fun x -> V3i(x, 1)) |> TraceCommand.TraceCmd

    static member Trace(size : IMod<V3i>) =
        TraceCommand.TraceCmd size

    static member TraceToTexture(texture : IMod<IBackendTexture>, size : IMod<V3i>) = [
        TraceCommand.TransformLayout(texture, TextureLayout.General)
        TraceCommand.Trace(size)
        TraceCommand.TransformLayout(texture, TextureLayout.ShaderRead)
    ]

    static member TransformLayout(texture : IMod<IBackendTexture>, layout : TextureLayout) =
        TraceCommand.TransformLayoutCmd(texture, layout)

    static member Sync(buffer : IMod<IBackendBuffer>, src : ResourceAccess seq, dst : ResourceAccess seq) =
        TraceCommand.SyncBufferCmd(buffer, src, dst)

    static member Sync(buffer : IMod<IBackendBuffer>, src : ResourceAccess, dst : ResourceAccess) =
        TraceCommand.SyncBufferCmd(buffer, Seq.singleton src, Seq.singleton dst)

    static member Sync(texture : IMod<IBackendTexture>, src : ResourceAccess seq, dst : ResourceAccess seq) =
        TraceCommand.SyncTextureCmd(texture, src, dst)

    static member Sync(texture : IMod<IBackendTexture>, src : ResourceAccess, dst : ResourceAccess) =
        TraceCommand.SyncTextureCmd(texture, Seq.singleton src, Seq.singleton dst)

type ITraceTask =
    inherit IDisposable

    abstract member Run : token : AdaptiveToken -> commands : List<TraceCommand> -> unit

type ITraceRuntime =
    inherit IAccelerationStructureRuntime

    abstract member CompileTrace : TraceScene -> ITraceTask