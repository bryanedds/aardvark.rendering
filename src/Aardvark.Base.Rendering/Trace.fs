namespace Aardvark.Base

open System
open Aardvark.Base.Incremental

// TODO: What am I supposed to use here?
type MyBuffer = {
    buffer : IBackendBuffer
    count : int
    offset : nativeint
    format : Type
}

type TraceGeometry =
    | Triangles of vertexBuffer: MyBuffer * indexBuffer : option<MyBuffer>
    | AABBs of buffer : IBuffer<Box3f>

type IAccelerationStructure =
    abstract member Handle : obj
    abstract member Geometries : list<TraceGeometry>

type IAccelerationStructureRuntime =
    inherit IBufferRuntime

    abstract member CreateAccelerationStructure : list<TraceGeometry> -> IAccelerationStructure
    abstract member DeleteAccelerationStructure : IAccelerationStructure -> unit

type TraceObject = {
    transform           : IMod<Trafo3d>
    closestHitShader    : option<byte[]>
    anyHitShader        : option<byte[]>
    intersectionShader  : option<byte[]>
    geometry            : IAccelerationStructure
    userData            : SymbolDict<obj>           // TODO: Adaptive
}

type TraceScene = {
    raygenShader    : byte[]
    missShaders     : list<byte[]>
    callableShaders : list<byte[]>
    objects         : list<TraceObject>
    globals         : SymbolDict<IMod>
    buffers         : SymbolDict<IMod<IBackendBuffer>>
    textures        : SymbolDict<IMod<ITexture>>
}

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

    static member TraceToTexture(texture : IMod<IBackendTexture>) = [
        TraceCommand.TransformLayout(texture, TextureLayout.General)
        TraceCommand.Trace(texture |> Mod.map (fun t -> t.Size))
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