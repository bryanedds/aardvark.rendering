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
    transform           : Trafo3d                      // TODO: Adaptive
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
    globals         : SymbolDict<obj>              
    buffers         : SymbolDict<IBackendBuffer>    // TODO: Adaptive
    textures        : SymbolDict<IBackendTexture>   // TODO: Adaptive
}

type ITraceTask =
    inherit IDisposable

    //TODO: abstract member Update : unit -> unit
    abstract member Run : size : V3i -> unit

type ITraceRuntime =
    inherit IAccelerationStructureRuntime

    abstract member CompileTrace : TraceScene -> ITraceTask