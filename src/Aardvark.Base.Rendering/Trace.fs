namespace Aardvark.Base

open System
open Aardvark.Base.Incremental

type Geometry =
    | Triangles of vertexBuffer: IBackendBuffer * indexBuffer : option<IBackendBuffer> //TODO: BufferView?
    | AABBs of buffer : IBackendBuffer

type IAccelerationStructure =
    abstract member Geometries : list<Geometry>

type IAccelerationStructureRuntime =
    inherit IBufferRuntime

    abstract member CreateAccelerationStructure : list<Geometry> -> IAccelerationStructure
    abstract member DeleteAccelerationStructure : IAccelerationStructure -> unit


type TraceObject = {
    transform           : M34d                      // TODO: Adaptive
    closestHitShader    : obj
    anyHitShader        : option<obj>
    intersectionShader  : option<obj>
    geometry            : IAccelerationStructure
    userData            : SymbolDict<obj>           // TODO: Adaptive
}

type TraceScene = {
    raygenShader    : obj
    missShaders     : list<obj>
    objects         : list<TraceObject>
    globals         : SymbolDict<IMod>              
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