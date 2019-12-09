namespace FShade

open System

open Aardvark.Base

// TODO: Remove all this as soon as FShade supports RTX
type BufferInfo =
    {
        rank : int
        elementType : Type
    }

type SamplerInfo =
    {
        samplerType     : Type
        textureName     : string
        samplerState    : SamplerState
        dimension       : SamplerDimension
        isArray         : bool
        isShadow        : bool
        isMS            : bool
        valueType       : Type
    }

type TraceShaderInfo =
    {
        neededPayloads : hset<Type * Type>
        neededScenes   : Set<string>
        neededUniforms : MapExt<string, Type>
        neededSamplers : MapExt<string, SamplerInfo>
        neededBuffers  : MapExt<string, BufferInfo>
        payloadInType  : Type option
        payloadOutType : Type option
    }

type TraceShaderInterface =
    {
        uniformBuffers      : Map<int * int, string * list<string * Type>>
        samplers            : Map<int * int, string * SamplerInfo>
        buffers             : Map<int * int, string * int * Type>
        scenes              : Map<int * int, string>
        payloadLocations    : hmap<Type * Type, int>
        payloadInLocation   : int option
        payloadOutLocation  : int option
    }

type TraceShader =
    {
        binary : byte[]
        info : TraceShaderInfo
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceShader =

    let binary (shader : TraceShader) =
        shader.binary

    let info (shader : TraceShader) =
        shader.info

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceShaderInfo =

    let empty =
        {
            neededPayloads = HSet.empty
            neededScenes   = Set.empty
            neededUniforms = MapExt.empty
            neededSamplers = MapExt.empty
            neededBuffers  = MapExt.empty
            payloadInType  = None
            payloadOutType = None
        }

    let payload (payload : Type * Type) (info : TraceShaderInfo) =
        { info with neededPayloads = info.neededPayloads |> HSet.add payload }

    let scene (scene : string) (info : TraceShaderInfo) =
        { info with neededScenes = info.neededScenes |> Set.add scene }

    let uniform (name : string) (typ : Type) (info : TraceShaderInfo) =
        { info with neededUniforms = info.neededUniforms |> MapExt.add name typ }

    let sampler (name : string) (sampler : SamplerInfo) (info : TraceShaderInfo) =
        { info with neededSamplers = info.neededSamplers |> MapExt.add name sampler }

    let buffer (name : string) (buffer : BufferInfo) (info : TraceShaderInfo) =
        { info with neededBuffers = info.neededBuffers |> MapExt.add name buffer }

    let payloadIn (typ : Type option) (info : TraceShaderInfo) =
        { info with payloadInType = typ }

    let payloadOut (typ : Type option) (info : TraceShaderInfo) =
        { info with payloadOutType = typ }