namespace Aardvark.Rendering.Vulkan.Raytracing

open System
open FShade
open FShade.GLSL

// TODO: Remove all this as soon as FShade supports RTX
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

type RayHitInfo =
    {
        neededUniforms : Map<string, Type>
        neededSamplers : Map<string, SamplerInfo>
        neededBuffers  : Map<string, int * Type>
        payloadInType  : Type
        payloadOutType : Type
    }

type RayHitInterface =
    {
        uniformBuffers      : Map<int * int, string * list<string * GLSLType>>
        samplers            : Map<int * int, string * SamplerInfo>
        buffers             : Map<int * int, string * int * Type>
        payloadInLocation   : int
        payloadOutLocation  : int
    }