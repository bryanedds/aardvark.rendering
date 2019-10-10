namespace Aardvark.Rendering.Vulkan.Raytracing

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

type HitGroup = {
    mutable AnyHitShader : Option<byte[]>
    mutable ClosestHitShader : Option<byte[]>
    mutable IntersectionShader : Option<byte[]>
}

type Scene = {
    mutable Objects : list<Object>
    mutable RaygenShader : byte[]
    mutable MissShaders : list<obj>
    mutable HitGroups : list<HitGroup>
}
