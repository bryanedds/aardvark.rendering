﻿namespace Aardvark.Base.Rendering.Effects

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade

module DiffuseTexture = 

    let private diffuseSampler =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let internal diffuseTexture (v : Vertex) =
        fragment {
            let texColor = diffuseSampler.Sample(v.tc)
            return texColor
        }

    let Effect = 
        toEffect diffuseTexture
