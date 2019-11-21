namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base
open FShade
open System

type Binding<'key, 'value> =
    {
        key : 'key option
        value : 'value
    }

/// Bindings represented by a single int (e.g. payload location)
type Location<'a> = Binding<int, 'a>

/// Bindings in a descriptor set consisting of the set and the binding numbers
type DescriptorBinding<'a> = Binding<int * int, 'a>

/// Type representing all the resources of a scene and its shaders, alongside
/// their bindings
type TraceSceneBindings =
    {
        payloads    : Location<Type> hset
        scenes      : MapExt<string, DescriptorBinding<unit>>
        uniforms    : MapExt<string, DescriptorBinding<Type>>
        samplers    : MapExt<string, DescriptorBinding<SamplerInfo>>
        buffers     : MapExt<string, DescriptorBinding<BufferInfo>>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Binding =

    let unassigned value =
        {
            key = None
            value = value
        }

    let assign key binding =
        { binding with key = Some key }

    let assignWith f binding =
        assign (f binding.value) binding

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceSceneBindings =

    let create payloads scenes uniforms samplers buffers =
        {
            payloads = payloads
            scenes = scenes
            uniforms = uniforms
            samplers = samplers
            buffers = buffers
        }

    let empty =
        create HSet.empty MapExt.empty MapExt.empty MapExt.empty MapExt.empty

    let assign (assignDescriptor : string -> int * int) (assignPayload : Type -> int) (bindings : TraceSceneBindings) =
        {
            payloads = bindings.payloads |> HSet.map (Binding.assignWith assignPayload)
            scenes = bindings.scenes |> MapExt.map (fun n b -> Binding.assign (assignDescriptor n) b)
            uniforms = bindings.uniforms |> MapExt.map (fun n b -> Binding.assign (assignDescriptor n) b)
            samplers = bindings.samplers |> MapExt.map (fun n b -> Binding.assign (assignDescriptor n) b)
            buffers = bindings.buffers |> MapExt.map (fun n b -> Binding.assign (assignDescriptor n) b)
        }

    let ofShaderInfo (info : TraceShaderInfo) =

        let ofOption t = 
            t |> Option.map (Binding.unassigned >> List.singleton) |> Option.defaultValue []

        let ofSet set =
            set |> Set.map (fun name -> name, Binding.unassigned ()) |> MapExt.ofSeq

        let ofMap (map : MapExt<string, 'a>) =
            map |> MapExt.map (fun _ value -> Binding.unassigned value)            

        let payloads = HSet.ofList [
            yield! info.payloadInType |> ofOption
            yield! info.payloadOutType |> ofOption

            yield! info.neededPayloads
                |> HSet.toList
                |> List.collect (fun (x, y) -> [Binding.unassigned x; Binding.unassigned y])
        ]

        let scenes = info.neededScenes |> ofSet
        let uniforms = info.neededUniforms |> ofMap
        let samplers = info.neededSamplers |> ofMap
        let buffers = info.neededBuffers |> ofMap

        create payloads scenes uniforms samplers buffers

    let union (x : TraceSceneBindings) (y : TraceSceneBindings) =

        let resolve a b =
            if a = b then a else failwith "Name conflict in two maps to be merged"

        {
            payloads = HSet.union x.payloads y.payloads
            scenes   = MapExt.unionWith resolve x.scenes y.scenes
            uniforms = MapExt.unionWith resolve x.uniforms y.uniforms
            samplers = MapExt.unionWith resolve x.samplers y.samplers
            buffers  = MapExt.unionWith resolve x.buffers y.buffers
        }

    let unionMany (bindings : TraceSceneBindings seq) =
        bindings |> Seq.fold union empty

    let compute (shaders : TraceShader seq) =
        shaders |> Seq.map (fun x -> ofShaderInfo x.info) |> unionMany