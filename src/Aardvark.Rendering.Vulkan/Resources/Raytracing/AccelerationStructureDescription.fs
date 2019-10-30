namespace Aardvark.Rendering.Vulkan.Raytracing

open Aardvark.Base

type BottomLevelDescription = {
    geometries : TraceGeometry list
}

type TopLevelDescription = {
    instanceBuffer : InstanceBuffer
}

type AccelerationStructureDescription =
    | BottomLevel of BottomLevelDescription
    | TopLevel of TopLevelDescription

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BottomLevelDescription =

    let empty =
        { geometries = [] }

    let addGeometry geometry info =
        { info with geometries = geometry::info.geometries}

    let addIndexedTriangles vertexBuffer indexBuffer info =
        info |> addGeometry (
            Triangles(vertexBuffer, Some indexBuffer)
        )

    let addTriangles vertexBuffer info =
        info |> addGeometry (
            Triangles(vertexBuffer, None)
        )
    
    let indexedTriangles vertexBuffer indexBuffer =
        empty |> addIndexedTriangles vertexBuffer indexBuffer

    let triangles vertexBuffer =
        empty |> addTriangles vertexBuffer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TopLevelDescription =

    let create buffer =
        { instanceBuffer = buffer }