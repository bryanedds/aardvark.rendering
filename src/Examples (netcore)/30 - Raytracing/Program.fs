open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Application
open Aardvark.Rendering.Vulkan
open Aardvark.SceneGraph

open System
open System.IO
open Aardvark.Base

let quad =
    let quad =
        let index = [|0;1;2; 0;2;3|]
        let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]
        let uvs = [| V2f.OO; V2f.IO; V2f.II; V2f.OI |]

        IndexedGeometry(IndexedGeometryMode.TriangleList, index, 
            SymDict.ofList [
                DefaultSemantic.Positions, positions :> Array
                DefaultSemantic.DiffuseColorCoordinates, uvs :> System.Array
            ], SymDict.empty)

    quad |> Sg.ofIndexedGeometry

[<EntryPoint>]
let main argv = 
    // first we need to initialize Aardvark's core components
    Ag.initialize()
    Aardvark.Init()

    use window = 
        window {
            display Display.Mono
            samples 8
            backend Backend.Vulkan
            debug true
        }
    let runtime = window.Runtime

    let raygenShader = File.ReadAllBytes "primary.rgen.spv"
    let missShader = File.ReadAllBytes "primary.rmiss.spv"
    let chitShader = File.ReadAllBytes "primary.rchit.spv"
    let sphereIntShader = File.ReadAllBytes "sphere.rint.spv"

    let cubeVertexBuffer, cubeIndexBuffer =
        
        let box = Box3d.FromCenterAndSize(V3d.Zero, V3d.One)
        let geometry = IndexedGeometryPrimitives.Box.solidBox box C4b.White

        let positions : V3f[] =
            geometry.IndexedAttributes.[DefaultSemantic.Positions]
                |> unbox

        let indices : uint32[] =
            geometry.IndexArray
                |> unbox

        let vb = runtime.CreateBuffer(positions)
        let ib = runtime.CreateBuffer(indices)

        {
            buffer = vb.Buffer
            count = vb.Count
            offset = vb.Offset
            format = typeof<V3f>
        },
        {
            buffer = ib.Buffer
            count = ib.Count
            offset = ib.Offset
            format = typeof<uint32>
        }

    let cubeAS =
        runtime.CreateAccelerationStructure([TraceGeometry.Triangles (cubeVertexBuffer, Some cubeIndexBuffer)])

    let sphereBuffer =
        runtime.CreateBuffer([| Box3f(V3f(-1), V3f(1)) |])

    let sphereAS =
        runtime.CreateAccelerationStructure([TraceGeometry.AABBs sphereBuffer])

    let dynamicRotation speed =
        let startTime = System.DateTime.Now
        window.Time |> Mod.map (fun t ->
            let t = (t - startTime).TotalSeconds
            Trafo3d.RotationZ (speed * t)
        )

    let obj1 : TraceObject = {
        transform = dynamicRotation 0.25 |> Mod.map (fun rot -> rot * Trafo3d.Translation(0.0, -5.0, -5.0))
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = None
        geometry = cubeAS
        userData = SymDict.empty
    }
       

    let obj2 : TraceObject = {
        transform = dynamicRotation -2.0 |> Mod.map (fun rot -> rot * Trafo3d.Translation(-1.0, 0.0, -2.0))
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = None
        geometry = cubeAS
        userData = SymDict.empty
    }

    let obj3 : TraceObject = {
        transform =  dynamicRotation 1.0 |> Mod.map (fun rot -> Trafo3d.Translation(4.0, 0.0, -2.0) * rot)
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = Some sphereIntShader
        geometry = sphereAS
        userData = SymDict.empty
    }

    let resultImage =
        let mutable current = None

        Mod.custom (fun token ->
            let s = window.Sizes.GetValue token
            current |> Option.iter runtime.DeleteTexture
            current <- Some <| runtime.CreateTexture(s, TextureFormat.Rgba8, 1, 1)
            current.Value
        )

    let viewInv, projInv =
        let invTrans (t : Trafo3d[]) =
            M44f.op_Explicit t.[0].Backward.Transposed

        window.View |> Mod.map invTrans,
        window.Proj |> Mod.map invTrans

    let bounces, tmin, tmax =
        ~~0u, ~~0.0f, ~~100.0f

    let scene : TraceScene = {
        raygenShader = raygenShader
        missShaders = [missShader]
        callableShaders = []
        objects = [obj1; obj2; obj3]
        globals = SymDict.ofList [
            Symbol.Create "viewInverse@", viewInv :> IMod
            Symbol.Create "projInverse@", projInv :> IMod
            Symbol.Create "maxBounces@", bounces :> IMod
            Symbol.Create "tmin@", tmin :> IMod
            Symbol.Create "tmax@", tmax :> IMod
        ]
        buffers = SymDict.empty
        textures = SymDict.ofList [Symbol.Create "resultImage", resultImage |> Mod.map unbox]
    }

    let task = runtime.CompileTrace scene

    let output =
        Mod.custom (fun self ->
            let target = resultImage.GetValue self
            task.Run self <| TraceCommand.TraceToTexture resultImage
            target :> ITexture
        )

    window.Scene <-
        quad |> Sg.diffuseTexture output
             |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]

    window.Run(true)

    task.Dispose()

    runtime.DeleteTexture (resultImage.GetValue())
    runtime.DeleteAccelerationStructure cubeAS
    runtime.DeleteBuffer cubeVertexBuffer.buffer
    runtime.DeleteBuffer cubeIndexBuffer.buffer
    runtime.DeleteAccelerationStructure sphereAS
    runtime.DeleteBuffer sphereBuffer.Buffer

    0