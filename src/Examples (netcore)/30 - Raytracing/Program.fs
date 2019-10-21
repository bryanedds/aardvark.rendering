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

    let app = new Slim.VulkanApplication(true)
    let window = app.CreateGameWindow(1)
    let runtime = app.Runtime :> IRuntime

    let raygenShader = File.ReadAllBytes "primary.rgen.spv"
    let missShader = File.ReadAllBytes "primary.rmiss.spv"
    let chitShader = File.ReadAllBytes "primary.rchit.spv"
    let whiteShader = File.ReadAllBytes "white.rchit.spv"
    let sphereIntShader = File.ReadAllBytes "sphere.rint.spv"

    let triangleVertexBuffer : MyBuffer =
        let a = 1.0
        let h = (sqrt 3.0) * a / 2.0

        let b = runtime.CreateBuffer([| V3f(-a / 2.0, -h / 2.0, 0.0); V3f(a / 2.0, -h / 2.0, 0.0); V3f(0.0, h / 2.0, 0.0) |])
        {
            buffer = b.Buffer
            count = b.Count
            offset = b.Offset
            format = typeof<V3f>
        } 
        

    let triangleIndexBuffer =
        let b = runtime.CreateBuffer([| 0u; 1u; 2u |])
        {
            buffer = b.Buffer
            count = b.Count
            offset = b.Offset
            format = typeof<uint32>
        }

    let triangleAS =
        runtime.CreateAccelerationStructure([TraceGeometry.Triangles (triangleVertexBuffer, Some triangleIndexBuffer)])

    let sphereBuffer =
        runtime.CreateBuffer([| Box3f(V3f(-1), V3f(1)) |])

    let sphereAS =
        runtime.CreateAccelerationStructure([TraceGeometry.AABBs sphereBuffer])

    let obj1 : TraceObject = {
        transform = Trafo3d.Scale(8.0) * Trafo3d.Translation(0.0, 0.0, 5.0)
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = None
        geometry = triangleAS
        userData = SymDict.empty
    }

    let obj2 : TraceObject = {
        transform =  Trafo3d.Scale(0.5) * Trafo3d.RotationZInDegrees(90.0) * Trafo3d.Translation(1.0, 0.0, 2.0)
        closestHitShader = Some whiteShader
        anyHitShader = None
        intersectionShader = None
        geometry = triangleAS
        userData = SymDict.empty
    }

    let obj3 : TraceObject = {
        transform = Trafo3d.Translation(0.0, 0.0, 3.0)
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = Some sphereIntShader
        geometry = sphereAS
        userData = SymDict.empty
    }

    let resultImage : IBackendTexture =
        runtime.CreateTexture(V2i(1024, 1024), TextureFormat.Rgba8, 1, 1)

    let scene : TraceScene = {
        raygenShader = raygenShader
        missShaders = [missShader]
        callableShaders = []
        objects = [obj1; obj2; obj3]
        globals = SymDict.empty
        buffers = SymDict.empty
        textures = SymDict.ofList [Symbol.Create "resultImage", resultImage]
    }

    let task = runtime.CompileTrace scene
    task.Run resultImage.Size
    task.Dispose()

    let sg =
        quad |> Sg.texture DefaultSemantic.DiffuseColorTexture ~~(resultImage :> ITexture)
             |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]

    window.RenderTask <- runtime.CompileRender(window.FramebufferSignature, sg)
    window.RenderAsFastAsPossible <- true
    window.Run()

    runtime.DeleteTexture resultImage
    runtime.DeleteAccelerationStructure triangleAS
    runtime.DeleteBuffer triangleVertexBuffer.buffer
    runtime.DeleteBuffer triangleIndexBuffer.buffer
    runtime.DeleteAccelerationStructure sphereAS
    runtime.DeleteBuffer sphereBuffer.Buffer

    0