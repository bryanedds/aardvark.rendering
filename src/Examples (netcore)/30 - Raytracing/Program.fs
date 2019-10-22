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

let camera = Camera.create (CameraView.LookAt (V3d(0, 0, 3), V3d.OOO, V3d.OIO)) (Frustum.perspective 75.0 0.1 1000.0 1.0)

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

    let cubeVertexBuffer : MyBuffer =
        
        let positions : V3f[] = 
            Primitives.unitBox.IndexedAttributes.[DefaultSemantic.Positions]
                |> unbox

        let b = runtime.CreateBuffer(positions)
        {
            buffer = b.Buffer
            count = b.Count
            offset = b.Offset
            format = typeof<V3f>
        } 

    let cubeAS =
        runtime.CreateAccelerationStructure([TraceGeometry.Triangles (cubeVertexBuffer, None)])

    let sphereBuffer =
        runtime.CreateBuffer([| Box3f(V3f(-1), V3f(1)) |])

    let sphereAS =
        runtime.CreateAccelerationStructure([TraceGeometry.AABBs sphereBuffer])

    let obj1 : TraceObject = {
        transform = Trafo3d.RotationZInDegrees(8.0) * Trafo3d.Translation(0.0, -5.0, -5.0)
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = None
        geometry = cubeAS
        userData = SymDict.empty
    }

    let obj2 : TraceObject = {
        transform = Trafo3d.Translation(1.0, 0.0, -2.0) * Trafo3d.RotationZInDegrees(15.0)
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = None
        geometry = cubeAS
        userData = SymDict.empty
    }

    let obj3 : TraceObject = {
        transform = Trafo3d.Translation(-2.0, 0.0, -3.0)
        closestHitShader = Some chitShader
        anyHitShader = None
        intersectionShader = Some sphereIntShader
        geometry = sphereAS
        userData = SymDict.empty
    }

    let resultImage : IBackendTexture =
        runtime.CreateTexture(V2i(1024, 1024), TextureFormat.Rgba8, 1, 1)

    let camera =
        CameraUniform(
            M44f.op_Explicit camera.cameraView.ViewTrafo.Backward.Transposed,
            M44f.op_Explicit (Camera.projTrafo camera).Backward.Transposed
        )

    let settings = RaytracingSettings(0u, 0.0f, 100.0f)

    let scene : TraceScene = {
        raygenShader = raygenShader
        missShaders = [missShader]
        callableShaders = []
        objects = [obj1; obj2; obj3]
        globals = SymDict.ofList [
            Symbol.Create "camera", camera :> obj
            Symbol.Create "raytracingSettings", settings :> obj
        ]
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
    runtime.DeleteAccelerationStructure cubeAS
    runtime.DeleteBuffer cubeVertexBuffer.buffer
    runtime.DeleteAccelerationStructure sphereAS
    runtime.DeleteBuffer sphereBuffer.Buffer

    0