open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Application
open Aardvark.Rendering.Vulkan
open Aardvark.SceneGraph

open System
open System.IO


let quad (color : C4b) =
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
    let device = app.Runtime.Device

    let raygenShader = File.ReadAllBytes "dummy.spv"

    let vb : MyBuffer =
        let b = runtime.CreateBuffer([| V3f(-0.5, -0.5, 0.0); V3f(0.5, -0.5, 0.0); V3f(0.0, 0.5, 0.0) |])
        {
            buffer = b.Buffer
            count = b.Count
            offset = b.Offset
            format = typeof<V3f>
        } 
        

    let ib =
        let b = runtime.CreateBuffer([| 0u; 1u; 2u |])
        {
            buffer = b.Buffer
            count = b.Count
            offset = b.Offset
            format = typeof<uint32>
        } 

    let accelerationStructure =
        runtime.CreateAccelerationStructure([TraceGeometry.Triangles (vb, Some ib)])

    let obj : TraceObject = {
        transform = M34d.Identity
        closestHitShader = Unchecked.defaultof<_>
        anyHitShader = None
        intersectionShader = None
        geometry = accelerationStructure
        userData = SymDict.empty
    }

    let resultImage : IBackendTexture =
        runtime.CreateTexture(V2i(100, 100), TextureFormat.Rgba8, 1, 1)

    let scene : TraceScene = {
        raygenShader = raygenShader
        missShaders = []
        objects = [obj]
        globals = SymDict.empty
        buffers = SymDict.empty
        textures = SymDict.ofList [Symbol.Create "resultImage", resultImage]
    }

    let task = runtime.CompileTrace scene
    task.Run resultImage.Size
    task.Dispose()

    let sg =
        quad C4b.White
            |> Sg.texture DefaultSemantic.DiffuseColorTexture ~~(resultImage :> ITexture)
            |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]

    window.RenderTask <- runtime.CompileRender(window.FramebufferSignature, sg)    
    window.Run()

    runtime.DeleteTexture resultImage
    runtime.DeleteAccelerationStructure accelerationStructure
    runtime.DeleteBuffer ib.buffer
    runtime.DeleteBuffer vb.buffer

    0