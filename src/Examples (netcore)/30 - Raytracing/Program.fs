open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Application
open Aardvark.Rendering.Vulkan
open Aardvark.SceneGraph

open System
open System.IO
open System.Runtime.InteropServices

open FShade
open FShade.Formats

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

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CameraUniform =
    struct
        new(v, p) = {viewInverse = v; projInverse = p}

        val viewInverse : M44f
        val projInverse : M44f
    end

[<Struct; StructLayout(LayoutKind.Sequential)>]
type RaytracingSettings =
    struct
        new(bounces, min, max) = {maxBounces = bounces; tmin = min; tmax = max}

        val maxBounces : uint32
        val tmin : float32
        val tmax : float32
    end

[<EntryPoint>]
let main argv = 
    // first we need to initialize Aardvark's core components
    Ag.initialize()
    Aardvark.Init()

    let app = new Aardvark.Application.Slim.VulkanApplication(true)
    let window = app.CreateGameWindow(1, false)

    //use window = 
    //    window {
    //        display Display.Mono
    //        samples 8
    //        backend Backend.Vulkan
    //        debug false
    //    }
    let runtime = window.Runtime :> IRuntime

    let raygenShader =
        {
            binary = File.ReadAllBytes "primary.rgen.spv"
            info =
                TraceShaderInfo.empty
                |> TraceShaderInfo.uniform "resultImage" typeof<IntImage2d<rgba8i>>
                |> TraceShaderInfo.uniform "camera" typeof<CameraUniform>
                |> TraceShaderInfo.uniform "raytracingSettings" typeof<RaytracingSettings>
                |> TraceShaderInfo.scene "scene"
        }

    let missShader =
        {
            binary = File.ReadAllBytes "primary.rmiss.spv"
            info = TraceShaderInfo.empty
        }

    let chitShader =
        {
            binary = File.ReadAllBytes "primary.rchit.spv"
            info =
                TraceShaderInfo.empty
                |> TraceShaderInfo.buffer "color" { rank = 1; elementType = typeof<C4f> }
        }

    let sphereIntShader =
        {
            binary = File.ReadAllBytes "sphere.rint.spv"
            info = TraceShaderInfo.empty
        }

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

    let createObject =
        let rand = RandomSystem()
        
        let time =
            let startTime = DateTime.Now
            window.Time |> Mod.map (fun t -> (t - startTime).TotalSeconds)

        let colors = [|
            C4f.DarkRed; C4f.DarkGreen; C4f.DarkCyan; C4f.DarkMagenta; C4f.DarkYellow; C4f.DarkBlue
        |]

        fun dynamic cube ->
            let axis, turnRate, moveSpeed, initialTrafo =
                let r = rand.UniformDouble() * 25.0 + 10.0
                let phi = rand.UniformDouble() * Constant.PiTimesTwo

                let rot = 
                    let r = rand.UniformV3dDirection() 
                    let angle = rand.UniformDouble() * Constant.PiTimesTwo
                    Trafo3d.Rotation(r, angle)

                let p = V3d(r * cos phi, r * sin phi, 0.0)

                rand.UniformV3dDirection(),
                rand.UniformDouble() * 2.0,
                (rand.UniformDouble() - 0.5) * 0.4 + 1.0,
                rot * Trafo3d.Translation(p)

            let trafo =
                if dynamic then time else (Mod.constant 0.0)
                    |> Mod.map (fun mt ->
                        let rot = Trafo3d.Rotation(axis, turnRate * mt * 1.5)
                        let trans = initialTrafo * Trafo3d.RotationZ (moveSpeed * 0.25 * mt)
                        rot * trans
                    )

            let color =
                let i = rand.UniformInt(colors.Length)
                ~~colors.[i]

            let attributes =
                SymDict.ofList [
                    Symbol.Create "color", color :> IMod
                ]

            if cube then
                TraceObject(trafo, None, Some chitShader, None, cubeAS, attributes)
            else
                TraceObject(trafo, None, Some chitShader, Some sphereIntShader, sphereAS, attributes)

    let objects = Mod.init HRefSet.empty

    let resultImage =
        let mutable current = None

        window.Sizes |> Mod.map(fun s ->
            current |> Option.iter runtime.DeleteTexture
            current <- Some <| runtime.CreateTexture(s, TextureFormat.Rgba8, 1, 1)
            current.Value
        )

    let initialView = CameraView.LookAt(V3d(2.0,2.0,2.0), V3d.Zero, V3d.OOI)
    let frustum = 
        window.Sizes 
            |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 50.0 (float s.X / float s.Y))
    let cameraView = DefaultCameraController.control window.Mouse window.Keyboard window.Time initialView

    let cameraUniform =
        let invTrans (t : Trafo3d) =
            M44f.op_Explicit t.Backward.Transposed

        Mod.map2 (fun v f ->
            CameraUniform(
                v |> CameraView.viewTrafo |> invTrans,
                f |> Frustum.projTrafo |> invTrans
            )
        ) cameraView frustum

    let raytracingSettings =
       ~~RaytracingSettings(0u, 0.0f, 500.0f)

    let scene : TraceScene =
        TraceScene(
            raygenShader, [missShader], [],
            [chitShader; sphereIntShader],
            objects |> ASet.ofMod,
            SymDict.ofList [
                Symbol.Create "camera", cameraUniform :> IMod
                Symbol.Create "raytracingSettings", raytracingSettings :> IMod
            ],
            SymDict.empty,
            SymDict.ofList [
                Symbol.Create "resultImage", resultImage |> Mod.map unbox
            ]
        )

    let task = runtime.CompileTrace scene

    let final = 
        quad |> Sg.diffuseTexture (resultImage |> Mod.map (fun t -> t :> ITexture))
             |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]
             |> Sg.compile runtime window.FramebufferSignature

    let myRender =
        let size = resultImage |> Mod.map (fun x -> x.Size)

        RenderTask.custom (fun (t, rt, fbo) ->
            task.Run t <| (TraceCommand.TraceToTexture(resultImage, size))
            final.Run(t, rt, fbo)
        )

    window.RenderAsFastAsPossible <- true
    window.RenderTask <- myRender

    window.Keyboard.DownWithRepeats.Values.Add(fun k ->
        match k with
            | Keys.Enter ->
                transact (fun () ->
                    let rand = System.Random()
                    let set = Mod.force objects
                    let dynamic = rand.NextDouble() > 0.5
                    let cube = rand.NextDouble() > 0.5
                    Mod.change objects (set |> HRefSet.add (createObject dynamic cube))
                )
            | Keys.Delete ->
                transact (fun () ->
                    let set = Mod.force objects

                    if set.Count > 0 then
                        let x = set.Count / 2 |> Array.get (HRefSet.toArray set)
                        Mod.change objects (set |> HRefSet.remove x)
                )
            | _ -> ()
    )

    window.Run()

    final.Dispose()
    task.Dispose()

    runtime.DeleteTexture (resultImage.GetValue())
    runtime.DeleteAccelerationStructure cubeAS
    runtime.DeleteBuffer cubeVertexBuffer.buffer
    runtime.DeleteBuffer cubeIndexBuffer.buffer
    runtime.DeleteAccelerationStructure sphereAS
    runtime.DeleteBuffer sphereBuffer.Buffer

    window.Dispose()
    app.Dispose()
    0