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

    let app = new Aardvark.Application.Slim.VulkanApplication()
    let window = app.CreateGameWindow(1, false)

    //use window = 
    //    window {
    //        display Display.Mono
    //        samples 8
    //        backend Backend.Vulkan
    //        debug false
    //    }
    let runtime = window.Runtime :> IRuntime

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

    let createObject =
        let rand = RandomSystem()
        
        let time =
            let startTime = DateTime.Now
            window.Time |> Mod.map (fun t -> (t - startTime).TotalSeconds)

        fun () ->
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
                time |> Mod.map (fun mt ->
                    let rot = Trafo3d.Rotation(axis, turnRate * mt * 1.5)
                    let trans = initialTrafo * Trafo3d.RotationZ (moveSpeed * 0.25 * mt)
                    rot * trans
                )

            TraceObject(trafo, None, Some chitShader, None, cubeAS, SymDict.empty)

    let objects = Mod.init (HRefSet.single (createObject()))

    (*let dynamicRotation speed =
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
    }*)

    let resultImage =
        let mutable current = None

        Mod.custom (fun token ->
            let s = window.Sizes.GetValue token
            current |> Option.iter runtime.DeleteTexture
            current <- Some <| runtime.CreateTexture(s, TextureFormat.Rgba8, 1, 1)
            current.Value
        )

    let initialView = CameraView.LookAt(V3d(2.0,2.0,2.0), V3d.Zero, V3d.OOI)
    let frustum = 
        window.Sizes 
            |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 50.0 (float s.X / float s.Y))
    let cameraView = DefaultCameraController.control window.Mouse window.Keyboard window.Time initialView

    let viewInv, projInv =
        let invTrans (t : Trafo3d) =
            M44f.op_Explicit t.Backward.Transposed

        cameraView |> Mod.map (CameraView.viewTrafo >> invTrans),
        frustum |> Mod.map (Frustum.projTrafo >> invTrans)

    let bounces, tmin, tmax =
        ~~0u, ~~0.0f, ~~500.0f

    let scene : TraceScene =
        TraceScene(
            raygenShader, [missShader], [],
            objects |> ASet.ofMod,
            SymDict.ofList [
                Symbol.Create "viewInverse@", viewInv :> IMod
                Symbol.Create "projInverse@", projInv :> IMod
                Symbol.Create "maxBounces@", bounces :> IMod
                Symbol.Create "tmin@", tmin :> IMod
                Symbol.Create "tmax@", tmax :> IMod                
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
        RenderTask.custom (fun (t, rt, fbo) ->
            task.Run t <| TraceCommand.TraceToTexture resultImage
            final.Run(t, rt, fbo)
        )

    window.RenderAsFastAsPossible <- true
    window.RenderTask <- myRender

    window.Keyboard.DownWithRepeats.Values.Add(fun k ->
        match k with
            | Keys.Enter ->
                transact (fun () ->
                    let set = Mod.force objects
                    Mod.change objects (set |> HRefSet.add (createObject()))
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

    task.Dispose()

    runtime.DeleteTexture (resultImage.GetValue())
    runtime.DeleteAccelerationStructure cubeAS
    runtime.DeleteBuffer cubeVertexBuffer.buffer
    runtime.DeleteBuffer cubeIndexBuffer.buffer
    runtime.DeleteAccelerationStructure sphereAS
    runtime.DeleteBuffer sphereBuffer.Buffer

    0