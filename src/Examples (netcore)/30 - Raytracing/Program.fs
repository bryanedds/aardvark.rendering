open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Application
open Aardvark.Rendering.Vulkan

open System.IO

[<EntryPoint>]
let main argv = 
    // first we need to initialize Aardvark's core components
    Ag.initialize()
    Aardvark.Init()

    let app = new Slim.VulkanApplication(true)
    let window = app.CreateGameWindow(1)
    let runtime = app.Runtime
    let device = runtime.Device

    let raygenShader = File.ReadAllBytes "dummy.spv"

    let obj = Raytracing.Object.Create()
    obj.Positions <- [| V3f(-0.5, -0.5, 0.0); V3f(0.5, -0.5, 0.0); V3f(0.0, 0.5, 0.0) |]
    obj.Indices <- [| 0; 1; 2 |]
    obj.Trafo <- Mod.constant Trafo3d.Identity

    let objects = ASet.single (obj :> IRenderObject)

    let cmd = RuntimeCommand.Trace(~~raygenShader, AList.empty, AList.empty, objects)
    let task = new Temp.CommandTask(device, unbox window.FramebufferSignature, cmd)
    
    window.RenderTask <- task
    window.Run()

    0