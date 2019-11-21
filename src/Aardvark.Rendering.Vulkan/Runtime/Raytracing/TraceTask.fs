namespace Aardvark.Rendering.Vulkan


open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Vulkan.Raytracing
open Aardvark.Rendering.Vulkan.NVRayTracing

open System

[<AutoOpen>]
module ``Trace Command Extensions`` =

     type Command with

            static member BindPipeline(pipeline : VkPipeline) =
                { new Command() with
                    member x.Compatible = QueueFlags.Graphics
                    member x.Enqueue cmd =
                        cmd.AppendCommand()
                        VkRaw.vkCmdBindPipeline(cmd.Handle, VkPipelineBindPoint.RayTracingNv, pipeline)
                        Disposable.Empty
                }

            static member BindDescriptorSet(descriptorSet : VkDescriptorSet, pipelineLayout : VkPipelineLayout) =
                { new Command() with
                    member x.Compatible = QueueFlags.Graphics
                    member x.Enqueue cmd =
                        cmd.AppendCommand()
                        pin (fun ptr ->
                            VkRaw.vkCmdBindDescriptorSets(cmd.Handle, VkPipelineBindPoint.RayTracingNv, pipelineLayout,
                                0u, 1u, ptr, 0u, NativePtr.zero)
                        ) descriptorSet
                        Disposable.Empty
                }

            static member TraceRays(raygenShaderBindingTableBuffer : VkBuffer, 
                                    raygenShaderBindingOffset : VkDeviceSize,
                                    missShaderBindingTableBuffer : VkBuffer, 
                                    missShaderBindingOffset : VkDeviceSize, 
                                    missShaderBindingStride : VkDeviceSize,
                                    hitShaderBindingTableBuffer : VkBuffer,
                                    hitShaderBindingOffset : VkDeviceSize,
                                    hitShaderBindingStride : VkDeviceSize,
                                    callableShaderBindingTableBuffer : VkBuffer,
                                    callableShaderBindingOffset : VkDeviceSize,
                                    callableShaderBindingStride : VkDeviceSize,
                                    width : uint32, height : uint32, depth : uint32) =
                { new Command() with
                    member x.Compatible = QueueFlags.Graphics
                    member x.Enqueue cmd =
                        cmd.AppendCommand()
                        VkRaw.vkCmdTraceRaysNV (cmd.Handle, raygenShaderBindingTableBuffer, raygenShaderBindingOffset,
                            missShaderBindingTableBuffer, missShaderBindingOffset, missShaderBindingStride,
                            hitShaderBindingTableBuffer, hitShaderBindingOffset, hitShaderBindingStride,
                            callableShaderBindingTableBuffer, callableShaderBindingOffset, callableShaderBindingStride,
                            width, height, depth)
                        Disposable.Empty
                }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TraceCommand =

        let toCommand (token : AdaptiveToken) (sbt : ShaderBindingTable) = function
            | TraceCommand.TraceCmd size ->
                let size = size.GetValue token
                Command.TraceRays(sbt.Handle, sbt.RaygenShaderBindingOffset,
                                  sbt.Handle, sbt.MissShaderBindingOffset, sbt.MissShaderBindingStride,
                                  sbt.Handle, sbt.HitShaderBindingOffset, sbt.HitShaderBindingStride,
                                  sbt.Handle, sbt.CallableShaderBindingOffset, sbt.CallableShaderBindingStride,
                                  uint32 size.X, uint32 size.Y, uint32 size.Z)

            | TraceCommand.SyncBufferCmd(buffer, src, dst) ->
                let buffer = unbox (buffer.GetValue token)
                Command.Sync(buffer, VkAccessFlags.ofResourceAccess src, VkAccessFlags.ofResourceAccess dst)

            | TraceCommand.SyncTextureCmd(texture, src, dst) ->
                let image = unbox<Image> (texture.GetValue token)
                Command.ImageBarrier(image.[ImageAspect.Color], VkAccessFlags.ofResourceAccess src, VkAccessFlags.ofResourceAccess dst)

            | TraceCommand.TransformLayoutCmd(texture, layout) ->
                let image = unbox (texture.GetValue token)
                Command.TransformLayout(image, VkImageLayout.ofTextureLayout layout)
            
type TraceTask(device : Device, scene : TraceScene) =
    inherit AdaptiveObject()

    let locks = ReferenceCountingSet<ILockedResource>()

    let user =
        { new IResourceUser with
            member x.AddLocked l = lock locks (fun () -> locks.Add l |> ignore)
            member x.RemoveLocked l = lock locks (fun () -> locks.Remove l |> ignore)
        }

    let manager = new ResourceManager(user, device)
    let resources = new ResourceLocationSet(user)

    let preparedScene =
        let prep = manager.PrepareTraceScene(scene)
        prep.resources |> List.iter resources.Add

        prep

    member x.Run (token : AdaptiveToken) (commands : List<TraceCommand>) =
        x.EvaluateAlways token (fun t ->
            resources.Update t |> ignore

            let descriptorSet = preparedScene.descriptorSet.Update t

            let pipeline = (preparedScene.pipeline.Update token).handle
            let pipelineLayout = preparedScene.pipelineLayout

            let sbt = (preparedScene.shaderBindingTable.Update token).handle

            device.eventually {
                do! Command.BindPipeline pipeline.Handle
                do! Command.BindDescriptorSet(descriptorSet.handle.Handle, pipelineLayout.Handle)

                for cmd in commands do
                    do! TraceCommand.toCommand token sbt cmd
            }
        )

    member x.Dispose() =
        preparedScene.Dispose()

    interface ITraceTask with
        member x.Run (token : AdaptiveToken) (commands : List<TraceCommand>) =
            x.Run token commands

    interface IDisposable with
        member x.Dispose() =
            x.Dispose()