﻿namespace Aardvark.Rendering.NanoVg

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL

module Statistics =
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Threading

    let private tickFunctions = Dictionary<obj, unit -> unit>()

    let private tick (state : obj) =
        let functions = lock tickFunctions (fun () -> tickFunctions |> Seq.map (fun (KeyValue(_,cb)) -> cb) |> Seq.toArray)
        for f in functions do
            f()

    let private timer = new Timer(TimerCallback(tick), null, 10, 10)

    type Interlocked with
        static member TryChange(cell : byref<'a>, f : 'a -> 'a) =
            let captured = cell
            let newValue = f captured
            let result = Interlocked.CompareExchange(&cell, newValue, captured)

            System.Object.ReferenceEquals(result, captured)

        static member TryChange2(cell : byref<'a>, f : 'a -> 'a * 'b) =
            let captured = cell
            let newValue,res = f captured
            let result = Interlocked.CompareExchange(&cell, newValue, captured)

            if System.Object.ReferenceEquals(result, captured) then
                Some res
            else
                None

        static member Change(cell : byref<'a>, f : 'a -> 'a) =
            while not (Interlocked.TryChange(&cell, f)) do
                Thread.Sleep(0)
        
    type TimeFrame<'a when 'a : not struct>(length : TimeSpan, zero : 'a, add : 'a -> 'a -> 'a, sub : 'a -> 'a -> 'a, div : 'a -> int -> 'a) =
        let changer = Mod.custom id
        let values = ConcurrentQueue<DateTime * 'a>()
        let mutable tickInstalled = false

        let mutable sum = zero
        let mutable count = 0

        let rec prune (rc : int) (t : DateTime) =
            match values.TryPeek() with
                | (true, (tv,v)) ->
                    if tv < t - length then
                        match values.TryDequeue() with
                            | (true, (tv,v)) ->
                                Interlocked.Decrement &count |> ignore
                                Interlocked.Change(&sum, fun s -> sub s v)
                                prune (rc + 1) t
                            | _ -> 
                                rc
                    else
                        rc
                | _ -> 
                    rc

        let emit (value : 'a) =
            let now = DateTime.Now
            
            // prune everything and add the new value
            prune 0 now |> ignore
            values.Enqueue (now, value)

            // change count and sum accordingly
            Interlocked.Increment &count |> ignore
            Interlocked.Change(&sum, fun s -> add s value)

            // finally mark the changer since the average will be outdated
            transact (fun () -> changer.MarkOutdated())


        let tick() =
            // prune all old values
            let pruned = prune 0 DateTime.Now

            // if values have been pruned we're outdated
            if pruned > 0 then
                transact (fun () -> changer.MarkOutdated())

        let installTick (self : TimeFrame<'a>) =
            if not tickInstalled then
                tickInstalled <- true
                lock tickFunctions (fun () -> 
                    tickFunctions.Add(self, tick)
                )

        let sumAndCountMod =
            lazy (
                changer |> Mod.map (fun () ->
                    // prune all old values
                    prune 0 DateTime.Now |> ignore

                    (sum,count)
                )
            )

        let averageMod =
            lazy ( sumAndCountMod.Value |> Mod.map (fun (s,c) -> if c = 0 then zero else div s c) )

        let sumMod =
            lazy ( sumAndCountMod.Value |> Mod.map fst )


        let countMod =
            lazy ( sumAndCountMod.Value |> Mod.map snd )


        override x.Finalize() =
            if tickInstalled then
                try
                    lock tickFunctions (fun () -> 
                        tickFunctions.Remove(x) |> ignore
                    )  
                with _ -> ()

        member x.Emit (value : 'a) =
            installTick x
            emit value

        member x.Average =
            installTick x
            averageMod.Value

        member x.Count =
            installTick x
            countMod.Value

        member x.Sum =
            installTick x
            sumMod.Value

    let inline timeFrame (length : TimeSpan) =
        TimeFrame(length, LanguagePrimitives.GenericZero, (+), (-), LanguagePrimitives.DivideByInt)


module DefaultOverlays =
    
    let timeString (t : TimeSpan) =
        let ticks = t.Ticks
        if ticks >= TimeSpan.TicksPerDay then 
            sprintf "%.3fdays" (float ticks / float TimeSpan.TicksPerDay)
        elif ticks >= TimeSpan.TicksPerHour then
            sprintf "%.3fh" (float ticks / float TimeSpan.TicksPerHour)
        elif ticks >= TimeSpan.TicksPerMinute then
            sprintf "%.3fh" (float ticks / float TimeSpan.TicksPerMinute)
        elif ticks >= TimeSpan.TicksPerSecond then
            sprintf "%.2fs" (float ticks / float TimeSpan.TicksPerSecond)
        elif ticks >= TimeSpan.TicksPerMillisecond then
            sprintf "%.2fms" (float ticks / float TimeSpan.TicksPerMillisecond)
        elif ticks >= TimeSpan.TicksPerMillisecond / 1000L then
            sprintf "%.1fµs" (1000.0 * float ticks / float TimeSpan.TicksPerMillisecond)
        else
            "0µs"

    let mapKind (k : ResourceKind) =
        match k with 
         | ResourceKind.Buffer -> "B"
         | ResourceKind.Texture -> "T"
         | ResourceKind.Framebuffer -> "F"
         | ResourceKind.SamplerState -> "S"
         | ResourceKind.Renderbuffer -> "R"
         | ResourceKind.StreamingTexture -> "ST"
         | ResourceKind.ShaderProgram -> "P"
         | ResourceKind.UniformLocation -> "UL"
         | ResourceKind.UniformBuffer -> "U"
         | ResourceKind.VertexArrayObject -> "V"
         | _ -> "?"
         
    let printResourceUpdateCounts (r : Map<ResourceKind,float>) =
        let sorted = r |> Map.filter (fun k v -> v <> 0.0) |> Map.toArray
        sorted.QuickSortDescending(snd)
        let takes = min (Array.length sorted) 3
        let mutable result = ""
        for i in 0 .. takes - 1 do
            let k,v = sorted.[i] 
            if i <> 0 then
             result <- sprintf "%s/%.0f%s" result v (mapKind k)
            else result <- sprintf "%.0f%s" v (mapKind k) 
        if result = "" then "none" else result
        
    let memoryString (mem : uint64) =
        if mem > 1073741824UL then
            sprintf "%.3fGB" (float mem / 1073741824.0) 
        elif mem > 1048576UL then
            sprintf "%.3fMB" (float mem / 1048576.0) 
        elif mem > 1024UL then
            sprintf "%.3fkB" (float mem / 1024.0)
        else
            sprintf "%db" mem

    let statisticsTable (s : FrameStatistics) =
        [
            "draw calls", sprintf "%.0f" s.DrawCallCount
            "instructions", (if s.InstructionCount = s.ActiveInstructionCount then sprintf "%.0f" s.InstructionCount else sprintf "%.0f (%.0f)" s.ActiveInstructionCount s.InstructionCount)
            "primitives", sprintf "%.0f" s.PrimitiveCount
            "execute", timeString s.ExecutionTime
            "resource update", timeString s.ResourceUpdateTime
            "resource updates", printResourceUpdateCounts s.ResourceUpdateCounts
            "instruction update", timeString s.InstructionUpdateTime
            "renderjobs", sprintf "+%.0f/-%.0f" s.AddedRenderJobs s.RemovedRenderJobs
            "resources", sprintf "%.0f" s.ResourceCount
            "memory", memoryString s.ProgramSize
        ]

    let tableString (t : list<string * string>) =
        let labelwidth = t |> List.map fst |> List.map String.length |> List.max
        let suffix = ": "

        let addws (str : string) =
            if str.Length < labelwidth then
                str + suffix + String(' ', labelwidth - str.Length)
            else
                str + suffix

        t |> List.map (fun (l,v) ->
            let l = addws l
            l + v
          )
          |> String.concat "\r\n"


    let statisticsOverlay (runtime : IRuntime) (m : IMod<FrameStatistics>) =
        let content = m |> Mod.map (statisticsTable >> tableString)

        let text =
            content 
                |> Nvg.text
                |> Nvg.systemFont "Consolas" FontStyle.Bold
                |> Nvg.fillColor ~~C4f.White
                |> Nvg.fontSize ~~13.0
        
        let text = Nvg.ContextApplicator(runtime.GetNanoVgContext(), Mod.constant text)

        let overall = ref Box2d.Invalid
        let rect =
            text.LocalBoundingBox() 
               |> Mod.map (fun bb -> 
                    let mutable b = bb.EnlargedBy(V2d(10.0, 10.0))
                    overall := b.Union(!overall)
                    !overall
                  )
               |> Mod.map (fun b -> RoundedRectangle(b, 10.0))

               |> Nvg.fill
               |> Nvg.fillColor ~~(C4f(0.0, 0.0, 0.0, 0.5))
               

        let sg = 
            Nvg.ofList [rect; text]
                |> Nvg.trafo ~~(M33d.Translation(V2d(20.0, 20.0)))

        runtime.CompileRender sg

    type AnnotationRenderTask(real : IRenderTask, annotation : IRenderTask, emit : RenderingResult -> unit) as this =
        inherit AdaptiveObject()

        let mutable upToDateExec = 0

        do real.AddOutput this
           annotation.AddOutput this

        interface IRenderTask with
            member x.Dispose() =
                real.RemoveOutput x
                annotation.RemoveOutput x

            member x.Runtime = real.Runtime

            member x.Run f =
                base.EvaluateAlways (fun () ->
                    // TODO: 1) does not work when rendering continuously
                    //       2) does also not work when annotating the same task multiple times
                    if not x.OutOfDate then upToDateExec <- upToDateExec + 1
                    else upToDateExec <- 0


                    if real.OutOfDate || upToDateExec > 1 then
                        let real = real.Run f
                        emit real
                        let annotation = annotation.Run f
                        RenderingResult(annotation.Framebuffer, real.Statistics + annotation.Statistics)
                    else
                        let real = real.Run f
                        let annotation = annotation.Run f
                        RenderingResult(annotation.Framebuffer, real.Statistics + annotation.Statistics)
                )

    let withStatistics (t : IRenderTask) =
        match t.Runtime with
            | Some runtime ->
                let frame = Statistics.timeFrame (TimeSpan.FromMilliseconds 100.0)
                let emit (r : RenderingResult) = frame.Emit r.Statistics

                let overlay = statisticsOverlay runtime frame.Average

                let task = new AnnotationRenderTask(t, overlay, emit)
                task :> IRenderTask
            | _ -> 
                Log.warn "could not determine the original task's runtime"
                t
   
       

    
