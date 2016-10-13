namespace QGeneticProgramming

open System.Diagnostics
open System.Threading

open GeneticProgramming
open GeneticProgramming.AST

type EvolutionWorker(initialGeneration, strategy: IEvolutionStrategy<Expression list, int64>) as this =
    [<VolatileField>]
    let mutable index = 0L
    [<VolatileField>]
    let mutable generation = initialGeneration
    [<VolatileField>]
    let mutable best = System.Int64.MaxValue
    [<VolatileField>]
    let mutable pause = false

    let thread = new Thread(ThreadStart(this.Run), IsBackground = true, Priority = ThreadPriority.BelowNormal)

    member this.CurrentGeneration = generation
    member this.ProcessedCount = index
    member this.Best = best
    member this.Pause with get() = pause and set v = pause <- v

    member this.Start() = thread.Start()

    member this.ThreadID = thread.ManagedThreadId

    member private this.Run() =
        while true do
            while pause do Thread.Sleep(1000)
            this.Evolve()

    member private this.Evolve() =
        let newGeneration, newBest = strategy.Evolve generation
        if List.length newGeneration = 0 then
            Trace.TraceError("all individuals are dead")
            #if DEBUG
            eprintfn "all individuals are dead"
            #endif
        elif newGeneration = generation then
            Trace.TraceError("evolution stucked")
            #if DEBUG
            eprintfn "evolution stucked"
            #endif
        else
        generation <- newGeneration
        best <- newBest
        index <- index + 1L