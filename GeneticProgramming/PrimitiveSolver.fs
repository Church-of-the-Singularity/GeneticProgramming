namespace GeneticProgramming

open Lost.Into
open GeneticProgramming.AST


type PrimitiveSolver<'a, 'P when 'P :> IInto<int>>(strategy: IEvolutionStrategy<ERef<'P> list, 'a>) =
    let mutable generation = 0L
    let mutable currentGeneration = []
    let mutable isRunning = false

    let iterationCompleted = Event<System.EventHandler, System.EventArgs>()

    member this.GenerationCount = generation
    member this.CurrentGeneration = currentGeneration

    member this.IterationCompleted = iterationCompleted.Publish
    member this.LoadGeneration(newGeneration, ?index) =
        currentGeneration <- newGeneration
        generation <- defaultArg index 0L

    member this.Continue() =
        if isRunning then invalidOp "Solver is running"
        if currentGeneration.IsEmpty then
            invalidOp "You must load non-empty generation first"

        this.Run()

    interface ISolver<'P> with
        member this.GenerationCount = this.GenerationCount
        member this.CurrentGeneration = this.CurrentGeneration
        member this.IterationCompleted = this.IterationCompleted
        member this.LoadGeneration(generation, ?index) =
            match index with
            | None -> this.LoadGeneration(generation)
            | Some(index) -> this.LoadGeneration(generation, index)
        member this.Continue() = this.Continue()

    member private this.Run() =
        if isRunning then invalidOp "Solver is running"
        isRunning <- true
        
        while isRunning do
            let newGeneration, _ = strategy.Evolve currentGeneration
            System.Diagnostics.Trace.Assert(not newGeneration.IsEmpty, "Evolved to empty generation")
            currentGeneration <- newGeneration
            generation <- generation + 1L
            iterationCompleted.Trigger(this, System.EventArgs.Empty)
            