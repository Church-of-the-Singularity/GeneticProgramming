namespace GeneticProgramming

open GeneticProgramming.AST

type IRandomReset =
    abstract Reset: unit -> unit

type Randomized(random) =
    let mutable random = random

    member this.Random = random

    interface IRandomReset with
        member this.Reset() =
            random <- System.Random()

type IEvolutionStrategy<'gen, 'e> =
    abstract Evolve: 'gen -> 'gen * 'e

type IMutator<'gene> =
    abstract Mutate: 'gene -> 'gene

type IInterBreeder<'gene> =
    abstract Cross: 'gene * 'gene -> 'gene

type IFitness<'i, 'fv> =
    abstract OnNewGeneration: unit -> unit
    abstract Fitness: 'i -> 'fv

open GeneticProgramming.AST.Pooled
open Lost.Into

type GenerationsCollectedEventArgs(generationCount) =
    inherit System.EventArgs()

    member this.GenerationCount = generationCount

type ISolver<'P when 'P :> IInto<int>> =
    abstract GenerationCount: int64
    abstract IterationCompleted: IEvent<System.EventHandler, System.EventArgs>
    abstract CurrentGeneration: ERef<'P> list
    abstract LoadGeneration: ERef<'P> list * ?index:int64 -> unit
    abstract Continue: unit -> unit
