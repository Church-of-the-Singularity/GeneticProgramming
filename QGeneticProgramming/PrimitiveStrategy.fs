namespace QGeneticProgramming

open Microsoft.FSharp.Collections

open GeneticProgramming
open GeneticProgramming.AST

[<System.Serializable>]
type PrimitiveStrategy (mutator: IMutator<Expression>,
                        breeder: IInterBreeder<Expression>,
                        fitness: IFitness<Expression, int64>) =
    let luckers = 20
    let generationSize = 400
    
    member this.GenerationSize = generationSize
    member this.Luckers = luckers

    member this.Evolve(generation: Expression list) =
        fitness.OnNewGeneration()

        let error = memoize fitness.Fitness

        let mutants =
            generation
            |> Seq.map(fun individual -> mutator.Mutate(individual))
            |> Seq.toList

        let cross =
            List.append generation mutants
            |> Seq.sortBy(fun func -> error func)
            |> Seq.toList
            |> Seq.truncate luckers
            |> List.ofSeq

        let children =
            Seq.cartesian cross cross
            |> Seq.filter (fun (a, b) -> a <&> b)
            |> Seq.map (fun (a, b) -> breeder.Cross(a, b))
            |> Seq.toList

        let newGeneration =
            Seq.concat [children; mutants; cross]
            |> Seq.distinctBy box
            |> Seq.sortBy error
            |> Seq.truncate generationSize
            |> Seq.toList

        let newGeneration =
            newGeneration
            |> Seq.filter(fun f -> error f < int64 System.Int32.MaxValue)
            |> Seq.toList

        // printfn " best error: %d" (error newGeneration.[0])
        newGeneration, error newGeneration.Head

    interface IEvolutionStrategy<Expression list, int64> with
        member this.Evolve(generation) = this.Evolve(generation)

    interface IRandomReset with
        member this.Reset() =
            let reset (obj: obj) =
                match obj with
                | :? IRandomReset as resettable ->
                    resettable.Reset()
                | _ -> ()
            reset mutator
            reset breeder
            reset fitness