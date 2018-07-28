namespace GeneticProgramming

open GeneticProgramming.AST
open GeneticProgramming.AST.Pooled
open Lost.Into
open Lost.Pointers.Pools

[<System.Serializable>]
type PrimitiveInterBreeder<'P when 'P :> IInto<int>>(random: System.Random, pool: ExpressionPool<'P>) =
    inherit Randomized(random)

    [<Literal>]
    static let probabilityPrecision = 1000000

    member private this.Subexpressions eref =
        let expr = Ref pool.Underlying eref
        let children = getRecSubexpressions pool.Underlying expr

        let attachFreeVariables(eref: ERef<'P>) =
            let variables = getFreeVariables pool.Underlying eref
            let closed =
                Map.fold
                    (fun eref var vtype -> Lambda({Term = var; Type = vtype}, eref) |> GcNew pool)
                    eref variables
            assert((getFreeVariables pool.Underlying closed).Count = 0)
            #if DEBUG
            assert(computeType pool.Underlying eref = computeType pool.Underlying closed)
            #endif
            closed

        let append exprMap eref =
            let exprType = computeType pool.Underlying eref
            let exprs = Map.getOrDef exprType [] exprMap
            Map.add exprType (eref :: exprs) exprMap

        children
        |> List.filter (fun e -> (getFreeVariables pool.Underlying e).IsEmpty)
        |> List.map attachFreeVariables
        |> List.fold append Map.empty

    member this.Crossover(crossoverProbability, e1, e2: ERef<'P>): ERef<'P> =
        let e1, e2 = if this.Random.Next(2) = 0 then e1, e2 else e2, e1
        let e2map = this.Subexpressions e2
        let cut expr =
            let exprType = computeType pool.Underlying expr
            if this.Random.Next(probabilityPrecision) >= crossoverProbability then expr
            else
                match Map.tryFind exprType e2map with
                | None -> expr
                | Some genes ->
                    let gene = this.Random.Next(genes.Length)
                    List.item gene genes
        map pool cut e1

    member this.Crossover(crossoverProbability, e1, e2) =
        this.Crossover(int <| (crossoverProbability * float probabilityPrecision),
                        e1, e2)

    interface IInterBreeder<ERef<'P>> with
        member this.Cross(e1, e2) =
            let mutable result = this.Crossover(probabilityPrecision / 20, e1, e2)
            while nodeCount pool.Underlying result > 160 do
                result <- this.Crossover(probabilityPrecision / 20, e1, e2)
            result
