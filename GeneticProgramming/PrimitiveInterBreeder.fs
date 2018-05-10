namespace GeneticProgramming

open GeneticProgramming.AST

[<System.Serializable>]
type PrimitiveInterBreeder(random: System.Random) =
    inherit Randomized(random)

    [<Literal>]
    static let probabilityPrecision = 1000000

    member private this.Subexpressions expr =
        let children = getRecSubexpressions expr

        let attachFreeVariables expr =
            let variables = getFreeVariables expr
            let closed =
                Map.fold
                    (fun expr var vtype -> Lambda({Term = var; Type = vtype}, expr))
                    expr variables
            assert(closed.GetFreeVariables().Count = 0)
            closed

        let append exprMap expr =
            let exprType = computeType expr
            let exprs = Map.getOrDef exprType [] exprMap
            Map.add exprType (expr :: exprs) exprMap

        children
        |> List.map attachFreeVariables
        |> List.fold append Map.empty

    member this.Crossover(crossoverProbability, e1, e2) =
        let e1, e2 = if this.Random.Next(2) = 0 then e1, e2 else e2, e1
        let e2map = this.Subexpressions e2
        let cut expr =
            let exprType = computeType expr
            if this.Random.Next(probabilityPrecision) >= crossoverProbability then expr
            else
                match Map.tryFind exprType e2map with
                | None -> expr
                | Some genes ->
                    let gene = this.Random.Next(genes.Length)
                    List.item gene genes
        map cut e1

    member this.Crossover(crossoverProbability, e1, e2) =
        this.Crossover(int <| (crossoverProbability * float probabilityPrecision),
                        e1, e2)

    interface IInterBreeder<Expression> with
        member this.Cross(e1, e2) =
            this.Crossover(probabilityPrecision / 20, e1, e2)