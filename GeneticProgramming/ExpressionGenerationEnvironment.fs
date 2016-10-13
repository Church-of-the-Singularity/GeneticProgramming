module GeneticProgramming.ExpressionGenerationEnvironment

open GeneticProgramming
open GeneticProgramming.AST
open GeneticProgramming.Types

type VariableInfo =
    {   Type: ExpressionType
        Recursive: bool }

/// Expression tree generation environment
type ExpressionGenerationEnvironment =
    {   VarTypes: Map<ExpressionType, int Set>
        Vars: Map<int, VariableInfo>
        Random: System.Random
        Depth: int }

    static member Empty =
        {   VarTypes = Map.empty; Vars = Map.empty;
            Random = System.Random(); Depth = 1 }

    static member Create random =
        {   VarTypes = Map.empty; Vars = Map.empty;
            Random = random; Depth = 1 }

let declare (term: TermInfo) recVal env =
    let newVars = Map.add term.Term { Type = term.Type; Recursive = recVal } env.Vars
    let newTypeVars =
        match Map.tryFind term.Type env.VarTypes with
        | None -> Set.singleton term.Term
        | Some vars -> Set.add term.Term vars
    { env with
        VarTypes = Map.add term.Type newTypeVars env.VarTypes;
        Vars = newVars  }

let declareRandom vtype recVal env =
    let varID = env.Random.Next()
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    declare { Term = varID; Type = vtype } recVal env, varID