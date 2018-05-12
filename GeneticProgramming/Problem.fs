namespace GeneticProgramming

open Microsoft.FSharp.Quotations

open GeneticProgramming.Types

type Problem =
    {   TargetType: ExpressionType
        Etalone: Expr
        Fitness: Expr   }