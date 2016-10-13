namespace GeneticProgramming

open Microsoft.FSharp.Quotations

type Problem =
    {   TargetType: ExpressionType
        Etalone: Expr
        Fitness: Expr   }