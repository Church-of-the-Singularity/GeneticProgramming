namespace GeneticProgramming.Execution

open GeneticProgramming.AST

type DlrCompiler() =
    member this.Compile(expr: Expression): ICompiledExpression<'args, 'result> =
        let linqExpr = GeneticProgramming.Execution.DlrCompilerInternals.compile expr
        let linqExprWithinLambda =
            [| linqExpr |]
            |> System.Linq.Expressions.Expression.Block
            |> System.Linq.Expressions.Expression.Lambda
        let compiledExpr = linqExprWithinLambda.Compile()
        let mdelegate = compiledExpr.DynamicInvoke() :?> System.Delegate
        let compiled = DlrCompiledExpression<'args, 'result>(mdelegate)
        upcast compiled

    member this.DataModel = DlrDataModel.Instance

    interface IExpressionCompiler with
        member this.Compile(expr) = this.Compile(expr)
        member this.DataModel = upcast this.DataModel

