namespace GeneticProgramming.Execution

open ComputationResult

open GeneticProgramming.AST

type DlrExecutor<'i,'o>() =
    static let compile(expr: Expression) =
        try
            let compiledExpr = GeneticProgramming.Execution.DlrCompilerInternals.compile expr
            let fsexpr =
                [| compiledExpr |]
                |> System.Linq.Expressions.Expression.Block
                |> System.Linq.Expressions.Expression.Lambda
            let mdelegate = fsexpr.Compile().DynamicInvoke() :?> System.Delegate
            fun o -> mdelegate.DynamicInvoke(o: obj)
        with e ->
            Unchecked.defaultof<obj -> obj>

    member this.Execute(expr: Expression, input: obj): obj =
        let func = compile expr
        if func =&= null then
            raise <| System.InvalidProgramException()
        else
            func(input)

    member this.Execute(expr: Expression, input: 'i):ComputationResult<'o, exn> =
        try
            Success(downcast this.Execute(expr, box input))
        with
            e ->
                Fail(e.ActualCause)

    interface IExpressionExecutor<'i,'o> with
        member this.Execute(_, expr, input) =
            this.Execute(expr, input)