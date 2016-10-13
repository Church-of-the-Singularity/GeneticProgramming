namespace GeneticProgramming.Execution

//#region Opens
open ComputationResult

open Swensen.Unquote

open GeneticProgramming.AST
//#endregion

type QuotationExecutor<'i, 'o>() =
    static let compile (expr: Expression) =
        let fsexpr = QuotationCompilerInternals.compile expr
        #if DEBUG
        //let source = fsexpr.Decompile()
        #endif
        let func = fsexpr.Eval()
        let invoke = func.GetType().GetMethod("Invoke")
        fun o -> invoke.Invoke(func, [|o|])

    member this.Execute(expr: Expression, input: obj): obj =
        let func = compile expr
        if func =&= null then
            raise <| System.InvalidProgramException()
        else
            func(input)

    member this.Execute(expr: Expression, input: 'i):ComputationResult<'r, exn> =
        try
            Success(downcast this.Execute(expr, box input))
        with
            e ->
                Fail(e.ActualCause)

    interface GeneticProgramming.Execution.IExpressionExecutor<'i, 'o> with
        member this.Execute(_, expr, input) =
            this.Execute(expr, input)