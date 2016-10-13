namespace GeneticProgramming.Execution

open Swensen.Unquote

open GeneticProgramming.AST

type QuotationCompiler() =
    member this.Compile(expr: Expression): ICompiledExpression<'args, 'result> =
        let fsexpr = QuotationCompilerInternals.compile expr
        #if DEBUG
        //let source = fsexpr.Decompile()
        #endif
        let func = fsexpr.Eval()
        let compiled = CompiledQuotation<'args, 'result>(func)
        upcast compiled

    static member PrettyPrint expr =
        let fsexpr = QuotationCompilerInternals.compile expr
        fsexpr.Decompile()

    member this.DataModel = GeneticProgramming.FSharpDataModel.Instance

    interface IExpressionCompiler with
        member this.Compile(expr) = this.Compile(expr)
        member this.DataModel = upcast this.DataModel
