namespace GeneticProgramming.Execution

open GeneticProgramming
open GeneticProgramming.AST
open Lost.Into

module Cancellation =
    open System.Threading

    let callCounter = new ThreadLocal<int>()
    let token = new ThreadLocal<CancellationToken>()

    let inline private checkCancel() =
        if token.Value.IsCancellationRequested then
            System.OperationCanceledException() |> raise

    let callCheck() =
        let newValue = callCounter.Value + 1
        callCounter.Value <- newValue

        System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

        if newValue &&& 0xFFF = 0 then
            checkCancel()

type IExpressionExecutor<'P, 'i, 'o when 'P :> IInto<int>> =
    abstract Execute: timeLimit:int * expr: ERef<'P> * input: 'i ->
                       ComputationResult.ComputationResult<'o, exn>

type ICompiledExpression<'args, 'result> =
    /// Evalueates expression value in native data model (F# lists + int32)
    abstract Invoke: parameters: 'args -> ComputationResult.ComputationResult<'result, exn>
    /// Evalueates expression value in compiler data model (IExpressionCompiler.DataModel)
    abstract Invoke: parameters: obj [] -> ComputationResult.ComputationResult<obj, exn>

type IExpressionCompiler<'P> =
    /// Compiles specified expression
    abstract Compile: expr: ERef<'P> -> ICompiledExpression<'args, 'result>
    /// Compiler data module (for translating parameters)
    abstract DataModel: IDataModel