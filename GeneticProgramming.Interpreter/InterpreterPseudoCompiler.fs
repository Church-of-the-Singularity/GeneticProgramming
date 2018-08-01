namespace GeneticProgramming.Interpreter

open ComputationResult

open Lost.Pointers
open Lost.Pointers.Pools

open GeneticProgramming.AST
open GeneticProgramming.Execution
open GeneticProgramming.ShortDataModel

type private InterpreterPseudoExpression<'P, 'args, 'result>
             (lambda, dataModel: ShortDataModel<DataType>) =
  member this.Invoke(parameters: 'args): ComputationResult<'result, exn> =
    let input = dataModel.Convert parameters
    let output = lambda input
    Success(downcast dataModel.ConvertBack(output, typeof<'result>))

  interface ICompiledExpression<'args, 'result> with
    member this.Invoke([<System.ParamArray>] parameters: obj[]): ComputationResult<obj, exn> = notImplemented()
    member this.Invoke(parameters: 'args) =
      this.Invoke(parameters)

type InterpreterPseudoCompiler<'P>
     (codePool: IPool<'P, Expression<'P>>)=
  let interpreter = Interpreter(codePool)
  let dataModel = ShortDataModel<DataType>(interpreter.DataPool, interpreter.DataGC)

  member private this.CompileInternal(eref: ERef<_>) =
    match interpreter.Evaluate(eref) with
    | Value.Word v -> notImplemented()
    | Value.Lambda lambda ->
      let wordOnlyLambda arg =
        match lambda (Value.Word arg) with
        | Value.Word word -> word
        | Value.Lambda _ -> notImplemented()
      
      InterpreterPseudoExpression(wordOnlyLambda, dataModel)

  member this.DataModel = dataModel

  interface IExpressionCompiler<'P> with
    member this.Compile<'args, 'result>(eref: ERef<'P>):ICompiledExpression<'args,'result> = upcast this.CompileInternal(eref)
    member this.DataModel = upcast this.DataModel

