namespace GeneticProgramming.Execution

open ComputationResult

type private DlrCompiledExpression<'args, 'result> internal(compiled: System.Delegate) =
    member this.Invoke([<System.ParamArray>]parameters: obj[]): ComputationResult<obj, exn> =
        notImplemented()

    member this.Invoke(parameters: 'args): ComputationResult<'result, exn> =
        let input = DlrDataModel.Convert parameters
        let output = compiled.DynamicInvoke(input)
        Success(downcast DlrDataModel.ConvertBack output)

    interface ICompiledExpression<'args, 'result> with
        member this.Invoke([<System.ParamArray>]parameters: obj[]) = this.Invoke(parameters)
        member this.Invoke(parameters: 'args) = this.Invoke(parameters)

