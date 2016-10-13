namespace GeneticProgramming.Execution

open ComputationResult

type private CompiledQuotation<'args, 'result> internal(value: obj) =
    static let argumentCount =
        if Reflection.FSharpType.IsTuple typeof<'args> then
            let elements = Reflection.FSharpType.GetTupleElements typeof<'args>
            elements.Length
        else 1

    static let convertInput =
        match argumentCount with
        | 0 -> fun _ -> invalidOp "Should never be called"
        | 1 -> fun x -> [| x |]
        | _ ->
            Reflection.FSharpValue.PreComputeTupleReader typeof<'args>

    static let argTypes =
        if argumentCount < 1 then
            null
        elif argumentCount = 1 then
            [| typeof<'args> |]
        else
            let value = Reflection.FSharpType.GetTupleElements typeof<'args>
            assert(value.Length = argumentCount)
            value

    static let functionType =
        let genericType = Lost.FSharp.Reflection.makeGenericFunctionType argumentCount
        if argumentCount = 0 then
            genericType
        else
            genericType.MakeGenericType(Array.append argTypes [| typeof<'result> |])

    static let invoke =
        match argumentCount with
        | 1 ->
            let invoke = functionType.GetMethod "Invoke"
            fun func args -> invoke.Invoke(func, args)

        | n when n > 1 && n <= 5 ->
            let invokeFast =
                functionType.GetMethods()
                |> Seq.filter (fun m -> m.Name = "InvokeFast" && m.GetParameters().Length = argumentCount)
                |> Seq.exactlyOne
            fun func args ->
                invokeFast.Invoke(func, args)

        | n when n > 5 ->
            // TODO: optimize if necessary
            let invoke = functionType.GetMethod "Invoke"
            fun func args ->
                let mutable value = func
                for arg in args do
                    value <- invoke.Invoke(value, [| arg |])
                value

        | _ -> argOutOfRange "argumentCount"

    //#region Messages
    static let incorrectParameterCount = sprintf "%d parameters expected" argumentCount
    static let valueParametersMismatch = "Parameters in value do not match 'args"
    //#endregion

    do
        if isNull value then
            nullArg "value"

        if CompiledQuotation<'args, 'result>.IsFunction then
            let actualType = clrType value
            let actualArguments = Lost.FSharp.Reflection.getFunctionArguments actualType
            let args = Seq.take argumentCount actualArguments
            // TODO: check result type
            let matches = (argTypes, args)
                          ||> Seq.forall2 (fun actual expected -> expected.IsAssignableFrom(actual))
            if not matches then
                invalidArg "value" valueParametersMismatch

    static member IsFunction = argumentCount > 0

    member this.Invoke(parameters: 'args): ComputationResult.ComputationResult<'result, exn> =
        if argumentCount = 0 then
            ComputationResult.Success(unbox value)
        else
        let compiledParameters = convertInput parameters
        let value = this.Invoke(compiledParameters)
        ComputationResult.map cast value

    member private this.Invoke([<System.ParamArray>]parameters: obj[]): ComputationResult<obj, exn> =
        if parameters.Length <> argumentCount then
            invalidArg "parameters" incorrectParameterCount

        if argumentCount = 0 then
            ComputationResult.Success(value)
        else
        try
            Success(invoke value parameters)
        with
            e -> Fail(e)

    interface ICompiledExpression<'args, 'result> with
        member this.Invoke([<System.ParamArray>] parameters: obj []) = this.Invoke(parameters)
        member this.Invoke(parameters: 'args) = this.Invoke(parameters)