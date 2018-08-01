namespace GeneticProgramming.Execution

open ComputationResult
open Lost.Into

open GeneticProgramming.AST


type PrimitiveExecutor<'i, 'o, 'P when 'P :> IInto<int>>(compiler: IExpressionCompiler<'P>, ?cache, ?cacheLimit) =
    let cache: System.Collections.Generic.Dictionary<ERef<'P>, System.WeakReference> = defaultArg cache null
    let cacheLimit = defaultArg cacheLimit 256*1024

    static let executed = ref 0L
    static let failed = ref 0L

    let clearCache() = Dict.removeWhereKey(fun expr -> not cache.[expr].IsAlive) cache

    let compile = 
        let compile =
            fun (expr: ERef<'P>) ->
                let compiledExpression = compiler.Compile<'i,'o>(expr)
                fun o -> compiledExpression.Invoke(o: 'i)

        if cache =&= null then compile
        else memoizeWeakWithCache cache compile

    member this.Cache = cache
    static member Executed = !executed
    static member Failed = !failed

    member private this.Compile(expr: ERef<'P>) =
        if cache <&> null && cache.Count > cacheLimit then
            clearCache()

        let func = compile expr
        if func =&= null then
            raise <| System.InvalidProgramException()
        else
            func
            // invoke.Invoke(func, [| input |])

    member this.Execute(expr: ERef<'P>, input: 'i):ComputationResult<'r, exn> =
        System.Threading.Interlocked.Increment(executed) |> ignore
        try
            let compiled = this.Compile expr
            let result = compiled input
            ComputationResult.map unbox result
        with
            e ->
                System.Threading.Interlocked.Increment(failed) |> ignore
                Fail(e.ActualCause)

    interface IExpressionExecutor<'P, 'i, 'o> with
        member this.Execute(timeLimit, expr, input) =
            use canceller = new System.Threading.CancellationTokenSource()
            canceller.CancelAfter(timeLimit)

            #if ASYNC
            let task = async {
                    Cancellation.token.Value <- canceller.Token
                    //let fsexpr = expr.Compile()
                    //let func = fsexpr.Eval()
                    return
            #endif
                        this.Execute(expr, input)
            #if ASYNC
                }
            Async.RunSynchronously(task, timeLimit)
            #endif
