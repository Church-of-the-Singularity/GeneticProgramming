module QGeneticProgramming.App

open System.Threading

open Microsoft.FSharp.Collections

open GeneticProgramming
open GeneticProgramming.AST
open GeneticProgramming.Execution

open QGeneticProgramming

let cacheItemLimit = 65536

type private ThreadLocal<'a> = System.Threading.ThreadLocal<'a>

let args = System.Environment.GetCommandLineArgs() |> Seq.skip 1 |> Seq.toArray
let cliOptions = CliOptions()
if not <| CommandLine.Parser.Default.ParseArguments(args, cliOptions) then
    let helpText = CommandLine.Text.HelpText()
    let parsingErrors = helpText.RenderParsingErrorsText(cliOptions, 0)
    eprintf "%s" parsingErrors
    System.Environment.Exit(-1)

let compilerFactory =
    match cliOptions.Compiler with
    | "DLR" -> fun () -> DlrCompiler() :> IExpressionCompiler
    | "Unquote" -> fun () -> QuotationCompiler() :> IExpressionCompiler
    | "Mix" ->
        let dlr = ref true
        fun () ->
            dlr := not !dlr
            if !dlr then
                DlrCompiler() :> IExpressionCompiler
            else
                QuotationCompiler() :> IExpressionCompiler

    | _ -> System.NotSupportedException(cliOptions.Compiler) |> raise

//do
//    System.AppDomain.CurrentDomain.AssemblyLoad.Add(fun assemblyArgs ->
//        printfn "%s" assemblyArgs.LoadedAssembly.FullName)
module private RunnerHider =
    let compiler = new ThreadLocal<IExpressionCompiler>(compilerFactory)
    let dlrCompiler = new ThreadLocal<IExpressionCompiler>(fun () -> DlrCompiler() :> IExpressionCompiler)
    let unquoteCompiler = new ThreadLocal<IExpressionCompiler>(fun () -> QuotationCompiler() :> IExpressionCompiler)
    let dataModel = new ThreadLocal<IDataModel>(fun () -> compiler.Value.DataModel)
//    let runnerObj = new System.Threading.ThreadLocal<IExpressionExecutor>(fun () ->
//                        PrimitiveExecutor(System.Collections.Generic.Dictionary(HashIdentity.LimitedStructural 50))
//                            :> IExpressionExecutor
//                     )
// let runner() = RunnerHider.runnerObj.Value

//#region sort
//#region framework
let rec inversions list acc =
    match list with
    | a :: b :: rest ->
        let error = if b < a then 1 else 0
        inversions (b::rest) (acc+error)
    | [_] | [] -> acc

let listError (input: int list) expected (func: ICompiledExpression<int list, int list>) =
    let actual =
        try
            func.Invoke(input)
        with :? System.Reflection.TargetInvocationException as e ->
            ComputationResult.Fail e.ActualCause

    match actual with
    | ComputationResult.Fail e ->
        if is<System.InsufficientExecutionStackException> e then
            System.Diagnostics.Debug.WriteLine("survived stack overflow!")
        int64(System.Int32.MaxValue)
    | ComputationResult.Success actual ->
    let error =
        let eset = Set.ofList expected
        let aset = Set.ofList actual
        let missing = Set.difference eset aset
        let spare = Set.difference aset eset

        let inversions = inversions actual 0

        let mainPropError = inversions + missing.Count + spare.Count + 65536
        if mainPropError = 65536 then
            let commonLen = min expected.Length actual.Length
            let different =
                Seq.zip expected actual
                |> Seq.take commonLen
                |> Seq.sumBy(fun (a, b) -> if a = b then 0 else 1)
            abs(expected.Length - actual.Length) * 2 + different
        else
            mainPropError

    int64 error

let canceledCount = ref 0L
let errorCount = ref 0L
let runCount = ref 0L

let checkCompilerEquality expr (inputs: int list list) =
    let dlrCompiled = RunnerHider.dlrCompiler.Value.Compile<int list, int list>(expr)
    let unquoteCompiled = RunnerHider.unquoteCompiler.Value.Compile<int list, int list>(expr)

    let dlrOut = inputs |> List.map dlrCompiled.Invoke
    let unquoteOut = inputs |> List.map unquoteCompiled.Invoke
    for dlr, unquote in List.zip dlrOut unquoteOut do
        if dlr <> unquote then
            printfn "%A <> %A" dlr unquote

let generateInputs(random: System.Random) =
    Seq.initInfinite (fun _ -> List Integer)
    |> Seq.choose(fun inType ->
        try
            let length = random.Next(64)
            let list = List.init length (fun _ -> random.Next())
            Some(list)
        with _ -> None)
    |> Seq.take 10
    |> List.ofSeq

let sortFitness inputs =
    let expected: int list list = inputs |> List.map List.sort

    fun (expr: Expression) ->
        use canceller = new CancellationTokenSource()
        canceller.CancelAfter(500)

        try
            System.Threading.Interlocked.Increment(runCount) |> ignore
            #if ASYNC
            let task = async {
                    let! token = Async.CancellationToken
                    Cancellation.token.Value <- token
                    let compiledExpr =
                        try
                            RunnerHider.compiler.Value.Compile<int list, int list>(expr)
                        with :? System.Reflection.TargetInvocationException as e ->
                            raise e.ActualCause

                    // checkCompilerEquality expr inputs
                    return
            #endif
                        List.map2 (fun input expected ->
                            listError input expected compiledExpr) inputs expected
                        |> List.sum
            #if ASYNC
                }
            Async.RunSynchronously(task, 500, canceller.Token)
            #endif
        with
            | :? System.OperationCanceledException
            | :? System.TimeoutException ->
                if cliOptions.Verbose then
                    eprintfn "individual run for too long"
                System.Threading.Interlocked.Increment(canceledCount) |> ignore
                int64 System.Int32.MaxValue

            | e ->
                if cliOptions.Verbose then
                    eprintfn "individual error: %O: %s" (clrType e) e.Message
                System.Threading.Interlocked.Increment(errorCount) |> ignore
                int64 System.Int32.MaxValue

//type SafeRandom() =
//    inherit System.Random()
//
//    let safe = System.Random()
//
//    override this.Next() = lock(safe) (fun () -> safe.Next())
//    override this.Next(up) = lock(safe) (fun () -> safe.Next(up))

[<System.Serializable>]
type SortFitness(random) =
    inherit Randomized(random)

    let mutable inputs = generateInputs(random)

    interface IFitness<Expression, int64> with
        member this.OnNewGeneration() = inputs <- generateInputs(random)
        member this.Fitness(expr) = sortFitness inputs expr

let sortType = Function(List(Integer), List(Integer))

//let random = SafeRandom()
//let mutator = PrimitiveMutator(random)
//let breeder = PrimitiveInterBreeder(random)
//
//let fitness = SortFitness(random)
//
//let strategy = PrimitiveStrategy(mutator, breeder, fitness)
//
//let initialGeneration =
//    List.init strategy.GenerationSize (fun _ -> mutator.RandomValue(sortType))
//#endregion

//do
//    
//    let mutable generationIndex = 0L
//    let mutable currentGeneration = initialGeneration
//    let timer = System.Diagnostics.Stopwatch()
//    while true do
//        timer.Restart()
//        printf "evolving %d..." generationIndex
//        generationIndex <- generationIndex + 1L
//        currentGeneration <- evolve (List Integer) List.sort currentGeneration
//        printfn "took %dms; %d individuals left"
//            timer.ElapsedMilliseconds
//            currentGeneration.Length
        //runner.PrintStats()
//#endregion sort

let tailTest() =
    let tailTest = <@   let rec sum a b =
                            if a > 0 then
                                System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
                                sum(a - 1) (b + 1)
                            else b
                        sum (1024*1024*128) 0 @>
    // let tailTestVal = tailTest.Eval()
    ()

// tailTest()

open System.Diagnostics

let dump generation fileName =
    use stream = new System.IO.StreamWriter(fileName: string)
    for e: Expression in generation do
        e.PrettyPrint(stream, 0)
        stream.WriteLine()
    stream.Close()

let overseerTest() =
    System.Console.Title <- cliOptions.Compiler

    let saveFile = "gen.bin"
    let sampleFile = "individ.txt"
    let stopwatch = Stopwatch()
    stopwatch.Start()

    #if !DEBUG
    let thisProc = System.Diagnostics.Process.GetCurrentProcess()
    ProcessOverseer.Program.attach
        [ProcessOverseer.ExitCodes.CtrlC] thisProc.Id
        (System.Reflection.Assembly.GetExecutingAssembly().Location) []
    |> ignore
    #endif
    Thread.CurrentThread.Priority <- ThreadPriority.Highest

    let workerCount = cliOptions.WorkerCount

    let generation, index =
        if System.IO.File.Exists(saveFile) then
            printf "loading..."
            let gen, idx = SolverStateSerializer.Deserialize(saveFile)
            printfn "loaded generation %d" idx
            gen, idx
        else
            printfn "started from scratch"
            let random = System.Random()
            let mutator = PrimitiveMutator(random)
            let breeder = PrimitiveInterBreeder(random)

            let fitness = SortFitness(random)

            let strategy = PrimitiveStrategy(mutator, breeder, fitness)
            let itemsToGenerate = strategy.GenerationSize * workerCount
            let initialGeneration = List.init strategy.GenerationSize (fun _ -> mutator.RandomValue(sortType))
            initialGeneration, 0L
    let generation = List.toArray generation

    printf "starting workers..."
    let workers = Array.init workerCount
                    (fun workerIndex ->
                        let random = System.Random()
                        let mutator = PrimitiveMutator(random)
                        let breeder = PrimitiveInterBreeder(random)

                        let fitness = SortFitness(random)

                        let strategy = PrimitiveStrategy(mutator, breeder, fitness)
                        let personalGeneration =
                            List.init (generation.Length / workerCount)
                                (fun i -> generation.[workerIndex + i * workerCount])
                        let worker = EvolutionWorker(personalGeneration, strategy)
                        worker.Start()
                        worker)
    printfn "done %d workers" workers.Length

    let threadIDs = workers |> Array.map (fun w -> w.ThreadID)

    printfn "Thread IDs: %A" threadIDs

    while true do
        Thread.Sleep(60000)

        printfn "time is out"

        let best = workers |> Array.map (fun w -> w.Best) |> Array.min
        let gens = workers |> Array.map (fun w -> w.ProcessedCount)
        let generation = workers |> Seq.collect (fun w -> w.CurrentGeneration) |> List.ofSeq
        let processedFromStart = workers |> Seq.sumBy(fun w -> w.ProcessedCount)

        let elapsed = stopwatch.ElapsedMilliseconds
        let currentGen = processedFromStart + index

        printf "saving..."
        SolverStateSerializer.Serialize(generation, currentGen, saveFile)
        printfn "saved generation %d in %d ms" currentGen (stopwatch.ElapsedMilliseconds - elapsed)
        
        if cliOptions.DumpSources then
            printf "saving samples..."
            try
                dump generation sampleFile
                printfn "DONE"
            with e ->
                printfn "FAILED: %s" e.Message
//        for worker in workers do
//            worker.Pause <- true
        printfn "gens: %A" gens
        printfn "average time on 1 generation: %d ms" (elapsed / max 1L processedFromStart)
        printfn "best result so far: %d" best
        printfn "program execution fail rate: %d%% (%d of %d; %d time out)"
            (!errorCount * 100L / !runCount)
            !errorCount !runCount
            !canceledCount
        printfn ""
//        printf "press Enter to continue... "
//        System.Console.ReadLine() |> ignore
//
//        for worker in workers do
//            worker.Pause <- false

ShortPointers.poolTest()

overseerTest()

//let problemSolverTest() =
//    let saveFile = "gen.bin"
//    let stopwatch = Stopwatch()
//    let startGeneration = ref 0L
//    stopwatch.Start()
//    let thisProc = System.Diagnostics.Process.GetCurrentProcess()
//    thisProc.PriorityClass <- System.Diagnostics.ProcessPriorityClass.Idle
//    let solver: ISolver =
//        //upcast ProblemSovler.ProblemSolver(strategy, Period = 60*1000)
//        upcast PrimitiveSolver(strategy)
//    let passedGenerations() = solver.GenerationCount - !startGeneration
//    solver.IterationCompleted.Add(fun _ ->
//        if solver.CurrentGeneration.Length > 5 then
//            SolverStateSerializer.Serialize(solver, saveFile)
//        printfn "current generation: %d; started %d seconds ago"
//            solver.GenerationCount (stopwatch.ElapsedMilliseconds / 1000L)
//        printfn "average time on 1 generation: %d ms"
//            (stopwatch.ElapsedMilliseconds / passedGenerations())
//    )
//    if System.IO.File.Exists(saveFile) then
//        SolverStateSerializer.Deserialize(solver, saveFile)
//        startGeneration := solver.GenerationCount
//        printfn "loaded generation №%d" !startGeneration
//    else
//        solver.LoadGeneration(initialGeneration)
//        printfn "started from scratch"
//    solver.Continue()

// TODO: single slave process
        
// problemSolverTest()
//let xxxTest() =
//    let testExpr =
//        Function(Integer, List(Integer))
//        |> makeDefaultValue
//        |> fst
//    let testExprValue = testExpr.Eval()
//    printfn "val test: %A = %A" testExpr testExprValue
//
//    let testType = List Integer
//    let strategy = PrimitiveStrategy(System.Random())
//    for i = 0 to 99 do
//        let tree = strategy.RandomValue(testType)
//        let value = tree.Compile()
//        printf "%A = " (value.Decompile())
//        printfn "%A" (value.Eval())
//
//        let mutated = strategy.Mutate(0.1, tree).Compile()
//        printf "mutated %A = " (mutated.Decompile())
//        printfn "%A" (mutated.Eval())
//
//    dpause()