namespace QGeneticProgramming.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open GeneticProgramming
open GeneticProgramming.AST
open GeneticProgramming.Execution

type ExecutorTestsBase(compilerFactory) =
    static let longTimeout = 30*1000
    static let timeout = 500

    let applyTwice (func2: Expression) =
        match func2.ComputeType() with
        | Function(argType, Function(argType', _)) when argType = argType'->
            let v = int 'v'
            let vVar = makeTerm v argType
            Lambda(termOfTuple(v, argType), Apply(Apply(func2, vVar), vVar))
        | _ -> invalidArg "func2" "Must be of type 'a -> 'a -> 'b"

    let double =
        let a, b, s, v = int 'a', int 'b', int 's', int 'v'
        let sumType = Function(Integer, Function(Integer, Integer))
        let aVar, bVar, sVar, vVar = makeTerm a Integer, makeTerm b Integer, makeTerm s sumType, makeTerm v Integer

        let aPrev = BinOp(Diff, aVar, One)
        let bNext = BinOp(Sum, bVar, One)
        let sumPrevNext = Apply(Apply(sVar, aPrev), bNext)
        let sumBody = Cond(IsZero(aVar), bVar, sumPrevNext)
        let sumFunc = Lambda({ Term = a; Type = Integer}, Lambda({Term = b; Type = Integer}, sumBody))
        let sumLetRec = Let{Recursive=true;Term=s;Value=sumFunc;Expression=sVar}
        // let s = \a -> \b -> if a=0 then b else s (a-1) (b+1)
        // let s = \a -> if a = 0 then \b -> b else \b -> s(a-1) (b+1)
        applyTwice sumLetRec 
        //Lambda({ Term = int 'v'; Type = Integer}, Apply(Apply(sumLetRec, vVar), vVar))

    let isEven =
        let n,e = int 'n', int 'e'
        let evenType = Function(Integer, Integer)
        let nVar, eVar = makeTerm n Integer, makeTerm e evenType

        let nMinus2 = BinOp(Diff, nVar, IntegerConstants.two)
        let onZeroOne = IsZero(nVar)
        let evenBody = Cond(BinOp(Less, nVar, IntegerConstants.two),
                                onZeroOne,
                                Apply(eVar, nMinus2))
        let evenFunc = Lambda({ Term = n; Type = Integer}, evenBody)
        let evenLetRec = Let{Recursive=true;Term=e;Value=evenFunc;Expression=eVar}
        // let e = n -> if n < 2 then isZero n else e(n-2)
        evenLetRec

    let customFold =
      function Function(stateType, Function(elemType, stateType')) as folderType when stateType = stateType' ->
                let fold, folder, state, list = int 'f', int 'F', int 's', int 'l'
                let head, tail = int 'h', int 't'
                let foldType = ExpressionType.CurriedFunction([folderType; stateType; List elemType], stateType)
//                let rec fold folder state list =
//                    match list with
//                    | [] -> state
//                    | head :: tail -> fold folder (folder state head) tail
                let foldTerm = makeTerm fold foldType
                let folderTerm = makeTerm folder folderType
                let stateTerm = makeTerm state stateType
                let ``rec`` =
                    applyMultiple
                        foldTerm
                        [   folderTerm
                            applyMultiple folderTerm [stateTerm; makeTerm head elemType]
                            makeTerm tail (List elemType)]
                let lambdaBody =
                    Match{  List = makeTerm list (List elemType);
                            EmptyCase = stateTerm;
                            HeadTail = Lambda(termOfTuple(head, elemType),
                                        Lambda(termOfTuple(tail, List elemType), ``rec``)) }
                let body =
                    Lambda(termOfTuple(folder, folderType),
                        Lambda(termOfTuple(state, stateType),
                            Lambda(termOfTuple(list, List elemType), lambdaBody)))

                Let{ Recursive = true; Term = fold; Value = body; Expression = foldTerm }

    let customLength elemType =
        let l, u = int 'l', int 'u'
        // let length = fold (fun l u -> l + 1) 0
        let fold =
            ExpressionType.CurriedFunction([Integer; elemType], Integer)
            |> customFold
        let folder =
            curriedLambda [ termOfTuple(l, Integer); termOfTuple(u, elemType)]
                          (Plus (makeTerm l Integer) One)
        applyMultiple fold [folder; Zero]
    
    let customReverse elemType =
        let r, e = int 'r', int 'e'
        // let rev = List.fold (fun r e -> e :: r) []
        let fold =
            ExpressionType.CurriedFunction([List elemType; elemType], List elemType)
            |> customFold
        let folder =
            curriedLambda [ termOfTuple(r, List elemType); termOfTuple(e, elemType) ]
                          (Cons (makeTerm e elemType, makeTerm r (List elemType)))
        applyMultiple fold [folder; EmptyList elemType]

    let customFilter elemType =
        let cond, r, e = int 'c', int 'r', int 'e'
        let list = int 'l'
        let condType = Function(elemType, Integer)
        let filter =
            fun cond list ->
                List.rev(List.fold (fun r e -> if cond e then e :: r else r) [] list)
        let resultTerm = makeTerm r (List elemType)
        let elemTerm = makeTerm e elemType
        let fold =
            ExpressionType.CurriedFunction([List elemType; elemType], List elemType)
            |> customFold
        let filterExpr =
            Cond(Apply(makeTerm cond condType, elemTerm),
                 Cons(elemTerm, resultTerm),
                 resultTerm)
        let folder =
            curriedLambda [ termOfTuple(r, List elemType); termOfTuple(e, elemType) ]
                          filterExpr
        curriedLambda [ termOfTuple(cond, condType); termOfTuple(list, List elemType) ]
            (Apply(  customReverse elemType,
                    applyMultiple fold [folder; EmptyList elemType; makeTerm list (List elemType)]))

    let customAppend elemType =
        let res, e, a, b = int 'r', int 'e', int 'a', int 'b'
        // fun a b -> List.fold(fun res e -> e :: res) b (List.rev a)
        let fold =
            ExpressionType.CurriedFunction([List elemType; elemType], List elemType)
            |> customFold
        let folder =
            curriedLambda [ termOfTuple(res, List elemType); termOfTuple(e, elemType) ]
                          (Cons (makeTerm e elemType, makeTerm res (List elemType)))
        let rev = customReverse elemType
        curriedLambda[ termOfTuple(a, List elemType); termOfTuple(b, List elemType) ]
            (applyMultiple fold
                [   folder
                    makeTerm b (List elemType)
                    Apply(rev, makeTerm a (List elemType)) ])

//    let customQsort =
//        
//        let rec qsort list =
//            match list with
//            | [] -> []
//            | e :: rest ->
//                let less = List.filter (fun x -> x < e) rest
//                let notLess = List.filter (fun x -> x >= e) rest
//                append less (e :: notLess)
                          
    let createExecutor(): IExpressionExecutor<'a, 'b> =
        let compiler = compilerFactory()
        upcast PrimitiveExecutor(compiler)

    let testListLength (list: int list) =
        let len = List.length list
        let executor: IExpressionExecutor<int list, int> = createExecutor()
        let computed = executor.Execute(timeout, customLength Integer, list).Value
        Assert.AreEqual(len, computed)

    let testReverse (list: int list) =
        let reversed = List.rev list
        let executor: IExpressionExecutor<int list, int list> = createExecutor()
        let computed = executor.Execute(timeout, customReverse Integer, list).Value
        Assert.AreEqual(box reversed, computed)

    let testCustomFilter(list: int list) =
        let filtered = List.filter(fun e -> e > 0) list
        let filter = Lambda(
                        termOfTuple(1, Integer),
                        BinOp(Less, Zero, makeTerm 1 Integer))
        let filterNonPositive = Apply(customFilter Integer, filter)
        let executor: IExpressionExecutor<int list, int list> = createExecutor()
        let computed = executor.Execute(timeout, filterNonPositive, list).Value
        Assert.AreEqual(filtered, computed)

    let testCustomAppend(a: int list) =
        let sum = a ++ a |> List.ofSeq
        let selfAppend =
            let append = customAppend Integer
            applyTwice append
        let executor: IExpressionExecutor<int list, int list> = createExecutor()
        let computed = executor.Execute(timeout, selfAppend, a).Value
        Assert.AreEqual(sum, computed)

    [<TestMethod>]
    member this.DoubleIsCorrect() =
        let executor: IExpressionExecutor<int, int> = createExecutor()
        let twoHundreds = executor.Execute(timeout, double, 100).Value
        Assert.AreEqual(box 200, twoHundreds)

    [<TestMethod>]
    member this.IsEvenIsCorrect() =
        let executor: IExpressionExecutor<int, int> = createExecutor()
        let even103 = executor.Execute(timeout, isEven, 103).Value
        Assert.AreEqual(box 0, even103)

        let even240 = executor.Execute(timeout, isEven, 240).Value
        Assert.AreNotEqual(box 0, even240)

    [<TestMethod>]
    member this.ComplexTailCallsWork() =
        let executor: IExpressionExecutor<int, int> = createExecutor()
        let twoThousands = executor.Execute(timeout, double, 1000).Value
        Assert.AreEqual(2000, twoThousands)

        let twoBillions = executor.Execute(longTimeout, double, 1000000000).Value
        Assert.AreEqual(2000000000, twoBillions)

    [<TestMethod>]
    member this.SimpleTailCallsWork() =
        let executor: IExpressionExecutor<int, int> = createExecutor()
        let even3 = executor.Execute(timeout, isEven, 3).Value
        Assert.AreEqual(box 0, even3)

        let even10000003 = executor.Execute(timeout, isEven, 10000003).Value
        Assert.AreEqual(box 0, even10000003)

    [<TestMethod>]
    member this.MatchTailCallSmall() =
        testListLength [4; -1; 10]
    
    [<TestMethod>]
    member this.MatchTailCallLarge() =
        List.init 1000000 id |> testListLength

    [<TestMethod>]
    member this.ReverseSmall() =
        testReverse [-100; 50; 0; 41]

    [<TestMethod>]
    member this.FilterSmall() =
        testCustomFilter [40; 0; -10; -10; 20; 20]

    [<TestMethod>]
    member this.AppendSmall() =
        testCustomAppend [-3; 0; -1; 42; 11]

module Compilers =
    let unquoteCompilerFactory(): IExpressionCompiler = upcast QuotationCompiler()
    let dlrCompilerFactory(): IExpressionCompiler = upcast DlrCompiler()

[<TestClass>]
type UnquoteExecutorTests() =
    inherit ExecutorTestsBase(Compilers.unquoteCompilerFactory)

[<TestClass>]
type DlrExecutorTests() =
    inherit ExecutorTestsBase(Compilers.dlrCompilerFactory)