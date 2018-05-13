module private GeneticProgramming.Execution.QuotationCompilerInternals

open System.Reflection

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

open Monads.Maybe

#if DEBUG
open Swensen.Unquote
#endif

open GeneticProgramming
open GeneticProgramming.AST
open GeneticProgramming.Types

let private stackCheck =
    <@@ Cancellation.callCheck() @@>

type private Helper private() =
    static member MakeMatch<'e, 'r>(list, empty, nempty) =
        let expr =
          if typeof<'r> = typeof<unit> then
            <@@ match %%list: 'e list with
            | [] -> %%empty : unit
            | head :: tail -> %%nempty : unit @@>
          else
            <@@ match %%list: 'e list with
            | [] -> %%empty : 'r
            | head :: tail -> (%%nempty : 'e -> 'e list -> 'r) head tail @@>
        let free = expr.GetFreeVars() |> List.ofSeq
        expr
    static member GetDefaultOf<'t>() = Unchecked.defaultof<'t>

let private genericMakeMatch =
        typeof<Helper>.GetMethod("MakeMatch",
            BindingFlags.NonPublic ||| BindingFlags.Static)
//let private defaultOf ``type`` =
//    let defOf = typeof<Helper>.GetMethod("GetDefaultOf",
//                    BindingFlags.NonPublic ||| BindingFlags.Static)
//    let getDefOf = defOf.MakeGenericMethod[| ``type`` |]
//    Expr.Value(Expr.Call(getDefOf, []), ``type``)

let private genericList = typeof<int list>.GetGenericTypeDefinition()
let private genericFunc = typeof<int -> int>.GetGenericTypeDefinition()
let private listCases concreteList =
    // let concreteList = genericList.MakeGenericType[| elemType |]
    let [| empty; cons |] = FSharpType.GetUnionCases(concreteList)
    empty, cons

type private TailCallInfo =
    {   Var: TermInfo
        Result: System.Type
        Arguments: Var list  }

type private TranslationEnvironment =
    {   TailCall: option<TailCallInfo>;
        Vars: Map<byte, Var>;
        SystemVars: Map<string, Var> }

let private emptyTranslationEnvironment = { TailCall = None; Vars = Map.empty; SystemVars = Map.empty }

let private makeRef (expr: Expr) =
    let refType = FSharpType.MakeReferenceType expr.Type
    let ctor = refType.GetConstructor([| expr.Type |])
    Expr.NewRecord(refType, [expr])

let private refValue(expr: Expr) = expr.Type.GetProperty("Value")

let private getRef (expr: Expr) = Expr.PropertyGet(expr, refValue expr)

let private setRef expr value = Expr.PropertySet(expr, refValue expr, value)

//#region Variables
let private varNameS suffix term ``type`` =
    Var(sprintf "v%d%s" term suffix, ``type``)

let private varName = varNameS ""

let varResult term ``type`` =
    let rtype = FSharpType.MakeReferenceType ``type``
    varNameS "Result" term rtype
let varRun term = varNameS "Run" term (FSharpType.MakeReferenceType typeof<bool>)
let varArgs term ``type`` =
    let argsType = FSharpType.MakeReferenceType ``type``
    varNameS "Args" term argsType
let private varMap vars = Seq.map (fun (var:Var) -> var.Name, var) vars |> Map.ofSeq
//#endregion

#if PROFILE
let private tags = FSharpType.GetUnionCases typeof<Expression>
let nodeCount = System.Collections.Generic.Dictionary<string, int64>()
#endif

let private addVars vars env = { env with Vars = vars |> Map.fold (fun vars id (var: Var) -> Map.add id var vars) env.Vars }

let rec private compileRec this env =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

    let compileFull expr newEnv =
    #if DEBUG
      maybe {
        let! overwrite = newEnv.Vars.TryFind 0uy
        let! old = env.Vars.TryFind 0uy
        assert(System.Object.ReferenceEquals(overwrite, old))
        return()
      } |> ignore
    #endif
      compileRec expr newEnv
    let compile expr = compileRec expr { env with TailCall = None }
    let compilePass expr = compileRec expr env

    let doReturn expr =
        match env.TailCall with
        | None -> expr
        | Some{ Var = var; Result = resultType} ->
            let result =
              Map.find (varResult var.Term resultType).Name env.SystemVars
              |> Expr.Var
            let run =
              Map.find (varRun var.Term).Name env.SystemVars
              |> Expr.Var
            let assignResult = setRef result expr
            //assert(expr.Type.Equals(resultType))
            <@@ %%assignResult
                %%run := false @@>

    #if PROFILE
    lock(nodeCount)(fun () ->
        let case,_ = FSharpValue.GetUnionFields(this, typeof<Expression>)
        nodeCount.[case.Name] <- Dict.getOrDefault case.Name 0L nodeCount + 1L

        if false then
            let v =
                nodeCount
                |> Seq.sortBy (fun kv -> -kv.Value)
                |> Seq.map (fun kv -> sprintf "%s = %d" kv.Key kv.Value)
                |> Seq.toArray
            System.Diagnostics.Debug.Write(System.String.Join("\n", v))
    )
    #endif
    
    match this with
    | Zero -> doReturn <@@ 0 @@>

    | Lambda(term, expr) ->
        let variable = Var(sprintf "v%d" term.Term, toClrType term.Type)
        let env = { env with Vars = Map.add term.Term variable env.Vars }
        Expr.Lambda(variable, compileFull expr env)
        |> doReturn

    | BinOp(opType, v1, v2) ->
        let v1 = compile v1
        let v2 = compile v2
        match opType with
        | Sum -> doReturn <@@ %%v1 + %%v2 @@>
        | Diff -> doReturn <@@ %%v1 - %%v2 @@>
        | Mul -> doReturn <@@ %%v1 * %%v2 @@>
        | Less -> doReturn <@@ if (%%v1: int) < (%%v2: int) then 1 else 0 @@>

    | Apply(func, value) ->
        match env.TailCall, func with
        | Some{ Var = recVar; Arguments = args }, TermApplication(var, innerArgs) when var = recVar ->
            // let args = varArgs var.Term argsTupleType |> Expr.Var
            let values = List.rev (value :: innerArgs)
            // let argsTuple = List.map compile allArgs |> Expr.NewTuple
            // setRef args argsTuple
            let assignments =
                List.map2 (fun var value -> Expr.VarSet(var, compile value)) args values
            assignments
            |> List.reduce(curry Expr.Sequential)

        | _ ->
            let compiledFunction = compile func
            let compiledArgument = compile value
            let application = Expr.Application(compiledFunction, compiledArgument)
            //Expr.Sequential(stackCheck, application)
            application
            |> doReturn

    | EmptyList(t) ->
        let empty, _ = listCases((ListType t).ToClrType())
        Expr.NewUnionCase(empty, [])
        |> doReturn

    | Term(var) ->
        let var = Map.getOrDef var.Term Unchecked.defaultof<Var> env.Vars
        assert(var <> Unchecked.defaultof<Var>)
        Expr.Var(var)
        |> doReturn

    | Let { Recursive = isRec
            Term = var
            Value = value
            Expression = expr } ->
        match isRec, value with
        | true, MultiArityLambda(parameters, recExpr) ->
            //let varInfo = { Term = var; Type = value.ComputeType() }
            let valueType = value.ComputeType()
            let varExpr = varName var (toClrType valueType)
            let runVar = varRun var

            let resultType = toClrType <| recExpr.ComputeType()
            let resultVar = varResult var resultType
            let env = { env with SystemVars = Map.merge (varMap [| runVar; resultVar |]) env.SystemVars }

            let argVars =
                parameters
                |> List.map (fun p -> p.Term, Var(sprintf "v%d" p.Term, toClrType p.Type, true))
            let argList = argVars |> List.map (fun (_, v) -> v)
//            let argsTupleType =
//                parameters
//                |> List.toArray
//                |> Array.map (fun p -> toClrType p.Type)
//                |> FSharpType.MakeTupleType
//            let argsVar = varArgs var argsTupleType
//            let argsValue =
//                parameters
//                |> List.map (fun p -> varName p.Term (toClrType p.Type) |> Expr.Var)
//                |> Expr.NewTuple

            let tailInfo = {    Var = { Term = var; Type = valueType }
                                Result = resultType
                                Arguments = argList }
            let env = { env with Vars = Map.add var varExpr env.Vars }
            let valueEnv = addVars (Map.ofList argVars) env
            let valueBody = compileFull recExpr { valueEnv with TailCall = Some tailInfo }
            
            let defResult =
                if resultType = typeof<int> then Expr.Value 0
                else Expr.Value(null, resultType)
            let startVars =
                [   runVar, <@@ ref true @@>
//                    argsVar, makeRef argsValue
                    resultVar, makeRef defResult ]
            let resultExpr = getRef(Expr.Var resultVar)
            let loopBody =
                Expr.Sequential(
                        <@@ while !(%%(Expr.Var(runVar))) do
                                Cancellation.callCheck()
                                %%valueBody @@>,
                        resultExpr)
            let functionBody =
                List.fold (fun e (var, value) -> Expr.Let(var, value, e)) loopBody startVars
                |> List.foldBack (fun var e -> Expr.Let(var, Expr.Var(var), e)) argList
            let functionLambda =
                List.foldBack (fun (_, param: Var) e ->
                            Expr.Lambda(param, e))
                          argVars functionBody 
            let exprValue = compileFull expr { env with TailCall = None }
            let letRec = Expr.LetRecursive([varExpr, functionLambda], exprValue)
            #if DEBUG
            //let source = letRec.Decompile()
            #endif
            doReturn letRec

        | _ ->
            let varValue = compile value
            let variable = Var(sprintf "v%d" var, value.ComputeType().ToClrType())
            let env = { env with Vars = Map.add var variable env.Vars }
            let exprValue = compileFull expr { env with TailCall = None }
            if isRec then
                Expr.LetRecursive([variable, varValue], exprValue)
            else
                Expr.Let(variable, varValue, exprValue)
            |> doReturn

    | One -> doReturn <@@ 1 @@>

    | Cond(cond, onTrue, onFalse) ->
        let condExpr = compile cond
        let trueExpr = compilePass onTrue
        let falseExpr = compilePass onFalse
        Expr.IfThenElse(<@@ %%condExpr <> 0 @@>, trueExpr, falseExpr)

    | Cons(h, t) ->
        let head = compile h
        let tail = compile t
        let _, cons = listCases(t.ComputeType().ToClrType())
        Expr.NewUnionCase(cons, [head; tail])
        |> doReturn

    | Match {   List = list
                EmptyCase = empty
                HeadTail = nempty   } ->
        let listExpr = compile list
        let emptyExpr = compilePass empty
        //let unused = HashSet.ofSeq [|0uy..255uy|]
        //unused.ExceptWith(this.GetUsedVariables() |> List.map (fun v -> v.Term))
        //unused.ExceptWith(Map.keys env.Vars)
        //let matchHead = Seq.head unused
        //unused.Remove matchHead |> ignore
        //let matchTail = Seq.head unused

        match list.ComputeType(), empty.ComputeType() with
        List(elemType), resultType ->
            let elemTypeClr = elemType.ToClrType()
            let resultTypeClr = resultType.ToClrType()
            let tailTypeClr = toClrType(ListType elemType)

            let nemptyExpr = compilePass nempty
                //let localHead = { Term = byte matchHead; Type = elemType }
                //let localTail = { Term = byte matchTail; Type = ListType elemType }
                //let reducedBody = Apply(Apply(nempty, Term localHead), Term localTail) |> AST.reduce

                //let headVar = varName localHead.Term elemTypeClr
                //let tailVar = varName localTail.Term tailTypeClr

                //let bodyEnv = addVars (Map.ofList [(localHead.Term, headVar); (localTail.Term, tailVar)]) env

                //let body = compileFull reducedBody bodyEnv

                //let head = Var("head", elemTypeClr)
                //let tail = Var("tail", tailTypeClr)
                //Expr.Let(headVar, Expr.Var head,
                //    Expr.Let(tailVar, Expr.Var tail, body))

            #if DEBUG
            let nemptyAst = nempty.ToString()
            let nemptySource = nemptyExpr.Decompile()
//            let emptySource = emptyExpr.Decompile()
            #endif

            let matchResultType = if env.TailCall = None then resultTypeClr else typeof<unit>
            let makeMatch = genericMakeMatch.MakeGenericMethod[| elemTypeClr; matchResultType |]
            let compiledMatch =
              try
                  let matchExpr = makeMatch.Invoke(null, [| listExpr; emptyExpr; nemptyExpr |])
                  downcast matchExpr
              with
                  :? TargetInvocationException as e ->
                      reraise()
            compiledMatch


    | IsZero(v) ->
        let value = compile v
        doReturn <@@ if %%value = 0 then 1 else 0 @@>

    | TriOp(opType, v1, v2, zero) ->
        let v1 = compile v1
        let v2 = compile v2
        let zero = compile zero
        let op =
            match opType with
            | Div -> <@@ (%%v1: int) / %%v2 @@>
            | Mod -> <@@ (%%v1: int) % %%v2 @@>
        doReturn <@@ if %%v2 = 0 then (%%zero: int) else %%op @@>

    | Length(expr) ->
        let list = compile expr
        let listType = expr.ComputeType().ToClrType()
        Expr.PropertyGet(list, listType.GetProperty("Length"))
        |> doReturn

    | Rand v ->
        let upperBound = compile v
        invalidOp "Rand"

let compile expr = compileRec expr emptyTranslationEnvironment