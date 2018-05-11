module internal GeneticProgramming.Execution.DlrCompilerInternals

open System.Reflection

type DlrExpression = System.Linq.Expressions.Expression
type DlrVariable = System.Linq.Expressions.ParameterExpression
type private TranslationEnvironment =
    {   Variables: Map<string, DlrVariable>
        TailCallJump: option<byte * System.Linq.Expressions.LabelTarget * DlrVariable list>  }

let private emptyTranslationEnvironment =
    {   Variables = Map.empty
        TailCallJump = None }

open GeneticProgramming
open GeneticProgramming.AST

open GeneticProgramming.DynamicRuntimeCompiler

let mutable memoryLimit = System.Int64.MaxValue

type RuntimeHelper private() =
    static member CallChecks() = Cancellation.callCheck()

let toFSharpList(flist: FList<_>) = flist.AsFSharpList

let private dlrZero = DlrExpression.Constant(box 0)
let private dlrOne = DlrExpression.Constant(box 1)

let private callChecks: DlrExpression =
    upcast(typeof<RuntimeHelper>.GetMethod("CallChecks", BindingFlags.Static ||| BindingFlags.NonPublic) |> DlrExpression.Call)

let private genericList = typeof<int FList>.GetGenericTypeDefinition()
let private genericFunc = typeof<System.Func<int, int>>.GetGenericTypeDefinition()

let rec lastExpression expression =
    match expression: DlrExpression with
    | :? System.Linq.Expressions.BlockExpression as block when block.Expressions.Count > 0 ->
        lastExpression block.Expressions.[block.Expressions.Count - 1]
    | _ -> expression

let rec toClrType = function
    | Integer -> typeof<int>
    | List elemType ->
        genericList.MakeGenericType[| elemType |> toClrType |]
    | Function(arg, res) ->
        genericFunc.MakeGenericType[| arg |> toClrType; res |> toClrType |]
    | _ -> invalidArg "ExpressionType" "TyVars do not have appropriate CLR types" 

let rec private compileRec expr env: DlrExpression =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

//    let compile expr env =
//        let result = compile expr env
//        if result.NodeType = System.Linq.Expressions.ExpressionType.Lambda then
//            (result :?> System.Linq.Expressions.LambdaExpression).Compile() |> ignore
//        result

    let compileEnv = compileRec
    let compile expr = compileRec expr { env with TailCallJump = None }
    let compileWith (var: DlrVariable) expr =
        let newEnv = Map.add var.Name var env.Variables
        compileEnv expr { Variables = newEnv; TailCallJump = None }
//    let compileRec (var: DlrVariable) varID argVar label expr =
//        let newEnv = Map.add var.Name var env.Variables
//        compileEnv expr { Variables = newEnv; TailCallJump = Some(varID,argVar,label) }
//    let compileWithRec (var: DlrVariable) expr =
//        let newEnv = Map.add var.Name var env.Variables
//        compileEnv expr { env with Variables = newEnv }
    let doReturn (expr: DlrExpression) =
        match env.TailCallJump with
        | None -> expr
        | Some(_,target,_) -> upcast DlrExpression.Break(target, expr)
    match expr with
    | Zero -> doReturn dlrZero
    | One -> doReturn dlrOne
    | BinOp(opType, v1, v2) ->
            let v1v = compile v1
            let v2v = compile v2
            
            match opType with
            | Sum -> doReturn <| DlrExpression.Add(v1v, v2v)
            | Diff -> doReturn <| DlrExpression.Subtract(v1v, v2v)
            | Mul -> doReturn <| DlrExpression.Multiply(v1v, v2v)
            | Less ->
                let isLess = DlrExpression.LessThan(v1v, v2v)
                doReturn <| DlrExpression.Condition(isLess, dlrOne, dlrZero)

    | TriOp(opType, v1, v2, zero) ->
        let v1v = compile v1
        let v2v = compile v2
        let zeroV = compile zero
        let op =
            match opType with
            | Div -> DlrExpression.Divide(v1v, v2v)
            | Mod -> DlrExpression.Modulo(v1v, v2v)
        doReturn <| DlrExpression.Condition(DlrExpression.Equal(v2v, dlrZero), zeroV, op)

    | Apply(func, value) ->
        match env.TailCallJump, func with
//        | Some(fterm, label, args), Term(var, vtype) when var = fterm ->
//            let valueV = compile value
//            upcast DlrExpression.Assign(arg, valueV)
        | Some(fterm, label, parameters), TermApplication(var, innerArgs) when var.Term = fterm ->
            let args = value :: innerArgs
            let argValues = List.map compile args |> List.rev
            let assigns = List.map2(fun param arg -> DlrExpression.Assign(param, arg) :> DlrExpression)
                                parameters argValues
            upcast DlrExpression.Block(callChecks :: assigns)

        | _ ->
            let funcV = compile func
            let valueV = compile value
            let application = DlrExpression.Invoke(funcV, valueV)
            doReturn <| DlrExpression.Block(callChecks, application)

    | Cons(h, t) ->
        let headV = compile h
        let tailV = compile t
        let listType = t.ComputeType() |> toClrType
        let cons = listType.GetMethod("Cons")
        doReturn <| DlrExpression.Call(cons, headV, tailV)

    | EmptyList(t) ->
        let listType = ExpressionType.List t |> toClrType
        doReturn <| DlrExpression.Property(null, listType, "Empty")

    | IsZero(v) ->
        let value = compile v
        let isZero = DlrExpression.Equal(value, dlrZero)
        doReturn <| DlrExpression.Condition(isZero, dlrOne, dlrZero)

    | Lambda(var, expr) ->
        let variable = DlrExpression.Parameter(toClrType var.Type, sprintf "v%d" var.Term)
        let body = compileWith variable expr
        doReturn <| DlrExpression.Lambda(body, true, variable)

    | Length(expr) ->
        let list = compile expr
        let listType = expr.ComputeType() |> toClrType
        doReturn <| DlrExpression.Property(list, "Length")

    | Let { Recursive = isRec
            Term = var
            Value = value
            Expression = expr } ->
        match isRec, value with
        | true, MultiArityLambda(parameters, recExpr) ->
                let valueType = value.ComputeType()
                let recType = valueType |> toClrType
                let varLambda = DlrExpression.Variable(recType, sprintf "v%d" var)

                let recArgs = 
                    parameters
                    |> List.map(fun p -> 
                        DlrExpression.Parameter(p.Type |> toClrType, sprintf "v%d" p.Term))
                            

                let lambdaResultType = recExpr.ComputeType()
                let branchTarget = DlrExpression.Label(toClrType lambdaResultType, sprintf "r%d" var)
                let innerVars = env.Variables.Add(varLambda.Name, varLambda)
                let lambdaBody = compileEnv recExpr {
                        Variables = recArgs
                                    |> List.fold(fun vars param -> vars.Add(param.Name, param))
                                                 innerVars
                        TailCallJump = Some(var, branchTarget, recArgs)
                    }
                let lambdaLoop = DlrExpression.Loop(lambdaBody, branchTarget) :> DlrExpression
                let lambda =
                    recArgs
                    |> List.rev
                    |> List.fold(fun (expr: DlrExpression) param ->
                                    DlrExpression.Lambda(expr, param) :> DlrExpression)
                                lambdaLoop

                let exprValue = compileWith varLambda expr
                let recBlock =
                    [   DlrExpression.Assign(varLambda, lambda) :> DlrExpression
                        exprValue   ]
                doReturn <| DlrExpression.Block([| varLambda |], recBlock)

        | _ ->
            let varValue = compile value
            let varType = value.ComputeType() |> toClrType
            let variable = DlrExpression.Variable(varType, sprintf "v%d" var)
            let exprValue = compileWith variable expr
            let block =
                [   DlrExpression.Assign(variable, varValue) :> DlrExpression
                    exprValue   ]
            doReturn <| DlrExpression.Block([|variable|], block)
                    
    | Cond(cond, onTrue, onFalse) ->
        let conditionValue = compile cond
        let isTrue = DlrExpression.NotEqual(conditionValue, dlrZero)
        let trueExpr = compileEnv onTrue env
        let falseExpr = compileEnv onFalse env
        let cond = 
            match lastExpression falseExpr with
            | :? System.Linq.Expressions.BinaryExpression as b
                when b.NodeType = System.Linq.Expressions.ExpressionType.Assign ->
                DlrExpression.IfThenElse
            | :? System.Linq.Expressions.GotoExpression as g
                when g.Kind = System.Linq.Expressions.GotoExpressionKind.Break ->
                DlrExpression.IfThenElse
            | _ -> DlrExpression.Condition
        upcast cond(isTrue, trueExpr, falseExpr)

    | Match {   List = list
                EmptyCase = empty
                HeadTail = nempty   } ->
        let listExpr = compile list
        let emptyExpr = compileEnv empty env
        let nemptyExpr = compile nempty

        match list.ComputeType(), empty.ComputeType() with
        ExpressionType.List(elemType), resultType ->
            let elemTypeClr = elemType |> toClrType
            let resultTypeClr = resultType |> toClrType
            let isEmpty = DlrExpression.Property(listExpr, "IsEmpty")
            let head = DlrExpression.Property(listExpr, "Head")
            let tail = DlrExpression.Property(listExpr, "Tail")
            let nemptyCall =
                DlrExpression.Invoke(
                    DlrExpression.Invoke(nemptyExpr, head),
                    tail)
                |> doReturn
            upcast DlrExpression.Condition(isEmpty, emptyExpr, nemptyCall)

    | Rand v ->
        let upperBound = compile v
        invalidOp "Rand"

    | Term(var) ->
        let varName = sprintf "v%d" var.Term
        doReturn env.Variables.[varName]

let compile expr = compileRec expr emptyTranslationEnvironment