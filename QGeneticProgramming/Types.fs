module QGeneticProgramming.Types

open GeneticProgramming
open GeneticProgramming.Types

let private genericList = typeof<int list>.GetGenericTypeDefinition()
let private genericFunc = typeof<int -> int>.GetGenericTypeDefinition()

let rec toExpressionType (exprType: System.Type) =
    if exprType = typeof<int> then Some(Integer)
    elif exprType.GetGenericArguments().Length = 0 then
        None
    elif exprType.GetGenericTypeDefinition() = genericList then
        Some(List (toExpressionType <| exprType.GetGenericArguments().[0]).Value)
    elif exprType.GetGenericTypeDefinition() = genericFunc then
        let [| fromType; toType |] =
            exprType.GetGenericArguments()
            |> Array.map (toExpressionType >> Option.get)
        Some(Function(fromType, toType))
    else
        raise <| System.InvalidOperationException()

let rec toClrType (exprType: ExpressionType) =
    match exprType with
    | Integer -> typeof<int>
    | List elemType ->
        let elemType = toClrType elemType
        typedefof<_ list>.MakeGenericType(elemType)
    | Function(argType,resultType) ->
        let argType = toClrType argType
        let resultType = toClrType resultType
        typedefof<_ -> _>.MakeGenericType(argType, resultType)
    | _ ->
        raise <| System.ArgumentException()