module QGeneticProgramming.Types

open GeneticProgramming.Types

let private genericList = typeof<int list>.GetGenericTypeDefinition()
let private genericFunc = typeof<int -> int>.GetGenericTypeDefinition()

let rec toExpressionType (exprType: System.Type) =
    if exprType = typeof<int> then Some(Integer)
    elif exprType.GetGenericArguments().Length = 0 then
        None
    elif exprType.GetGenericTypeDefinition() = genericList then
        Some(ListType (toExpressionType <| exprType.GetGenericArguments().[0]).Value)
    elif exprType.GetGenericTypeDefinition() = genericFunc then
        let [| fromType; toType |] =
            exprType.GetGenericArguments()
            |> Array.map (toExpressionType >> Option.get)
        Some(Function(fromType, toType))
    else
        raise <| System.InvalidOperationException()
