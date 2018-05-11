module QGeneticProgramming.Values

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

open GeneticProgramming

let rec makeDefaultValue etype =
    match etype with
    | Integer -> <@@ 0 @@>, typeof<int>
    | List(elemType) ->
        let defElemTypeValue, memberType = makeDefaultValue elemType
        let listType = typedefof<_ list>.MakeGenericType(memberType)
        let [|empty; cons|] = FSharpType.GetUnionCases(listType)
        let emptyList = Expr.NewUnionCase(empty, [])
        Expr.NewUnionCase(cons, [defElemTypeValue; emptyList]), listType
    | Function(argType, resultType) ->
        let resultDefaultValue, resultType = makeDefaultValue resultType
        let _, argType = makeDefaultValue argType
        Expr.Lambda(Quotations.Var("_", argType), resultDefaultValue),
        typedefof<_ -> _>.MakeGenericType(argType, resultType)
    | _ -> failwith "invalid etype"
