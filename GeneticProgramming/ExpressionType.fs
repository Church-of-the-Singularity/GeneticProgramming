namespace GeneticProgramming

module private Helper =
    let genericList = typeof<int list>.GetGenericTypeDefinition()
    let genericFunc = typeof<int -> int>.GetGenericTypeDefinition()

open Helper

type ExpressionType =
    | Integer
    | TyVar of int
    | List of ExpressionType
    | Function of ExpressionType * ExpressionType

    member this.ToClrType() =
        match this with
        | Integer -> typeof<int>
        | List elemType ->
            genericList.MakeGenericType[| elemType.ToClrType() |]
        | Function(arg, res) ->
            genericFunc.MakeGenericType[| arg.ToClrType(); res.ToClrType() |]
        | _ -> invalidArg "ExpressionType" "TyVars do not have appropriate CLR types"

    override this.ToString() =
        match this with
        | Integer -> "int"
        | TyVar(n) -> "'" + string(char(int 'a' + n))
        | List(subtype) -> sprintf "list<%O>" subtype
        | Function(Integer as arg, result) | Function((TyVar _) as arg, result)
        | Function((List _) as arg, result) ->
            sprintf "%O -> %O" arg result
        | Function(arg, result) -> sprintf "(%O) -> %O" arg result

    static member CurriedFunction(arguments, resultType) =
        match arguments with
        | arg :: [] -> Function(arg, resultType)
        | arg :: restArgs ->
            Function(arg, ExpressionType.CurriedFunction(restArgs, resultType))
        | _ -> invalidArg "arguments" "Argument list must not be empty"