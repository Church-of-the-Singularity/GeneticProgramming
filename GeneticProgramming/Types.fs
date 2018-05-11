module GeneticProgramming.Types

let rec curriedFunction argTypes resultType =
    match argTypes with
    | [] -> resultType
    | argType :: restArgs ->
        Function(argType, curriedFunction restArgs resultType)

let matchNEmptyHandler elemType resultType =
    curriedFunction [elemType; List elemType] resultType

let matchType elemType resultType =
    curriedFunction [
        List elemType;
        resultType;
        matchNEmptyHandler elemType resultType
    ] resultType
