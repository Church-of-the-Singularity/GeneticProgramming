module GeneticProgramming.Types

open System
open System.Collections.Specialized

[<Struct>]
[<CustomEquality>]
[<CustomComparison>]
type ExpressionType =
    private | ExpressionType of int32

    member private i.AsInt =
        let (ExpressionType code) = i
        code

    override i.Equals(other) =
        match other with
        | :? ExpressionType as other -> i.AsInt = other.AsInt
        | _ -> false
    
    override i.GetHashCode() = i.AsInt.GetHashCode()

    interface IEquatable<ExpressionType> with
        member i.Equals(other) = i.AsInt = other.AsInt
    interface IComparable<ExpressionType> with
        member i.CompareTo(other: ExpressionType) = i.AsInt.CompareTo(other.AsInt)
    interface IComparable with
        member i.CompareTo(other: obj) =
            match other with
            | :? ExpressionType as other -> i.AsInt.CompareTo(other.AsInt)
            | _ -> invalidArg "other" "can only compare to other ExpressionType"

let Integer: ExpressionType = ExpressionType(0)

let private sectionCount = 32 / 2
let private sectionMaxValue = 2s
let private sectionSize = 2s
let private maxSectionOffset = 30s
let private codeList = 1
let private codeFunc = 2
let private codeNone = 0
let private firstSection =
    let section = BitVector32.CreateSection(sectionMaxValue)
    assert(section.Offset = 0s)
    section
let private nextSection section = BitVector32.CreateSection(sectionMaxValue, section)
let private nextSectionO (section: BitVector32.Section) = if section.Offset < maxSectionOffset then Some(nextSection section) else None
let private sections =
    Array.unfold
        (Option.map (fun s -> (s, nextSectionO s)))
        (Some firstSection)

let rec private firstNonZeroPairRec (bits: BitVector32) (section: BitVector32.Section) =
    if bits.[section] <> 0 then Some(section)
    elif section.Offset = maxSectionOffset then None
    else firstNonZeroPairRec bits (BitVector32.CreateSection(sectionMaxValue, section))
let private firstNonZero (bits: BitVector32) = sections |> Array.tryFindBack (fun section -> bits.[section] <> 0)
let private firstNonZeroOrDefault bits = defaultArg (firstNonZero bits) firstSection

let (|ExpressionType|) exprType =
    let (ExpressionType code) = exprType
    BitVector32(code)

let TryList itemType =
    let mutable (ExpressionType code) = itemType
    match firstNonZero code with
    | None ->
        code.[firstSection] <- codeList
        Some(ExpressionType(code.Data))
    | Some(section) when section.Offset < maxSectionOffset ->
        code.[nextSection section] <- codeList
        Some(ExpressionType(code.Data))
    | Some _ -> None

let ListType itemType =
    match TryList itemType with
    | Some(list) -> list
    | None -> raise <| System.NotSupportedException("type nesting is too deep")

let TryFunction(argType,resType) =
    let mutable (ExpressionType arg, ExpressionType res) = (argType, resType)
    let (argSection, resSection) = Tuple.map firstNonZeroOrDefault (arg, res)
    let partSize = sectionSize + max argSection.Offset resSection.Offset
    let tagOffset = partSize * 2s
    if tagOffset > maxSectionOffset then
        None
    else
    let mutable func = arg
    for i = int partSize/int sectionSize to int partSize/int sectionSize * 2 - 1 do
        func.[sections.[i]] <- res.[sections.[i - int partSize/int sectionSize]]
    func.[sections.[int tagOffset / int sectionSize]] <- codeFunc
    Some(ExpressionType func.Data)

let Function(argType,resType) =
    match TryFunction(argType, resType) with
    | Some(func) -> func
    | None -> raise <| System.NotSupportedException("type nesting is too deep")

let (|Integer|List|Function|) exprType =
    let mutable (ExpressionType code) = exprType
    match firstNonZero code with
    | Some(section) when code.[section] = codeList ->
        code.[section] <- codeNone
        List(ExpressionType code.Data)
    | Some(section) when code.[section] = codeFunc ->
        let mutable arg = code
        let partSize = section.Offset / 2s
        arg.[section] <- codeNone
        for i = int partSize/int sectionSize to int partSize/int sectionSize * 2 - 1 do
            arg.[sections.[int i]] <- codeNone
        
        let mutable res = BitVector32()
        for i = 0 to int partSize / int sectionSize - 1 do
            res.[sections.[i]] <- code.[sections.[i + int partSize / int sectionSize]]
        Function(ExpressionType arg.Data, ExpressionType res.Data)
    | _ -> Integer

let rec toClrType = function
    | Integer -> typeof<int>
    | List elemType ->
        let elemType = toClrType elemType
        typedefof<_ list>.MakeGenericType(elemType)
    | Function(argType,resultType) ->
        let argType = toClrType argType
        let resultType = toClrType resultType
        typedefof<_ -> _>.MakeGenericType(argType, resultType)

type ExpressionType with
    member this.ToClrType() = toClrType this

    override this.ToString() =
        match this with
        | Integer -> "int"
        | List(subtype) -> sprintf "list<%O>" subtype
        | Function(Integer as arg, result) | Function((List _) as arg, result) ->
            sprintf "%O -> %O" arg result
        | Function(arg, result) -> sprintf "(%O) -> %O" arg result

    static member CurriedFunction(arguments, resultType) =
        match arguments with
        | arg :: [] -> Function(arg, resultType)
        | arg :: restArgs ->
            Function(arg, ExpressionType.CurriedFunction(restArgs, resultType))
        | _ -> invalidArg "arguments" "Argument list must not be empty"

let rec curriedFunction argTypes resultType =
    match argTypes with
    | [] -> resultType
    | argType :: restArgs ->
        Function(argType, curriedFunction restArgs resultType)

let matchNEmptyHandler elemType resultType =
    curriedFunction [elemType; ListType elemType] resultType

let matchType elemType resultType =
    curriedFunction [
        ListType elemType;
        resultType;
        matchNEmptyHandler elemType resultType
    ] resultType

let isSane() =
    let intFunc = Function(ListType(Integer), Integer)
    match intFunc with Function(_, _) ->()
    let intList = ListType(Integer)
    match intList with List(_) -> ()
    let someType = Function(ListType(Integer), Function(Integer, Integer))
    let (Function (List(a), Function(b, c))) = someType
    assert(a = Integer)
    assert(b = Integer)
    assert(c = Integer)

    let (Function (a, r)) = someType
    assert(a = ListType(Integer))
    assert(r = Function(Integer, Integer))