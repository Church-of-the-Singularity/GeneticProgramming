namespace GeneticProgramming.Execution

open System.Reflection

type private FListNode<'a>(value: 'a, next: FListNode<'a>) =
    member this.Value = value
    member this.Next = next

type FList<'a> private(head: FListNode<'a>, length) =
    static let empty = FList(Unchecked.defaultof<FListNode<_>>, 0)

    member private this.HeadNode = head
    member this.Tail =
        if length = 0 then
            System.InvalidOperationException() |> raise
        else
            FList(head.Next, length - 1)
    member this.Head = head.Value
    member this.Length = length
    member this.IsEmpty = length = 0

    static member Empty = empty
    static member Cons(head: 'a, tail: FList<_>) = FList(FListNode(head, tail.HeadNode), tail.Length + 1)

    static member From(list: 'a list) = empty |> List.foldBack (fun i curr -> FList.Cons(i, curr)) list

    static member internal From<'b>(list, converter: MethodInfo) =
        list |> List.map (fun (i: 'b) -> converter.Invoke(null, [| i |]) |> unbox) |> FList<'a>.From

    member internal this.ToList<'b>(elemConverter: MethodInfo) =
        this.AsFSharpList |> List.map (fun i -> elemConverter.Invoke(null, [| i |]) |> unbox<'b>)

    member this.AsSeq =
        seq {
            let head = ref head
            let length = ref length
            while !length > 0 do
                yield (!head).Value
                head := (!head).Next
                decr length
        }
    member this.AsFSharpList = this.AsSeq |> List.ofSeq

    interface System.Collections.IStructuralEquatable with
        member this.GetHashCode(comparer) =
            let hashes = this.AsSeq |> Seq.map comparer.GetHashCode
            let hash = hashes |> Seq.fold (fun a b -> a ^^^ b) 0
            hash

        member this.Equals(other, comparer: System.Collections.IEqualityComparer) =
            let other = other :?> FList<'a>
            if isNull other || this.Length <> other.Length then
                false
            else
                (this.AsSeq, other.AsSeq)
                ||> Seq.forall2(fun e1 e2 ->
                                comparer.Equals(e1, e2))

type internal DlrConverter<'defType> private () =
    static let dlrType =
        if typeof<'defType> = typeof<int> then
            typeof<int>
        else
            let elemType = typeof<'defType>.GetGenericArguments().[0]
            let dlrElementType = DlrDataModel.GetDlrType(elemType)
            typedefof<FList<_>>.MakeGenericType[| dlrElementType |]

    static let convert =
        if typeof<'defType> = typeof<int> then
            id
        else
            let elemType = typeof<'defType>.GetGenericArguments().[0]
            let elemDlrConverter = typedefof<DlrConverter<int>>.MakeGenericType(elemType)
            let elemConverter = elemDlrConverter.GetMethod("Convert", BindingFlags.Static ||| BindingFlags.NonPublic)
            let elemDlrType = DlrDataModel.GetDlrType(elemType)
            let listConverterGeneric = dlrType.GetMethod("From", BindingFlags.Static ||| BindingFlags.NonPublic)
            let listConverter = listConverterGeneric.MakeGenericMethod[| elemType |]
            
            fun o -> listConverter.Invoke(null, [| o; elemConverter |])

    static let convertBack =
        if typeof<'defType> = typeof<int> then
            id
        else
            let elemType = typeof<'defType>.GetGenericArguments().[0]
            let elemDlrConverter = typedefof<DlrConverter<int>>.MakeGenericType(elemType)
            let elemConverter = elemDlrConverter.GetMethod("ConvertBack", BindingFlags.Static ||| BindingFlags.NonPublic)
            let listConverterGeneric = dlrType.GetMethod("ToList", BindingFlags.Instance ||| BindingFlags.NonPublic)
            let listConverter = listConverterGeneric.MakeGenericMethod[| elemType |]

            fun o -> listConverter.Invoke(o, [| elemConverter |])

    static member DlrType = dlrType

    static member Convert(o: obj) = convert o
    static member ConvertBack(o: obj) = convertBack o

and DlrDataModel private() =
    static let instance = DlrDataModel()

    static member Instance = instance

    static member GetDlrType(defType: System.Type) =
        let converter = typedefof<DlrConverter<int>>.MakeGenericType(defType)
        let property = converter.GetProperty("DlrType", BindingFlags.Static ||| BindingFlags.NonPublic)
        property.GetValue(null, null) :?> System.Type

    static member GetDefaultType(dlrType: System.Type) =
        if dlrType = typeof<int> then dlrType
        elif dlrType.GetGenericTypeDefinition() = typedefof<FList<int>> then
            let dlrElemType = dlrType.GetGenericArguments().[0]
            let elemType = DlrDataModel.GetDefaultType dlrElemType
            typedefof<int list>.MakeGenericType(elemType)
        else
            System.ArgumentException() |> raise

    static member Convert(o: obj) =
        let converter = typedefof<DlrConverter<int>>.MakeGenericType(o.GetType())
        let convert = converter.GetMethod("Convert", BindingFlags.Static ||| BindingFlags.NonPublic)
        convert.Invoke(null, [| o |])

    static member ConvertBack(o: obj) =
        let converter = typedefof<DlrConverter<int>>.MakeGenericType(o.GetType())
        let convert = converter.GetMethod("ConvertBack", BindingFlags.Static ||| BindingFlags.NonPublic)
        convert.Invoke(null, [| o |])

    interface GeneticProgramming.IDataModel with
        member this.Convert o = DlrDataModel.Convert o
        member this.ConvertBack o = DlrDataModel.ConvertBack o
