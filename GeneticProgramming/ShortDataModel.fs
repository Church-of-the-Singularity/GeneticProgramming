module GeneticProgramming.ShortDataModel

module ShortList =
  open Operators
  open Lost.Into
  open Lost.Pointers
  open Lost.Pointers.Pools
  open Lost.Pointers.StructPool

  let private oom() = raise(System.OutOfMemoryException())

  let inline empty
      (pool: StructPool<_,_>)
      gc = genericNull<Ptr<'T>, 'T>

  let rec cons pool gc head tail =
    match GcNewArray pool gc [| head; getAddress tail |] with
    | None -> oom()
    | Some address when Pointer.isNull address -> cons pool gc head tail
    | Some address -> address

  let private tail<'T when 'T: struct and 'T:> System.IConvertible> 
        (pool: StructPool<Ptr<'T>,'T>)
        (list: TypedPointer<Ptr<'T>,'T>) =
    let address = into<int>(getAddress list) + 1
    let untypedPtr = Ptr<'T>.op_Explicit address
    let typedPtr = Pointer.makeTyped<Ptr<'T>,'T> untypedPtr
    let tail: 'T = Ref pool typedPtr
    TypedPointer<Ptr<'T>,'T>(Ptr<'T>.op_Explicit tail)

  let switch<'T, 'R when 'T: struct and 'T:> System.IConvertible> 
        (pool: StructPool<Ptr<'T>,'T>)
        (list: TypedPointer<Ptr<'T>,'T>) (onEmpty: unit -> 'R) onPopulated =
    if Pointer.isNull list then
      onEmpty()
    else
    let head: 'T = Ref pool list
    let tail = tail pool list
    onPopulated head tail

  let length pool list =
    let current = ref list
    let mutable result = 0
    while not(isNull current) do
      result <- result + 1
      current := tail pool !current
    result

open System.Reflection
open Lost.Pointers

type private GenericListHelper<'T> private () =
  static member Empty = List.empty<'T>
  static member Cons(head:'T, tail) = List.Cons(head, tail)
  static member Rev(list: list<'T>) = List.rev list

type ShortDataModel<'W when 'W: struct and 'W :> System.IConvertible>(pool: StructPool.StructPool<Ptr<'W>, 'W>, gc) =
    let cons = ShortList.cons pool gc
    let empty: TypedPointer<Ptr<'W>, 'W> = ShortList.empty pool gc

    member this.Convert(value: int): 'W =
      let ptr = Ptr<'W>.op_Explicit value
      Pointers.getAddress ptr
    
    member this.Convert(value: int list): 'W =
      List.foldBack
        (fun item tail ->
          let convertedItem = this.Convert(item: int)
          cons convertedItem (getAddress tail))
        value
        empty
      |> getAddress |> getAddress

    member this.Convert(o: obj): 'W =
        match o with
        | :? int as i -> this.Convert i
        | :? list<int> as simpleList -> this.Convert simpleList
        | :? System.Collections.IEnumerable as nested ->
          Seq.foldBack
            (fun (item: obj) tail ->
                let convertedItem = this.Convert item
                ShortList.cons pool gc convertedItem (getAddress tail))
            (Seq.cast<obj> nested)
            (ShortList.empty pool gc)
          |> getAddress |> getAddress
        | _ -> invalidArg "o" "unsupported type"

    member this.ConvertBack(value: 'W, targetType): obj =
        if targetType = typeof<int> then
          box(value.ToInt32 null)
        elif targetType = typeof<list<int>> then
          let list = TypedPointer<Ptr<'W>, 'W>(Ptr<'W>.op_Explicit(value))
          let rec toReversed list =
            ShortList.switch pool list
              (fun () -> [])
              (fun head tail -> head.ToInt32(null) :: toReversed tail)
          let result: int list = toReversed list |> List.rev
          box result
        else
          let list = TypedPointer<Ptr<'W>, 'W>(Ptr<'W>.op_Explicit(value))
          let itemType = targetType.GetGenericArguments().[0]
          let helper = typedefof<GenericListHelper<int list>>.MakeGenericType(itemType)
          let empty = helper.GetProperty("Empty", BindingFlags.Static ||| BindingFlags.Public).GetValue(null)
          let cons = helper.GetMethod("Cons", BindingFlags.Static ||| BindingFlags.Public)
          let rec toReversed list =
            ShortList.switch pool list
              (fun () -> empty)
              (fun head tail ->
                cons.Invoke(null, [|
                  this.ConvertBack(head, itemType),
                  this.ConvertBack(getAddress(getAddress(tail)), targetType)|]))
          let reversed = toReversed list
          helper.GetMethod("Rev", BindingFlags.Static ||| BindingFlags.Public)
                .Invoke(null, [|reversed|])

    interface GeneticProgramming.IDataModel with
        member this.Convert o = box <| this.Convert o
        member this.ConvertBack(o, targetType) = this.ConvertBack(o :?> 'W, targetType)
    



