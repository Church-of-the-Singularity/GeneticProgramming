module GeneticProgramming.ShortDataModel

module ShortList =
  open Operators
  open Lost.Into
  open Lost.Pointers
  open Lost.Pointers.Pools
  open Lost.Pointers.StructPool

  let private oom() = raise(System.OutOfMemoryException())

  let inline empty<'T when 'T: struct and 'T:> System.IConvertible>
      (pool: IPool<Ptr<'T>, 'T>)
      gc = genericNull<Ptr<'T>, 'T>

  let cons pool gc head tail =
    match GcNewArray pool gc [| head; getAddress tail |] with
    | None -> oom()
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



