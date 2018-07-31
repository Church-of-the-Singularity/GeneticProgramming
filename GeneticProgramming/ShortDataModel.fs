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
      (roots: unit -> TypedPointer<Ptr<'T>, 'T>[]) = genericNull<Ptr<'T>, 'T>

  let cons pool roots head tail =
    match GcNewArray pool roots [| head; getAddress tail |] with
    | None -> oom()
    | Some address -> address

  let private tail<'T when 'T: struct and 'T:> System.IConvertible and 'T :> IInto<int>> 
        (pool: StructPool<'T,'T>)
        (list: TypedPointer<'T,'T>) =
    let address = into<int>(getAddress list) + 1
    let untypedPtr = Ptr<'T>.op_Explicit address
    let typedPtr: TypedPointer<'T,'T> = Pointer.makeTyped<'T,'T> untypedPtr
    let tail: 'T = Ref pool typedPtr
    TypedPointer<'T,'T>(tail)

  let switch<'T when 'T: struct and 'T:> System.IConvertible and 'T :> IInto<int>> 
        (pool: StructPool<'T,'T>)
        (list: TypedPointer<'T,'T>) onEmpty onPopulated =
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



