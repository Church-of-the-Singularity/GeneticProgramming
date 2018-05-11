module ShortPointers

type IPointer<'p> =
    interface
    end

[<Struct>]
type IntPtr<'P> internal(ptr: 'P) =
    member this.Address = ptr

    interface IPointer<'P>

type ITypedPointer<'P, 'T when 'T: struct> =
    inherit IPointer<'P>

[<Struct>]
type TypedPointer<'P, 'T when 'T: struct> internal(ptr: IntPtr<'P>) =
    new (ptr: 'P) = TypedPointer<'P, 'T>(IntPtr(ptr))

    member this.Address = ptr.Address

    interface ITypedPointer<'P, 'T>

type PhantomData<'T> = Phantom of unit

let phantom<'T> : PhantomData<'T> = Phantom()

type StructPool<'P, 'T when 'T: unmanaged> =
    internal {
        data: 'T[];
        allocated: System.Collections.BitArray;
        phantom: PhantomData<'P>;
    }

let inline makePool<'P, 'T when 'T: unmanaged> size =
    { data = Array.zeroCreate<'T> size;
      allocated = System.Collections.BitArray(size);
      phantom = phantom<'P> }

let rec inline private NewRec< ^P, 'T
        when ^P : (static member op_Explicit: int -> ^P)
         and 'T: struct
         and 'T: unmanaged> pool start (value: 'T) =
    int start |> ignore
    if start >= pool.data.Length then None
    elif pool.allocated.[start] then NewRec pool (start + 1) value
    else
        pool.allocated.[start] <- true
        pool.data.[start] <- value
        let ptr = (^P : (static member op_Explicit: int -> ^P) (start))
        Some(TypedPointer<'P, 'T>(ptr))

let inline New pool value = NewRec pool 0 value
let inline Release< ^P, 'T when 'T : struct and 'T : unmanaged 
                            and ^P: (static member op_Explicit: ^P -> int) > (pool: StructPool<'P,'T>) (pointer: TypedPointer<'P, 'T>) =
    if pool.allocated.[int pointer.Address] then
        pool.allocated.[int pointer.Address] <- false
    else
        invalidOp "double deallocation"
let inline Ref (pool: StructPool<'P,'T>) (pointer: TypedPointer<'P, 'T>) = &pool.data.[int pointer.Address]