module ShortPointers

type IPointer<'P> =
    abstract member Address: 'P

type ITypedPointer<'P, 'T when 'T: struct> =
    inherit IPointer<'P>

[<Struct>]
type TypedPointer<'P, 'T when 'T: struct>(ptr: 'P) =
    member this.Address = ptr

    interface ITypedPointer<'P, 'T> with
        member this.Address = this.Address

type PhantomData<'T> = Phantom of unit

let phantom<'T> : PhantomData<'T> = Phantom()

type StructPool<'P, 'T when 'T: struct> =
    { data: 'T[];
      allocated: System.Collections.BitArray;
      phantom: PhantomData<'P>; }

let inline makePool<'P, 'T when 'T: struct> size =
    { data = Array.zeroCreate<'T> size;
      allocated = System.Collections.BitArray(size);
      phantom = phantom<'P> }

let inline New< ^P, 'T
            when ^P : (static member op_Explicit: int -> ^P)
             and ^P : equality // WTF?
             and 'T: struct > (pool: StructPool< ^P, 'T >) (value: 'T) =
    let mutable result = None
    for start = 0 to pool.allocated.Length - 1 do
        if result <> None then ()
        elif not pool.allocated.[start]
        then
            pool.allocated.[start] <- true
            pool.data.[start] <- value
            let ptr = (^P : (static member op_Explicit: int -> ^P) (start))
            result <- Some(TypedPointer< ^P, 'T >(ptr))
    result

let inline Release< ^P, 'T when 'T : struct and 'T : unmanaged 
                            and ^P: (static member op_Explicit: ^P -> int) > (pool: StructPool<'P,'T>) (pointer: TypedPointer<'P, 'T>) =
    if pool.allocated.[int pointer.Address] then
        pool.allocated.[int pointer.Address] <- false
    else
        invalidOp "double deallocation"
let inline Ref (pool: StructPool<'P,'T>) (pointer: TypedPointer<'P, 'T>) = &pool.data.[int pointer.Address]

[<Struct>]
type BytePtr =
    | BytePtr of byte
    static member op_Explicit (value: int) = BytePtr(byte value)
    static member op_Explicit (value: BytePtr) =
        let (BytePtr value) = value
        int value

let poolTest() =
    let pool = makePool<BytePtr, int> 10
    let wordPool = makePool<uint16, int> 10
    let allocated = Array.map (New pool) [|0..10|]
    assert(Array.last allocated = None)
    let (Some first) = Array.head allocated
    for i = 0 to 4 do
        let (Some ptr) = allocated.[i * 2]
        let ref1 = Ref pool ptr
        let ref2 = Ref pool ptr
        assert(ref1 = i * 2)
        ref1 <- i * 3
        assert(ref1 = ref2)
        Release pool ptr
