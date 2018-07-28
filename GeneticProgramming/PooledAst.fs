module GeneticProgramming.AST.Pooled

open Lost.Into
open Lost.Pointers
open Lost.Pointers.Pools
open Lost.Pointers.StructPool

type private ERef<'P> = TypedPointer<'P, Expression<'P>>
type private CacheEntry<'P> = Lazy<ERef<'P>>

type ExpressionPool<'P when 'P :> IInto<int>> =
  private {
    Pool: StructPool<'P, Expression<'P>>;
    mutable Roots: ERef<'P> list;
    Zero: CacheEntry<'P>;
    One: CacheEntry<'P>;
    Two: CacheEntry<'P>;
  }

  member this.Underlying = this.Pool

let private _new_ pool value =
  let roots = Seq.cast pool.Roots
  match GcNew pool.Pool roots value with
  | Some ptr -> ptr
  | _ -> System.OutOfMemoryException() |> raise

let GcNew = _new_
  

let makePoolAuto size =
  let pool = StructPool.makePoolGeneric<'P, Expression<Ptr<'P>>> size
  let rec result =
    let one = lazy(_new_ result One)
    { Pool = pool;
      Roots = [];
      Zero = lazy(_new_ result  Zero); 
      One = one;
      Two = lazy(_new_ result (BinOp(Sum, one.Value, one.Value)));}
  result

module IntegerConstants =
  let two pool = pool.Two.Value

  let make pool n =
      let rec make n =
        if n = 0 then pool.Zero.Value
        elif n = 1 then pool.One.Value
        else
          let expr: Expression<'P> =
            if n = -1 then
              BinOp(Diff, pool.Zero.Value, pool.One.Value)
            elif n % 2 = 0 then
              BinOp(Mul, pool.Two.Value, make(n/2))
            elif n < 0 then
              BinOp(Diff, make(n + 1), pool.One.Value)
            else
              BinOp(Sum, pool.One.Value, make(n - 1))
          _new_ pool expr
      make n

let rec curriedLambda pool args body =
    match args with
    | [] -> body
    | arg :: restArgs ->
      let curry = curriedLambda pool restArgs body
      Lambda(arg, _new_ pool curry)

let rec applyMultiple pool (eref: ERef<_>) args =
    let applyMultiple = applyMultiple pool
    match args with
    | [] -> eref
    | arg :: restArgs ->
    let firstApplication =
      Apply(eref,arg)
      |> _new_ pool
    applyMultiple firstApplication restArgs

/// TODO: Check, that this function is used correctly. It is NOT recursive!
let map pool func eref =
    let expr = Ref pool.Pool eref
    match expr with
    | Zero -> Zero
    | One -> One
    | Rand limit -> Rand(func limit)
    | BinOp(op, a, b) -> BinOp(op, func a, func b)
    | TriOp(op, a, b, c) -> TriOp(op, func a, func b, func c)
    | IsZero v -> IsZero(func v)
    | EmptyList t -> EmptyList t
    | Cons(h, t) -> Cons(func h, func t)
    | Cond(cond, onTrue, onFalse) -> Cond(func cond, func onTrue, func onFalse)
    | Match{    List = list
                EmptyCase = empty
                HeadTail = nempty } ->
        Match{  List = func list
                EmptyCase = func empty
                HeadTail = func nempty }

    | Let{  Recursive = isRec
            Term = var
            Value = value
            Expression = expr } ->
        Let{    Recursive = isRec
                Term = var
                Value = func value
                Expression = func expr  }

    | Apply(f, v) -> Apply(func f, func v)
    | Lambda(term, expr) -> Lambda(term, func expr)
    | Length list -> Length(func list)
    | Term(term) -> Term(term)
    // roots: (Seq.append roots (Seq.cast e.Subexpressions))
    |> fun e -> _new_ pool e

let replace pool term replaceWith bodyRef =
    let rec replacer pool eref =
        let expr = Ref pool.Pool eref
        match expr with
        | Term(t) when t = term -> replaceWith
        | _ -> map pool (replacer pool) eref
    replacer pool bodyRef

let rec reduce pool (eref: ERef< ^P>) =
    let expr = Ref pool.Pool eref
    match expr with
    | MultiApplication pool.Pool (Lambda(term, body), arg :: restArgs) ->
        let newBody = replace pool term arg body
        reduce pool (applyMultiple pool newBody restArgs)
    | _ -> eref