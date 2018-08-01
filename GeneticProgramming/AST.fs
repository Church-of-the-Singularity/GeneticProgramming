[<AutoOpen>]
module GeneticProgramming.AST.AST

open System.Diagnostics
open System.IO

open Lost.Pointers
open Lost.Pointers.Pools

open GeneticProgramming.Types
open Operators

[<Struct>]
type BinOpType =
    | Sum | Diff | Mul
    /// fun x y -> x < y
    | Less

    member this.Char =
        match this with
        | Sum -> '+'
        | Diff -> '-'
        | Mul -> '*'
        | Less -> '<'

[<Struct>]
type TriOpType =
    | Div | Mod

    member this.Char =
        match this with
        | Div -> '/'
        | Mod -> '%'

//[<Struct>]
//[<CLIMutable>] 
type TermInfo =
    {   Term: byte
        Type: ExpressionType    }
    override this.ToString() = sprintf "v%d: %O" this.Term this.Type

type TermAllocator() =
    let mutable current = 0uy

    member i.MakeVar(varType) =
        let result = { Term = current; Type = varType }
        current <- Checked.(+) current 1uy
        result

type ERef<'P> = TypedPointer<'P, Expression<'P>>
and [<Struct>] Expression<'P> =
    /// 0
    | Zero
    /// 1
    | One
    /// Random integer value between 0 and (Expression - 1)
    | Rand of rand:ERef<'P>

    | BinOp of binOp:BinOpType * left:ERef<'P> * right:ERef<'P>

    /// IF right = 0 THEN fallback ELSE left OP right @@>
    | TriOp of triOp:TriOpType * tleft:ERef<'P> * tright:ERef<'P> * fallback:ERef<'P>

    /// if cond then onTrue else onFalse
    | Cond of cond:ERef<'P> * onTrue:ERef<'P> * onFalse:ERef<'P>

    /// (=) 0
    | IsZero of isZero:ERef<'P>

    | EmptyList of listType:ExpressionType

    | Cons of head:ERef<'P> * tail:ERef<'P>
    | Match of switch:MatchExpression<'P>
    | Length of len:ERef<'P>

    | Term of term:TermInfo
    | Let of binding:LetExpression<'P>
    | Lambda of arg:TermInfo * body:ERef<'P>
    | Apply of func:ERef<'P> * applyTo:ERef<'P>

    member this.Subexpressions =
      match this with
      | Zero | One -> []
      | Rand limit -> [limit]
      | BinOp(_, a, b) -> [a; b]
      | TriOp(_, a, b, c) -> [a; b; c]
      | IsZero value -> [value]
      | EmptyList _ -> []
      | Cons(h, t) -> [h; t]
      | Cond(cond, onTrue, onFalse) -> [cond; onTrue; onFalse]

      | Match{    List = list
                  EmptyCase = empty
                  HeadTail = nempty } ->
          [ list; empty; nempty ]

      | Let{  Value = value
              Expression = expr } ->
          [ value; expr ]

      | Apply(func, value) -> [ func; value ]
      | Lambda(_, expr) -> [ expr ]
      | Length list -> [ list ]
      | Term _ -> []

    interface IManaged<'P, Expression<'P>> with
      member this.GetReferences() =
        notImplemented()
        seq []


and MatchExpression<'P> = {
        /// Input list
        List:       ERef<'P>;
        /// Expression, computed if list is empty
        EmptyCase:  ERef<'P>;
        /// Function, that is of form 'a -> 'a list -> 'b.
        /// It will accept head and tail, and return result value
        HeadTail:   ERef<'P>;
    }

and LetExpression<'P> = {
        /// Bound value
        Value:  ERef<'P>;
        /// Expression to be computed with bound term
        Expression: ERef<'P>;
        Recursive:  bool;
        /// Bound term
        Term: byte;
    }

let subexpressions pool (eref: ERef<'P>) =
  let expr = Ref pool eref
  expr.Subexpressions

let rec private getUsedVariablesImpl pool addTo eref =
  let expr = Ref pool eref
  match expr with
  | Term var -> var :: addTo
  | _ ->
    subexpressions pool eref
    |> List.fold (getUsedVariablesImpl pool) addTo

let getUsedVariables pool = getUsedVariablesImpl pool []

let rec private NodeCount(pool: #IPool<'P, Expression<'P>>)
                         (expr: ERef<'P>)  acc : int =
  let nodeCount = NodeCount pool
  let expr = Ref pool expr
  match expr with
  | Zero | One | Term _ | EmptyList _ -> acc + 1
  | Rand a | IsZero a | Length a | Lambda (_, a) ->
    nodeCount a (acc + 1)
  | BinOp(_, a, b) | Cons (a, b) 
  | Let { Value = a
          Expression = b }
  | Apply (a, b) ->
    acc + 1
    |> nodeCount a
    |> nodeCount b
  | TriOp (_, a,b,c) | Cond(a,b,c)
  | Match {   List = a
              EmptyCase = b
              HeadTail = c } ->
    acc + 1
    |> nodeCount a
    |> nodeCount b
    |> nodeCount c

let nodeCount pool expr = NodeCount pool expr 0

let rec computeType pool eref =
  let expr = Ref pool eref
  let computeType = computeType pool
  match expr with
  | Zero | One | Rand _
  | BinOp _ | TriOp _
  | IsZero _ -> Integer

  | EmptyList(elemType) -> ListType(elemType)

  | Cons(head, tail) ->
      let headType = computeType head
      #if DEBUG
      assert(ListType(headType) = computeType tail)
      #endif
      ListType(headType)
  | Cond(cond,onTrue,onFalse) ->
      let trueType = computeType onTrue
      #if DEBUG
      assert(computeType cond = Integer)
      let falseType = computeType onFalse
      assert(falseType = trueType)
      #endif
      trueType
  | Match{ List = list
           EmptyCase = empty
           HeadTail = headTail } ->
      let emptyType = computeType empty
      #if DEBUG
      let (List itemType) = computeType list
      let (Function (handlerItemType, (Function (handlerListType, handlerResult)))) = computeType headTail
      Debug.Assert((itemType = handlerItemType), "handler item type mismatch")
      Debug.Assert((handlerListType = ListType(itemType)), "handler list type mismatch")
      Debug.Assert((handlerResult = emptyType), "handler non-empty does not match empty")
      #endif
      emptyType

  | Length _ -> Integer

  | Term{ Type = t } -> t
  | Let{ Expression = expr } -> computeType expr
  | Lambda({ Type = varType }, result) -> Function(varType, computeType result)
  | Apply(func, arg) ->
      match computeType func with
      | Function(argType, targetType) ->
          #if DEBUG
          assert(argType = computeType arg)
          #endif
          targetType
      | _ -> failwith "bad function type"

let checkType pool eref =
  computeType pool eref |> ignore
  eref

let rec getFreeVariables pool eref =
  let expr = Ref pool eref
  let getFreeVariables = getFreeVariables pool
  match expr with
  | Zero | One -> Map.empty

  | Rand limit -> getFreeVariables limit

  | BinOp (_, a, b) ->
      getFreeVariables a
      |> Map.merge (getFreeVariables b)

  | TriOp (_, a, b, c)
  | Cond (a, b, c) ->
      getFreeVariables a
      |> Map.merge (getFreeVariables b)
      |> Map.merge (getFreeVariables c)

  | IsZero limit -> getFreeVariables limit

  | EmptyList _ -> Map.empty

  | Cons(head, tail) ->
      getFreeVariables head
      |> Map.merge (getFreeVariables tail)

  | Match{    List = list
              EmptyCase = empty
              HeadTail = nempty } ->
      getFreeVariables list
      |> Map.merge (getFreeVariables empty)
      |> Map.merge (getFreeVariables nempty)

  | Length list -> getFreeVariables list

  | Term{ Term = var; Type = t} -> Map.add var t Map.empty

  | Let{  Recursive = false
          Term = var
          Value = value
          Expression = expr } ->
      let exprFree =
          getFreeVariables expr
          |> Map.remove var
      getFreeVariables value
      |> Map.merge exprFree

  | Let{  Recursive = true
          Term = var
          Value = value
          Expression = expr } ->
      getFreeVariables value
      |> Map.merge (getFreeVariables expr)
      |> Map.remove var

  | Lambda({ Term = var }, result) ->
      getFreeVariables result
      |> Map.remove var

  | Apply(func, arg) ->
      getFreeVariables func
      |> Map.merge (getFreeVariables arg)

let rec prettyPrint pool eref (target: TextWriter) indent =
  let makeIndent() =
      for i = 0 to indent-1 do
          target.Write '\t'
  makeIndent()
  let next = indent + 1
  let subprint e = prettyPrint pool e target next
  let expr = Ref pool eref
  match expr with
  | Zero -> target.WriteLine '0'
  | One -> target.WriteLine '1'
  | Rand e ->
      target.WriteLine "RND"
      subprint e
  | BinOp(op, left, right) ->
      target.WriteLine op.Char
      subprint left
      subprint right
  | TriOp(op, left, right, fallback) ->
      target.WriteLine op.Char
      subprint left
      subprint right
      subprint fallback
  | Cond(cond, onTrue, onFalse) ->
      target.WriteLine "COND?"
      subprint cond
      subprint onTrue
      subprint onFalse
  | IsZero e ->
      target.WriteLine "NOT"
      subprint e
  | EmptyList _ -> target.WriteLine "[]"
  | Cons(head, tail) ->
      target.WriteLine "CONS"
      subprint head
      subprint tail

  | Match{    List = list
              EmptyCase = empty
              HeadTail = nempty } ->
      target.WriteLine "MATCH"
      subprint list
      makeIndent()
      target.WriteLine "WITH EMPTY:"
      subprint empty
      makeIndent()
      target.WriteLine "NEMPTY:"
      subprint nempty

  | Length list ->
      target.WriteLine "LEN"
      subprint list

  | Term t ->
      target.WriteLine t

  | Let{  Recursive = isRec
          Term = var
          Value = value
          Expression = expr } ->
      target.Write "LET "
      if isRec then target.Write "REC "
      target.WriteLine(sprintf "v%d =" var)
      subprint value
      makeIndent()
      target.WriteLine "IN"
      subprint expr

  | Lambda(var, result) ->
      target.Write var
      target.WriteLine " =>"
      subprint result

  | Apply(func, arg) ->
      target.WriteLine "APPLY"
      subprint func
      subprint arg

  ()

let Minus a b = BinOp(Diff, a, b)
let Plus a b = BinOp(Sum, a, b)
let Not a = IsZero(a)
let IsLess a b = BinOp(Less, a, b)

let termOfTuple (term, ``type``) = { Term = term; Type = ``type`` }
let makeTerm<'a> term ``type``: Expression<'a> = Term{ Term = term; Type = ``type`` }

let getSubexpressions (expr: Expression<_>) = expr.Subexpressions

let rec getRecSubexpressions pool expr =
    let children = getSubexpressions expr
    children @ List.collect (fun childRef ->
      let child = Ref pool childRef
      getRecSubexpressions pool child) children

let rec private (|MultiApplicationRev|_|) pool expression =
    match expression with
    | Apply(eref, value) ->
        let expr = Ref pool eref
        match expr with
        | MultiApplicationRev pool (func, args) ->
          Some(func, value :: args)
        | _ -> Some(expr, [value])
    | _ -> None

let (|MultiApplication|_|) pool expression =
    match expression with
    | MultiApplicationRev pool (func, args) -> Some(func, List.rev args)
    | _ -> None

let rec (|TermApplication|_| ) pool eref =
    let expression = Ref pool eref
    match expression with
    | Term(term) -> Some(term, [])
    | Apply(eref, value) ->
        match eref with
        | TermApplication pool (term, args) -> Some(term, value :: args)
        | _ -> None
    | _ -> None

let rec (|MultiArityLambda|_|) pool eref =
    let expression = Ref pool eref
    match expression with
    | Lambda(term, body) ->
        match body with
        | MultiArityLambda pool (vars, body) -> Some(term :: vars, body)
        | _ -> Some([term], body)
    | _ -> None
