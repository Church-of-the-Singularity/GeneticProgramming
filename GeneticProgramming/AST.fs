module GeneticProgramming.AST

open System.Diagnostics

open GeneticProgramming.Types

open ShortPointers

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

    member this.NodeCount() = this.NodeCount(0)

    member private this.NodeCount(acc) =
        match this with
        | Zero | One | Term _ | EmptyList _ -> acc + 1
        | Rand a | IsZero a | Length a | Lambda (_, a) -> a.NodeCount(acc + 1)
        | BinOp(_, a, b) | Cons (a, b) 
        | Let { Value = a
                Expression = b }
        | Apply (a, b)  -> b.NodeCount(a.NodeCount(acc + 1))
        | TriOp (_, a,b,c) | Cond(a,b,c)
        | Match {   List = a
                    EmptyCase = b
                    HeadTail = c } -> c.NodeCount(b.NodeCount(a.NodeCount(acc + 1)))

and MatchExpression<'P> = {
        /// Input list
        List:       Expression<'P>;
        /// Expression, computed if list is empty
        EmptyCase:  Expression<'P>;
        /// Function, that is of form 'a -> 'a list -> 'b.
        /// It will accept head and tail, and return result value
        HeadTail:   Expression<'P>;
    }

and LetExpression<'P> = {
        /// Bound value
        Value:  Expression<'P>;
        /// Expression to be computed with bound term
        Expression: Expression<'P>;
        Recursive:  bool;
        /// Bound term
        Term: byte;
    }

type PooledExpression<'P> =
    { Expr: Expression<'P>;
      Pool: StructPool<'P, Expression<'P>>; }
    

    member this.Subexpressions =
        match this.Expr with
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
    
    member private this.GetUsedVariables(addTo) =
      match this with
      | Term var -> var :: addTo
      | _ ->
        this.Subexpressions |> List.fold (fun addTo expr -> expr.GetUsedVariables(addTo)) addTo

    member this.GetUsedVariables() = this.GetUsedVariables[]

    member this.ComputeType() =
        match this.Expr with
        | Zero | One | Rand _
        | BinOp _ | TriOp _
        | IsZero _ -> Integer

        | EmptyList(elemType) -> ListType(elemType)

        | Cons(head, tail) ->
            let headType = head.ComputeType()
            #if DEBUG
            assert(ListType(headType) = tail.ComputeType())
            #endif
            ListType(headType)
        | Cond(cond,onTrue,onFalse) ->
            let trueType = onTrue.ComputeType()
            #if DEBUG
            assert(cond.ComputeType() = Integer)
            let falseType = onFalse.ComputeType()
            assert(falseType = trueType)
            #endif
            trueType
        | Match{ List = list
                 EmptyCase = empty
                 HeadTail = headTail } ->
            let emptyType = empty.ComputeType()
            #if DEBUG
            let (List itemType) = list.ComputeType()
            let (Function (handlerItemType, (Function (handlerListType, handlerResult)))) = headTail.ComputeType()
            Debug.Assert((itemType = handlerItemType), "handler item type mismatch")
            Debug.Assert((handlerListType = ListType(itemType)), "handler list type mismatch")
            Debug.Assert((handlerResult = emptyType), "handler non-empty does not match empty")
            #endif
            emptyType

        | Length _ -> Integer

        | Term{ Type = t } -> t
        | Let{ Expression = expr } -> expr.ComputeType()
        | Lambda({ Type = varType }, result) -> Function(varType, result.ComputeType())
        | Apply(func, arg) ->
            match func.ComputeType() with
            | Function(argType, targetType) ->
                #if DEBUG
                assert(argType = arg.ComputeType())
                #endif
                targetType
            | _ -> failwith "bad function type"

    member this.GetFreeVariables() =
        match this.Expr with
        | Zero | One -> Map.empty

        | Rand limit -> limit.GetFreeVariables()

        | BinOp (_, a, b) ->
            a.GetFreeVariables()
            |> Map.merge (b.GetFreeVariables())

        | TriOp (_, a, b, c)
        | Cond (a, b, c) ->
            a.GetFreeVariables()
            |> Map.merge (b.GetFreeVariables())
            |> Map.merge (c.GetFreeVariables())

        | IsZero limit -> limit.GetFreeVariables()

        | EmptyList _ -> Map.empty

        | Cons(head, tail) ->
            head.GetFreeVariables()
            |> Map.merge (tail.GetFreeVariables())

        | Match{    List = list
                    EmptyCase = empty
                    HeadTail = nempty } ->
            list.GetFreeVariables()
            |> Map.merge (empty.GetFreeVariables())
            |> Map.merge (nempty.GetFreeVariables())

        | Length list -> list.GetFreeVariables()

        | Term{ Term = var; Type = t} -> Map.add var t Map.empty

        | Let{  Recursive = false
                Term = var
                Value = value
                Expression = expr } ->
            let exprFree =
                expr.GetFreeVariables()
                |> Map.remove var
            value.GetFreeVariables()
            |> Map.merge exprFree

        | Let{  Recursive = true
                Term = var
                Value = value
                Expression = expr } ->
            value.GetFreeVariables()
            |> Map.merge (expr.GetFreeVariables())
            |> Map.remove var

        | Lambda({ Term = var }, result) ->
            result.GetFreeVariables()
            |> Map.remove var

        | Apply(func, arg) ->
            func.GetFreeVariables()
            |> Map.merge (arg.GetFreeVariables())

    member this.PrettyPrint(target: System.IO.TextWriter, indent) =
        let makeIndent() =
            for i = 0 to indent-1 do
                target.Write '\t'
        makeIndent()
        let next = indent + 1
        let subprint (e: Expression) = e.PrettyPrint(target, next)
        match this with
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

    override this.ToString() =
      use writer = new System.IO.StringWriter()
      this.PrettyPrint(writer, 0)
      writer.ToString()


module IntegerConstants =
    let two = BinOp(Sum, One, One)
    let minusOne = BinOp(Diff, Zero, One)

    let rec make n =
        if n = 0 then Zero
        elif n = 1 then One
        elif n = -1 then BinOp(Diff, Zero, One)
        elif n % 2 = 0 then BinOp(Mul, two, make(n/2))
        elif n < 0 then BinOp(Diff, make(n + 1), One)
        else BinOp(Sum, One, make(n - 1))

let Minus a b = BinOp(Diff, a, b)
let Plus a b = BinOp(Sum, a, b)
let Not a = IsZero(a)
let IsLess a b = BinOp(Less, a, b)

let termOfTuple (term, ``type``) = { Term = term; Type = ``type`` }
let makeTerm term ``type`` = Term{ Term = term; Type = ``type`` }

let typeOf (expr: PooledExpression<_>) = expr.ComputeType()
let checkType expr =
    typeOf expr |> ignore
    expr

let getSubexpressions (expr: PooledExpression<_>) = expr.Subexpressions

let getRecSubexpressions expr =
    let children = getSubexpressions expr
    children @ List.collect getSubexpressions children

let getFreeVariables (expr: Expression) = expr.GetFreeVariables()

let computeType (expr: Expression) = expr.ComputeType()

/// TODO: Check, that this function is used correctly. It is NOT recursive!
let map func expr =
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

let rec curriedLambda args body =
    match args with
    | [] -> body
    | arg :: restArgs -> Lambda(arg, curriedLambda restArgs body)

let rec applyMultiple expr args =
    match args with
    | [] -> expr
    | arg :: restArgs -> applyMultiple (Apply(expr, arg)) restArgs

let rec private (|MultiApplicationRev|_|) expression =
    match expression with
    | Apply(expr, value) ->
        match expr with
        | MultiApplicationRev(func, args) -> Some(func, value :: args)
        | _ -> Some(expr, [value])
    | _ -> None

let (|MultiApplication|_|) expression =
    match expression with
    | MultiApplicationRev(func, args) -> Some(func, List.rev args)
    | _ -> None

let replace term replaceWith body =
    let rec replacer = function
        | Term(t) when t = term -> replaceWith
        | expr -> map replacer expr
    replacer body

let rec reduce expression =
    match expression with
    | MultiApplication(Lambda(term, body), arg :: restArgs) ->
        let newBody = replace term arg body
        reduce(applyMultiple newBody restArgs)
    | _ -> expression

let rec (|TermApplication|_| ) expression =
    match expression with
    | Term(term) -> Some(term, [])
    | Apply(expr, value) ->
        match expr with
        | TermApplication(term, args) -> Some(term, value :: args)
        | _ -> None
    | _ -> None

let rec (|MultiArityLambda|_|) expression =
    match expression with
    | Lambda(term, body) ->
        match body with
        | MultiArityLambda(vars, body) -> Some(term :: vars, body)
        | _ -> Some([term], body)
    | _ -> None
