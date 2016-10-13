module GeneticProgramming.AST

open System.Reflection

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

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

type TriOpType =
    | Div | Mod

    member this.Char =
        match this with
        | Div -> '/'
        | Mod -> '%'

type TermInfo =
    {   Term: int
        Type: ExpressionType    }
    override this.ToString() = sprintf "v%d: %O" this.Term this.Type

type Expression =
    /// 0
    | Zero
    /// 1
    | One
    /// Random integer value between 0 and (Expression - 1)
    | Rand of Expression

    | BinOp of BinOpType * Expression * Expression

    /// IF right = 0 THEN fallback ELSE left OP right @@>
    | TriOp of TriOpType * Expression * Expression * Expression

    /// if cond then onTrue else onFalse
    | Cond of Expression * Expression * Expression

    /// (=) 0
    | IsZero of Expression

    | EmptyList of ExpressionType

    | Cons of Expression * Expression
    | Match of MatchExpression
    | Length of Expression

    | Term of TermInfo
    | Let of LetExpression
    | Lambda of TermInfo * (*body*)Expression
    | Apply of Expression * Expression

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

    member this.ComputeType() =
        match this with
        | Zero | One | Rand _
        | BinOp _ | TriOp _
        | IsZero _ -> Integer

        | EmptyList(elemType) -> List(elemType)

        | Cons(head, _) -> List(head.ComputeType())
        | Cond(_,onTrue,_) -> onTrue.ComputeType()
        | Match{ EmptyCase = empty } -> empty.ComputeType()

        | Length _ -> Integer

        | Term{ Type = t } -> t
        | Let{ Expression = expr } -> expr.ComputeType()
        | Lambda({ Type = varType }, result) -> Function(varType, result.ComputeType())
        | Apply(func, _) ->
            match func.ComputeType() with
            Function(_, targetType) -> targetType

    member this.GetFreeVariables() =
        match this with
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
            target.WriteLine var
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
        

and MatchExpression = {
        /// Input list
        List:       Expression;
        /// Expression, computed if list is empty
        EmptyCase:  Expression;
        /// Function, that is of form 'a -> 'a list -> 'b.
        /// It will accept head and tail, and return result value
        HeadTail:   Expression;
    }

and LetExpression = {
        Recursive:  bool;
        /// Binded term
        Term: int;
        /// Binded value
        Value:  Expression;
        /// Expression to be computed with binded term
        Expression: Expression;
    }

module IntegerConstants =
    let two = BinOp(Sum, One, One)
    let minusOne = BinOp(Diff, Zero, One)

    let rec make n =
        if n < 0 then notImplemented()
        elif n = 0 then Zero
        elif n = 1 then One
        elif n % 2 = 0 then BinOp(Mul, two, make(n/2))
        else BinOp(Sum, One, make(n - 1))

let Minus a b = BinOp(Diff, a, b)
let Plus a b = BinOp(Sum, a, b)

let termOfTuple (term, ``type``) = { Term = term; Type = ``type`` }
let makeTerm term ``type`` = Term{ Term = term; Type = ``type`` }

let typeOf (expr: Expression) = expr.ComputeType()

let getSubexpressions (expr: Expression) = expr.Subexpressions

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
