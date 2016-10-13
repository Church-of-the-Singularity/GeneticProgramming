namespace GeneticProgramming.Interpreter

open GeneticProgramming.AST

type olist = obj list

type private InterpreterState =
    {   Random: System.Random;
        Bindings: Map<int, obj>;    }

    static member Create() =
        {   Random = System.Random();
            Bindings = Map.empty;   }

    member this.Bind(term, obj) =
        {   this with Bindings = Map.add term obj this.Bindings }
    

type Interpreter() =
    member private this.Evaluate(expr: Expression) =
        let state = InterpreterState.Create()
        this.Evaluate(expr, state)

    member private this.Evaluate(expr, state: InterpreterState): obj =
        let eval expression = this.Evaluate(expression, state)
        match expr with
        | Zero -> box 0
        | One -> box 1
        | Rand max ->
            let maxValue = eval max
            box <| state.Random.Next(unbox maxValue)

        | BinOp(op, a, b) ->
            let operator =
                match op with
                | Sum -> (+)
                | Diff -> (-)
                | Mul -> (*)
                | Less -> fun x y -> if x < y then 1 else 0
            let aValue = eval a
            let bValue = eval b
            operator (unbox a) (unbox b)
            |> box

        | TriOp(op, a, b, onZero) ->
            let operator: int -> int -> int =
                match op with
                | Div -> (/)
                | Mod -> (%)
            if unbox b = 0 then
                eval onZero
            else
                let aValue = eval a
                let bValue = eval b
                operator (unbox a) (unbox b)
                |> box

        | Cond(cond, onTrue, onFalse) ->
            let condValue = eval cond
            if unbox condValue <> 0 then
                eval onTrue
            else
                eval onFalse
            |> box

        | IsZero(v) ->
            let value = eval v
            if unbox value = 0 then
                box 1
            else
                box 0 |> box

        | EmptyList exprType -> upcast []

        | Apply(fn, v) ->
            let func = eval fn
            let arg = eval v
            (unbox func) arg

        | Cons(head, tail) ->
            let headValue = eval head
            let tailValue = eval tail
            headValue :: unbox tailValue
            |> box

        | Lambda(term, body) ->
            fun o ->
                let innerState = state.Bind(term.Term, o)
                this.Evaluate(body, innerState)
            |> box

        | Length(listExpr) ->
            let listValue = eval listExpr
            List.length (unbox listValue)
            |> box

        | Let{  Recursive = false;
                Term = term;
                Value = valueExpr;
                Expression = exprExpr;  } ->
            let value = eval valueExpr
            let innerState = state.Bind(term, value)
            this.Evaluate(exprExpr, innerState)

        | Let{  Recursive = true;
                Term = term;
                Value = valueExpr;
                Expression = exprExpr;  } ->
            notImplemented()