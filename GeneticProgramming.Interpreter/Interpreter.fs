namespace GeneticProgramming.Interpreter

open GeneticProgramming.AST
open GeneticProgramming.ShortDataModel
open GeneticProgramming.Execution
open Lost.Pointers
open Lost.Pointers.Pools

type olist = obj list

type private InterpreterState =
    {   Random: System.Random;
        Bindings: obj[];    }

    static member Create() =
        {   Random = System.Random();
            Bindings = Array.create 256 null;   }

    member this.Bind(term: byte, obj, func) =
      let original = this.Bindings.[int term]
      this.Bindings.[int term] <- obj
      let result = func()
      this.Bindings.[int term] <- original
      result

    member this.Reset() = this.Bindings.Initialize()

    member this.Clone() =
      { Random = this.Random;
        Bindings = downcast this.Bindings.Clone(); }
    

type Interpreter<'P>(pool: IPool<'P, Expression<'P>>) =
    let dataPool = StructPool.makePoolGeneric<uint16, uint16> 4096
    
    let state = InterpreterState.Create()

    member private this.Evaluate(expr: ERef<'P>) =
        state.Reset()
        this.Evaluate(expr, state)

    member private this.Evaluate(eref, state: InterpreterState): obj =
        let eval eref = this.Evaluate(eref, state)
        let expr = Ref pool eref
        match expr with
        | Zero -> box 0
        | One -> box 1
        | Rand max ->
            let maxValue = eval max
            box <| state.Random.Next(unbox maxValue)
        
        | Term term ->
          state.Bindings.[int term.Term]

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
                operator (unbox aValue) (unbox bValue)
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

        | EmptyList exprType -> upcast ShortList.empty dataPool

        | Apply(fn, v) ->
            Cancellation.callCheck()
            let func = eval fn
            let arg = eval v
            (unbox func) arg

        | Cons(head, tail) ->
            let headValue = eval head
            let tailValue = eval tail
            ShortList.cons dataPool headValue (unbox tailValue)
            |> box

        | Lambda(term, body) ->
            fun o -> state.Bind(term.Term, o, fun () -> this.Evaluate(body, state))
            |> box

        | Length(listExpr) ->
            let listValue = eval listExpr
            ShortList.length dataPool (unbox listValue)
            |> box

        | Let{  Recursive = false;
                Term = term;
                Value = valueExpr;
                Expression = exprExpr;  } ->
            let value = eval valueExpr
            state.Bind(term, value, fun() -> this.Evaluate(exprExpr, state))

        | Let{  Recursive = true;
                Term = term;
                Value = valueRef;
                Expression = exprExpr;  } ->
            let valueExpr = Ref pool valueRef
            match valueExpr with
            | Lambda(argTerm, body) ->
              let environment = state.Clone()
              let rec func arg =
                environment.Bind(term, func, fun () ->
                environment.Bind(argTerm.Term, arg, fun () ->
                  eval body))
              state.Bind(term, func, fun() -> eval exprExpr)
            | _ -> raise(System.NotSupportedException())
        
        | Match {   List = list
                    EmptyCase = empty
                    HeadTail = nempty   } ->
            ShortList.switch dataPool list
              (fun () -> eval empty)
              (fun head tail ->
                Cancellation.callCheck()
                let func = eval nempty
                (unbox func) (eval head) (eval tail))
