namespace GeneticProgramming.Interpreter

open GeneticProgramming.AST
open GeneticProgramming.ShortDataModel
open GeneticProgramming.Execution
open Lost.Pointers
open Lost.Pointers.Pools
open Lost.Pointers.StructPool
open System.Collections.Generic

type olist = obj list
type DataType = int16

type private UntypedDataPtr = Ptr<DataType>
type private DataPtr = TypedPointer<UntypedDataPtr, DataType>
type private IDataPtr = ITypedPointer<UntypedDataPtr, DataType>

[<Struct>]
type internal Value =
  | Word of word:DataType
  | Lambda of func:(Value -> Value)

  static member Zero = Word(LanguagePrimitives.GenericZero)
  static member op_Implicit(value: DataType) = Word(value)
  static member op_Implicit(value: Value -> Value) = Lambda(value)

  member this.int =
    let (Word word) = this
    word
  member this.ptr: UntypedDataPtr = UntypedDataPtr.op_Explicit this.int
  member this.lambda =
    let (Lambda lambda) = this
    lambda

[<AutoOpen>]
module private Tools =
  let inline toDataType value = int16 value
  let word value = Word(value)
  let lambda value = Lambda(value)
  let toTypedPtr value = DataPtr(UntypedDataPtr.op_Explicit(value))

type private InterpreterState =
    {   Random: System.Random;
        Bindings: Value[];    }

    static member Create() =
        {   Random = System.Random();
            Bindings = Array.create 256 Value.Zero;   }

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
    let environmentsStack = Stack()
    let dataPool = StructPool.makePoolGeneric<DataType, DataType> 4096
    let state = InterpreterState.Create()
    do environmentsStack.Push state

    member private this.GetDataRoots() =
      environmentsStack
      |> Seq.collect (fun env -> env.Bindings)
      |> Seq.choose(function Lambda _ -> None
                           | Word word when word >= LanguagePrimitives.GenericZero && int word < dataPool.Size -> Some word
                           | _ -> None)
      |> Seq.collect(fun word -> if int word + 1 < dataPool.Size then [word; word + LanguagePrimitives.GenericOne] else [word])
      |> Seq.map (fun address -> upcast toTypedPtr address : IDataPtr)

    member internal this.DataPool = dataPool
    member internal this.DataGC = unmanagedGC (this.GetDataRoots())

    member private this.EvaluatePrepare() =
        state.Reset()
        dataPool.Reset()

    member internal this.Evaluate(expr: Expression<_>) =
        this.EvaluatePrepare()
        this.Evaluate(expr, state)

    member internal this.Evaluate(eref: ERef<'P>) =
        this.EvaluatePrepare()
        this.Evaluate(eref, state)

    member private this.Evaluate(eref: ERef<'P>, state: InterpreterState): Value =
        let expr = Ref pool eref
        this.Evaluate(expr, state)

    member private this.Evaluate(expr, state: InterpreterState): Value =
        let eval(eref: ERef<'P>) = this.Evaluate(eref, state)
        match expr with
        | Zero -> word(LanguagePrimitives.GenericZero<DataType>)
        | One -> word(LanguagePrimitives.GenericOne<DataType>)
        | Rand max ->
            let maxValue = eval max
            state.Random.Next(int maxValue.int)
            |> toDataType
            |> word
        
        | Term term ->
          state.Bindings.[int term.Term]

        | BinOp(op, a, b) ->
            let operator: DataType -> DataType -> DataType =
                match op with
                | Sum -> (+)
                | Diff -> (-)
                | Mul -> (*)
                | Less -> fun x y -> if x < y then LanguagePrimitives.GenericOne else LanguagePrimitives.GenericZero
            let aValue = eval a
            let bValue = eval b
            operator aValue.int bValue.int
            |> word

        | TriOp(op, a, b, onZero) ->
            let operator: DataType -> DataType -> DataType =
                match op with
                | Div -> (/)
                | Mod -> (%)
            if unbox b = 0 then
                eval onZero
            else
                let aValue = eval a
                let bValue = eval b
                operator aValue.int bValue.int
                |> word

        | Cond(cond, onTrue, onFalse) ->
            let condValue = eval cond
            if condValue.int <> LanguagePrimitives.GenericZero then
                eval onTrue
            else
                eval onFalse

        | IsZero(v) ->
            let value = eval v
            if value.int = LanguagePrimitives.GenericZero then
                LanguagePrimitives.GenericOne<DataType>
            else
                LanguagePrimitives.GenericZero<DataType>
            |> word

        | EmptyList exprType -> word(getAddress(getAddress(ShortList.empty dataPool this.DataGC: DataPtr)))

        | Apply(fn, v) ->
            Cancellation.callCheck()
            let func = eval fn
            let arg = eval v
            (unbox func) arg

        | Cons(head, tail) ->
            let headValue = eval head
            let tailValue = eval tail
            let list = ShortList.cons dataPool this.DataGC headValue.int tailValue.ptr
            list
            |> getAddress
            |> getAddress
            |> word

        | Expression.Lambda(term, body) ->
            fun o -> state.Bind(term.Term, o, fun () -> this.Evaluate(body, state))
            |> lambda

        | Length(listExpr) ->
            let listValue = unbox(eval listExpr)
            ShortList.length dataPool listValue
            |> toDataType
            |> word

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
            | Expression.Lambda(argTerm, body) ->
              let environment = state.Clone()
              environmentsStack.Push(environment)
              let rec func arg =
                environment.Bind(term, lambda func, fun () ->
                environment.Bind(argTerm.Term, arg, fun () ->
                  eval body))
              let result = state.Bind(term, lambda func, fun() -> eval exprExpr)
              environmentsStack.Pop() |> ignore
              result

            | _ -> raise(System.NotSupportedException())
        
        | Match {   List = list
                    EmptyCase = empty
                    HeadTail = nempty   } ->
            let listPtr = DataPtr((eval list).ptr)
            ShortList.switch dataPool listPtr
              (fun () -> eval empty)
              (fun head tail ->
                Cancellation.callCheck()
                let func = (eval nempty).lambda
                let headDone = func (word head)
                let tailPtr = getAddress(getAddress tail)
                headDone.lambda (word tailPtr))
