namespace GeneticProgramming

open Monads.Maybe

open GeneticProgramming.AST
open GeneticProgramming.GenerationUtils
open GeneticProgramming.ExpressionGenerationEnvironment
open GeneticProgramming.Types

[<System.Serializable>]
type PrimitiveMutator(random: System.Random) as this =
    inherit Randomized(random)

    static let probabilityPrecision = 1000000

//    static let genericMakeMatch =
//        typeof<PrimitiveStrategy>.GetMethod("MakeMatch",
//            BindingFlags.NonPublic ||| BindingFlags.Static)

    static let doubleDepth env = { env with Depth = env.Depth * 2 }
    static let incDepth env = { env with Depth = env.Depth + 1 }
    static let randomComplexDepth = incDepth
    static let complexDepth = incDepth

    let randomComplexValue =
        [   160, fun env vtype allowRec ->
                // variable
                let env = randomComplexDepth env
                maybe {
                    let! vars = Map.tryFind vtype env.VarTypes
                    let vars =  if allowRec then vars
                                else Set.filter (fun v -> not env.Vars.[v].Recursive) vars
                    if vars = Set.empty then return! None
                    else
                        let index = this.Random.Next(Set.count vars)
                        let varName = Seq.nth index vars
                        return makeTerm varName env.Vars.[varName].Type
                        //return Expr.Var(Var(varName, toClrType env.Vars.[varName].Type))
                }

            40, fun env vtype allowRec ->
                // let
                let env = randomComplexDepth env
                let varType = this.RandomType()
                let varValue = this.RandomValue(varType, env)
                let innerEnv, varName = declareRandom varType false env
                let innerValue = this.RandomValue(vtype, innerEnv)
                Let{    Recursive = false
                        Term = varName
                        Value = varValue
                        Expression = innerValue }
                |> Some

            10, fun env vtype allocRec ->
                // let rec
                let env = randomComplexDepth env
                let varType = this.RandomType()
                let varEnv, varName = declareRandom varType true env
                let varValue = this.RandomValue(varType, varEnv)
                let innerEnv = declare { Term = varName; Type = varType } false env
                let innerValue = this.RandomValue(vtype, innerEnv)
                Let{    Recursive = true
                        Term = varName
                        Value = varValue
                        Expression = innerValue }
                |> Some

            40, fun env vtype allowRec ->
                // function application
                let env = randomComplexDepth env
                let sourceType = this.RandomType()
                let sourceValue = this.RandomValue(sourceType, env)
                let funcType = Function(sourceType, vtype)
                let funcValue = this.RandomFunction env sourceType vtype true
                Some(Apply(funcValue, sourceValue))

            20, fun env vtype allowRec ->
                // if cond <> 0 then onTrue else onFalse
                let env = randomComplexDepth env
                let cond = this.RandomInteger(env)
                let onTrue = this.RandomValue(vtype, env)
                let onFalse = this.RandomValue(vtype, env)
                Some(Cond(cond, onTrue, onFalse))

            5, fun env vtype allowRec ->
                // match list with [] -> empty | head :: tail -> nempty head tail
                let env = randomComplexDepth env
                let listElemType = this.RandomType()
                let list = this.RandomList env listElemType
                let empty = this.RandomValue(vtype, env)
                let listType = List listElemType
                let nemptyTail = this.RandomFunction env (List listElemType) vtype true
                let nempty = this.RandomFunction env listElemType (Function(listType, vtype)) true
                Some(Match{ List=list; EmptyCase=empty; HeadTail=nempty })

            50, fun env vtype allowRec -> None   ]
        |> makeWeightenedGenerator

    let randomTypes =
        [   200,    fun () -> Integer;
            20,     fun () -> List(this.RandomType());
            10,     fun () -> Function(this.RandomType(), this.RandomType()) ]
        |> makeWeightenedGenerator

    let randomIntegers =
        [   40,    fun env -> Zero;
            20,     fun env -> One;

            4,     fun env ->
                let op1, op2 = this.RandomInteger(env), this.RandomInteger(env)
                BinOp(Sum, op1, op2);
            2,      fun env ->
                let op1, op2 = this.RandomInteger(env), this.RandomInteger(env)
                BinOp(Diff, op1, op2);
            2,      fun env ->
                let op1, op2 = this.RandomInteger(env), this.RandomInteger(env)
                BinOp(Mul, op1, op2);
            2,      fun env ->
                let op1, op2 = this.RandomInteger(env), this.RandomInteger(env)
                let zero = this.RandomInteger(env)
                TriOp(Div, op1, op2, zero);
            1,      fun env ->
                let op1, op2 = this.RandomInteger(env), this.RandomInteger(env)
                let zero = this.RandomInteger(env)
                TriOp(Mod, op1, op2, zero);

            2,      fun env ->
                let op1, op2 = this.RandomInteger(env), this.RandomInteger(env)
                BinOp(Less, op1, op2);
            
            10,      fun env ->
                let op1 = this.RandomInteger(env)
                IsZero(op1);
        
            1,      fun env ->
                let elemType = this.RandomType()
                this.RandomMatch env Integer elemType;
        
            2,      fun env ->
                let elemType = this.RandomType()
                let list = this.RandomList env elemType
                Length(list);
                ]

        |> makeWeightenedGenerator

    let randomLists =
        [   100,    fun env elemType -> EmptyList elemType;
            50,     fun env elemType ->
                let elem = this.RandomValue(elemType, env)
                let tail = this.RandomList env elemType
                Cons(elem, tail);   ]
        |> makeWeightenedGenerator

    member val MaxDepth = 16 with get,set
   
    member this.RandomGenerator = this.Random

    member this.RandomType() = randomTypes this.Random ()

    member this.RandomInteger(env: ExpressionGenerationEnvironment) =
        if env.Depth >= this.MaxDepth then
            randomIntegers this.Random { env with Depth = env.Depth + 1 }
        else
        let complexEnv = complexDepth env
        match randomComplexValue this.Random complexEnv Integer false with
        | None -> randomIntegers this.Random env
        | Some expr -> expr

    member this.RandomList env elemType =
        let listType = List elemType
        if env.Depth >= this.MaxDepth then
            randomLists this.Random { env with Depth = env.Depth + 1 } elemType
        else
        let value =
            let complexEnv = complexDepth env
            match randomComplexValue this.Random complexEnv listType false with
            | None -> randomLists this.Random env elemType
            | Some expr -> expr
        // assert(toExpressionType value.Type = Some(List elemType))
        value

    member this.RandomFunction env fromType toType allowRec =
        let funcType = Function(fromType, toType)
        let simple() =
            let varID = this.Random.Next()
            let innerEnv = declare { Term = varID; Type = fromType } false env
            let value = this.RandomValue(toType, innerEnv)
            Lambda({ Term = varID; Type = fromType}, value)
        if env.Depth >= this.MaxDepth then
            simple()
        else
        let complexEnv = complexDepth env
        match randomComplexValue this.Random complexEnv funcType allowRec with
        | None ->
            simple()
        | Some expr -> expr

    member this.RandomValue(valueType, ?env, ?allowRec) =
        let env =
            Option.defaultLazy
                (lazy ExpressionGenerationEnvironment.Create(this.Random))
                env
        let allowRec = defaultArg allowRec false
        match valueType with
        | Integer -> this.RandomInteger(env)
        | List(elemType) -> this.RandomList env elemType
        | Function(fromType, toType) -> this.RandomFunction env fromType toType allowRec
        | _ -> raise <| System.NotImplementedException()

    member private this.RandomMatch env resultType elemType =
        let list = this.RandomList env elemType
        let empty = this.RandomValue(resultType, env)
        let nemptyType = matchNEmptyHandler elemType resultType
        let nempty = this.RandomValue(nemptyType, env)
        Match{
            List = list;
            EmptyCase = empty;
            HeadTail = nempty;
        }

    member this.Mutate(mutationProbability, expr: Expression, ?genEnv) =
        System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

        let genEnv = defaultArg genEnv ExpressionGenerationEnvironment.Empty
        if this.Random.Next(probabilityPrecision) >= mutationProbability then
            // ExprTree.map (this.Mutate mutationProbability genEnv) expr
            let mutate env = fun expr -> this.Mutate(mutationProbability, expr, env)
            let mutateDefault = mutate { genEnv with Depth = genEnv.Depth + 1 }
            match expr with
            | Apply(f, v) ->
                Apply(mutateDefault f, mutateDefault v)

            | Let{  Recursive = false;
                    Term = var;
                    Value = value;
                    Expression = code   } ->
                let vtype = typeOf value
                let innerEnv = declare { Term = var; Type = vtype } false genEnv
                Let{
                    Recursive = false;
                    Term = var;
                    Value = mutateDefault value;
                    Expression = mutate innerEnv code}

            | Let{  Recursive = true;
                    Term = var;
                    Value = value;
                    Expression = code   } ->
                let vtype = typeOf value
                let varEnv = declare { Term = var; Type = vtype } true genEnv
                let innerEnv = declare { Term = var; Type = vtype } false genEnv
                Let{
                    Recursive = false;
                    Term = var;
                    Value = mutate varEnv value;
                    Expression = mutate innerEnv code}

            | Term(term) -> Term(term)

            | Lambda(term, expr) ->
                let innerEnv = declare term false genEnv
                Lambda(term, mutate innerEnv expr)

            | Cons(head, tail) -> Cons(mutateDefault head, mutateDefault tail)

            | BinOp(op, v1, v2) ->
                BinOp(op, mutateDefault v1, mutateDefault v2)

            | TriOp(op, v1, v2, v3) ->
                TriOp(op, mutateDefault v1, mutateDefault v2, mutateDefault v3)

            | EmptyList(t) -> EmptyList(t)

            | IsZero(v) -> IsZero(mutateDefault v)

            | Length(v) -> Length(mutateDefault v)

            | Cond(cond, onTrue, onFalse) -> Cond(mutateDefault cond, mutateDefault onTrue, mutateDefault onFalse)

            | Match{    List = list;
                        EmptyCase = empty;
                        HeadTail = headTail;    } ->
                Match{  List = mutateDefault list;
                        EmptyCase = mutateDefault empty;
                        HeadTail = mutateDefault headTail   }

            | One -> One
            | Zero -> Zero

            | Rand(v) -> Rand(mutateDefault v)
            
        else
            let valueType = typeOf expr
            let newValue = this.RandomValue(valueType, genEnv)
            newValue

    member this.Mutate(mutationProbability: float, expr, ?genEnv) =
        let genEnv = defaultArg genEnv ExpressionGenerationEnvironment.Empty
        this.Mutate(int <| (mutationProbability * float probabilityPrecision),
                     expr, genEnv)

    interface IMutator<Expression> with
        member this.Mutate expr = this.Mutate(probabilityPrecision / 10, expr)