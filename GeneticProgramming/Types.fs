module GeneticProgramming.Types

open GeneticProgramming.AST

let rec curriedFunction argTypes resultType =
    match argTypes with
    | [] -> resultType
    | argType :: restArgs ->
        Function(argType, curriedFunction restArgs resultType)

let matchNEmptyHandler elemType resultType =
    curriedFunction [elemType; List elemType] resultType

let matchType elemType resultType =
    curriedFunction [
        List elemType;
        resultType;
        matchNEmptyHandler elemType resultType
    ] resultType
                

//let rec inferType expression minVar environment =
//    let tvs = minVar + 1
//    let tv = minVar
//    match expression with
//    | Term(_) ->
//        match Map.tryFind expression environment with
//        | None -> Map.add expression (TyVar minVar) environment, tvs
//        | Some _ -> environment, minVar
//    | Lambda(n, assigned) ->
//        match Map.tryFind (Term n) environment with
//        | None ->
//            // inferring assinged type tp'
//            let env', tvs' = inferType assigned tvs environment
//            let tp' = (Map.tryFind assigned env').Value
//            // type<Term n> = type<assigned>
//            match Map.tryFind (Term n) env' with
//            | None ->
//                // Term n is not used in assigned itself (not recursive)
//                let env'' = Map.add (Term n) (TyVar minVar) env'
//                Map.add expression (Function((TyVar minVar), tp')) env'', tvs'
//            | Some(tp) ->
//                Map.add expression (Function(tp, tp')) env', tvs'
//        | Some _ ->
//            failwith "duplicate bound variable"
//    | Apply(func, value) ->
//        // value type
//        let env', tvs' = inferType value tvs environment
//        let tp' = (Map.tryFind value env').Value
//        match func with
//        | Term(_) ->
//            match Map.tryFind func env' with
//            | None ->
//                env'
//                |> Map.add func (Function(tp', TyVar minVar))
//                |> Map.add expression (TyVar minVar)
//                |> flip Tuple.make tvs'
//            | Some(tp) ->
//                let env'' = unifyTypes env' (Function(tp', TyVar minVar), tp)
//                match Map.tryFind func env'' with
//                Some(Function(tp'', tp''')) ->
//                    Map.add expression tp''' env'', tvs'
//        | Apply(_, _) ->
//            let env'', tvs'' = inferType func tvs' env'
//            let tp = (Map.tryFind func env'').Value
//            let env''' = unifyTypes env'' (tp, Function(tp', TyVar minVar))
//            match Map.tryFind func env''' with
//            Some(Function(tp'', tp''')) ->
//                Map.add expression tp''' env''', tvs''
//        | Lambda(_, _) ->
//            // func type argT -> valueT
//            let env'', tvs'' = inferType func tvs' env'
//            match Map.tryFind func env'' with
//            Some(Function(argT, valueT)) ->
//                if not(areTypesCompatible(argT, tp')) then failwith "can't apply"
//                let env''' = unifyTypes env'' (argT, tp')
//                match Map.tryFind func env''' with
//                Some(Function(funcT', tp'')) ->
//                    Map.add expression tp'' env''', tvs''
//    | One | Zero -> Map.add expression Integer environment, minVar
//    | Rand arg | IsZero arg ->
//        let env', tvs' = inferType arg tvs environment
//        let tp' = (Map.tryFind arg env').Value
//        if not(areTypesCompatible(tp', Integer)) then failwith "arg must be int"
//        let env'' = unifyTypes env' (tp', Integer)
//        Map.add expression Integer env'', tvs'
//    | Sum(a1, a2) | Diff(a1, a2) | Mul(a1, a2) // | Div(a1, a2) | Mod(a1, a2)
//    | Less(a1, a2) | Less(a1, a2) ->
//        let env', tvs' = inferType a1 tvs environment
//        let a1T = (Map.tryFind a1 env').Value
//        if not(areTypesCompatible(Integer, a1T)) then failwith "left arg must be int"
//        let unified1 = unifyTypes env' (Integer, a1T)
//        let env'', tvs'' = inferType a2 tvs' unified1
//        let a2T = (Map.tryFind a2 env'').Value
//        if not(areTypesCompatible(Integer, a2T)) then failwith "right arg must be int"
//        let unified2 = unifyTypes env'' (Integer, a2T)
//        Map.add expression Integer unified2, tvs''
//    | EmptyList -> Map.add expression (List(TyVar tv)) environment, tvs
//    | Cons(elem, list) ->
//        let env', tvs' = inferType list tvs environment
//        match (Map.tryFind list env').Value with
//        | List(inferredElemT) ->
//            let elemTMap, tvs'' = inferType elem tvs' env'
//            let elemT = (Map.tryFind elem elemTMap).Value
//            if not(areTypesCompatible(inferredElemT, elemT)) then failwith "incompatible element type"
//            let unified = unifyTypes env' (inferredElemT, elemT)
//            Map.add expression (List inferredElemT) unified, tvs''
//        | TyVar n ->
//            let elemTMap, tvs'' = inferType elem tvs' env'
//            let elemT = (Map.tryFind elem elemTMap).Value
//            let unified = unifyTypes elemTMap (TyVar n, List elemT)
//            Map.add expression (List elemT) unified, tvs''
//        | _ -> failwith "second argument of cons must be a list"
//
//    | Length(list) ->
//        let env, tvs = inferType list tvs environment
//        let listT = (Map.tryFind list env).Value
//        match listT with
//        | List(elemT) -> Map.add expression Integer env, tvs
//        | TyVar n ->
//            let elemT, tvs = TyVar tvs, tvs + 1
//            let unified = unifyTypes env (TyVar n, List elemT)
//            Map.add expression Integer env, tvs
//        | _ -> failwith "argument of length must be of type list<'a>"
//
//    | Match{    List = input;
//                EmptyCase = caseEmpty;
//                HeadTail = caseNEmpty;  } ->
//        let env, tvs = inferType caseEmpty tvs environment
//        let env, tvs = inferType input tvs env
//        let env, tvs = inferType caseNEmpty tvs env
//        let resultFromEmpty = (Map.tryFind caseEmpty env).Value
//        let nemptyFunc = (Map.tryFind caseNEmpty env).Value
//        match (Map.tryFind input env).Value with
//        | List(elemT) ->
//            let inferredLambda = Function(elemT, Function(List elemT, resultFromEmpty))
//            if not(areTypesCompatible(inferredLambda, nemptyFunc)) then
//                failwith "non-empty case lambda mistyped"
//            let env = unifyTypes env (inferredLambda, nemptyFunc)
//            let result = (Map.tryFind caseEmpty env).Value
//            Map.add expression result env, tvs
//        | TyVar n ->
//            let elemT, tvs = TyVar tvs, tvs + 1
//            let env = unifyTypes env (TyVar n, List elemT)
//            let inferredLambda = Function(elemT, Function(List elemT, resultFromEmpty))
//            if not(areTypesCompatible(inferredLambda, nemptyFunc)) then
//                failwith "non-empty case lambda mistyped"
//            let env = unifyTypes env (inferredLambda, nemptyFunc)
//            let result = (Map.tryFind caseEmpty env).Value
//            Map.add expression result env, tvs
//        | _ -> failwith "match input must be of list type"
//
//    | Let{  Recursive = true;
//            Term = n;
//            Value = value;
//            Expression = expr;  } ->
//        let termT, tvs = TyVar tv, tvs + 1
//        let env = Map.add (Term n) termT environment
//        let env, tvs = inferType value tvs env
//        let valueT = (Map.tryFind value env).Value
//        let termT = match Map.tryFind (Term n) env with None -> termT | Some(t) -> t
//        if not(areTypesCompatible(termT, valueT)) then failwith "invalid recursive definition"
//        let env = unifyTypes env (termT, valueT)
//        let termT = (Map.tryFind (Term n) env).Value
//
//        let env = Map.add (Term n) valueT environment
//        let env, tvs = inferType expr tvs env
//        let exprT = env.[expr]
//        Map.add expression exprT env, tvs
//
//    | _ -> failwith "not implemented"
//
//and substitutionCorrect n arg =
//    let rec substitutionCorrect arg =
//        match arg with
//        | TyVar n' when n' = n -> false
//        | List arg -> substitutionCorrect arg
//        | Function(arg, result) -> substitutionCorrect arg && substitutionCorrect result
//        | TyVar _ | List _ | Integer -> true
//
//    match arg with
//    | List arg -> substitutionCorrect arg
//    | Function(arg, result) -> substitutionCorrect arg && substitutionCorrect result
//    | _ -> true
//
//and unifyTypes env = function
//    | a, b when a = b -> env
//    | List a, List b -> unifyTypes env (a,b)
//    | TyVar n, t | t, TyVar n when substitutionCorrect n t ->
//        Map.map (fun _ -> substituteTyVar n t) env
//    | Function(arg, result), Function(arg', result') ->
//        let env' = unifyTypes env (arg, arg')
//        unifyTypes env' (result, result')
//    | List e1, List e2 -> unifyTypes env (e1, e2)
//    | what, with' -> failwith "not implemented"
//and substituteTyVar n t = function
//    | Integer -> Integer
//    | TyVar(n') when n = n' -> t
//    | TyVar(n') -> TyVar n'
//    | List(subtype) -> List(substituteTyVar n t subtype)
//    | Function(t1, t2) -> Function(substituteTyVar n t t1, substituteTyVar n t t2)
//and areTypesCompatible = function
//    | Integer, List _ | List _ , Integer | Integer, Function _ | Function _ , Integer
//    | List _, Function _ | Function _, List _ -> false
//    | Integer, _ -> true
//    | List _, _ -> true
//    | TyVar _, _ -> true
//    | Function(t1, t2), Function(t1', t2') ->
//        areTypesCompatible(t1, t1') && areTypesCompatible(t2, t2')
//    | _ -> false