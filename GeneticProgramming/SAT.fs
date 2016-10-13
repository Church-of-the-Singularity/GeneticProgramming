module SAT

module Variable =
    let isTrue variables index =
        if index < 0 then not(Array.get variables -index)
        else Array.get variables index

    let next variables =
        let firstFalse = Array.tryFindIndex id variables
        match firstFalse with
        | None -> false
        | Some(index) ->
            for i in 1 .. index-1 do variables.[i] <- false
            variables.[index] <- true
            true

    let anySet maxIndex cond =
        let variables = Array.zeroCreate<bool> (maxIndex + 1)
        let rec anySet() =
            if cond variables then Some(variables)
            elif next variables then anySet()
            else None
        anySet()

module Disjunction =
    let isTrue variables = List.exists (Variable.isTrue variables)
    let getMaxVariableIndex = List.maxBy abs

module BooleanFormula =
    let isTrue variables = List.forall (Disjunction.isTrue variables)
    let getMaxVariableIndex = List.map Disjunction.getMaxVariableIndex >> List.max
    let isSatisfiable formula =
        let maxIndex = getMaxVariableIndex formula
        Variable.anySet maxIndex (fun vars -> isTrue vars formula)
