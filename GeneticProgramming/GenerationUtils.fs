module GeneticProgramming.GenerationUtils

let makeWeightenedGenerator options =
    let totalWeight = options |> List.sumBy fst
    let sorted =
        options
        |> List.sortBy(fun (weight, _) -> -weight)
        |> List.fold (fun (sum, l) (weight, v) ->
            let newSum = sum + weight
            newSum, (newSum, v) :: l) (0, [])
        |> snd
        |> List.rev
    fun (random: System.Random) ->
        let value = random.Next(totalWeight)
        sorted
        |> List.find(fun (sum, _) -> value < sum)
        |> snd

