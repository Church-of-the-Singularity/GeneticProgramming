namespace QGeneticProgramming

open System.IO

open GeneticProgramming

type SolverStateSerializer private() =
    static member Serialize(solver: ISolver, fileName) =
        SolverStateSerializer.Serialize(solver.CurrentGeneration, solver.GenerationCount, fileName)

    static member Serialize(individuals: AST.Expression list, generations: int64, fileName) =
        let formatter =
            System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use output = new FileStream(fileName, FileMode.Create, FileAccess.Write)
        formatter.Serialize(output, generations)
        formatter.Serialize(output, individuals)
        output.Close()

    static member Deserialize(solver: ISolver, fileName) =
        let individuals, generations = SolverStateSerializer.Deserialize(fileName)
        solver.LoadGeneration(individuals, generations)

    static member Deserialize(fileName) =
        let formatter =
            System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use input = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
        let generations: int64 = unbox(formatter.Deserialize input)
        let individuals: AST.Expression list = downcast(formatter.Deserialize input)
        input.Close()
        individuals, generations
