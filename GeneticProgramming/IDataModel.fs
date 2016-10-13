namespace GeneticProgramming

type IDataModel =
    abstract Convert: value:obj -> obj
    abstract ConvertBack: value:obj -> obj

type FSharpDataModel private () =
    static let instance = FSharpDataModel()

    interface IDataModel with
        member this.Convert o = id o
        member this.ConvertBack o = id o

    static member Instance = instance