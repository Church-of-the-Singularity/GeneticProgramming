namespace GeneticProgramming

type IDataModel =
    abstract Convert: value:obj -> obj
    abstract ConvertBack: value:obj * targetType:System.Type -> obj

type FSharpDataModel private () =
    static let instance = FSharpDataModel()

    interface IDataModel with
        member this.Convert o = id o
        member this.ConvertBack(o, _) = id o

    static member Instance = instance