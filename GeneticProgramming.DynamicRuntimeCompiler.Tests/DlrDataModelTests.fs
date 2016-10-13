namespace GeneticProgramming.Execution

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type DlrDataModelTests() =
    [<TestMethod>]
    member this.DlrType() =
        Assert.AreEqual(typeof<int>, DlrConverter<int>.DlrType)
        Assert.AreEqual(typeof<int FList>, DlrConverter<int list>.DlrType)
        Assert.AreEqual(typeof<int FList FList>, DlrConverter<int list list>.DlrType)

    [<TestMethod>]
    member this.ListListForwardConversionTest() =
        let original = [ [2;3]; [4;5;6]; [] ]
        let converted = DlrDataModel.Convert(original) :?> System.Collections.IStructuralEquatable
        let expected = original |> List.map FList.From |> FList.From
        Assert.IsTrue(converted.Equals(expected, System.Collections.StructuralComparisons.StructuralEqualityComparer))

    [<TestMethod>]
    member this.ListListBackwardConversionTest() =
        let expected = [ [2;3]; [4;5;6]; [] ]
        let converted = expected |> List.map FList.From |> FList.From
        let convertedBack = DlrConverter<int list list>.ConvertBack(converted) :?> System.Collections.IStructuralEquatable
        Assert.IsTrue(convertedBack.Equals(expected, System.Collections.StructuralComparisons.StructuralEqualityComparer))
