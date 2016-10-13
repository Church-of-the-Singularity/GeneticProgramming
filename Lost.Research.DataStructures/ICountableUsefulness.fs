namespace Lost.Research.DataStructures

open System.Diagnostics.Contracts

/// Provides the ability to choose randomly one of all infinite integer set,
/// prortional to its usefulness (which is not necessary some real value).
/// Initial usefulness large for small integers, and small for large ones.
/// You can increase usefulness of any value by unspecified amount as many times as you want.
type ICountableUsefulness =
    /// Gets random integer. The probability to receive any particular integer depends on its usefulness
    [<Pure>]
    abstract GetRandom: unit -> bigint
    /// Increases usefulness of specified integer
    abstract Useful: bigint -> unit