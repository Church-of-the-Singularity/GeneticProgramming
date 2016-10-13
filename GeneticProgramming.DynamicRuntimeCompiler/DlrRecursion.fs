namespace GeneticProgramming.DynamicRuntimeCompiler

open System.Linq.Expressions

type DelayedApplication<'i, 'o>(func, argument: 'i) =
    member this.Value: 'o = func argument

type Variable(expr: ParameterExpression, isRecursive: bool) =
    member this.Expression = expr
    member this.IsRecursive = isRecursive
//type DlrRecursion<'i,'o>(func: 'i -> 'o) =
    