module GeneticProgramming.ContextAwareEvolution.Contexts

open GeneticProgramming
open GeneticProgramming.AST

/// Holds context for genetic operations
type GeneticContext =
    {
        /// Current expression
        Expression: Expression;
        /// Parents for the current expression. In current-to-root order.
        Parents: Expression list;
    }

    /// Creates context from root expression
    static member FromRoot expression = { Expression = expression; Parents = [] }

/// Applies specified function to each subexpression of context's Expression, passing new context
let map mapper context =
    let newParents = context.Expression :: context.Parents
    let applyMapper subExpression = mapper { Expression = subExpression; Parents = newParents }
    AST.map applyMapper context.Expression

/// <summary>Mutates subtree at specified context using specified mutator and cut strategy.</summary>
/// <param name="cutStrategy">Determines if current context is suitable for mutation</param>
/// <param name="mutator">Used for mutating nodes</param>
/// <param name="context">Specifies the context to perform mutation in</param>
let rec mutate cutStrategy mutator context =
    if cutStrategy context then
        mutator context
    else
    map (mutate cutStrategy mutator) context