namespace GeneticProgramming

open CommandLine

type CliOptions() =
    [<Option("compiler", Required = true)>]
    member val Compiler = "" with get, set

    [<Option("sources", Required = false)>]
    member val DumpSources = false with get, set

    [<Option('p', Required = false)>]
    member val WorkerCount = System.Environment.ProcessorCount with get, set

    [<Option('v', "verbose", Required = false)>]
    member val Verbose = false with get, set

    [<ParserState>]
    member val LastParserState = null:IParserState with get, set

    [<HelpOption>]
    member this.GetUsage() =
        @"QGeneticProgramming --compiler COMPILER [--sources] [-p WORKER_THREADS] [-v|--verbose]

  COMPILER: AST compiler/interpreter to use - DLR, Unquote or Mix
    DLR: translates to System.Linq.Expressions, then tries to compile them
    Unquote: translates to F# quotations, then uses Unquote to run them
    Mix: randomly picks each time between DLR and Unquote
  
  --sources: if set, generates F# source for the best specimen of the current generation
  -p: sets the number of worker threads/processes
  --verbose: prints more of internal kitchen

"

