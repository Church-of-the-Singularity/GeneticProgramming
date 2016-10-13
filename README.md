## Goal
GeneticProgramming is an F#-based implementation of 
[Genetic Programming](https://en.wikipedia.org/wiki/Genetic_programming).
It generates random programs in a simplified functional language,
and then crossbreeds and mutates them in an attempt to **produce a sorting algorithm**.

## Building
Just clone the solution and build it using Visual Studio or MSBuild.
Will probably work on Mono with little to no modification.

## Running
Main executable is QGeneticProgramming.exe

```
QGeneticProgramming --compiler COMPILER [--sources] [-p WORKER_THREADS] [-v|--verbose]

  COMPILER: AST compiler/interpreter to use - DLR, Unquote or Mix
    DLR: translates to System.Linq.Expressions, then tries to compile them
    Unquote: translates to F# quotations, then uses Unquote to run them
    Mix: randomly picks each time between DLR and Unquote
  
  --sources: if set, generates F# source for the best specimen on each iteration
  -p: sets the number of worker threads/processes
  --verbose: prints more of internal kitchen
```

It will automatically make a checkpoint (hardcoded to gen.bin in the current directory)
and update it now and then. If terminated, it will load the latest generation
from the checkpoint.

It will also output some statistics and fitness of the latest generation.

## Performance
Unquote 2.2.2 is about 20% faster, than Unquote 3.1.2

DLR is about 3 times slower, than Unquote. It uses Dynamic Language Runtime,
because simplified language lamdas are a bit more powerful, than .NET generics.

TODO: It might be possible to generate static .NET code instead of DLR
by expanding lambdas similarly to C++ templates (e.g. one implementation per
generic type argument).

## Current state
Running this for a couple of weeks on Intel i7 3930k did not produce any useful
result whatsoever (100,000,000+ generations).

## Possible development
* improve performance to churn through generations faster
* be more intelligent regarding mutation and/or crossbreeding
* be more intelligent with the fitness function
