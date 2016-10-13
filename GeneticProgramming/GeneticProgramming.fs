module GeneticProgramming.Test

open GeneticProgramming.AST
open GeneticProgramming.Types

//let testExpression = Lambda(0,
//                        Lambda(1,
//                            Lambda(2,
//                                Apply(
//                                    Apply(Term(0), Term(2)),
//                                    Term(1)
//                                )
//                            )
//                        )
//                    )
//
//let compose = Lambda(0,
//                Lambda(1,
//                    Lambda(2,
//                        Apply(
//                            Term(0),
//                            Apply(Term(1), Term(2))
//                        )
//                    )
//                )
//            )
//
//let rand = Lambda(0,
//            Rand(Term 0)
//        )
//
//let matchTest = Lambda(0,
//                    Match({ List = Term(0);
//                            EmptyCase = Zero;
//                            HeadTail =
//                                Lambda(1, // head
//                                    Lambda(2, // tail
//                                        Sum(One, Length(Term 2))
//                                    )
//                                );}
//                    )
//                )
//
//let recTest = Let{  Recursive = true;
//                    Term = 0;
//                    Value =
//                        Lambda(1,
//                            Match{  List = Term(1);
//                                    EmptyCase = Zero;
//                                    HeadTail =
//                                        Lambda(2, // head
//                                            Lambda(3, // tail
//                                                Sum(One, Apply(Term(0), Term(3)))
//                                            )
//                                        );  }
//                        );
//                    Expression = Term(0);}
//
//let mapTest = Let{  Recursive = true;
//                    Term = 0;
//                    Value =
//                        Lambda(1, // func
//                            Lambda(2, // list
//                                Match{  List = Term(2);
//                                        EmptyCase = EmptyList;
//                                        HeadTail =
//                                            Lambda(3, // head
//                                                Lambda(4, // tail
//                                                    Cons(Apply(Term(1), Term(3)),
//                                                        Apply(Apply(Term(0), Term(1)), Term(4)))
//                                                )
//                                            );}
//                            )
//                        );
//                    Expression = Term(0);}
//
//let pipe = Let{ Recursive = true;
//                Term = 0;
//                Value =
//                    Lambda(1, // source
//                        Lambda(2, // dest
//                            Match{  List = Term(1);
//                                    EmptyCase = Term(2);
//                                    HeadTail =
//                                        Lambda(3, // head
//                                            Lambda(4, // tail
//                                                Apply(
//                                                    // pipe tail
//                                                    Apply(Term(0), Term(4)),
//                                                    // head :: dest
//                                                    Cons(Term(3), Term(2))
//                                                )
//                                            )
//                                        );}
//                        );
//                    );
//                Expression = Term(0);   }
//
//let fail = Let{ Recursive = true;
//                Term = 0;
//                Value =
//                    Lambda(1, Sum(Term(0), Term(1)));
//                Expression = Term(0);   }
//
//let exprType expr =
//    let exprTypeMap, _ = inferType expr 0 Map.empty
//    exprTypeMap.[expr]
//
//let testType = exprType testExpression
//let composeType = exprType compose
//printfn "flip: %O" testType
//printfn "compose: %O" composeType
//printfn "rand: %O" (exprType rand)
//printfn "len: %O" (exprType matchTest)
//printfn "rec: %O" (exprType recTest)
//printfn "map: %O" (exprType mapTest)
//printfn "pipe: %O" (exprType pipe)
//// printfn "fail: %O" (exprType fail)
//
////printfn "map: %s" (toFSharpSource mapTest)
////
////compile "TestModule" [  "flip", testExpression;
////                        "map", mapTest ]
////|> printfn "%A"
//
//do
//    printfn "TEST"