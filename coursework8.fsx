(*
  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences and computation expressions

  ------------------------------------------------------------------------------
  Name:
  Student ID:
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework8.fsx in directory coursework8.


  The deadline for completing the above procedure is Friday, December 1, 2017.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Define a sequence
  
  evenOnes : int seq
  
  that contains integer representation of numbers for which the binary
  represenation contains an even number of ones in ascending order.
  
  e.g.   0 -> 0
        11 -> 3
       101 -> 5 
       110 -> 6
etc
*)


(*
  Task 2:

  Define a function fourthRoot : float -> float option that returns Some x if x
  is the 4th root of the argument, and None if the argument has no 4th root. In
  your implementation, use the squareRoot function from the lecture and
  computation expressions for the option type as defined in the lecture.
*)

(*
  Task 3:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> Map<string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values. Use
  computation expressions for reader computations in your implementation. Note
  that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.
*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const of int
  | Ident of string
  | Sum   of Expr * Expr
  | Diff  of Expr * Expr
  | Prod  of Expr * Expr
  | Let   of string * Expr * Expr

//let a = 5 in (a + 1) * 6
//let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6)
//eval(expr)