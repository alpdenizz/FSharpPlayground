(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name:
  Student ID:
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Friday, November 10, 2017.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function minInList : float list -> float that returns the minimum element
  in the given list. Make sure your implementation uses tail recursion.
*)

(*
  Task 2:

  Write a function swapElementsInList : 'a list -> 'a list that swaps the 1st
  element with the second, the 3rd with the 4th, etc. Make sure your
  implementation uses tail recursion.
*)

(*
  Task 3:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function minInTree : float Tree -> float that returns the minimum label in the
  given tree. Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree


(*
  Task 4:

  Write a function minInTree' : int Tree -> int that returns the minimum label
  in the given tree, like the function minInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

