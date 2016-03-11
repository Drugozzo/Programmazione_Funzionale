module BOARD 
  type board
  val newb : int -> board
  val complete : board -> bool
  val place : board * int -> board
  val safe : board * int -> bool
  val size : board -> int
  val positions : board -> (int * int) list
  val show : board -> int * int * (int * int) list

