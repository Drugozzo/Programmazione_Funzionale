// interface for finite  set of any type 'a, whose equality is reasonable

module FSet

// the type specification:

type FSet<'a when 'a : equality> 

(*
-  no implementation: type is kept abstract

- why the constraint 'a when 'a : equality?

  -> the type specified by the signature cannot be more general than
     its representation in the implementation module. Since we will
     use lists and characteristic functions as implementation, which
     use '=', we need to put this in the spec.  

Here the spec of the functions (the interface) -- a subset of the Set library
*)
val empty : FSet<'a> 

val isEmpty : FSet<'a> -> bool 

val contains  : 'a ->  FSet<'a> -> bool 

val add : 'a -> FSet<'a> -> FSet<'a> 

val union : FSet<'a> -> FSet<'a> -> FSet<'a> 

val ofList :  'a list -> FSet<'a> 

val toList :  FSet<'a>  -> 'a list 

// val fold :  ('a -> 'b -> 'a) -> 'a -> FSet<'b> -> 'a

val foldBack : (('a -> 'b -> 'b) -> FSet<'a> -> 'b -> 'b)

(*SOLUZIONI*)

val count : FSet<'a> -> int

val map : ('a -> 'b) -> FSet<'a> -> FSet<'b>
