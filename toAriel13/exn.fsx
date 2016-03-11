// we have seen how to protect from run-time errors (DividebyZero) with options
// odiv: (int -> int -> int option)

let odiv x y =
    if y = 0
        then None
            else Some (x/y);;
(*
 EXCEPTIONS are more flexible and informative

In FP expressions
	- always have (unique) type
	- may have a value (upon termination)
	- may have an effect.
	
an *effect* is any action resulting from
evaluation of an expression other than returning a value. 

1. Exceptions. Evaluation may be aborted by signaling an exceptional
condition.

2. Mutation. Storage may be allocated and modified during evaluation.

3. Input/output. It is possible to read from an input source and write to
an output sink during evaluation.

4. Communication. Data may be sent to and received from communication
channels.

-- Static violations are signalled by type checking errors; 
-- dynamic violations are signalled by raising exceptions.

The primary benefits of the exceptions

1. They force you to consider the exceptional case (if you don’t, you’ll
get an uncaught exception at run-time), and

2. They allow you to segregate the special case from the normal case in
the code (rather than clutter the code with explicit checks).


--- pre-defined exn in F#
*)
// let x = 1 / 0

// ===> System.DivideByZeroException: Division by zero

// let _ = List.head ([]:int list)
// ===>  System.ArgumentException: The input list was empty.

// let [] =  [1;2]

// ==> MatchFailureException

// let req = System.Net.WebRequest.Create("not a URL");;
//  ==> System.UriFormatException: Invalid URI: The format of the URI could not be determined

(*
   --- RAISING excn
val failwith : string -> 'a
val raise: #exn -> 'a
val failwithf : StringFormat<'a,'b> -> 'a
val invalid_arg : string -> 'a

 exceptions can be used to effect non-local transfers of control
 handling: try e with p1 -> e1 | ..... | pn -> en
 where pi is like a 'pattern' of match cases, but of type exception (more later)

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Types and exn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
What is the type  τ exn  of exception values?
(In f# exn synonym of System.Exception)

- It must be **one and the same** for all exceptions in a program. 

1. A very naive choice: type of strings. 

	This allows one to associate an “explanation” with an exception. 
	For example, one may write
		raise "Division by zero error."

	-> but a handler for such an exception would have to interpret the
	string if it is to distinguish one exception from another,


2.  int, so that exceptional conditions are encoded as error numbers
that describe the source of the error. 
	By dispatching on the numeric code the handler can determine how to recover from it. 

	-> one must establish a globally agreed-upon system of numbering, 
		no modular decomposition and component reuse. 
		
	-> it is practically impossible to associate meaningful data with an exceptional condition

3.  sum type whose classes are the exceptional conditions.  

	For example

	type exn = Div of unit | FileNotFound of string, . . .

	-> it imposes a static classification of the sources of
        failure in a program. There must be one, globally agreed-upon type


SOLUTION:

----> An **extensible sum type** is one that permits new tags to be
created dynamically so that the collection of possible tags on values
of the type is not fixed statically, but only at runtime.

e ::= ... | raise(e) | try e with p1 -> e1 | ..... | pn -> en

	Γ |-  e : τ exn
	--------------------
	Γ |- raise(e) : τ
	Γ|-  e1 : τ      Γ, p : τ exn |-  e2 : τ
	-----------------------------------
	Γ|- try e1 with p -> e2 : τ
        *)

// with (predefined) exn
// val divide0 : int -> int -> string

// note same types in both alternatives (here string)
let divide0 x y =
   try
      string (x / y)
   with
      | :? System.DivideByZeroException -> "Division by zero!"

let result1 = divide0 100 ( 3 - 3)
let result2 = divide0 100 ( 10 - 5)

// excn AND options
// val divide1 : x:int -> y:int -> int option
let divide1 x y =
   try
      Some (x / y)
   with
      | :? System.DivideByZeroException ->
        printfn "Division by zero!"
        None

let result3 = divide1 100 3
let result3 = divide1 100 0

(*
 note that exceptions are objects
 System.DivideByZeroException is a class that inherits from System.Exception
 here ex is a value of type  System.DivideByZeroException
 and the pattern ':? class' checks whether ex is such a value
*)



(*
EXERCISE: define the head of a list **using** List.head and catching
the exception

val safe_head : 'a list -> 'a option

let sh = safe_head ([]:int list)
    > val sh : int option = None
let st = safe_head [1;2]
    > val st : int option = Some 1

*)


let safe_head xs =
  try
    Some (List.head xs)
  with
  :? System.ArgumentException ->
     printfn "Empty list!"
     None;;


// user declared exceptions w/o values

exception NegFactorial

let check_fact n =
    let rec fact = function
        | 0 -> 1
        | m -> m * fact (m - 1)
    if n >= 0 then fact n else raise NegFactorial

// val handle_fact_s : n:int -> string
let handle_fact_s n =
    try
        string(check_fact n)
    with
        | NegFactorial -> "negative integer not valid"
       
let p1 = handle_fact_s 4
let p2 = handle_fact_s -42
       
// more informative, with sprintf to create the return formatted string
// val handle_fact : n:int -> string
let handle_fact n =
    try
        let res = check_fact n
        sprintf "input: %d, Output: %d" n res
    with
        | NegFactorial -> sprintf " your input: %d is negative!" n
 
let p4 = handle_fact 4
let p5 = handle_fact -42


(* ***************************************** *)
// exc to change the flow of computation
//     breaking out of a loop/recursion

// #time;; 
let zeros =  List.init  1000000 (fun i -> 0);;

let fmult_list = List.fold ( * ) 1

let res = fmult_list zeros

exception Found_zero;;

let mult_list l =
    let rec mult_aux  l =
      match l with
        | [] ->  1
        | 0 :: _ -> raise Found_zero
        | n :: xs -> n * (mult_aux xs)
    try
        mult_aux l
    with
        |Found_zero -> 0;;

let m1 = mult_list zeros
