module  Stack

type Stack<'a> = S of 'a list

exception EmptyStack;;

let empty = S [];;

let push x (S ss) = S (x :: ss);;

let top = function
  S [] -> raise EmptyStack
 | S (x :: xs) -> x;;

let size (S xs) = List.length xs

let pop = function
  S [] -> raise EmptyStack
 | S (x :: xs) -> (x, S xs);;

let isEmpty ss = (ss = empty);;

let rec push_list xs st =
  match xs with
    [] -> st
    | x :: ys ->  (push_list ys ( push x st))

let toList (S ss) = ss;; 

let ofList ss = push_list ss empty;;

let append  s1 (S s2) = S( s1 @ s2)

(*
#if INTERACTIVE

let prop_top (x : 'a, st : 'a Stack) =
	let ls = (x :: (toList st))
	top (push x st) = (List.head ls);;

Check.Quick prop_top;;

#endif

*)




