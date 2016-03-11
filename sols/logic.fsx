type form =
    | P of bool
    | Not of form
    | And of form * form
    | Or of form * form;;

let rec eval = function
  | P b -> b
  | Not f -> not (eval f)
  | And(f1,f2) -> (eval f1) && (eval f2)
  | Or(f1,f2) -> (eval f1) || (eval f2)


let rec toString = function
  | P b -> string b
  | Not f -> "- (" +  (toString f) + ")"
  | And(f1,f2) -> "(" + (toString f1) + " /\ " + (toString f2) + ")"
  | Or(f1,f2) -> "(" + (toString f1) +  " \/ " + (toString f2)  + ")"

let main f =
  "the result of evaluating " + (f |> toString) + " is " + string (eval f)

let test = main (Not(And(P true, Or(P (2 < 3), P( 3 = 4)))))
