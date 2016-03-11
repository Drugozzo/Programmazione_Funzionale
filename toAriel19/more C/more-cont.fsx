(*

Modelling exceptions via continuations: we give a small CPS
  interpreter for a language with aritmetics and exceptions. We take
  it slow

  *)

type env = Map<string,int>

type exn = 
  | Exn of string

type expr = 
  | CstI of int
  | Var of string
  | Add of expr * expr
  | Raise of exn
  | TryWith of expr * exn * expr

  (* try
      e1
    with
      exn -> e2 *)

// the result of the evaluation
type answer = 
  | Result of int
  | Abort of string

// some examples

// uncaught exn, aborting  
let ex3 = Add(CstI 11, Raise (Exn "outahere"))

// exn which is thrown *and* caught returning 99
let ex4 = TryWith(Add(CstI 11, Raise (Exn "Outahere")),
                  Exn "Outahere", 
                  CstI 999)

// exn which is thrownm but **not**, aborting
let ex5 = TryWith(Add(CstI 11, Raise (Exn "Outahere")),
                  Exn "OopsANotherExn", 
                  CstI 999)

// a first (direct style) interpreter that does not catch exn
let rec eval e env: answer =
    match e with
    | CstI i -> Result i
    | Var x  -> 
      match Map.tryFind x env with
      |  Some i ->  i
      | _     -> Abort ("undeclared var: " + x)
    | Add(e1,e2) ->
      match (eval e1 env, eval e1 env) with
        | (Result i1, Result i2) -> Result (i1 + i2)
        | _ -> Abort "run time error"
        
    | Raise (Exn s) -> Abort ("raised exn: " + s)
    | TryWith (e1, exn, e2) -> Abort "Not implemented"

let ev = eval (Add(CstI 2, CstI 5)) Map.empty 
let ev1 = eval (Var "x") Map.empty 
let ev2 = eval (Raise (Exn "DivByZ")) Map.empty 

// Now, to address catching exceptions we start by adding
// a success continuation, which is threaded in the computation

(* This interpreter coEval takes the following arguments:

    * An expression e to evaluate.
    * An environment env in which to evalute it.
    * A success continuation cont which accepts as argument the value
      of the expression.

   It returns an answer: Result i or Abort s.  When the evaluation
   of e succeeds, it applies the success continuation to its result,
   and when e raises an exception (Exn s), it returns Abort s.
   Since there is no error continuation, there is no provision for
   handling raised exceptions.  
*)

let rec coEval e env
                (cont : int -> answer) : answer =
    match e with
    | CstI i -> cont i
    | Var x  -> 
      match Map.tryFind  x env with
      |  Some i -> cont i 
      | _     -> Abort ("undeclared var: " + x)
    | Add(e1,e2) ->
      coEval e1 env (fun x1 -> coEval e2 env (fun x2 -> cont (x1 + x2)))
    | Raise (Exn s) -> Abort ("Uncaught exception: " + s)
    | TryWith (e1, exn, e2) -> Abort "Not implemented"

// we not need to recursively match on Add

let eval1 e env = coEval e env (fun v -> Result v)
let run1 e = eval1 e Map.empty

let aa3 = run1 ex3
let aa4 = run1 ex4
let aa5 = run1 ex5


let tt =   run1 (Add(CstI 2, CstI 5)) 

// Note that the suc cont is ignored when an error is thrown

let tt1 = run1 (Var "x") 


// Now, we handle the exception
(* This interpreter coEval2H takes the following arguments:

    * An expression e to evaluate.
    * An environment env in which to evaluate it.
    * A success continuation cont which accepts as argument the value
      of the expression.
    * A error continuation fc, which is applied when an exception
      is thrown

   It returns an answer: Result i or Abort s.  When the evaluation
   of e succeeds, it applies the success continuation to its result,
   and when e raises an exception exn, it applies the failure
   continuation to exn.  The failure continuation may choose to catch
   the exception.  
*)


let rec coEvalH e env (cont : int -> answer) (fc : exn -> answer) =
    match e with
    | CstI i -> cont i
    | Var x  -> 
      match Map.tryFind  x env with
      |  Some i -> cont i 
      | _     -> Abort ("undeclared var: " + x)
    | Add(e1,e2) ->
      coEvalH e1 env (fun x1 -> coEvalH e2 env (fun x2 -> cont (x1 + x2)) fc) fc
    | Raise e -> fc e
    | TryWith(e1,exn,e2) ->
       let fc1 throw =
         if throw = exn then coEvalH e2 env cont fc else (fc throw)
       coEvalH e1 env cont fc1

let eval2 e env = 
    coEvalH e env 
        (fun v -> Result v) 
        (fun (Exn s) -> Abort ("Uncaught exception: " + s))

let run2 e = eval2 e Map.empty

let a3 = run2 ex3
let a4 = run2 ex4
let a5 = run2 ex5

