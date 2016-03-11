// using the dll

#r "listFS.dll"
// #r "treeFS.dll"
open FSet

// let's create a set
let oneset = ofList [1..10]

// what? I can't see squat!

// utility to see what's going on, but still return a set
let print ss =
  let xs = toList ss
  printf "result set: %A\n" xs
  ss

let _ = print oneset

let ss = add 3 empty |> add 5 |> add 2 |> add 77 |> print

let ss1 = (add 3 ss) |> print;;

let u = union oneset ss |> print;;

// after, try with other representation



// let's test our Set implementation wrt the Set collection
#r "FsCheck"
open FsCheck

(*
 - a suite of tests, as a list, each with a label to identify the culprit

- note that use of List as a mediation betweeb FSet and Set

- note that use of List.sort to avoid false postives due to different orderings

*)


let test_set (x : int) s1 s2 =
  [
    empty |> toList = (Set.empty |> Set.toList) |@ "empty set";
    isEmpty (ofList s1) = Set.isEmpty (Set.ofList s1) |@ "is empty"
    contains x (ofList s1) = Set.contains x (Set.ofList s1) |@ "contains";
    (add x (ofList s1) |> toList |> List.sort) = (Set.add x (Set.ofList s1) |> Set.toList |> List.sort) |@ "add"
    (union  (ofList s1) (ofList s2) |> toList |> List.sort) = (Set.union  (Set.ofList s1)  (Set.ofList s2)|> Set.toList |> List.sort) |@ "union"
    (ofList s1 |> toList |> List.sort ) = (Set.ofList s1 |> Set.toList |> List.sort) |@ "list"
    ]

do Check.Quick test_set

let test_fold (s : int list)  seed f =
      foldBack f (ofList s)  seed = Set.foldBack f (Set.ofList s)  seed |@ "fold is wrongs, but we knew that"

do Check.Quick test_fold




// Let's do something more intersting with this: econding maps for an
// interpreter for arithmetic expressions

type expv =
  | V of string          
  | C of int
  | Sum of expv * expv

// we choose 'a to be (string * int)
type envt = FSet<(string * int)> ;;

// we buind a sample env
let envxyz = ofList [ ("x",1) ; ("y",2) ; ("z", 3)] ;;   

(* Now we have to implement a lookup function in an anviroment,
i.e. the equivalienf of

Map.find: 'a -> Map<'a,'b> -> b
  
  given : 'a  and  m : Map<'a,'b>

      Map.find a m  = b  if  m(a) = b  
      
  Raise an excpetion if a \not\in  Dom(m).

We cannot resonable expect the server side for FSet<'<> to provide us
 with such a function. It would be easy to write it with explicit
 recursion, but we do not have access to the internal representation
 of FSet. So all is lost? No! We have been given the magic tool: fold

*)
let mapfind k env exn =
  FSet.foldBack (fun  (x,v) ys -> if k = x then v else ys) env exn


// Note: exn is a default value rather than retunring None or raising
// exception when we do not find our key

// e.g.
let v1 = mapfind "x" envxyz 0
let v2 = mapfind "xsss" envxyz 0
  
// and now our interpreter ...

let rec evalv e  env  =
    match e with
    | V x ->  mapfind x env 0  // calcola env(x)  
    | C n -> n
    | Sum(e1,e2)   -> evalv e1 env  + evalv e2 env 

let vv = evalv  (Sum(Sum ( V "y" , V "z" ),  C 10)) envxyz



