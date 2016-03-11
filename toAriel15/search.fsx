(* 
   %%%% SEARCH %%%%

A **directed** graph is as set of nodes N with a binary relation R on N.

We see directed graphs as finite maps from nodes to the list of their
immediate succesors.

- we treat here only **acyclic** graphs here for simplicity, but the
  representation would be the same for cyclic ones, while algorithms
  need to carry around a list of *visited* nodes to avoid getting lost
  in a cycle during serach.

- note that trees are a degenerate case of directed graphs where evry
  node has at most one ancestor

- there are billions of other representations from using just relations
  to semi-imperative ones such as in OCaml, see

http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora125.html
*)

type graph<'a when 'a : comparison> = Map<'a,'a list> ;;

(*                       1
                       /  \
                      3     8
                     /     /\ \
                    6        
                          5   4  12
*)

let atree = Map.ofList [ (1,[3;8])  ; (3,[6])  ; 
                      (8,[5; 4; 12]) ; (6,[]) ; (4,[]) ; (5,[]) ; (12,[])];;

(*
                  a
               /    \

              b      C
               \    /
                 d
*)

let agraph = Map.ofList[  ('a',['b';'C']);  ('b',['d']);  ('C',['d']);  ('d',[])  ]


(*
depth-first search of a graph:

- traverse the graph using a stack to hold the nodes to visit next

- at each stage the top of the stack is popped (considered visited and
returned) and its successors are pushed

*)

// val dfvisit : gr:Map<'a,'a list> -> root:'a -> 'a list when 'a : comparison

let dfvisit gr  root = 
 let rec dfs = function
     | [] -> []
     | x :: xs -> let succ_x = Map.find x gr
                  x :: (dfs (succ_x @ xs))                   
 dfs [root];;

                 
let dv = dfvisit agraph   'a';;
// val dv : char list = ['a'; 'b'; 'd'; 'C'; 'd']

(* a trace
stack     succ of top      return
[a]      [b,c]             a
[b,c]    [d]               b
[d,c]    []                d
[c]      [d]               c
[d]      []                d
[]            exit
 *)

// do not have to start with the root
let dv1 = dfvisit agraph   'd';;

let dv2 = dfvisit atree  1;;
//val dv2 : int list = [1; 3; 6; 8; 5; 4; 12]

(*
   breadth-first search instead stores the successor of a node in a queue

*)
// val bfvisit : gr:Map<'a,'a list> -> root:'a -> 'a list when 'a : comparison
let bfvisit gr root = 
 let rec bfs = function
     | [] -> []
     | x :: xs -> let succ_x = Map.find x gr
                  x :: (bfs (xs @ succ_x)) 
 bfs [root];;

let bv = bfvisit agraph   'a';;

// val bv : char list = ['a'; 'b'; 'C'; 'd'; 'd']
(*
stack     succ of top      return
[a]      [b,C]             a
[b,C]    [d]               b
[C,d]    [d]               C
[d,d]     []               d
[d]      []                d
[]            exit
 *)


// Variations

// tail recursive df, w/o append but with an accumulator, a double
// call to the function and a final reverse

let itdfvisit tree root = 
 let rec df  list  visited = 
  match list with
     | [] -> visited
     | x :: xs ->  let succ_x = Map.find x tree
                   let nvs = (df  succ_x (x :: visited))          
                   (df  xs nvs)                    
 df  [root] [] |> List.rev;;

// Search according to a predicate
// df : gr:graph<'a,'a list> -> pred:('a -> bool) -> root:'a -> 'a list

let df gr pred root = 
 let rec dfs = function
     | [] -> []
     | x :: xs -> let succ_x = Map.find x gr
                  if pred x then x :: (dfs (succ_x @ xs)) 
                     else  (dfs (succ_x @ xs))
 dfs [root];;

// same as dfvisit
let  df_all graph root =
  df graph  (fun x-> true) root

// two predicates
let is_even x = x % 2 = 0 
let lower x = System.Char.IsLower x

let de = df atree is_even 1;;
let ds2 = df agraph lower 'a';;


// breadth-first modulo predicate

let bf gr pred root = 
 let rec bfs = function
     | [] -> []
     | x :: xs -> let succ_x = Map.find x gr
                  if pred x then x :: (bfs (xs @ succ_x)) 
                     else  (bfs (xs @ succ_x)) 
 bfs [root];;

let  bf_all graph root =
  bf graph  (fun x-> true) root

let bt = bf_all atree  1;;
let be = bf atree is_even 1;;
let bs2 = bf agraph lower 'a';;





