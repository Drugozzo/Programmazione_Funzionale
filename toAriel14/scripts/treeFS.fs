module  FSet

// we use binary search trees as an alternative implementation of sets

type 'a FSet = Leaf | Node of 'a * 'a FSet * 'a FSet

// First, a technicality
let (.<) small big = 
      match (Unchecked.compare small big) with
      | -1 ->  true 
      | _ ->   false;;


(* why do we redefine comparison? In general this is *not* a good idea,
as it will allow to compare things that we should not
compare such as functions. However, here we would need 'a to be both 

'a : comparison and 
'a : equality

for this implementation, and this mean multiple inheritance 

So we suppress the static check using Uncheched.equals
*)


let empty =   Leaf

let isEmpty t = (t = Leaf)

let rec contains  x  btree =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) || 
            ( (x .< r) && contains  x left ) ||
            ( not (x .< r) && contains  x right );;  


let rec add  x  btree  =
    match btree with
        | Leaf -> Node(x, Leaf, Leaf)  
        | Node(r, left, right) when x = r ->  btree 
        | Node(r, left, right) when x .< r ->  Node(r,  (add  x left) , right )
        | Node(r, left, right)  ->  Node(r , left, (add x  right) ) 


let ofList list = List.fold (fun t x -> add x t ) Leaf list


// foldback :  f_node:('a -> 'b -> 'b -> 'b) -> tree:'a FSet -> f_leaf:'b -> 'b
let rec fold_treeBack f_node tree f_leaf  = 
  match tree with
    | Leaf -> f_leaf
    | Node (x, left, right) -> f_node x (fold_treeBack f_node left f_leaf ) (fold_treeBack f_node right f_leaf )

let rec union  s1 s2 = 
    match s1 with
     | Leaf -> s2
     | Node(x,ltr,rtr) -> let ts = add x s2
                          let tsl = union  ltr ts
                          union  rtr tsl;;


// preorder here
let toList tree  = fold_treeBack (fun x l r -> x :: l @ r) tree [] 

let foldBack (f : ('a -> 'b -> 'b) ) (tree : 'a FSet) (seed : 'b) = seed

(* Variations

let rec toList btree =
    match btree with
        | Leaf -> []
        | Node ( r , left, right ) ->
            toList left @ [r] @  toList right 



let ofList list =
 let rec addFromList (list, tree) =
    match list with
        | [] -> tree
        | x :: xs ->
            let tree1 =  add  x tree
            addFromList( xs , tree1) 
 addFromList(list,Leaf);;

   *)

(*

let unionf s1 s2 = fold_treeBack (fun x l r -> add x l) s1 s2 

#r "FsCheck"
open FsCheck

let prop_u s1 (s2 : int FSet) =
  (union s1 s2 |> toList |> List.sort) =
      (unionf s1 s2 |> toList |> List.sort)

do Check.Quick prop_u
*)

(*SOLUZIONI ESERCIZI*)

let rec count t =
  match t with
  |Leaf -> 0
  |Node(r,left,right) -> 1 + count left + count right;;

let rec map f s = 
  match s with
  |Leaf -> Leaf
  |Node(r,left,right) -> 
    let sx = map f left
    let dx = map f right
    let tr = union sx dx
    let n = f r
    if (not(contains n tr)) then add n tr
    else tr;;

