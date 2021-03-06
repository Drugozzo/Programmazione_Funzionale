#r "FsCheck";;
open FsCheck;;

// Standard presentation of binary trees
type 'a binTree =
    | Leaf    // empty tree
    | Node of 'a  * 'a binTree * 'a binTree ;;   // Node(root, left, right)

let t1 = Node (2, Leaf, Leaf)
let t2 = Node (2, Leaf, Node ( 4 , Leaf, Leaf ) );;
let t7 = Node (7, Leaf, Node (10, Leaf, Node ( 13 , Leaf, Leaf ))) ;; 
let t8 = Node ( 8, Node ( 11, Leaf, Leaf), Leaf ) ;; 
let t5 = Node ( 5, t7, t8 ) ;;


let rec intToFloatTree btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
            Node ( float r , intToFloatTree left , intToFloatTree right ) ;;

let t2r = intToFloatTree t2

// HO?
let rec mapTree f btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
             Node ( f r, mapTree f left, mapTree f right );;

let t2rH = mapTree float t2;;

let prop_float (ts : int binTree) =
   intToFloatTree ts =  mapTree float ts
do Check.Quick prop_float ;;



// we can extend HO combinators to trees or to other user-defined datatypes
// Remember search?
let rec search  (x, btree) =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) ||  search  (x,left)  ||  search (x,right)  ;;

// It's an instance of the exists combinator

let rec exTree  p btree =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            p r ||  exTree p left  ||  exTree p right  ;;


let prop_exS  x (ts : int binTree) =
   search(x,ts) = exTree (fun n -> x = n) ts
do Check.Quick prop_exS  ;;


// what about filter? It's easy to (inorder) filter to a list

let rec filterToList  pred btree =
    match btree with
        | Leaf -> []
        | Node ( r, left, right ) ->
            let fleft  =  filterToList pred left
            let fright =  filterToList pred right
            if pred r then fleft @ [r] @  fright else  fleft  @  fright;;

(*
but what about filterTree : pred:('a -> bool) -> btree:'a binTree -> 'a  binTree ?
where glue merges the two sub-trees, but in which order?

   *)

let rec glueleft t2 = function
  Leaf -> t2
  | Node(x,l,r) -> Node(x, glueleft l t2, r);;

let rec filterTree  pred btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
            let fleft =   filterTree pred left
            let fright =  filterTree pred right
            if pred r then Node(r,fleft, fright)
            else  glueleft fleft fright;;

let nt5 = t5, filterTree (fun x -> x > 8) t5;;


// a different solution that preserves the structure of the tree

let rec filterTreeOpt  pred btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
            let fleft =   filterTreeOpt pred left
            let fright =  filterTreeOpt pred right
            if pred r then Node(Some r,fleft, fright)
            else  Node(None,fleft,fright);;

let nt5o = t5, filterTreeOpt (fun x -> x > 8) t5;;

// towards fold

//summing the values in a int binTree

let rec sum   btree =
    match btree with
        | Leaf -> 0
        | Node ( r, left, right ) -> r + sum left + sum right;;

// number of nodes
let rec count   btree =
    match btree with
        | Leaf -> 0
        | Node ( r, left, right ) -> 1 + count left + count right;;

// depth of a tree
let rec depth   btree =
    match btree with
        | Leaf -> 0
        | Node ( r, left, right ) -> 1 + (max (depth left) ( depth right));;

(*
Those functions follow the same pattern and should be implementable
 with a fold-like combinator:

 You can think of fold as a function that replaces the constructor
 of a datatype with other functions

 Take for now a list

 1 ::( 2 :: (3 :: [])).

       We can sum it up with fold(Back) by taking :: to be + and [] to be 0 

 1 + (2 + (3 + 0)).

*)

// (('a -> 'b -> 'b) -> 'a list -> 'b -> 'b)
let rec foldBack f_cons xs f_nil =
   match xs with
     | [] -> f_nil 
     | x::xs' -> f_cons x (foldBack f_cons xs' f_nil);;

// a complicated way to do nothing

let idlist xs =
  let f_cons = (fun y ys -> y :: ys)
  let f_nil = []
  foldBack  f_cons xs f_nil ;;


// the FOLD combinator  for trees
let rec fold_tree f_node f_leaf tree = 
  match tree with
    | Leaf -> f_leaf
    | Node (x, left, right) -> f_node x (fold_tree f_node f_leaf left) (fold_tree f_node f_leaf right);;

// sum of elements
let sumbtf = fold_tree (fun x l r -> x + l + r) 0 ;;

// # of elements
let countf ts = fold_tree (fun _ l r -> 1 + l + r) 0 ts;;

let c = countf t2

// depth of tree
let depthtf ts = fold_tree (fun _ l r ->  1 + (max l  r))  0 ts;;

let d = depthtf t2;;

// inorder traversal

let inOrder tr = fold_tree (fun x l r -> l @ [x] @ r) []  tr;;

// defining filterOpt
let filterTreeOptF p tr =
  fold_tree (fun x l r -> if p x then Node(Some x,l,r) else Node(None,l,r) ) Leaf tr 


(*LEFTOVERS

// filter list as a fold

// let reduceTree (Node(n, ts)) = 


// reduce on binary trees -- first attempt
//  reduce : ('a -> 'a -> 'a) -> 'a binTree -> 'a

let rec reducebt1 f = function
  | Leaf -> failwith "empty tree"
  | Node(n,Leaf,Leaf) -> n
  | Node(n,l,r) -> f (reducebt1 f l) (reducebt1 f r);;

let sumbt1  tree = reducebt1 (+) tree;;
let t2c =sumbt1 t2;;

// Oops, it should fail on the empty tree, not on leaves

let rec reducebt f = function
  | Leaf -> failwith "empty tree"
  | Node(n,Leaf,Leaf) -> n
  | Node(n,Leaf,r) -> f n (reducebt f r)
  | Node(n,l,Leaf) -> f n (reducebt f l)
  | Node(n,l,r) -> f (reducebt f l) (reducebt f r);;


   *)caffe sigaretta fuga


