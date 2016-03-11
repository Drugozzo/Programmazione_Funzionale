#r "FsCheck"
open FsCheck

(*** Finitely-branching trees ***)
type   'a fbtree = Node of 'a * (('a fbtree) list) 

let leaf  = Node(0,[]) ;;

let mytree1 
  = Node(0,
         [Node(2, 
               [Node(3,[]);
                Node(7,[]);
                Node(5,[])]);
          Node(9,[]);
          Node(10,[])]) ;; 

(* mytree1:

               0 
               |
        ---------------
        |      |      |
        2      9     10
      / | \
     3  7   5

*)

let mytree2
  = Node(0,
         [Node(2,
               [Node(4,
                     [Node(1,[])]);
                Node(9,[]);
                Node(6,[])]);
          Node(8,
               [Node(11,[]);
                Node(13,[])]);
          Node(10,[])]) 


(* mytree2:
                     0 
                     |
        ----------------------
        |            |       |
        2            8       10
      / | \         / \
     4  9  6       11  13
     |
     1

*)   


// counting the number of nodes

let rec count  (Node (_, bs)) =
  1 +  (List.map count bs |> List.sum)
   
// count : fbtree -> int

let c1 = count leaf ;;  // 1
let c2 = count mytree1 ;;  // 7
let c3 = count mytree2 ;;  // 10


// membership of an element belongs to the tree if and only if either
// it is equal to the label of the root, or it belongs to one of its
// branches.

let rec belongs e (Node(v, bs)) =
  (e=v) ||  (List.exists (belongs e) bs) ;;
//  belongs : 'a -> 'a fbtree -> bool   when 'a : equality

let b1 = belongs 0  mytree2  ;;   // true
let b3 = belongs 1  mytree2  ;;   // true
let b4 = belongs 111  mytree2  ;; // false


   // From a tree to a list
let rec tree2list (Node (n, bs)) =
  n:: (List.map tree2list bs |> List.concat ) ;;
// tree2list : 'a fbtree -> 'a list

let l0 = tree2list leaf ;;  // [0]
let l2 = tree2list mytree2 ;;  // [0; 2; 4; 1; 9; 6; 8; 11; 13; 10]

// going forward
// sums
let rec sumt (Node (n, bs)) =  
    n +  (List.map sumt bs |> List.sum)

let s1 = sumt mytree1
let s2 = sumt mytree2

// depth
let rec depth =    function
   | Node (_, []) -> 0
   | Node (_, bs) -> 1 +  (List.map depth bs |> List.max)

let d1 = depth mytree1
let d2 = depth mytree2

// map
let rec maptree f (Node(n,ts)) =
  Node(f n, List.map (maptree f) ts)

let t1 = mytree1, maptree (fun x -> x * x * x) mytree1

// filter2list
let rec filter2list p (Node(n,ts)) =
  let fts = List.map (filter2list p) ts |> List.concat
  if (p n) then n :: fts else fts 

let ff = filter2list (fun n -> n % 2 = 1) mytree1

// filteropt
let rec filterOpt p (Node(n,ts)) =
  let fts = List.map (filterOpt p) ts
  if (p n) then Node(Some n, fts) else Node(None, fts) 

let ffo = filterOpt (fun n -> n % 2 = 1) mytree1

// fold_fbt :  f_node:('a -> 'b list -> 'b) -> f_acc:('a -> 'b) -> fbt:'a fbtree -> 'b

let rec fold_fbt f_node f_acc fbt =
  match fbt with
    | Node(n,[]) -> f_acc n
    | Node(n,ts) -> let nts = (List.map (fold_fbt f_node f_acc) ts)
                    f_node n nts 

let sumtree2 ts = fold_fbt (fun n xs -> n + List.sum xs ) (fun x -> x) ts

let ss1 = sumtree2 mytree1
