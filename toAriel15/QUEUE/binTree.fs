#indent "off"

module binTree

type binTree<'a> = 
	|Node of 'a * 'a binTree list * 'a binTree list

exception EmptyTree

let rec intToFloatTree (tr:binTree<int>)=
	match tr with
	|Node(r,[],[]) -> Node(float r,[],[])
	|Node(r,xs,ys) -> 
		Node(float r, List.map intToFloatTree xs, List.map intToFloatTree ys)

(*
let t2 = Node (2, [], [Node ( 4 , [], [])] );; 
let t7 = Node (7, [], [Node (10, [], [Node ( 13 , [], [] )])]) ;; 
let t8 = Node ( 8, [Node ( 11, [], [])], [] ) ;; 
let t5 = Node ( 5, [t7], [t8] ) ;;
let t9 = Node ( 9, [],   [Node (12, [], [])] );
let t6 = Node( 6, [t9], []) ;;
let t3 = Node(3, [t5], [t6] ) ;;
let t1 = Node (1, [t2], [t3] );; 
*)

let rec inorderToList tr =
	match tr with
	|Node(r,[],[]) -> [r]
	|Node(r,ls,rs) -> (List.collect inorderToList ls) @[r]@ (List.collect inorderToList rs)

(*
let search1 (n,tr)=
	let ls = inorderToList tr
	let rec ff ls = 
		match ls with
		|[] -> false
		|x0::xs -> (n = x0) || (ff xs)
	ff ls
*)