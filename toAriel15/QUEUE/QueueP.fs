// Implementation file for a simple Queue

module Queue

exception EmptyQueue

type Queue<'a> = {front: 'a list; rear: 'a list}

let empty = {front = []; rear = []}

let put y {front = fs; rear = rs} = {front = fs; rear = y::rs}

let rec get = function
              | {front = x::fs; rear = rs} ->
                    (x,{front = fs; rear = rs})
              | {front = []; rear = []} -> raise EmptyQueue
              | {front = []; rear = rs} ->
                    get {front = List.rev rs; rear = []}

// why didn't I write empty -> raise EmptyQueue ???


(*
// as an alternative I could have a normalization function and appply
// it at every get
                    
let norm = function
 {front = []; rear = rs} ->
                    {front = List.rev rs; rear = []}
 | q -> q;;

let rec getn = function
              | {front = []; rear = []} -> raise EmptyQueue
              | {front = x::fs; rear = rs} ->
                    (x,{front = fs; rear = rs} |> norm)
*)

//SOLUZIONI

let isEmpty qs = (qs = empty)

let rec size qs =
	match qs with
	|{front = [] ; rear = []} -> 0
	|{front = [] ; rear = rs} -> 
		size (norm {front = [] ; rear = rs})
	|{front = x::fs ; rear = rs} ->
		1 + size {front = fs ; rear = rs }

let rec ofList ls =
	match ls with
	|[] -> {front = [] ; rear = []}
	|x0::xs -> 
		put x0 (ofList xs)
		

let rec toList qs =
	match qs with
	|{front = [] ; rear = []} -> []
	|{front = [] ; rear = rs} -> 
		toList(norm {front = [] ; rear = rs})
	|{front = x::fs ; rear = rs} ->
		x :: toList {front = fs ; rear = rs }

let rec put_list ls qs=
	match ls with
	|[] -> qs
	|x0::xs -> put_list xs (put x0 qs)


            
