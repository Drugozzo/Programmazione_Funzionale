#indent "off"

module Q

exception EmptyQueue

type Queue<'a> = Q of 'a list * 'a list

let empty = Q([], [])

let put y (Q (fs, rs)) = Q (fs, (y::rs))

let rec get = function
              |Q((x::fs), rs) ->
                    (x,Q(fs, rs))
              |Q([], []) -> raise EmptyQueue
              |Q([], rs) ->
                    get (Q((List.rev rs),[]))

// why didn't I write empty -> raise EmptyQueue ???



// as an alternative I could have a normalization function and appply
// it at every get
                    
let norm = function
 Q([], rs) ->
                    Q((List.rev rs),[])
 | q -> q;;
(*
let rec getn = function
              |Q([], []) -> raise EmptyQueue
              |Q((x::qs), rs) ->
                    (x,Q(fs, rs)) |> norm)
*)

//SOLUZIONI

let isEmpty qs = (qs = empty)

let rec size qs =
	match qs with
	|Q([], [])  -> 0
	|Q([], rs) -> 
		size (norm (Q([], rs)))
	|Q((x::fs), rs) ->
		1 + size (Q(fs, rs))

let rec ofList ls =
	match ls with
	|[] -> Q([], [])
	|x0::xs -> 
		put x0 (ofList xs)
		

let rec toList qs =
	match qs with
	|Q([], [])  -> []
	|Q([], rs) -> 
		toList(norm (Q([], rs)))
	|Q((x::fs), rs) ->
		x :: toList (Q(fs, rs))

let rec put_list ls qs=
	match ls with
	|[] -> qs
	|x0::xs -> put_list xs (put x0 qs)


            
