
#r "QueueP.dll";;

open Queue;;


let q = put 3 empty |> put 5 |> put 2 |> put 77;;

let (x1,nq) = get q;;

let (x2,nq2) = get nq;;

// remember: queues are functional, so q is still here

let (a,_) = get q;;


