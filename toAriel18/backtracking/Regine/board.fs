module BOARD 

  (* representation: size, next free column, number placed, queens *)
  (* rep'n invariant: size >=0, 1<=next free<=size, length(queens) = placed
   (i,j) is (col,row) *)

type board = {sz : int ; ncol : int; nupl: int  ; placed :  (int * int) list}

let newb n = {sz = n ; ncol = 1; nupl= 0  ; placed = []}

let size {sz = n ; ncol = _; nupl=_ ; placed = _} = n

let complete {sz = n ; ncol = _; nupl= k ; placed = _} = (k=n)

let  positions {sz = _ ; ncol = _; nupl=_  ; placed = qs} = qs

let place ({sz = n ; ncol =i; nupl=k  ; placed = qs} ,j) =
    {sz = n ; ncol =i+1; nupl=k+1  ; placed = (i,j):: qs}

let  threatens ((i,j), (i',j')) =
      i=i' || j=j' || i+j = i'+j' || i-j = i'-j'

let rec conflicts (q, qs) =
  match qs with
    | [] -> false
    |q'::qs' -> threatens (q, q') || conflicts (q, qs')

// col i and row j are safe wrt bd
let  safe ({sz = _ ; ncol = i; nupl= _  ; placed = qs}, j) =
  not (conflicts ((i,j), qs))

let show    {sz = _ ; ncol = col; nupl= nu  ; placed = qs} =  (col,nu,qs)
