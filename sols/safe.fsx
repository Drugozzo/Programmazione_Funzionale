let safeDiv x y =
    match (x,y) with
    | (Some xx, Some yy) when yy <> 0 -> Some (xx / yy)
       | _ -> None;;

let d1 = safeDiv (Some 3) (Some 4);;
// d1 = Some 0

let d2 = safeDiv (Some 3) (Some 0);;
// d2 = None

let d3= safeDiv (Some 3) None;;
// d3 = None


// optMapBinary   : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option  
let optMapBinary  f x y = 
  match (x,y) with
   | (Some xx, Some yy) -> Some (f xx yy)
   | _ -> None;;

// esempi

let x1 =  optMapBinary (fun a -> fun b ->  2*(a + b) ) (Some 2) (Some 3)
// x1 = Some 10

let x2 =  optMapBinary (fun a -> fun b ->  2*(a + b) )  None (Some -2)
// x2 = None

let x3 =  optMapBinary (fun a -> fun b ->  2*(a + b) )  (Some 10) None
// x3 = None

// optPlus : (int option -> int option -> int option)
let optPlus = optMapBinary (+);;

// optTimes : (int option -> int option -> int option)
let optTimes = optMapBinary (*);;

// esempi

let y1 = optPlus (Some 3) (Some 1);;
// y1 = Some 4

let y2 = optPlus (Some 3) None
// y2  = None

let y3 = optTimes (Some 2) (Some -5)
// y3 = Some -10

let y4 =  optTimes  (safeDiv (Some 1) (Some 0)) (Some 1)
// y4 = None


