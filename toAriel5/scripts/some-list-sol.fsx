
#r "FsCheck.dll";;
open FsCheck;;


(*
 exercise: write two functions such that 
 1. remove even numbers from int list
    rmEven : (int list -> int list)
    rmEven [2;5;5;87;6;100;2] = [5; 5; 87]

 2. remove all elements in odd position from a list of floats
    considering the first elemnt an even position.
    rmOdd : rmOdd : float list -> float list
    rmOdd   [20.4; 21.4; 22.4; 23.4; 24.4; 25.4; 26.4; 27.4] =
            [20.4;       22.4;       24.4;       26.4] 
*)


let isEven n = (n%2 = 0);;

let rec rmEven xs =
    match xs with
    |[] -> []
    |x:: xs when x%2 = 0 -> rmEven xs
    |x:: xs  -> x:: (rmEven xs);;

let intGenerator = Arb.generate<int>;;

let _ =
  let xs = Gen.sample 8 10 intGenerator
  printf "Original list: %A,  list with even removed %A\n" xs (rmEven xs);;


// se rimuovo i pari, quanto resta è dispari
let prop_remEven xs =
  (rmEven xs ) |> List.forall (fun n -> not (isEven n)) 

let _ = Check.Quick prop_remEven

// rm odd positions: soluzione 1: con counter di posizione
let rmOdd xs =
    let rec rm_aux (i, xs) = 
        match (i,xs) with
        |(_,[]) -> []
        |(i,x:: xs) when (isEven i) -> x:: (rm_aux ((i + 1), xs))
        |(i,x:: xs)  -> (rm_aux ((i + 1), xs))
    rm_aux (0, xs)

// soluzione 2: con pm nidificato
let rec rmOdd2 xs =
    match xs with
        |[] -> []
        |x:: y:: xs  -> x:: rmOdd2  (xs)
        |x:: xs -> x:: rmOdd2 (xs)


let charGenerator = Arb.generate<char>

let _ =
  let xs = Gen.sample 1 8 charGenerator
  printf "Original list: %A,  list after rmOdd %A, list after rmOdd2 %A\n" xs (rmOdd xs) (rmOdd2 xs)


// le due funzioni sono equivalenti?

let prop_remOddOdd2 (xs : int list) =
  rmOdd xs =  rmOdd2 xs;;
let _ = Check.Quick prop_remOddOdd2;;


// una specifica debole
let prop_remOdd1 (xs : char list) =
  List.length (rmOdd xs) <= List.length xs;;
let _ = Check.Quick prop_remOdd1;;

// un po' meglio: la nuova lista è dimezzata 
let prop_remOdd (xs : unit list) =
  let n   = List.length xs
  let res = List.length (rmOdd xs)
  if (isEven n) then res =  n/ 2 else res = (n + 1) /2 

let _ = Check.Quick prop_remOdd;;

// più semantica: la  lista risultante non contiene elementi nuovi
let prop_NoNewStuff (xs : int list) =
  let res = (rmOdd xs)
  Set.isSubset (set res) (set xs)

let _ = Check.Quick prop_NoNewStuff;;

  
// generating lists -- the "hard" way
// lists of numbers from n to 0 and viceversa

let rec upto n =
    match n with
        | 0 -> [0]
        | _ ->  upto (n-1) @ [n] 

// più efficiente
let  fupto n =
    let rec up_acc (n, acc)=
        match (n,acc) with
        | (0, acc) -> 0 :: acc
        | (_, acc) ->  up_acc ((n-1), n::acc) 
    up_acc(n,[])

let prop_up n =
   n > 0 ==> lazy (upto n =  fupto n);;
let _ = Check.Quick prop_up;;

// why the n > 0 ?

// down to by step
let main(n,step) = 
   let rec down n = 
       match n with
        | 0 -> [0]
        | m when m < 0 -> []
        | _ -> n :: down (n-step) 
   if (n < step) then [n] else down n;;


let  downto0 n = main(n,1)
let ns = (downto0 6) = [0..-1..6]
