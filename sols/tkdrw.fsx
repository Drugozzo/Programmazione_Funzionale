#r "FsCheck"
open FsCheck

let rec takeWhile p = function
    |[] -> []
    | x :: xs when p x -> x :: takeWhile p xs
    | _ :: xs  ->  [];;

// esempi

let p1 = takeWhile (fun x -> x < 10) [ 1 .. 20]
// p1 = [1; 2; 3; 4; 5; 6; 7; 8; 9]

let p2 = takeWhile (fun x -> x < 0) [ 1 .. 100]
// p2 = []
    
let p3 =  takeWhile (fun x -> x % 2 = 0) [2;4;5;6];;
// p3 = [2; 4]

let p4 = takeWhile (fun x -> x % 2 = 1) [2;4;5;6];;
// p4 = []

// drops elements while the predicate is true and returns the rest. 
let rec dropWhile p ls =
  match ls with 
    |[] -> []
    | x :: xs when p x -> dropWhile p xs
    | _  ->  ls;;

let d1 = dropWhile (fun x -> x < 10) [ 1 .. 20]
// let d2 =  [10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]

let d2 = dropWhile (fun x -> x > 50) [ 1 .. 10]
// d2 = [1..10]

let d3 =  dropWhile (fun x -> x % 2 = 0) [2;4;5;6];;
//val d3 : int list = [5; 6]

let d4 = dropWhile (fun x -> x % 2 = 1) [2;4;5;6];;
// d4 = [2; 4; 5; 6]



let prop_tw p (xs : int list) =
  xs = (takeWhile p xs ) @ (dropWhile p xs)

do Check.Quick prop_tw


  

