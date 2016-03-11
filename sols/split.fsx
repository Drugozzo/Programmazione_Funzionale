#r "FsCheck"
open FsCheck

// split : 'a list -> 'a list * 'a list

let rec split list  =
    match list with
      |  [] -> ([],[])
      |  [x] -> ([x] , [])
      |  x0 :: (x1 :: xs) ->
          let (even, odd) = split xs
          (x0 :: even, x1 :: odd)



// esempi

let s3 = split ["anna" ; "barbara" ]      
//  s3 = (["anna"], ["barbara"])
let s4 = split ["ape" ; "bue" ; "cane"]  
//  s4 = (["ape"; "cane"], ["bue"])
let s5 = split [ 0 .. 10 ]    
//  s5 = ([0; 2; 4; 6; 8; 10], [1; 3; 5; 7; 9])
let s6 = split [ 0 .. 11 ]    
//  s6 = ([0; 2; 4; 6; 8; 10], [1; 3; 5; 7; 9; 11])


let prop_split (xs : char list) =
  let (ls,rs) = split xs
  (ls  @ rs) |> List.sort  = (List.sort xs)

do Check.Quick prop_split
