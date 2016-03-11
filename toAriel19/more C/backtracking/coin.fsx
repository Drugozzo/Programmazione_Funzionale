
// transfer of control: backtracking via exceptions
exception Change
// val change : int list -> int -> int list
let rec change_nt xs n = 
    match (xs,n) with
        | (_, 0) -> []              // OK, I'm done
        | ([], _) -> raise Change   // ran out of change
        | (coin::coins as cs, amt)  ->
            try 
                if coin > amt 
                    then change_nt coins amt 
                        else  (coin :: change_nt cs (amt-coin))
            with
                |Change -> change_nt coins amt
                
let rec change xs n = 
    match (xs,n) with
        | (_, 0) -> []              // OK, I'm done
        | ([], _) -> raise Change   // ran out of change
        | (coin::coins as cs, amt)  ->
            try 
                if coin > amt 
                    then (printf "coin=%d too big for amt=%d \n" coin amt)
                         change coins amt 
                        else  (printf "using coin=%d \n" coin)
                              (coin :: change cs (amt-coin))
            with
                |Change -> (printf "backtracking on coins=%A and amt=%d \n" coins amt)
                           change coins amt

let change_top coins amt = 
    try
        let c = (change coins amt)
        sprintf "Return the following change: %A for amount %d given coins %A" c amt coins
    with
        | Change -> "cannot give change"

let c1 = change_top [2] 4


(*
1. simple succesful run
change [2] 4 =
  2 :: change [2] 2 =
  2 :: 2 :: change [2] 0 =
    2 :: 2:: []

2. failed run with bt

change [2]  3 =
  2 :: change [2]  1 =
    (2 > 1) 
      2 :: change []  1 =          
    (raise C)
    change [] 1 =
      (raise C)
      change [] 3
      (raise C)
      exit with failure

3 . good run with bt

change [3;2] 4
 using coin=3 
 coin=3 too big for amt=1 
 coin=2 too big for amt=1 
   backtracking on coins=[] and amt=1 
   backtracking on coins=[2] and amt=1 
   coin=2 too big for amt=1 
    backtracking on coins=[] and amt=1 
    backtracking on coins=[2] and amt=4 
  using coin=2 
  using coin=2

Return the following change: [2; 2]"
   *)

// no backtrack   
let ch1 = change_top [5] 15
// 

let ch3 = change_top [2;3] 10
let ch3' = change_top [3;2] 10
let ch4 = change_top [5;2] 16
let noch = change_top [7;3] 11
