module E

// costo :  string * (string -> float) * (string -> int) -> float

let costo (cod : string, prezzoDi, scontoDi) =
    let pr = prezzoDi cod   // pr : float e' il prezzo di cod
    let sc = scontoDi cod   // sc : int e' lo sconto di cod
    pr - (pr * ( float  sc ) / 100.0) ;;   // valore della funzione



// funzioni prezzo di tipo string -> float

let prA = function
    | "cod1" -> 20.0
    | "cod2"->  50.50
    | _ -> failwith "not found";;
  
let prB = function
    | "cod1" -> 40.0
    | "cod2"-> 100.50
    | _ -> failwith "not found";;


// funzioni sconto di tipo string -> int

let scA = function
    | "cod1" -> 10
    | "cod2"-> 0
    | _ -> failwith "not found";;


let scB = function
    | "cod1" -> 5 
    | "cod2" -> 25
    | _ -> failwith "not found";;


// esempi di calcolo di costi

let c1AA = costo( "cod1" , prA, scA ) ;;
// val c1AA : float = 18.0

let c1BA = costo( "cod1" , prB, scA ) ;;
// val c1BA : float = 36.0

let c2BB = costo( "cod2" , prB, scB ) ;;
// val c2BB : float =  75.375
