//---------------   LEFT-FOLD  ---------------


// Somma gli elementi di una lista di interi ls

let sum ls = List.fold (fun v x -> v + x) 0 ls ;;
// sum : int list -> int

(*

Vengono accumulate le somme parziali.
Supponiamo che ls = [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

Allora:

 v0 = 0
 v1 = v0 + x0  =  x0  =   sum  [x0]
 v2 = v1 + x1  =  x0 + x1  =  sum [x0 ; x1]
 v3 = v2 + x2  =  x0 + x1 + x2  =   sum [x0 ; x1 ; x2]
 ...

Quando si arriva all'ultimo elemento della lista, si ha

 vn = sum [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

*)   

// OPPURE

let sum1 ls  = List.fold (+) 0 ls ;;
// (+) e' la funzione somma

// OPPURE

let sum2   = List.fold (+) 0 ;;  // applicazione parziale

// Analogamente:

let prod = List.fold ( * ) 1 ;;
//  ( * ) e' la funzione prodotto
   

//------------------------------------------------------


// Calcola n!

let fact n  =  List.fold ( * ) 1UL [1UL .. n ] ;;
// fact : uint64 -> uint64

// esempi

let n = fact 5UL ;; // notare che vanno usate costanti di tipo uint64


//-----------------------------------------------------------------------------

// Calcola la lunghezza della lista ls

let length ls  = List.fold (fun v _ -> v + 1)  0  ls ;;
// length : 'a list -> int

(*

La funzione   (fun v x -> v + 1) incrementa di uno il primo argomento
(il secondo argomento non e' usato)

Vengono accumulate le lunghezze parziali.
Supponiamo che ls = [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

Allora:

 v0 = 0
 v1 = v0 + 1  =  1  =  length [x0]
 v2 = v1 + 1  =  2  =  length [x0 ; x1]
 v3 = v2 + 1  =  3  =  length [x0 ; x1 ; x2]
 ...

Segue che vn = length [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

*)   


//-------------------------------------------------------------
 

// Calcola il minimo elemento della lista non vuota ls
let minList ls = List.fold min  ( List.head ls ) ls ;;

let minList' ls =
  if List.isEmpty ls then failwith "empty" else minList ls

// minList : 'a list -> 'a when 'a : comparison

//  min :  ('a -> 'a -> 'a) when 'a : comparison  calcola il minimo fra due valori

(*

Vengono accumulate i minimi locali.
Supponiamo che ls = [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

Allora:

 v0 = x0
 v1 = min(v0,x0)  =  minList  [x0]
 v2 = min(v1,x1)  =  minList  [x0 ; x1]
 v3 = min(v2,x2)  =  minList  [x0 ; x1 ; x2] 
 ...

Segue che vn = minList [ x0 ; x1 ; x2 ; ... ; x(n-1) ]


*)   


// esempi

let min1 = minList [ 5 ; -4 ; 10 ; -20 ; 3] ;; // -20
let min2 = minList [ 1 ; 20 ; 2 ; 4] ;; // 1
let min3 = minList [ "bue" ; "cavallo" ; "asino" ; "capra" ] ;;// "asino"




//-------------------------------------------------------------

// Inverte la lista ls

let rev ls = List.fold  ( fun  v  x  -> x :: v ) [] ls ;;
// rev : 'a list -> 'a list

(*

L'accumulatore costruisce la lista inversa della parte visitata.
Supponiamo che ls = [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

Allora:

 v0 = []
 v1 = x0 :: v0  = [x0]  = rev  [x0]
 v2 = x1 :: v1  = [x1; x0] =  rev  [x0 ; x1]
 v3 = x2 :: v2  = [x2; x1; x0] = rev  [x0 ; x1 ; x2] 
 ...

Segue che vn = rev [ x0 ; x1 ; x2 ; ... ; x(n-1) ]


*)  

// esempi

let l1 = rev [ 1 ..10 ] ;;
// [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]

let l2 = rev ["ananas" ; "banane" ; "castagne" ; "datteri" ] ;;
//  ["datteri"; "castagne"; "banane"; "ananas"]

let l3 = rev [] : int list  //  l'annotazione di tipo e' obbligatoria 
// []


//---------------   RIGHT-FOLD  ---------------


let map f ls = List.foldBack (fun x v -> f x :: v) ls [] ;; 
// map : ('a -> 'b) -> 'a list -> 'b list

(*

L'accumulatore costruisce la mapped list della parte visitata.
Supponiamo che ls = [ x0 ; x1 ; x2 ; ... ; x(n-1) ]

Allora:

 v0 = []
 v1 = f x(n-1) :: v0  = [ f x(n-1) ]   =  map f [x(n-1)]
 v2 = f x(n-2) :: v1  = [ f x(n-2) ; f x(n-1) ]  =   map  f [x(n-2) ; x(n-1)]
 v3 = f x(n-3) :: v2  = [ f x(n-3) ; f x(n-2) ; f x(n-1) ]  =   map f [x(n-3); x(n-2) ; x(n-1)]
 ...

Segue che vn = map f [ x0 ; x1 ; x2 ; ... ; x(n-1) ]


*)  

let map1 f ls = List.fold (fun xs x-> f x :: xs)  [] ls ;;


// append : 'a list -> 'a list -> 'a list
// Concatena le liste l1 e l2

let append  l1 l2 = List.foldBack ( fun x  xs -> x :: xs)  l1 l2

let app1 = append [1 .. 2] [ 3 .. 4] 
let app2 = append ['a' .. 'd'] []
let app3 = append ["nord" ; "sud"] ["est" ; "ovest"] 


// concat : string list -> string
// Restituisce la stringa  ottenuta concatenando una lista di stringhe.

let concat strList = List.foldBack (+) strList ""

let sp = " ";
let str1 = concat ["Esempio" ; sp ; "di" ; sp; "stringa" ; sp ; sp ; "concatenata"]


// unzip : ('a * 'b) list -> 'a list * 'b list
(*  Data la lista di coppie ls =  [(x1,y1) ; (x2,y2) ; ... ; (xn,yn)]
    costruisce la coppia di liste  ([x1;...;xn] , [y1;...;yn])
*)

let unzip ls  = List.foldBack ( fun (x,y) -> fun (xs,ys) -> ( x :: xs , y :: ys) ) ls ([],[]) 


let unz1 = unzip [ (1, "Anna") ; (2, "Barbara") ; (3, "Carlo") ; (4, "Davide") ]

