#r "FsCheck";;
open FsCheck;;

let sample size n gn =
  let _ = Runner.init.Force() |> ignore
  let rec sample i seed samples =
    if i = 0
    then samples else sample (i-1) (Random.stdSplit seed |> snd) (Gen.eval size seed gn :: samples)
  sample n (Random.newSeed()) []


(*
   FUNZIONI HIGHER-ORDER (LIST LIBRARY)
   ====================================


*   List.map :  ('a -> 'b) -> 'a list -> 'b list

    Dati f : 'a->'b  e   [x0; x1 ; ... ; xn] : a' list

      List.map f  [x0; x1 ; ... ; xn] = [ f x0 ; f x1 ; ... ; f xn]

Esempio:
*)


let m = List.map (fun n -> n*n) [1 .. 10] ;;
//  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

let rec isPosList = function
  | [] -> []
  | x :: xs -> (x > 0) :: isPosList xs

let intlist = sample 10 8 Arb.generate<int> 

// thunk or delay
let intgen = fun () -> sample 10 8 Arb.generate<int> 

// test
let i =
  let xs = (intgen())
  (xs,isPosList xs)

let isPosListMap xs = List.map (fun x -> x > 0) xs

// più brevemente
let isPosListMap2 = List.map (fun x -> x > 0) 

let prop_ispos xs =
  isPosListMap xs = isPosList xs
do Check.Quick prop_ispos

let rec addElems = function
  | [] -> []
  | (x,y)::zs -> (x+y)::addElems zs;;

let ps() = sample 10 8 Arb.generate<int*int> 

let i1 =
  let p = ps()
  p, addElems p;;

let addElemsMap  = List.map (fun (x, y) -> x + y) 

let prop_addel xs =
  addElemsMap xs = addElems xs
do Check.Quick prop_addel

// commutativity
let prop_map x =
  let f x = (float x) / 3. // int -> float
  let mkl x = [x]          // 'a -> 'a list 
  (f >> mkl) x = (mkl >> (List.map f)) x 

do Check.Quick prop_map

(*
 List.filter : ('a -> bool) -> 'a list -> 'a list

  Dati un predicato pred : 'a-> bool e una lista ls  : 'a list

  List.filter pred ls =  lista degli x in ls per cui 'pred x' vale
*)

let chs() = sample 1 8 Arb.generate<char> 
let f =
  let cs = chs()
  cs, List.filter System.Char.IsLetter cs

// altri esempi:

let f2 = List.filter (fun x -> x % 2 = 0) [1..10] ;; // [2; 4; 6; 8; 10]  
let f3 = List.filter (fun x -> x > 10 ) [1..10] ;;   // []  


// esercizio

let rec isMember x = function
  | [] -> false
  | y :: ys -> (x = y) || isMember x ys

// definzione esplicita

let rec interR ys  = function
  | [] -> []
  | x :: xs when isMember x ys -> x :: interR ys xs
  | _ :: xs -> interR ys xs 

let iie = interR [1..10] [5..15]







let inter xs ys = List.filter (fun x -> isMember x ys) xs

let ii = inter [1..10] [5..15]

// e se facessimo loop su xs?
let inter2 xs ys = List.filter (fun x -> isMember x xs) ys
let ii2 = inter2 [1..10] [5..15]

// sicuri?

let prop_inter (xs : int list) ys =
 inter xs ys = inter2 xs ys  
do Check.Quick prop_inter

// Ah! Bisogna non avere ripetizioni, altrimenti operazione non è commutativa

let prop_inter_comm (xs : int list) ys =
 inter xs ys = inter ys xs  
do Check.Quick prop_inter_comm

// sistemimo convertendo i risultati in insiemi
let prop_inter_comm2 (xs : int list) ys =
  (inter xs ys |> Set.ofList) = (inter ys xs |> Set.ofList)
do Check.Quick prop_inter_comm2



// Posiamo definire isMember con un combinatore?

(*   List.exists : ('a -> bool) -> 'a list -> bool

    Dati pred : 'a-> bool  e  ls : 'a list

      List.exists pred ls = true      se esiste x in ls tale che 'pred x' e' true     
                            false     altrimenti (per ogni x in ls, 'pred x' e' false)
*)

let rec exists p = function
  | [] -> false
  | x::xs -> p x || exists p xs;;

(*
let rec isMember x = function
  | [] -> false
  | y :: ys -> (x = y) || isMember x ys

   *)

let isMemberF x xs = List.exists (fun y -> y=x) xs;;

// altri esempi:

let e1 = List.exists (fun n -> n % 2 = 0 ) [1 .. 10] ;;  // true  
let e2 = List.exists (fun n -> n > 100 ) [1 .. 10] ;;    // false 


(*   List.forall : ('a -> bool) -> 'a list -> bool

    Dati pred : 'a-> bool e  ls  : 'a list

      List.forall pred ls = true      se, per ogni x in ls, 'pred x' e' true     
                            false     altrimenti (esiste  x in ls tale che 'pred x' e' false)
*)
// Esempi:

let a1 = List.forall (fun n -> n  < 11 ) [1 .. 10];;     // true
let a2 = List.forall (fun n -> n % 2 = 0 ) [1 .. 10];;   // false  

DISJOINT

let rec disjoint xs ys =
  match xs with
  |[]->true
  |x0::lx-> 
    if List.exists (fun y->y=x0) ys then false && disjoint lx ys
    else true && disjoint lx ys;;

OPPURE

let rec disjoint xs ys =
  match xs with
  |[]->true
  |x0::lx-> 
    not(List.exists (fun y->y=x0) ys) && disjoint lx ys;;

SUBSET

let rec subset xs ys =
  match xs with
  |[]->true
  |x0::lx-> 
    not(List.forall (fun y->y=x0) ys) && disjoint lx ys;;


(*

ESERCIZIO

 Definire la funzione exists analoga a List.exists usando
 List.forall;Usare le equivalenze di De Morgan:

  exists x P(x)  <==>  ~( forall x ~P(x) )
*)

// esercizio da libro

// esplicita
let rec disjR xs ys =
  let rec loop x = function
    | [] -> true
    | y :: ys when y <> x -> loop x ys
    | _ -> false

  match xs with
    | [] -> true
    | x :: xs' -> loop x ys && disjR xs' ys

let d3 = disjR [1..10] [20..24]
let d4 = disjR [1..10] [10..24]

// intermedia

let rec disjR1 xs ys =
  match xs with
    | [] -> true
    | x :: xs' -> not (isMember x ys) && disjR xs' ys








let disj xs ys = List.forall (fun x -> List.forall (fun y -> x <> y) ys) xs

let d1 = disj [1..10] [20..24]
let d2 = disj [1..10] [10..24]

let prop_disj (xs : int list) ys =
 disj xs ys = disjR xs ys  
do Check.Quick prop_disj



// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// REDUCE e FOLD


//  sumlist : int list -> int
let rec sumlist = function
  [] -> 0
  | x :: xs -> x + sumlist xs
  
// int list -> int
let rec prodlist = function
  [] -> 1
  | x :: xs -> x *  prodlist xs

let fact n =
  [1..n] |> prodlist

// 'a list -> 'a
let rec last = function
  [] -> failwith "empty"
  |[x] -> x
  |_ :: y :: xs -> last (y :: xs)


// val lenlist : 'a list -> int
let rec lenlist = function
  [] -> 0
  | x :: xs -> 1 + lenlist xs

(* stessa struttura: iterazione di una funzione (binaria, chiamiamola @) su una lista

 @ [xo .. xn] ---->  ( ... (xo @  x1) @ x2 ) @ ....@ xn)


Nel caso sumlist:  (  ... ( xo +  x1) + x2) + .... + xn)

Nel caso prodlist:  (  ... ( xo *  x1) * x2) * .... * xn)                  

Nel caso last, sia f la funzione che sceglie il secondo elemento (proiezione)

               f x y = y

                   ( .... f ((f x0 x1), x2) ....)

                   f è noto come il combinatore K* (Moses Ilyich Schönfinkel, 1924)
*)

//  List.reduce  : (('a -> 'a -> 'a) -> 'a list -> 'a) 

(* Applies a function to each element of the collection, threading an
accumulator argument through the computation.

This function first applies the function to the first two elements of
the list. Then, it passes this result into the function along with the
third element and so on. Finally, it returns the final result.

If the input function is f and the elements are i0...iN, then it
computes f (... (f i0 i1) i2 ...) iN.

The list must be **not** empty

*)
//

let sumlistR_e xs = List.reduce (fun x y -> x + y) xs

// point-free:

let sumlistR  = List.reduce (+)

let prodlistR  = List.reduce ( * )

// why not point-free ?
let lastR xs = List.reduce (fun _ y -> y) xs

// let lastR  = List.reduce (fun _ y -> y)

// make into a value
let lastR2  = fun xs -> List.reduce (fun _ y -> y) xs

let lenlistR = List.reduce (fun x y -> x + 1)

// cosa fa fun xs -> List.reduce (fun x _ -> x) xs ??

// la definizione esplicita
let rec reduce f = function
  |[] -> failwith "empty"
  |[x] -> x
  | x :: y :: xs -> let acc = (f x y)
                    reduce f (acc :: xs)

// pbt
let notEmp =
  Arb.filter (fun x -> x <> []) Arb.from<int list>

let prop_redRed f =
  Prop.forAll notEmp (fun (xs : int list) -> List.reduce f xs = reduce f xs)
do Check.Quick prop_redRed

// The Map/reduce paradigm

let map_red_ex = 
    intgen()   // genera lista di interi
    |> List.map string  // crea lista di strighe
    |> List.reduce (fun x y -> x + ", " + y)  // prodce un valore
    

(*
 Esiste una versione duale:
       List.reduceBack : ('T -> 'T -> 'T) -> 'T list -> 'T

Applies a function to each element of the collection, threading an
accumulator argument through the computation.

If the input function is f and the elements are i0...iN, then this
function computes f i0 (...(f iN-1 iN)).

   *)

let r1 = List.reduce (-) [1..3]
//  (1 - 2) - 3 = -4
let r2 = List.reduceBack (-) [1..3]
//  1 - ( 2 - 3) = 2

type Tree = 
  | Leaf of int
  | Node of Tree * Tree

let t1 =
  [ for n in 0 .. 3 -> Leaf n] |> List.reduce (fun a b -> Node(a, b))

let t2 =
  [ for n in 0 .. 3 -> Leaf n] |> List.reduceBack (fun a b -> Node(a, b))


let rec reduceBack f = function
  [] -> failwith "empty"
  |[x] -> x
  | x :: y :: xs ->  f x (reduceBack f (y :: xs))

let prop_redRedBack f =
  Prop.forAll notEmp (fun (xs : int list) -> List.reduceBack f xs = reduceBack f xs)
do Check.Quick prop_redRedBack

(*
//  Generalizziamo:
// Fold takes an explicit initial value for the accumulator while reduce uses the first element of the input list as the initial accumulator value.

-  una funzione f a due argomenti 
-  una lista ls = [x0; ... ; x(n-1)] 
-  un valore inizale v0

Il left-fold della lista ls con la funzione f e valore iniziale v0
e' ottenuto calcolando i valori v1, v2, ... (valori accumulati) nel seguente modo:

  v1 =  f v0 x0
  v2 =  f v1 x1
  v3 =  f v2 x2  
  ....    
  vn =  f v(n-1) x(n-1)

Il risultato e' il valore vn (se la lista ls e' vuota, il risultato e' il valore iniziale v0).

- 'S (Source type)  e' il tipo degli elementi della lista di input ls

- 'T (Target type)  e' il tipo dei valori accumulati v0, v1, ... 

- La lista in input e'
 
       ls = [x0; ... ; x(n-1)] :  'S list

   List.fold : ('T -> 'S -> 'T) -> 'T -> 'S list -> 'T

*)


let sumlistF  = List.fold (+) 0

let prodlistF  = List.fold ( * ) 1

let lenlistF xs = List.fold (fun v x -> v + 1)  0   xs;;

// Ma è molto più espressivo perché S e T possono essere diversi

let rev ls = List.fold  ( fun  tl  hs  -> hs :: tl ) [] ls ;;

let map1 f ls = List.fold (fun xs x-> f x :: xs)  [] ls ;;

