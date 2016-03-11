
// module exptrees


#r "FsCheck";;
open FsCheck;;


// Definizione di una lista polimorfa, i cui elementi hanno tipo 'a come tagged values
 
type  'a GList =
    | GNil
    | GCons of  'a  *  'a GList;;

// alternative syntax
(*    
type  GList2<'T> =
    | GNil
    | GCons of  'T  *  GList2<'T>;;
*)

let lg1 = GCons( 3 , GCons ( 4, GCons ( (1 + 4) , GNil)));;
let lg2 = GCons( 3 < 5 , GCons ( true, GCons ( 10 > 20 , GNil)));;


// definiamo il nostro generatore random anche in VS 2010
let sample size n gn =
  let rec sample i seed samples =
    if i = 0
    then samples else sample (i-1) (Random.stdSplit seed |> snd) (Gen.eval size seed gn :: samples)
  sample n (Random.newSeed()) []


let mylistGenerator = Arb.generate<char GList>

let cs = sample 10 8 mylistGenerator

// utility per usare un dato generatore gen con una funzione f
let use_gen bound listsize gen  f =
  let ss = sample bound listsize gen
  let res = List.map f ss
  printf "before list:\t %A\n\n" ss
  printf "after list: \t %A\n" res
  

let ex = use_gen 10 8 mylistGenerator (fun x -> x)
// GListToList : 'a GList -> 'a list
// Trasforma una GList in una 'a list

let rec GListToList gList =
    match gList with
        | GNil -> []
        | GCons ( x , tail) -> x :: ( GListToList tail );;


let list2 =  GListToList lg2;;
// val list2 : bool list = [true; true; false]

let ex1 = use_gen 10 8 mylistGenerator (GListToList)

// viceversa

let rec List2GList = function
   | [] -> GNil
   |  x :: tail  -> GCons ( x , List2GList tail);;

let l3 =  List2GList [1..10];;

// le due funzioni sono inverse
let  prop_list (xs : char list, ys : bool GList ) =
  [xs = (List2GList xs |> GListToList)
  ;
  ys = ( GListToList ys |> List2GList )]

Check.Quick prop_list

////////////////////////////////////////////////

// From Section 6.5 Expression trees
// w/o variables

//  e ::=  n | e1 + e2 |   e1 * e2 | n mod 2  
// make sure they understand BNF

type aexp =
  | C of int
  | Sum of aexp * aexp
  | Prod of aexp * aexp
  | Mod2 of aexp;;

// infix notation
let (++) m n = Sum(m,n);;
let (@@) m n = Prod(m,n);;

// examples
// ((3 + 4) mod 2 ) * 12 
let a1 =   (Mod2(C(3) ++ C(4))) @@ C(12);;

let aexpGen = Arb.generate<aexp>

// explain natural semantics: e >> v  and trace 2 * (3 + 5) >> 16

// eval  : aexp -> int

let rec eval t  =
    match t with
    | C n      -> n
    | Mod2 t      ->   (eval t ) % 2
    | Sum(t1,t2)   -> eval t1  + eval t2 
    | Prod(t1,t2)  -> eval t1  * eval t2 ;;

let r1 = eval a1;;

let _ = use_gen 5 10 aexpGen eval 

// un esempio di compilazione su AST: semplificazione con neutri
// val opt : e:aexp -> aexp
let rec opt e =
  match e with
    | C n -> C n
    | Sum(C 0,a) | Sum(a,C 0) ->  a
    | Sum(n1,n2) -> Sum(opt n1,opt n2)
    | Prod(C 0,_) | Prod(_,C 0) ->  C 0
    | Prod(C 1,a) | Prod(a,C 1) ->  a
    | Prod(n1,n2) -> Prod(opt n1,opt n2)
    | Mod2 (a) -> Mod2 (opt a)

let _ = use_gen 10 10  aexpGen opt

// validiamo che la trasformazione è corretta
let prop_opt a =
  eval a = (opt a |> eval)
Check.Quick prop_opt

// un altra trasformazione: constant folding


(*
let rec fold_aexp a =
  match a with
  *)  


/////////////// MUTREC

// Example of mutrec function definition:

let rec even n = 
    match n with
    | 0 -> true
    | n -> odd (n-1)
and odd n =
 match n with
    | 0 -> false
    | n -> even (n-1)

let prop_even n =
  (n >= 0 && even n)  ==> lazy (not (odd n))
Check.Quick prop_even

let prop_even_ev n =
  let b = (n % 2  = 0)
  [n >= 0 ==> lazy (even n = b );
   n >= 0 ==> lazy (odd n = not b )]  
Check.Quick prop_even_ev


// rimuovere mut rec

type flag = Ev | Od

let rec even_odd (n,f) =
  match (n,f) with
    | (0,Ev) -> true
    | (0,Od) -> false
    | (m,Ev) -> even_odd (m-1,Od)
    | (m,Od) -> even_odd (m-1,Ev)


let prop_even_ev_od n =
  [n >= 0 ==> lazy (even n = even_odd(n,Ev ));
   n >= 0 ==> lazy (odd n = even_odd(n,Od ))]  
Check.Quick prop_even_ev_od


// Example: File system
// FileSys ::= [] | Element :: FileSys
// Element ::= File(s) | Dir(s,FileSys)

type FileSys = Element list
and Element  = | File of string
               | Dir of string * FileSys;;

let d1 =
  Dir("d1",[File "a1";
            Dir("d2", [File "a2"; Dir("d3", [File "a3"])]);
            File "a4";
            Dir("d3", [File "a5"])
           ]);;


let f1 = [d1;File("f")];;

let rec namesFileSys fs =
  match fs with
    | []    -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement e =
  match e with
    | File s    -> [s]
    | Dir(s,fs) -> s :: (namesFileSys fs);;


let list = namesElement d1;;
let fl= namesFileSys f1;;
