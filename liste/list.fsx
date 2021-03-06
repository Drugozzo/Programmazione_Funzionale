
module E

(* ****LISTS***** *)


(*

Una lista e' una sequenza finita di elementi dello stesso tipo.
Il tipo di una lista e' definito mediante il costruttore  list.



DEFINIZIONE TIPO LISTA
^^^^^^^^^^^^^^^^^^^^^^

Sia T un qualunque tipo. Con
   
      T list 
  
denotiamo il tipo di una lista i cui elementi hanno tipo T

========    

Esempi:

            int list  ---> lista di interi 

            char list ---> lista di caratteri  

  (int * string) list ---> lista i cui elementi sono coppie int * string
   
    (int -> int) list ---> lista i cui elementi sono funzioni di tipo int -> int

    (int list) list   ---> lista i cui elementi sono liste di interi


Notare che:

-  Il costruttore list ha precedenza piu' alta di * e ->.

   Quindi:

      int * string list    equivale a      int * (string list)  
                                          // coppie (n, ls),  con n : int e ls : string list 
                                                            

      int -> float list    equivale a        int -> (float list)
                                             // funzioni da int a float list

- list e' associativo a sinistra

   Quindi

     int list list     equivale a     (int list) list
                                      // liste di liste di int   
                                          

Una lista i cui elementi hanno tipo T e' rappresentata
da un termine F# di tipo T list.


DEFINIZIONE TERMINI DI TIPO T LIST
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sia T un tipo.
I termini di tipo T list sono definiti induttivamente come segue:

(1) [] e' un termine di tipo T list che rappresenta la lista vuota

(2) Supponiamo che xs sia un termine di tipo T list e x un termine di tipo T.

    Allora il termine
   
       x :: xs

    costruito applicando l'operatore :: (cons)  a x e xs e' un termine di tipo T list   

    Il termine x :: xs rappresenta la lista ottenuta ponendo x in testa alla lista xs.

======

Nel termine  x :: xs, x e' detta la testa (head) della lista, xs la coda (tail).
 
Il termine x :: xs puo' essere rappresentato dall'albero
   
                       :: (cons)
                     /    \             x : T           
                    /      \           xs : T list
                   x       xs  
              (testa)       (coda) 
                 


======

Il significato della definizione e'  che un qualunque termine di tipo list T
deve poter essere costruito partendo dalla lista vuota applicando
un numero finito di volte il passo (2)

L'operatore :: (cons) associa a destra. Ad esempio 

   x0 :: x1 :: xs   equivale  a   x0 :: ( x1 :: xs )

e rappresenta la lista il cui primo elemento e' x0, il secondo e' x1,
seguita poi dagli elementi della lista xs.

======

Il tipo T degli elementi di una lista puo' essere un qualunque tipo (anche polimorfo).

In particolare, il termine [] rappresenta una lista vuota di un qualunque tipo T.
Il tipo di [] e' quindi:

   [] : 'a list    // termine polimorfo


*)


// Esempi  di liste

[] ;;    // lista vuota
// il tipo di []  e'  'a list (tipo polimorfo)

1 :: [] ;;
// il termine rappresenta la lista [1] e ha tipo int list

let l1 = 100 :: 1 :: [] ;;
//  l1 e' la lista [100 ; 1 ] di tipo int list

(*

  albero che descrive il termine l1

                ::
               /   \
              /     \
            100      ::
                     / \
                    /   \
                   1     []


*) 



let l2 = 200 :: l1  ;;
//  l2 e' la lista [200 ; 100 ; 1 ] di tipo int list


(*  SINTASSI

Come si e' visto negli esempi sopra, un altro modo per rappresentare
una lista e' scrivere i sui elementi fra quadre separati da ;

La sintassi che usa cons e' pero' piu comoda nel pattern matching.

*)  


let bl = [ true ; false ; true ] ;;
//val bl : bool list = [true; false; true]


let bl1 =  [ 1 < 2;  1 = 2 ;  true ] ;;
// val bl1 : bool list = [true; false; true]
// Notare la eager evaluation (gli elementi sono valutati prima di costruire la lista)

let fl  = [ 3.1 ; 2.6 ; 7.8 ; 1.56 ] ;;
// val fl : float list = [3.1; 2.6; 7.8; 1.56]
 
let pl = [ ("a",3) ; ("ggg",6) ] ;;
// val pl : (string * int) list = [("a", 3); ("ggg", 6)]
 

let funl = [ cos ; sin ;  fun x -> x * 3.5 ]  ;;
// val funl : (float -> float) list  - lista di funzioni float -> float

 
let ll= [ [1;2] ; [3;4] ] ;;
//val ll : int list list - lista di liste di interi
 
// Attenzione a non scrivere , al posto di ; (ricordare che , costruisce una tupla)

let x1l = [ "uno" ; "due" ] ;;
let x2l = [ "uno" , "due" ] ;;
// che differenza c'e' fra xl1 e xl2 ?



(**  RANGE EXPRESSION  **)

(*  Le range espression sono espressioni che permettono di generare liste *)   

// Esempi (vedere i dettagli sul libro)

[3 .. 10] ;;   // lista degli interi fra 3 e 10
// val it : int list = [3; 4; 5; 6; 7; 8; 9; 10]

[3 .. 2 .. 20] ;;   // lista degli interi fra 3 e 10 con passo 2
// val it : int list = [3; 5; 7; 9; 11; 13; 15; 17; 19]

[10 .. -1 .. 0] ;;   // passo negativo
// val it : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]

['c' .. 'f'];;  // lista dei caratteri fra 'c' e 'f'
// val it : char list = ['c'; 'd'; 'e'; 'f']

(*

Verificare come vengono valutati i termini

 1 :: "due" :: []      

 [ 1 ; "due" ; 3 ] 

Notare che

 "due" :: []

 e' una lista di tipo string list, ottenuta applicando cons a
 "due" : string  e   [] (che in questo contesto ha tipo string list).

Il termine
 
    1 :: ( "due" :: [] )

non e' corretto perche' 1 non ha tipo string
    

*)   


(* PATTERN SU LISTE *)

(* Per definire pattern su liste e' utile usare l'operatore :: (cons) *)

 
let y :: ys = ['a' .. 'f' ] ;;
//  val y : char = 'a'
//  val ys : char list = ['b'; 'c'; 'd'; 'e'; 'f']
(*  y e' legato alla testa della lista, ys alla coda *)

let x0 :: x1 :: xs = [0 .. 10 ] ;;

(*

val x0 : int = 0    // primo elemento
val x1 : int = 1    // secondo elemento 
val xs : int list = [2; 3; 4; 5; 6; 7; 8; 9; 10]  // resto della lista

*)

 
(** NOTA SULLE ESPRESSIONI POLIMORFE **)

(*

Alcune espressioni polimorfe non sono ammesse.

Ad esempio, consideriamo la funzione polimorfa 

   List.rev : 'a list -> 'a list

che inverte una lista.

Esempio

 List.rev [ 1 ; 2 ; 3 ] = [ 3 ; 2 ; 1 ] 
   

L'espressione polimorfa

  List.rev [] ;;

non e' ammessa, in quanto si sta applicando una funzione polimorfa a un termine polimorfo
(infatti il tipo di [] e' 'a list).

Per poter applicare List.rev alla lista vuota, occorre mediante una annotazione di tipo,
assegnare a [] un  tipo list concreto (non polimorfo).

Esempi di applicazioni corrette:


List.rev ( [] : int list );;
// [] ha tipo int list

List.rev ( [] : (char * int) list list );;
//  [] ha tipo   (char * int) list list

*)   




(* *** RICORSIONE SU LISTA **)


(*

La forma generale di una funzione ricorsiva che ha argomento
composto da una lista ls e':

let rec f ... ls ... =
 match ls with 
 | [] -> v
 | x::xs -> .... f xs ...

Vengono distinti due casi:

- CASO BASE:

  Se ls e' la lista vuota, la funzione f restituisce v

- CASO INDUTTIVO:

  Supponiamo che ls abbia la forma x :: xs
  Per calcolare il valore, si chiama ricorsivamente f  sulla lista xs

Il caso induttivo e' ben fondato in quanto la chiamata ricorsiva e' fatta
sulla lista xs che e' piu' piccola della lista ls di partenza.

Come si vede negli esempi e negli esercizi, lo schema sopra puo' richiedere
ulteriori raffinamenti (piu' casi da considerare).

Occorre prestare attenzione che le chiamate ricorsive coinvolgano liste
piu' piccole di quella di partenza.

Va inoltre evitato di esplodere inutilmente il pattern mactching,
considerando  'casi particolari' che non sono tali.


*)


(*

Definire la funzione ricorsiva

   sumlist : int list -> int 

che calcola la somma degli elementi di una lista.

====

Supponiamo che ls sia la lista

   ls = [ x0 ; x1 ; x2 ; ... ; xn ]

La ricorsione si basa sul fatto che

    x0 + x1 + x2 + .... + xn  =  x0  +  (x1 + x2 + .... + xn)

Per calcolare

   x1 + x2 + .... + xn

posso chiamare ricorsivamente sumlist sulla sottolista

  [ x1 ; x2 ; ... ; xn ]  // coda di ls

*)  


let rec sumlist ls =  
    match ls with 
        | [] -> 0    
        | x0::xs -> x0 + sumlist xs ;; 
 
let sum1 = sumlist [1 .. 10] ;;  // 55


 
(*

Definire la funzione ricorsiva

  sumprod : int list -> int * int
  
che data una list ls restituisce la coppia (sum,prod),
dove sum e' la somma degli elementi della lista
e prod e' il prodotto degli elementi della lista.

*)  

let rec sumprod ls =  
    match ls with 
        | [] -> (0,1) 
        | x0::xs ->  
            let (sum,prod) = sumprod xs 
            ( x0 + sum, x0 * prod) ;;

(*

L'idea ricorsiva e' come in sumlist

Occorre in questo caso prestare attenzione al valore corretto di

    sumprod []

Consideriamo la chiamata di sumprod su una lista contenente un unico elemento:

   sumprod [x]

Il valore che deve restituire e' (x,x).

Nella chiamata sumprod [x], il pattern matching determina il legame

      x0 ---> x           xs --->  []

Quindi:

  sumprod [x] =  ( x + sum ,  x * prod)
                 dove  (sum,prod) = sumprod []
                 
Poiche' sumprod [x] deve restituire (x,x), si deve porre

    sum = 0      prod = 1

Quindi il valore di  sumprod []  deve essere  la coppia (0,1).

*)   




let sp = sumprod [1..10] ;; // (55, 3628800)

 


(*

Definire la funzione ricorsiva (polimorfa)

     len : 'a list -> int

che calcola la lunghezza di una lista

NOTA
^^^^^

In F# e' definita l'analoga funzione List.length


*)
 
 
let rec len ls =  
    match ls with 
    | [] -> 0 
    | x::xs -> 1 + len xs 
 
len [3..7] ;;   // 5
len ['a'..'z'] ;;  // 26

(*

Notare che anche la soluzione

let rec len ls =  
    match ls with 
    | [] -> 0
    | [x] -> 1
    | x::xs -> 1 + len xs 

e' corretta.

Tuttavia non c'e' nessun motivo per trattare a parte il caso [x] (lista con un solo elemento).
Soluzioni di questo tipo, in cui si aggiungono casi particolari inutili,  vanno evitate

*)   



 
(*

Definire la funzione ricorsiva (polimorfa)

     append: 'a list * 'a list -> 'a list

che concatena due liste. Quindi

   
       append(l1, l2) = lista contenente gli elementi di l1
                        seguiti dagli elementi di l2
                          
NOTA
^^^^^
La funzione append e' gia' implementata in F# dall'operatore @,
che e' un operatore infisso con associativita' a destra.

Esempio

 [1 ; 2 ] @ [ 3 ; 4 ] =  [ 1 ; 2 ; 3 ; 4 ]

 [1 ; 2 ] @ [ 3 ; 4 ] @ [5]  =  [ 1 ; 2 ; 3 ; 4 ; 5 ]


*) 
 
let rec append (xs, ys) = 
    match xs with 
    | [] -> ys 
    | z::zs -> z :: append (zs, ys) 
 
 
append ( [ 1 ; 2] , [ 3 ; 4 ; 5 ] ) ;; //  [1; 2; 3; 4; 5]

(*

NOTA SU :: e @
^^^^^^^^^^^^^^

Attenzione a usare correttamente gli operatori :: (cons) e @ (append)

  ::   ha  operandi di tipo T e T list
   @   ha entrambi gli operandi T list.

Per inserire un elemento x : T  in testa a una lista xs : T list si puo' scrivere

   x  :: xs    OPPURE     [x] @ xs

E' preferibile usare  cons (piu' efficiente)

Per inserire un  elemento x : T  in coda a una lista xs : T list si puo' solo usare  @

   xs @  [x]

 mentre   xs :: x   non ha senso


*)   



(*

Definire la funzione ricorsiva (polimorfa)

   rev : 'a list -> 'a list

che inverte gli elementi di una lista (analoga a List.rev):

  rev [ x0 ; x1 ; .... ; x(n-1) ; xn ]  = [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

===

Notare che il pattern matching permette di estrarre il primo elemento della lista
ma non l'ultimo.

La lista 
 
     [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

puo' pero' essere vista come la concatenazione delle due liste

   [ xn ; x(n-1) ; ... ; x1 ]      [x0]

Inoltre [ xn ; x(n-1) ; ... ; x1 ] puo' essere costruita con la chiamata ricorsiva

  rev [ x1 ; .... ; x(n-1) ; xn ]   // l'argomento di rev e' la coda di ls
  


*)
 
let rec rev ls =  
    match ls with 
    | [] -> [] 
    | x :: xs -> append (rev xs, [x]) 
 

rev [1 .. 10] ;;        // [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]

(*

Definire la funzione ricorsiva (polimorfa)

   mem : 'a * 'a list -> bool   when 'a : equality

che verifica l'appartenenza di un elemento in una lista

Anche in questo caso e' gia' definita in F# l'analoga funzione List.mem


===

Si osserva che

    x  appartiene a  [ y0 ; y1 ; .... ;  yn ] 

se e solo se

 ( x = y0 )    OR   ( x appartiene a  [ y1 ; .... ;  yn ] )

In F# gli operatori booleani sono definiti come in C, Java ..

In particolare, l'OR corrisponde a || ed e' valutato in modalita' lazy.


*)   
 
let rec mem (x , ls) =  
    match ls with 
    | [] -> false 
    | y::ys -> x=y || mem (x, ys) 
     
mem ('b', ['a' .. 'h'] ) ;; // true
mem ('x', ['a' .. 'h'] ) ;; // false
 
 
