// ******  DATATYPES   ******

(******  ENUMERATION TYPES  ******)

type colore = Rosso | Blu | Verde ;;

(*

Viene definito il tipo colore i cui elementi sono Rosso, Blu e Verde.

Nota: construttori *devono* essere maiuscolo

*)   

(**  PATTERN MATCHING  con enumeration types **)

// La funzione valore associa un valore intero a ogni colore

let valore col =
    match col with
    | Rosso -> 1
    | Blu -> 2
    | Verde -> 3 ;;

// val  valore : colore -> int 

let v1 = valore Rosso ;;
// val v1 : int = 1


// altro esempio di  Enumeration Type

type month = January | February | March | April | May | June | July
             | August | September| October | November | December ;;

// la funzione daysOfMonth calcola il numero di giorni in un mese (anno non bisestile)

let daysOfMonth  = function  
  | February                            -> 28
  | April | June | September | November -> 30
  | _                                   -> 31 ;;

// val daysOfMonth : month -> int

let d = daysOfMonth March ;;
// val d : int = 31

// guarda chi si vede ...

type mbool = True | False ;;

// definizione di not usando tipo mbool
let myNot b = 
    match b with
    | True  -> False
    | False -> True ;;

// val myNot : mbool -> mbool

// conversione da mbool a bool

let mbool2bool  b = 
    match b with
    | True  -> true
    | False -> false ;;

// val mbool2bool : mbool -> bool

let bb = mbool2bool (myNot True) |> not ;;
// val bb : bool = true


(****  TAGGED VALUES  ****

Sono usati per raggruppare in un unico tipo elementi di forma diversa 
(discriminated union).

Esempio
^^^^^^^
Definiamo il tipo figura in cui ogni elemento puo' essere:

- un Rettangolo di cui si specifica la misura della base e dell'altezza

   OPPURE
 
- un Quadrato di cui si specifica la misura del lato;

   OPPURE

- un Triangolo di cui si specifica la misura della base e dell'altezza. 

Ogni alternativa e' identificata da un 'value constructor' (costruttore).

*)

//  definizione del datatype figura

type figura = 
   | Rettangolo of  int * int      // base * altezza
   | Quadrato   of  int            // lato
   | Triangolo  of  int * int  ;;  // base * altezza

(*

I costruttori usati nella definizione sono funzioni. In particolare:

 Rettangolo :  int * int -> figura 
 Quadrato   :  int -> figura 
 Triangolo  :  int * int -> figura 

Applicando un costruttore a un argomento si ottiene un 'tagged value'.

*)   


// Esempi di tagged value di tipo figura

let rett = Rettangolo (4, 5) ;;
//  rett : figura = Rettangolo (4,5) 

let quad1 = Quadrato 10  ;; 
// val quad1 : figura = Quadrato 10

let quad2 = Quadrato 5  ;;

let tr = Triangolo (5,3) ;;

#r "FsCheck"
open FsCheck

let figGenr = Arb.generate<figura>
Gen.sample 20 6 figGenr

(***  NOTA ***

Un enumeration type e' un  datatype in cui
tutti i costruttori hanno arieta' zero (non richiedono argomenti).

**)


(**  PATTERN MATCHING  con tagged values  **)



(* La funzione area calcola l'area di una figura fig.
Si assume, per ora, che fig sia una figura ben definita (le dimensioni non sono negative).

NOTA
====

L'area di un rettangolo e  di un quadrato e' intera, mentre l'area del triangolo
puo' non essere intera e va quindi rappresentata usando il tipo float.

La funzione area deve avere tipo

  area : figura -> float 

Quindi l'area del rettangolo e del quadrato devono essere convertite al tipo float

*)

let area fig =
   match fig with
   | Rettangolo(b,h) -> float ( b * h )   
   | Quadrato lato   -> float (lato * lato )
   | Triangolo(b,h)  -> float ( b * h )  / 2.0 ;;

(*

   Ricordarsi che / e' overloaded (divisione intera oppure divisione decimale);
   il significato di / e' dato dal tipo degli operandi. 

*)


// Esempi

let aRett = area rett ;;
// val aRett : float = 20.0

let aQ1 = area quad1  ;;
// val aQ1 : float = 100.0

let aQ2 = area quad2 ;;
// val aQ2 : float = 25.0

let _ =
  let rs = Gen.sample 20 6 figGenr
  printf "list of figures: %A, their area %A \n" rs (List.map area rs)


// *** Discriminated union con un solo costruttore (simile a una tupla)

type figuraColorata =  Col of figura * colore ;;

let f1 = Col (quad1, Rosso) ;;
// f1 e' un valore (tagged value)  di tipo figuraColorata


(*

La funzione valFigura calcola il valore di una figura colorata figCol,
che e' dato dal prodotto dell'area di figCol per il valore del colore di figCol
(vedi funzione valore : colore -> int definita sopra).
Si assume  che figCol sia ben definita.  *)

let valFigura figCol = 
   match figCol with
   |  Col(fig,col) -> (area fig) * float (valore col) ;;

// valFigura : figuraColorata -> float


let vQ1 = valFigura f1  ;;
// val vQ1 : float = 100.0


// ******  POLYMORPHIC DATATYPES  ******

(*

In F#  e' definito il tipo polimorfo 

type 'a option =   
    | None
    | Some of 'a

Molto utile per esprimere funzioni PARZIALI (alternativa a eccezioni).


*)

(* Esempio 1
   ^^^^^^^^^^ 

La funzione fattoriale definita la scorsa lezione ha tipo

  int -> int

Il fattoriale di n e' pero' definito solo per n >= 0.
Come gestire il caso in cui la funzione e' applicata a un intero n negativo?

Sono possibili due soluzioni:

(i)  Viene sollevata una eccezione.

(ii) Si definisce la funzione in modo che restituisca un valore di tipo 'int option'.

Piu' precisamente, definiamo la funzione

  facOpt : int -> int option

Il risultato R dell'applicazione

   facOpt n

e' un tagged value di tipo 'int option' della forma 'Some k' oppure 'None'.


- Se R = Some k, allora n >= 0 (il fattoriale di n e' definito)
  e k e' il fattoriale di n.   
  
- Se R = None, significa che l'argomento n e' negativo,
  quindi il fattoriale di n non e' definito.

*)

// (i):  with exceptions

let rec gfac n =
    match n with
    | 0 -> 1
    | n when  n > 0 -> n * gfac (n-1)
    | _ ->  failwith "Negative argument to fac" ;;
// notare l'uso di when nel secondo caso


// (ii): with options

let rec facOpt n =
    match n with
    | 0 -> Some 1
    | n  when n > 0 ->  Some (n * Option.get(facOpt (n-1)))  
    | _ -> None ;;

(*

La funzione

  Option.get : 'a option -> 'a

permette di 'estrarre' il  valore in un option type.

Ad esempio:

Option.get (Some 5) ;;
// val it : int = 5

Solleva una eccezione se applicata a None.

NOTA
====

In genere per estrarre l'argomento di Some non si usa Option.get,
ma il  pattern-matching (vedi esempi sotto).

*)

// esempi di applicazione di facOpt

let f1 = facOpt -2 ;;
// val f1 : int option = None

let f2 = facOpt 4 ;;
// val f2 : int option = Some 24


// calcola fattoriale di n, se definito, e restituisce una stringa col risultato

let printfact n = 
   match (facOpt n) with 
   | None ->  "negative input"
   | Some f ->  ("the factorial of " + (string n) + "  is: " + (string f)) ;;

// val printfact : int -> string

// esempi

let fs1 = printfact -3 ;;
// val fs1 : string = "negative input"

let fs2 = printfact 3 ;;
// val fs2 : string = "the factorial of 3  is: 6"


(* Esempio 2
   ^^^^^^^^^^ 

Definire la funzione areaOpt che calcola l'area di una figura fig, se definita.
La funzione restituisce:

-   None       se fig non e' ben definita (una delle dimensioni e' negativa); 
-   Some a     se fig e' ben definita e a e' l'area di fig.

Notare che 'None' e 'Some a' sono valori di tipo 'float option', quindi 

  areaOpt : figura -> float option

*)

let areaOpt fig =
   match fig with
   | Rettangolo (b,h) | Triangolo(b,h)  ->
       if b < 0 || h < 0 then None
       else Some (area fig)
   | Quadrato lato ->
       if lato < 0 then None
       else Some (area fig) ;;

(*

Non confondere l'espressione if-then-else di F# con l'omonima struttura di controllo
usata nei linguaggi di programmazione (C, Java, ..)

L'espressione

    if b < 0 || h < 0 then None
    else Some (area fig)

equivale all'espressione

   match  b < 0 || h < 0  with
     | true  ->  None
     | false ->  Some (area fig)

Analogamente,

   if lato < 0 then None
   else Some (area fig) 

e' equivalente a:

  match lato < 0 with
     |  true  ->  None
     |  false ->  Some (area fig)


*)   



// calcola area di fig, se definita, e restituisce una stringa col risultato
   
let printArea fig = 
   match (areaOpt fig) with 
   | None ->  "invariant not satisfied"
   | Some f ->  ("the area is: " + (string f)) ;;

// val printArea : figura -> string

// esempi

let as1 = printArea ( Quadrato 10 ) ;;
// val as1 : string = "the area is: 100"

let as2 = printArea ( Quadrato -10 ) ;;
// val as2 : string = "invariant not satisfied

