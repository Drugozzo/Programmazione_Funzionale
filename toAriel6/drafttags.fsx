module E

// ******  DATATYPES   ******

(******  ENUMERATION TYPES  ******)

type colore = Rosso | Blu | Verde ;;

(*

Viene definito il tipo colore i cui elementi sono le costanti Rosso, Blu e Verde.

Rosso, Blu e Verde sono detti *value constructor*.
In questa definizione abbiamo costruttori senza argomenti, che definiscono le costanti di tipo colore.

NOTA: I costruttori *devono* essere in maiuscolo

*)   

(**  PATTERN MATCHING  con enumeration types **)

(* Definire la  funzione

     valore : colore -> int

che  associa a ogni colore un intero nel modo seguente:

    Rosso ---> 1    Blu   ---> 2    Verde ---> 3 

*)

let valore col =
    match col with  // pattern matching su un valore di tipo colore
    | Rosso -> 1
    | Blu -> 2
    | Verde -> 3 ;;
// val  valore : colore -> int 


// OPPURE

(*

Quando una funzione e' definita per pattern matching sull'argomento,
si puo' usare anche la seguente sintassi in cui l'argomento e' sottointeso

*)
 
let valore1  = function
     | Rosso -> 1
     | Blu -> 2
     | Verde -> 3 ;;
// val  valore1 : colore -> int 

// Esempio di applicazione della funzione valore

let v1 = valore Rosso ;;
// val v1 : int = 1


// altro esempio di  enumeration type

type month = January | February | March | April | May | June | July
             | August | September| October | November | December ;;

// Definire la funzione daysOfMonth che calcola il numero di giorni in un mese (anno non bisestile)

let daysOfMonth m=
  match m with
  |February->28
  |April|June|September|November->30
  |_->31


// guarda chi si vede ...

type mbool = True | False ;;


(*

Definire le funzioni

*  myNot : mbool -> mbool  // operatore Not su mbool
  
*  mbool2bool : mbool -> bool   // trasforma un bool nel corrispondente valore mbool

*)   

let myNot b=
  match b with
  |True->False
  |False->True;;



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

Assumiamo che le misure delle dimensione siano intere
(in realta' sarebbe meglio usare i float, usiamo int per vedere l'uso dei cast).


*)

// Definizione del datatype figura

type figura = 
   | Rettangolo of  int * int      // base * altezza
   | Quadrato   of  int            // lato
   | Triangolo  of  int * int  ;;  // base * altezza

(*

I costruttori usati nella definizione sono funzioni. In particolare:

 Rettangolo :  int * int -> figura 
 Quadrato   :  int -> figura 
 Triangolo  :  int * int -> figura 

Applicando uno dei costruttore sopra a un argomento si ottiene un 'tagged value' di tipo figura-

NOTA

Un enumeration type (es., i tipi colore e month definiti sopra)
e' un  datatype in cui tutti i costruttori hanno arieta' zero (non hanno argomenti).

*)   


// Esempi di tagged value di tipo figura

let rett = Rettangolo (4, 5) ;;
//  rett : figura = Rettangolo (4,5) 

let quad1 = Quadrato 10  ;; 
// val quad1 : figura = Quadrato 10

let quad2 = Quadrato 5  ;;

let tr = Triangolo (5,3) ;;


// Esempio di generazione di figure con FsCheck 

#r "FsCheck"  ;;
open FsCheck  ;;

let figGenr = Arb.generate<figura>  ;;  // definizione di un  generatore di figure

Gen.sample 20 6 figGenr ;;  // genera 6 figure usando figGenr



(**  PATTERN MATCHING  con tagged value  **)



(*

Definiamo la funzione area che calcola l'area di una figura fig.
Si assume, per ora, che fig sia una figura ben definita (le dimensioni non sono negative).

  

NOTA
====

L'area di un rettangolo e  di un quadrato e' intera, mentre l'area del triangolo
puo' non essere intera e va quindi rappresentata usando il tipo float.

La funzione area deve avere tipo

  area : figura -> float 

*)

let area f=
  match f with
  |Rettangolo(b,a)->float (b*a)
  |Quadrato l-> float (l*l)
  |Triangolo (a,b)->float (b*a)/2.0;;

// Generazione di n figure con calcolo delle aree e stampa del risultato

let gen n =
  let figs = Gen.sample 20 n figGenr // lista con n figure
  printf "list of figures: %A, their area %A \n" figs (List.map area figs) ;;

(*

Supponiamo figs = [ fig0 ; fig1 ;  ... ; fig(n-1) ]   // n figure

L'espressione

     List.map area figs

applica la funzione area ad ogni figura in figs e costruisce la lista dei corrispondenti valori:

   [ area fig0 ; area fig1 ; ... ;  area fig(n-1) ]

La funzione printf e' analoga alle omonime funzioni C, Java.

La specifica di formato %A permette di stampare valori di qualunque tipo.
Il primo   %A  e' associato a figs (lista di figure),
il secondo %A  e' associato alla lista delle aree (tipo float list)

*)   

gen 4 ;;
// list of figures: [Triangolo (0,0); Quadrato -10; Quadrato 5; Triangolo (1,-1)], their area [0.0; 100.0; 25.0; -0.5]

// Notare che sono stata generate figure con dimensioni negative!


// *** Esempio di discriminated union con un solo costruttore (simile a una tupla)

type figuraColorata =  Col of figura * colore ;;

let f1 = Col (quad1, Rosso) ;;
// f1 e' un valore (tagged value)  di tipo figuraColorata


(*


Definire la funzione  valFigura che calcola il valore di una figura colorata figCol
determinato dalla seguente formula:

   valFigura (figCol)  =  area(figCol) *  valore(colore(figCol))  

dove il valore di un colore e' definito come sopra (funzione valore : colore -> int).
Si assume  che figCol sia ben definita (dimensioni non negative).  *)

let valFigura figCol=
  match figCol with
  |Col(fig,col)->area fig * float (valore col)





(*

OPTION TYPE
^^^^^^^^^^^

In F#  e' definito il tipo polimorfo 

type 'a option =   
    | None
    | Some of 'a

Molto utile per esprimere funzioni PARZIALI (alternativa a eccezioni).


*)

(* Esempio 1
   ^^^^^^^^^^ 

La funzione fattoriale definita le scorse lezioni ha tipo

  int -> int

Il fattoriale di n e' in realta' una funzione parziale su int,
in quanto e' definito solo per n >= 0.
Come gestire il caso in cui la funzione e' applicata a un intero n negativo?

Sono possibili due soluzioni:

(i)  Viene sollevata una eccezione.

(ii) Si definisce la funzione in modo che restituisca un valore di tipo 'int option'.

Piu' precisamente, definiamo la funzione

  factOpt : int -> int option

Il risultato R dell'applicazione

   factOpt n

e' un tagged value di tipo 'int option' della forma 'Some k' oppure 'None'.

- Se R = Some k, allora  k e' il fattoriale di n
  (in questo caso si ha n >= 0). 
  
- Se R = None, il fattoriale non e' definito
  (si e' applicata la funzione a  n < 0).


*)

// (i):  with exceptions

let rec gfact n =
    match n with
    | 0 -> 1
    | n when  n > 0 -> ome n * gfact (n-1)
    | _ ->  failwith "Negative argument to fact" ;;
// notare l'uso di when nel secondo caso (condizione su un pattern)


// (ii): with options

let rec factOpt n=
  match n with
  |0->Some 1
  |n when n>0->
    let (Some k) = factOpt(n-1)
    Some (n* k)
  |_->None;;




(*

Esempio 2
^^^^^^^^^^ 
Definire la funzione

      printfact : int -> string

che calcola il fattoriale di un intero n e restituisce una stringa che descrive il risultato;
se il fattoriale non e' definito, va restituito un opportuno messaggio.
Per calcolare il fattoriale, usare factOpt.


*)

let printfact n =
  match (factOpt n) with
  |Some k->string k + " e il fattoriale di " + string n
  |None ->"undefined";;

(* Esempio 3
   ^^^^^^^^^

Definire la funzione areaOpt che calcola l'area di una figura fig, se definita.
La funzione restituisce:

-   None       se fig non e' ben definita (una delle dimensioni e' negativa); 
-   Some a     se fig e' ben definita e a e' l'area di fig.

Notare che   'Some a'   e' un valore di tipo 'float option', quindi 
None e' visto come costante di tipo  'float option'  e

  areaOpt : figura -> float option

Definire la funzione

    printArea : figura -> string

che  calcola l'area di una figura  e restituisce una stringa col risultato;
se l'area non e' definita, va restituita un opportuno messaggio


*)

let areaOpt fig=
  match fig with
  |Rettangolo(a,b)|Triangolo(a,b) when (b>=0) && (a>=0)->Some(area fig)
  |Quadrato (l) when (l>=0)->Some (area fig)
  |_->None;;

// Generazione di n figure con calcolo delle aree usando areaOpt

let genOpt n =
  let figs = Gen.sample 20 n figGenr // lista con n figure
  printf "list of figures: %A, their area %A \n" figs (List.map areaOpt figs) ;;

genOpt 4 ;;
