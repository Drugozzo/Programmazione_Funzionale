module E


(****    SEQUENZE      ****)

(*

Una  sequenza e' una collezione  eventualmente infinita di elementi dello stesso tipo.

La notazione
            
    seq [ e0 ; e1 ; e2;  ... ]

rappresenta una sequenza i cui elementi sono e0, e1, e2, ...

Il tipo di una sequenza e' seq<'a>, dove 'a e' il tipo degli elementi.

Gli elementi di una sequenza vengono calcolati solamente quando richiesto (on demand),
e questo permette di lavorare su sequenze infinite.

Questa modalita' di valutazione, in cui la computazione effettiva degli elementi e' ritardata,
e' detta *lazy evaluation*.

Per i dettagli vedere il Cap. 11 del libro, in particolare le sezioni 11.1, 11.2, 11.6. 


Sequence expressions
^^^^^^^^^^^^^^^^^^^^

Le sequence expression (caso particolare di computation expression) sono utilizzate
per definire il processo che genera gli elementi di una sequenza.

L'espressione

  seq{ 
     seq_expr0   // sequence expression
     seq_expr1
     ...
     seq_exprn
     }

definisce la sequenza ottenuta eseguendo in successione
le sequence expression seq_expr0, seq_expr1, ...  ,seq_exprn.

Si ribadisce che gli elementi vengono effettivamente generati quando richiesto.

Esempi di sequence expression  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(Vedi Tabella 11.2 del libro)

o   yield  e    

    Aggiunge alla sequenza l'elemento e.     
      

o   yield! sq 

    Aggiunge alla sequenza tutti gli elementi della sequenza sq  (concatenazione).


o   let x = expr                  
    seq_expr

    Analoga a let definition di F#.


o   if bool_expr then seq_expr          
 
    Se bool_expr e' vera, allora viene valutata seq_expr (filter).


o   if bool_expr then seq_expr1  else  seq_expr2 

    Se bool_expr e' vera, allora viene valutata seq_expr1,
    altrimenti viene valutata seq_expr2 (conditional)


Nota
^^^^

Non confondere le  sequence expression con le espressioni F#.
Ad esempio, nelle sequence expression e' possibile usare il costrutto
'if-then' (senza else), che non ha senso nelle espressioni F#.




*)   

// Esempi


let seq1 = seq { yield 0  // yield genera un elemento
                 yield 1
                 yield 2
                 yield 3
             } ;;

// seq1 : seq<int> definisce la sequenza seq [ 0; 1; 2; 3 ]

let seq2 = seq{
        yield 100
        yield! seq1  // yield! aggiunge tutti gli elementi di seq1
        yield 200
        yield! seq1
    }

(* seq2 = seq[ 100 ;   0 ;  1  ;  2  ; 3  ; 200 ;   0  ; 1  ; 2  ;  3  ] 
                e0    e1   e2    e3   e4     e5    e6   e7   e8    e9
                      ^^^^^^^^^^^^^^^^^^           ^^^^^^^^^^^^^^^^^^
                           seq1                         seq1      

*)

(*

  La funzione

     Seq.nth : int -> seq<'a> -> 'a

  estrae da una sequenza l'elemento con indice specificato (gli indici partono da 0)
 
*)

Seq.nth 0 seq1 ;;  // 0   (e0)
Seq.nth 2 seq1 ;;  // 2   (e2)
Seq.nth 4 seq2 ;;  // 3   (e4)
Seq.nth 8 seq2 ;;  // 2   (e8)


(*

  La funzione

     Seq.take : int -> seq<'a> -> seq<'a>

  estrae la sottosequenza formata dai primi n elementi di una sequenza
  (n>=0 e' il primo argomento)
  
*)   

let seq3 = Seq.take 2 seq1 ;;
// seq3 = seq [ 0 ; 1]


(*
  La funzione 
   
    Seq.skip : int -> seq<'a> -> seq<'a>

  estrae la sottosequenza ottenuta saltando i primi n elementi di una sequenza
  (n>=0 e' il primo argomento)
  
*)   

let seq4 = Seq.skip 2 seq2 ;; 
// seq4 = seq [1; 2; 3; 200; 0; 1; 2; 3]

 Seq.nth 0 seq4 ;; // 1
 Seq.nth 1 seq4 ;; // 2


(*

Esercizio
^^^^^^^^^

Definire le funzioni

     head : seq<'a> -> 'a
     tail : seq<'a> -> seq<'a>
     
che estraggono rispettivamente la testa e  la coda di una sequenza non vuota.

*)   

let head sq = Seq.nth 0 sq ;;
let tail sq = Seq.skip 1 sq ;;  


(*

La funzione

  Seq.toList : seq<'a> -> 'a list ;;
 
trasforma una sequenza finita in una lista. 

*)

let l1 = Seq.toList seq1 ;;
// l1 : int list = [0; 1; 2; 3]

let l2 = Seq.toList seq2 ;;
// l2 : int list = [100; 0; 1; 2; 3; 200; 0; 1; 2; 3]



/////////////////////////////////////////////////////////////////////


(***  LAZY EVALUATION  VERSUS STRICT (O EAGER)  EVALUATION  ***)

(*

La valutazione *lazy (pigra)*  ritarda la computazione di una espressione
fino a quando il risultato deve essere utilizzato.

Le sequence expression sono valutate in modo lazy.
Durante l'esecuzione e' costruita solamenta la porzione finita della sequenza effettivamente usata.
Questo permette di lavorare su sequenze infinite.

*)   

// esempio di uso della lazy evaluation nella definizione di una sequenza

let seqlz = seq { yield 0 
                  yield 1
                  yield 2/0  // la valutazione di 2/0 solleva una eccezione 
                  yield 3
            } ;;

// La definizione di seqlz non produce errori (la definizione non genera alcun elemento).

Seq.nth 0  seqlz ;; // 0
// nella valutazione, viene generato solamente il primo elemento di seqlz

Seq.nth 1  seqlz ;; // 1
// nella valutazione, vengono  generati solamente i primi due elementi di seqlz

(*

Verificare cosa succede calcolando

Seq.nth 2  seqlz ;; 
Seq.nth 3  seqlz ;;

In entrambi i casi, per la valutazione occorre generare l'elemento 2/0,
e questo solleva una eccezione.


*)   


(*

Strict (eager) evaluation
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le espressioni F# viste finora sono valutate in modalita' *strict (eager, golosa)*,
che e' la modalita' usata nei linguaggi imperativi.

Quindi quando un operatore Op e' applicato a delle espressioni

   expr1  op expr2

le espressioni expr1 e expr2 sono valutate prima di applicare op   

Analogamente, data una chiamata di funzione

   f t1 t2 .... tn

gli argomenti t1 , t2 ... tn sono valutati prima di applicare f.


Esempi
^^^^^^^

Consideriamo la funzione

  first : 'a -> 'b -> 'a

che, applicata a due argomenti,  restituisce il primo:

  let first x y = x ;;  // il valore di y non e' usato in first

La chiamata 

  first 1 2/0 ;;

solleva una eccezione, in quanto l'argomento 2/0 viene valutato anche se
non e' usato nel calcolo di first.

/////////////////////////////////////////////////////////////////////////////

Anche le liste sono costruite in modo strict (e non lazy).

Infatti, la lista

  [1;2] 

corrisponde al termine

 1 :: ( 2 :: [] );

L'operatore :: (cons) e' valutato in modo  strict,
quindi tutti gli elementi nella lista sono subito valutati
(anche nel caso in cui i valori non vengano usati).

La definizione

 let listErr = [ 0; 1; 2/0; 3]

solleva una eccezione, dovuta alla presenza del termine 2/0.

/////////////////////////////////////////////////////////////////////////

Operatori con valutazione lazy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Esempi di operatori  valutati in modo  lazy, anche nei linguaggi imperativi, 
sono li operatori booleani && (and) e || (or).

Per valutare l'espressione booleana
  
   B1 && B2 

- viene prima valutata l'espressione booelana B1;
- se B1 e' false, il risultato della valutazione e' false (e B2 non viene valutata);
- se B2 e' true, il risultato e' dato dalla valutazione di B2.

La valutazione di B1 || B2 e' analoga.

Quindi

 (10 < 0 ) &&  ( 2/0 > 0 ) ;;

e' false  (se && venisse valutato in modo strict, la valutazione
di  ( 2/0 > 0  solleverebbe una eccezione).

Analogamente, il risultato della valutazione di

  (10 > 0 ) || ( 2/0 > 0 ) ;;

e' true  (l'espressione  2/0 > 0 non e' valutata).


Il costruttore seq
^^^^^^^^^^^^^^^^^^

E' possibile definire una sequenza applicando il costruttore seq a una lista.

Ad esempio

 seq [ 0 .. 10 ] ;;

definisce la sequenza di tipo seq<int>  contenente gli interi 0, 1, ... , 10.

 
 seq [ "asino" ; "bue" ; "cane" ] ;;

definisce la sequenza di tipo  seq<string> contenente le tre stringhe specificate.

 L'uso del costruttore  seq e' utile per definire sequenze finite.

Notare che la definizione
 
 let sq = seq [ 0; 1; 2/0; 3] ;;

solleva una eccezione in quanto, come visto sopra,
la lista e' valutata in modo strict (e non lazy).


*)   


/////////////////////////////////////////////////////

(****  SEQUENZE INFINITE  ****)

(*

Per definire una sequenza infinita si puo' usare la funzione

   Seq.initInfinite : (int -> 'a) -> seq<'a>

Data una funzione  f: int -> 'a

   Seq.initInfinite f  

costruisce la sequenza infinita

   seq[ f(0) ; f(1) : f(2) ; ... ]

In una sequenza infinita sqInf, per ogni n >= 0 l'elemento

  Seq.nth n sqInf

e' sempre definito. 



Esercizio
^^^^^^^^^

Usando  Seq.initInfinite definire le seguenti sequenze infinite:

- nat : sequenza dei numeri naturali 0, 1, 2, ...
- nat1: sequenza dei numeri naturali senza il numero 5
- nat2: sequenza dei numeri naturali in cui il numero 5 e' sostituito da -5
- even10 : sequenza dei numeri pari n >= 10  
- sqTrue : sequenza costante true, true, true, ....
- sqTrueFalse: sequenza true, false, true, false, true, false, ...

Per ciascuna sequenza generare la lista dei primi 10 elementi.

*)   




let nat =   Seq.initInfinite (fun x -> x) ;;
let nat1 =  Seq.initInfinite (fun x -> if x < 5 then x else x + 1) ;;
let nat2 =  Seq.initInfinite (fun x -> if x <> 5 then x else -5 ) ;;
let even10 =  Seq.initInfinite (fun x -> 10 + 2 *x) ;; 
let sqTrue =   Seq.initInfinite (fun x -> true) ;;
let sqTrueFalse =  Seq.initInfinite (fun x ->  x % 2  = 0)  ;;

let lnat = nat |> Seq.take 10 |> Seq.toList ;;
// [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

let lnat1 = nat1 |> Seq.take 10 |> Seq.toList ;;
// [0; 1; 2; 3; 4; 6; 7; 8; 9; 10]

let lnat2 = nat2 |> Seq.take 10 |> Seq.toList ;;
// [0; 1; 2; 3; 4; -5 ; 6; 7; 8; 9]

let leven10 = even10  |> Seq.take 10 |> Seq.toList ;;
// [10; 12; 14; 16; 18; 20; 22; 24; 26; 28]

let  lsqTrue = sqTrue  |> Seq.take 10 |> Seq.toList ;;
// [true; true; true; true; true; true; true; true; true; true]

let  lsqTrueFalse = sqTrueFalse |> Seq.take 10 |> Seq.toList ;;
// [true; false; true; false; true; false; true; false; true; false]


(*

Uso della ricorsione
^^^^^^^^^^^^^^^^^^^^

E' possibile definire una sequenza infinita mediante ricorsione,
sfruttando il fatto che le sequence expression sono valutate in modo lazy.

Esercizio
^^^^^^^^^

i) Definire la funzione ricorsiva

    intFrom : int -> seq<int>

che, dato un intero n,  genera la sequenza infinita degli interi k >= n.    

Notare che

  nat = intFrom 0 

SOLUZIONE

let rec intForm n = 
  seq{  yield n
        yield! intForm (n+1)
  } ;;

ii) Usando intFrom generare la sequenza infinita int10  degli elementi k >= -10.

Da int10, usando le funzioni sulle sequenze, estrarre la lista
 
  [-4; -3; -2; -1; 0; 1; 2; 3; 4]

*)

SOLUZIONE
let int10 = intForm -10 |> Seq.skip 6 |> Seq.take 9 |> Seq.toList;;

// genera sequenza n, n+1, n+2, ...
let rec intFrom n = seq { yield n  // primo elemeno della sequenza 
                          yield! intFrom (n + 1)  // elementi successivi
                                // seq[ n+1; n+2; ... ] 
                         } ;;


(*

Nota
^^^^

Verificare cosa succede definendo

  let rec intFromList n  =  n :: ( intFromList (n + 1) ) ;;

  let nat =  intFromList 0  ;;

Ricordarsi che :: (cons) e' valutato un modo strict,

Questo porta alla seguente valutazione non terminante:

 intFromList 0  =    0 :: ( intFromList (0 + 1) )
                =    0 :: ( intFromList 1 )  // devo valutare  intFromList 1 
                =    0 :: ( 1 :: intFromList (1 + 1) )
                =    0 :: ( 1 :: intFromList 2 ) // devo valutare  intFromList 2 
                =    0 :: ( 1 :: (2 :: intFromList (2 + 1)) )
                =    0 :: ( 1 :: (2 :: intFromList 3) )  // devo valutare  intFromList 3
                .....      


*)   



let int10 = intFrom -10 ;;

// [-4; -3; -2; -1; 0; 1; 2; 3; 4] 
//  lista dei primi 9 elementi della sottosequenza ottenuta da int10 saltando i primi 6 elementi

int10 |> Seq.skip 6 |> Seq.take 9 |> Seq.toList ;;




(*

Esercizio
^^^^^^^^^^

Ridefinire le sequenze infinite nat1, nat2, even10, sqTrue, sqTrueFalse
senza usare Seq.initInfinite e usando la ricorsione.

Per nat1, nat2, even10 vanno  definite delle opportune funzioni generatrici
(analoghe a intFrom).

SOLUZIONE

let rec intForm1 n =
  seq{
        if n <> 5 then yield n
        yield! intForm1(n+1)
  };;

let rec intForm2 n =
  seq{
        if n = 5 then yield -5 else yield n
        yield! intForm2 (n+1)
  };;

*)

// genera sequenza infinita n, n+1, n+2, ... senza il numero 5
let rec intFrom1 n =
    seq{ if n <> 5 then yield n 
         yield!  intFrom1 (n + 1) 
         } ;;
// notare l'uso di if-then come filtro (esclude 5 dalla sequenza)


let rnat1 = intFrom1 0 ;;

// genera sequenza infinita n, n+1, n+2, ... in cui 5 e' sostituito da -5
let rec intFrom2 n =
    seq{ if n <> 5 then yield n else yield -5
         yield!  intFrom2 (n + 1) 
         } ;;


let rnat2 = intFrom2 0 ;;


// genera sequenza infinita n, n+2, n+4, ....
let rec intFrom3 n =
    seq{ yield n 
         yield!  intFrom3 (n + 2) 
         } ;;

let  reven10 = intFrom3 10 ;;

// genera sequenza infinita true, true, true, true, ...
let rec rsqTrue =
    seq { yield true
          yield! rsqTrue } ;;

// genera sequenza infinita true, false, true, false, ...
let rec rsqTrueFalse = seq { yield true
                             yield false
                             yield! rsqTrueFalse } ;                    


(*  QUICKCHECK  *)



(*

Uso della lazy evaluation nella definizione di proprieta' per QuickCheck
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le property della forma

    Cond1 ==> Cond2     // se vale Cond1 allora valuta Cond2

sono valutate in modalita' strict


*)
#r "FsCheck";;
open FsCheck;;

(*

Esempio 1
^^^^^^^^^

Vogliamo verificare con  QuickCheck la seguente proprieta' invariante inv
dell'inverso di un intero n:

-  se n e' diverso da 0,  allora   n * (1/n) = 1

*)   

let inv n =
  n <> 0 ==>  ( n * 1/n = 1 ) ;;  // non dimenticare le parentesi
//val inv :int -> Property


(*

L'esecuzione 

  Check.Quick inv ;;

solleva una eccezione

 ...  exception_ System.DivideByZeroException: Division by zero ...

Infatti, poiche'  ==> e' valutato in modalita' strict, se n vale zero
viene subito calcolato il valore di

   0 * 1/0 = 0

prima di applicare ==>.

Per risolvere il problema, occorra forzare la lazy evaluation, in modo che la condizione
( n * 1/n = 1 ) venga valutato solo se necessario (ossia, se n e' diverso da 0)


*)


let invL n =
  n <> 0 ==>  lazy ( n * 1/n = 1 ) ;;
//val invL :int -> Property

Check.Quick invL ;;

(*

Ok, passed 100 tests.
val it : unit = ()

*)



(*

Esempio 2
^^^^^^^^^

Verificare con  QuickCheck la seguente proprieta' invariante invMin  sulle liste:

- se la lista ls non e' vuota, allora il minimo elemento di ls coincide con la testa  di List.sort
   

Ricordarsi che la proprieta' va definita su una lista con tipo non-polimorfo (int list, string list, ...)

*)   

let invMin (ls : int list) =
  ls <> []  ==>  lazy (  List.min ls  =     (List.sort ls |>  List.head) )  ;;
// val invMin : int list -> Property

Check.Quick invMin ;;

// anche in questo caso, se non si forza la valutazione lazy si ha errore in esecuzione (lista vuota)




