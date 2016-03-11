module E

(*

Tuple vs. record
^^^^^^^^^^^^^^^^^

Quando occorre aggregare piu' dati in una struttura, non sempre l'uso di una tupla e' la soluzione piu' conveniente.

Infatti:

- non e' possibile stabilire il tipo delle componenti di una tupla

- non sempre e' possibile distinguere il significato di una componente in base al tipo

Ad esempio, supponiamo di voler rappresentare un carrello della spesa contenente elementi della forma

       (  nome_prodotto  , peso , prezzo_unitario_prodotto )

Poiche' sia il peso che il prezzo unitario hanno tipo double, la tupla ha tipo

        string * double * double

e la seconda e terza componente sono indistinguibili.  

Una soluzione migliore e' usare i record.


*)

// Definizione di un tipo record che permette di rappresentare un elemento del carrello

type Item = { name : string ; weight : double ; uprice : double  } ;;

let it1 = { name = "mele" ; weight = 2.5 ; uprice = 2.5 } ;; 


// Uso del pattern matching per destrutturare un record

let   { name = n1 ; weight = w1 ; uprice = p1 } = it1 ;;


(*

Esercizio
^^^^^^^^^^

1) Definire la funzione

  cost : Item -> double

che dato un item ne calcola il costo complessivo

let cost it = 
  match it with
  |{name = n1 ; weight = w1; uprice = u1} -> w1*u1;;
  //|_->0.0;;

2) Definire la funzione

  tot : Item list -> double

che calcola la spesa totale degli item presi.
Usare le funzioni higher-order List.map e List.sum.

let rec tot t1=
  match t1 with
  |[]-> 0.0 : double
  |x0 :: xs -> cost x0 + tot xs;;

let rec tot ls = List.map cost ls |> List.sum;;

*)   

// ITERATION

(*

Consideriamo la funzione fattoriale.


*)  

let rec fact n=
  match n with 
    | 0 -> 1
    | n -> n * fact (n-1) ;;


// val fact : int -> int

(*

Esempio di valutazione

  fact 5 =  4 * ( fact 3 ) 
         =  4 * ( 3 *  ( fact 2 ) )
         =  4 * ( 3 *  ( 2 * ( fact 1 ) )) 
         =  4 * ( 3 *  ( 2 * ( 1 * (fact 0) ) )))
         =  4 * ( 3 *  ( 2 * ( 1 * 1 ) )))

Problema
^^^^^^^^

La moltiplicazione puo' essere fatta solo alla fine, quando si e' calcolato il fattoriale di 0.
Nello stack di sistema devono essere conservati i valori 4, 3, 2, 1 da moltiplicare.

In altri termini, lo stack di sistema deve conservare gli ambienti usati in tutte le chiamate ricorsive.


Si puo' gestire meglio l'uso della memoria?


L'idea e' di usare un accumulatore m in cui si calcola  gradualmente il risultato finale


Usiamo la funzione ausiliaria

    factA : int * int -> int

tale che

   fact(0,m)   =  m
   factA(n,m)  = factA(n-1 , n * m)  se n > 0
   
La funzione factA soddisfa la seguente proprieta':

(#)   factA(n,m) =  n!  * m

(si dimostra per induzione su n)

Quindi

  factA(n,1) = n! * 1 = n!


Esempio 
^^^^^^^

Per calcolare il fattoriale di 5 devo calcolare factA(5,1)


factA(5,1)  = factA(4,5*1) = factA(4,5)        
           
factA(4,5) = factA(3,4*5) = factA(3,20)       

factA(3,20) = // m = 20 = 4 * 5 
   factA(2,3*20) = factA(2,60)      


factA(2,60) =  // m = 60 = 3 * 4 * 5 
  factA(1,2*60) = factA(1,120)     


factA(1,120) = // m = 120 =  2 *  3 * 4 * 5 
  factA(0,1*120) = factA(0,120)   

factA(0,120) =    // m = 120 = 1 * 2 *  3 * 4 * 5 
   120                            

Quindi 5! = 120

Notare che a ogni chiamata il valore m del secondo parametro di factA ha la forma

   k * (k+1) * (k+2) * ... * n!

che corrisponde a una parte del calcolo di n!

A differenza di prima, il valore di n! non richiede la memorizzazione di tutti i fattori usati nel calcolo.

Corrisponde al ciclo

 k = n
 m = 1  // accumulatore
 while(k > 0){
    m = k * m
    n = n -1 
 }  

Al termine del ciclo vale

 k = 0  ,   m = n!




*)
   

let  ifact n = 
 let rec factA = function
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m)
 factA(n,1) ;;

(*

Funzioni ricorsive del tipo di factA sono dette  funzioni iterative.
Notare che la ricorsione e' effettuata in coda (tail recursion),
come ultima operazione.

Le funzioni iterative consentono un uso ottimizzato delle risorse
(non occorre memorizzare i valori temporanei calcolati nei passi ricorsivi).

NOTA
^^^^

La funzione fact *non* e' tail recursive in quanto nel caso n > 0 l'operazione da valutare e'

  n * fact (n - 1)

(chiamata ricorsiva per un valore).


*)   



