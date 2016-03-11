(**  ESPRESSIONI FUNZIONALI **)

(*

Tipi funzionali
^^^^^^^^^^^^^^^

Un tipo funzionale e' un tipo della forma

   T1 -> T2

dove T1 e T2 sono tipi.
E' il tipo di una funzione da T1 (dominio)  a T2 (codominio).

L'operatore ->:

-  Ha priorita' piu' bassa di *:

   int * string ->  float     equivale a    (int * string) ->  float

   int -> int * int           equivale a     int -> (int * int)


-  E' associativo a destra:

    int -> int -> int     equivale a     int -> ( int -> int )



Notare che il tipo  int -> int -> int  e' diverso da

    (int -> int) -> int

Una funzione di tipo (int -> int) -> int ha come argomento 
una funzione di tipo int->int  e risultato un valore di tipo int.
Funzioni che hanno come argomento altre funzioni sono dette higher-order (ordine superiore).


Espressioni   funzionali
^^^^^^^^^^^^^^^^^^^^^^^^

Una espressione funzionale  e' una espressione avente tipo funzionale.


Una esempio di espressione funzionale e' 
  
    fun x -> expr    

A differenza dei linguaggi imperativi, e' possibile definire 
espressioni funzionali senza assegnare loro un nome;
in tal caso si parla di  'funzioni anonime'.

Esempio, l'espressione funzionale 

  fun x -> x + 1 ;;

definisce una funzione anonima di tipo int -> int
che calcola il successore di x.


Applicazione di una funzione
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Data una espressione funzionale fExpr e un termine t aventi tipo

 fExpr : T1 -> T2      t : T1

l'espressione

   fExpr t

denota l'applicazione di fExpr al termine t.
Il risultato dell'applicazione e' un valore di tipo T2.

Notare che:

-  Le funzioni hanno un solo argomento.

-  A differenza dei linguaggi imperativi, non occorre scrivere l'argomento tra parentesi tonde.

-  Se si e' in presenza di tipi polimorfi (tipi che contengono variabili),
   l'applicazione e' possibile se le variabili possono essere istanziate
   in modo che il tipo di t e il tipo del dominio di f siano uguali (tipo T1).

   Ad esempio se 

       f :  'a * 'b ->  'a list         t  :  int * string

      
    l'applicazione di f a t e' possibile se 

              'a * 'b  =  int * string
    
    L'uguaglianze  vale per
     
         'a = int          'b = string.

    Quindi 

       T1 = int * string    T2 = int list

Valutazione dell'applicazione di una funzione in un ambiente
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Un ambiente Env e' una mappa cha associa a un identificatore un valore.
Env puo' essere rappresentato come una lista di legami (binding)

       x --> vx     // vx e' il valore associato a x 
  
La valutazione di    

      ( fun x -> expr ) t

in Env e' il valore v calcolato come segue:

1) Nell'ambiente Env si calcola il valore vt dell'argomento t

2) v e' il valore dell'espressione expr (espressione a destra di ->)  nell'ambiente 

      Env + ( x --> vt )

   ossia nell'ambiente ottenuto aggiungendo ai legami in Env il nuovo legame x --> vt 

   In altri termini, calcolo il valore di expr dove x e' sostituito con vt.

    
NOTE
====

1) L'ambiente costruito al punto 2  e' un ambiente 'provvisorio' usato solamente
per la valutazione dell'espressione funzionale.

Se in Env e' gia' definita l'associazione 

   x --> vx  

nell'ambiente al punto  2 il valore di x e' vt, non vx;
infatti,  il legame  x --> vt   oscura quello definito in Env.

Quando la valutazione dell'espressione funzionale e' stata effettuata,
il valore di  x e' ancora vx
(il nuovo legame ha senso solo durante la valutazione dell'espressione funzionale).

2) Notare che prima di applicare la funzione viene valutato l'argomento.

Ad esempio, la valutazione

   ( fun x -> 10 ) 4/0 ;;
   
non da' risultato 10, ma solleva una eccezione,
dovuta alla valutazione dell'argomento 4/0.

Questo tipo di valutazione e' chiamata 'eager evaluation'.

Al contrario, con la valutazione 'lazy' il valore di una espressione
richiede in genere solo una computazione parziale delle sue  sottoespressioni.

Esempi di operatori che vengono valutati in modalita' lazy
sono gli operatori booleani && (and) e || (or).

  ( 2 = 3 )  &&  (4/0 > 0) ;;  // false
// poiche' 2 = 3 e' false, il valore dell'espressione e' false
// ( non occorre valutare la sottoespressione   (4/0 > 0)

  
  ( 2 < 3 )  ||  (4/0 > 0) ;;  // true

Le valutazioni di 

   (4/0 > 0) &&  ( 2 = 3 )           (4/0 > 0)  || ( 2 < 3 )     

sollevano eccezioni.


*)

 

(*

Esempio di valutazione di espressioni funzionali in un ambiente
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Definiamo l'ambiente Env

  [  x --> 1  ;  y -->  2   ;  z --> 3   ]

*)
 

let x = 1 ;;
let y = 2 ;;
let z = 3 ;;

// valutazione in Env

(fun w -> w + 10) z ;; // 13 
(*
Il valore di z in Env e' 3
Occorre valutare  w + 10 nell'ambiente  Env + ( w --> 3)

    w + 10 = 3 + 10 = 13 
*)   


(fun w -> w + 10) (x + 2 * y ) ;; // 15 
(*
Il valore di (x + 2 * y) in Env e'  1 + 2 * 2 = 5
Devo valutare  w + 10 nell'ambiente  Env + ( w --> 5)

     w + 10 = 5 + 10 = 15
     
*)   


(fun  x -> x + y) (y + 10) ;; // 14
(*
Il valore di y + 10  in Env e' 2 + 10 = 12
Occorre valutare  x + y  nell'ambiente 

   Env + ( x --> 12) = [ x --> 12 ; y --> 2 ; z --> 3]

    x + y = 12 + 2 = 14


*)  

x ;; // 1
//notare che il legame x --> 1 di Env non e' cambiato

(fun  x -> 2 * x + z ) ( 2 * x + z) ;; //  13

(*

Il valore dell'argomento  (2 * x + z)  in Env e' 2 * 1 + 3 = 5
Occorre valutare  2* x + z (espressione a destra di -> ) nell'ambiente

   Env + ( x --> 5) = [ x --> 5 ; y --> 2 ; z --> 3]

   2 * x + z = 2 * 5 + 3 = 13

*) 

x ;; // 1



(*

Sintassi alternativa
^^^^^^^^^^^^^^^^^^^^

Una espressione funzionale e' un valore che puo' essere legata a un identificatore (let-binding)

  let f = fun x -> expr  // (1)

Dopo questa definizione,  'f'  e' legata all'espressione  'fun x -> expr'

Sintassi  equivalente:

 let f x = expr  // (2)

La definizione (2), in cui la x e' portata a sinistra di =,
e'  equivalente a (1) ed e' la notazione generalmente usata.

Questo procedimento  e' chiamato  'lambda lifting'
ed e' parte della fase di 'closure conversion' nella compilazione 
 
*)   

// Esempi

fun x -> x + 1 ;;  // funzione anonima 
//val it : int -> int 

// let binding

let succ =  fun x -> x + 1 ;;
// val succ : int -> int
// nell'ambiente corrente, l'identificatore succ e' legato alla funzione successore

succ 4;;
// val it : int = 5

// Altra notazione per definire succ

let succ1 x =   x + 1 ;;


// funzione identita'

let id = fun x -> x ;;
// val id : 'a -> 'a

// oppure:

let id1 x = x ;;
// val id2 : 'a -> 'a

id "id" ;; 
// val it : string = "id"

// esempio di funzione costante

let zero = fun x -> 0 ;;
//val zero : 'a -> int

let zero1 x = 0 ;;

zero ["uno" ; "due" ] ;;  // 0
(*
La valutazione di 0 (espressione a destra di ->) e' 0 in qualunque ambiente
*)   

zero zero ;;  // 0
zero ( fun x -> 2 * x ) ;;  // 0


(**


 In una espressione funzionale

    fun x -> expr

l'espressione expr puo' a sua volta essere una espressione funzionale. 


Esempio:

    fun x -> ( fun y ->  x + y ) ;;


Chiamiamo f la funzione definita sopra:

   let f = fun x -> ( fun y ->  x + y ) ;;

Che tipo ha f ?

Anzitutto, l'operatore '+' viene interpretato come la somma intera,
si assume quindi che x e y abbiano tipo int.

Segue che:

    fun y -> x + y                     ha tipo    int -> int

    fun x -> ( fun y ->  x + y )       ha tipo    int -> ( int -> int ) 

Dato che f ha tipo int -> (int -> int) , possiamo applicare f a un qualunque
termine di tipo int e il risultato ha tipo int -> int (e quindi e' una funzione).

   
Esempi di applicazione 
^^^^^^^^^^^^^^^^^^^^^^

Applichiamo f a 5. Il risultato deve essere una funzione di tipo int -> int.

Il valore di f 5 e' dato dal  valore di

  fun y ->  x + y  // espressione a destra di -> nella def. di f

dove  x e' legato a 5.

Quindi risulta:
  
  f 5  =   fun y -> 5 + y   // funzione   int -> int
 
Definiamo

    let g =  f 5 ;;  // g = fun y -> 5 + y : int -> int

Esempio di applicazione di g:

g 4  =  ( fun y -> 5 + y ) 4 
     =  9

E' possibile eseguire le due applicazioni scrivendo un'unica espressione:

     (f 5) 4 ;;    //  val it : int = 9  
   
Convenzioni sulla associativita'
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Abbiamo visto che  l'operatore ->  e' associativo a destra
L'applicazione di funzione e' invece associativa a sinistra.


     fun x ->  fun y  ->  expr       equivale a        fun x -> ( fun y  ->  expr ) 

                  fExpr t1 t2        equivale a       ( fExpr t1 ) t2
    
     fun x ->  fun y ->  x + y       equivale a        fun x -> ( fun y ->  x + y )

                         f 5 4       equivale a        (f 5) 4

Notare che 'f 5 4' assomiglia all'applicazione di una funzione con due argomenti.
In realta' corrisponde a due applicazioni in cascata:
prima si applica f a 5,  poi si applica la funzione ottenuta a 4 
 
  
Sintassi alternativa
^^^^^^^^^^^^^^^^^^^^
 
Quando si hanno piu' 'fun' annidati, si puo' usare una notazione piu' compatta:

     fun x1 ->  ( fun x2 -> .... -> ( fun xn expr) ... )

si puo' riscrivere come

     fun x1 x2 .... xn -> expr   // un solo fun con elenco variabili

Il 'lifting' si estende a espressioni funzionali con piu' variabili

   let fn =  fun x1 x2 .... xn -> expr

si puo' riscrivere come
  
    let fn x1 x2 ... xn = expr

La seguenti definizioni sono equivalenti  

     let f   =   fun x -> ( fun y ->  x + y )  // le parentesi si possono omettere
     let f   =   fun x y -> x + y
 let f x y   =   x + y 

L'ultima definizione assomiglia alla definizione di una funzione con due parametri x e y.

Quando viene applicata f istanziando solo il primo parametro,
si parla di 'applicazione parziale'

let succ = f 1
// applicazione parziale di f (solo la x viene istanziata) 
// succ = fun y -> 1 + y : int -> int

Questo meccanismo per cui si puo' simulare l'applicazione di n argomenti
mediante n chiamate a funzioni aventi un argomento si chiama "currying"
(Schoefinkel, Curry, 1930) 	       	  

Notare che nei linguaggi imperativi non e' possibile applicare parzialmente
una funzione (tutti i parametri della funzione vanno istanziati).


Analogia  con array bidimensionali
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Un meccanismo analogo alla applicazione parziale si ha con la rappresentazione
di matrici nei linguaggi imperativi (ad esempio in C).

Una matrice A di int e' implementata mediante un array di tipo  'array di array di int'.

Per ogni riga i della matrice,  A[i] e' un array di int,
contenente gli elementi della riga i della matrice
( A[i] e' analogo alla  applicazione parziale  f i )

L'elemento alla riga i e colonna j della matrice corrisponde all'elemento j dell'array A[i],
ossia all'elemento

         ( A[i] ) [j]      // analogo a  (f i) j    

che si puo' scrivere anche come

         A[i,j]      // analogo a  f i j
    
  
*)

// Esempi

let f =  fun x -> ( fun y ->  x + y ) ;;
// val f : int -> int -> int

let g = f 5 ;;  // applicazione parziale
// val g : (int -> int) = fun y -> 5 + y

g 4 ;; // 9

(f 5) 4 ;; // 9

f 5 4 ;;  // 9


// notazioni equivalenti per definire f

let f1 =  (fun x  y ->  x + y) ;;
let f2 x y = x + y ;;

f1 10 7 ;; // 17

let g1 = f1 4 ;; // applicazione parziale
// g1 : int -> int = fun y -> 4 + y

g1 5 ;; // 9



// Altri esempi



(*

Date le espressioni cond, e1, e2 tali che

  cond : bool       e1, e2  : 'a     
  
definire la funzione

  if_then_else cond e1 e2 

che calcola il valore dell'espressione

  if cond then e1 else e2

Il tipo della funzione if_then_else e':

 if_then_else : bool -> 'a -> 'a -> 'a


*)   

let if_then_else cond e1 e2 = if cond then e1 else e2;;

fun cond -> fun e1 -> fun e2 -> if cond then e1 else e2;;  

(*

Definire la funzione

    min : 'a -> 'a -> 'a when 'a : comparison

tale che

  min a b

e' il minimo fra a e b.
Usare una chiamata alla funzione if_then_else definita sopra.

*)  

let min a b = if_then_else (a<b) a b;;

(*

Definire la funzione

    modulo : int -> int

che calcola il modulo (valore assoluto) di un numero
usando la funzione if_then_else definita sopra.

*)  

let modulo n = if_then_else (n>0) n (-n);; 


(*

** ESERCIZIO **

Si considerino le seguenti funzioni definite per
applicazione parziale della funzione if_then_else

let p1 =  if_then_else true     :  int -> int -> int ;:
let p2 =  if_then_else false    :  int -> int -> int  ;;
let p3 =  if_then_else true  0  :  int ->int  ;;
let p4 =  if_then_else false 0  :  int ->int  ;;


Cosa calcolano ?


**)   



(***

Ancora sulla notazione delle espressioni funzionali
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finora abbiamo visto espressioni funzionali della forma

    fun x -> expr

E' anche possibile scrivere a sinistra di -> un termine strutturato.

Esempio
^^^^^^^

   fun (x,y) ->  x + y

e' una espressione funzionale di tipo

   int * int -> int

che definisce la funzione che calcola la somma di una coppia di interi.

La definizione

 let add = fun (x,y) ->  x + y  

si puo' riscrivere come (lifting):

 let add (x,y) = x + y 

Notare la differenza fra

   let   add (x,y) =  x + y ;;   // add : int * int -> int
   let     f x y   =  x + y ;;   //   f : int -> int -> int

Per calcolare la somma m+n:

 add   : prende  come unico argomento la coppia (m,n)
          
   f   : il calcolo di m + n si ottiene con la chiamata

         ( f m) n   // applico f a m e poi applico la funzione ottenuta a n


*)




(****    PIPE    ****

L'operatore |> (pipe) permette di scrivere l'argomento di una funzione prima della funzione stessa.

Quindi,  il valore di t |> f  e'  f t

Il tipo di (|>) e':

  (|>) :  'a -> ('a -> 'b) -> 'b

Utilita' delle pipe:

- le funzioni sono scritte nell'ordine in cui vengono applicate;

- si risparmiano parentesi.



*)

// Esempio

[ 1.. 5 ] |> List.rev |>  List.tail |>  List.head ;; //


// equivale a

List.head ( List.tail ( List.rev  [ 1.. 5 ] ) ) ;;




(****   COMPOSIZIONE DI FUNZIONI  *****


L'operatore >> permette di comporre due funzioni.

Date due funzioni 

  f : 'a -> 'b      g : 'b -> 'c

la funzione  f >> g  e' definita su t  applicando prima f e poi g

  t   |--f-->   f t   |-- g-->   g ( f t ) // notare le parentesi!

Quindi:

    f >> g  =  fun x ->  g ( f x )

Il tipo della funzione (>>) e':

   (>>)  : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

***)

// Esempi

let add10 = (+) 10;;

(*

  (+)         e' la funzione   fun x y -> x + y  :  int -> int -> int
  (+) 10      e' la funzione   fun y -> 10 + y   :  int -> int
       
*)

let  mult2 = ( * ) 2;;

(*

  ( * )        e' la funzione   fun x y -> x * y  :  int -> int -> int
  ( * ) 2      e' la funzione   fun y -> 2 * y    :  int -> int
       
*)

let h1 = add10 >> mult2 ;;

(*

 h1 : int -> int

 h1 n = mult2 ( add10 n)
      = mult2 ( 10 + n )
      = 2 * (10 + n)

Quindi:      
          
 h1 = fun x ->  2 * ( 10 + x)

*)  


h1 100 ;; //  220

let h2 = mult2 >> add10 ;;

// che funzione rappresenta h2 ? e' uguale a h1 ?

h2 100 ;; //  210

// NOTARE

//  t |> f |> g  =  g ( f  t )  = (f >> g) t  

