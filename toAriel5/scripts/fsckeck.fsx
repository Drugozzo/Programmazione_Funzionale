// %% MOTIVATING FsCheck %%%%%%%%%%%%%%%%%%%%%%
   
// alcune funzion standard su liste viste settimana scorsa

let rec append (xs, ys) = 
    match xs with 
    | [] -> ys 
    | z::zs -> z :: append (zs, ys) 

let rec rev ls =  
    match ls with 
    | [] -> [] 
    | x :: xs -> append (rev xs, [x]) 


(* Come è che ci convinciamo che sono corrette? Facciamo qualche test!  *)

append ( [ 1 ; 2] , [ 3 ; 4 ; 5 ] ) ;; //  [1; 2; 3; 4; 5]

rev [1 .. 10] ;;

(* A questo punto, ci diciamo "è tipata, sembra funzionare, andiamo avanti"

Siamo tutti colpevoli di questo stile.

Dal punto di vista del software engeenring, questo si chiama "unit testing":

- individuare una funzione/metodo da testare
- scegliere degli argomenti da passare
- avere un oracolo che ci dice quale dovrebbe essere il risultato
- controllare se esecuzione del metodo rispetta oracolo

Possiamo fare meglio?

===>  scriviamo delle **proprietà** che il nostro metodo deve soddisfare

- es: rev è involutiva

Per adesso una proprietà è una funzione booleana
*)
let prop_revRevIsOrig (xs:int list) =
  rev (rev xs) = xs;;

// val prop_revRevIsOrig : xs:int list -> bool


// A questo punto, faccio degli UT sulla proprietà
let t1 = prop_revRevIsOrig [1..4]
let t2 = prop_revRevIsOrig []
let t3 = prop_revRevIsOrig [1]

// Che noia! Non possiamo automatizzare? Yes we can -- FsCheck

#r "FsCheck";;

open FsCheck;;

let _ = Check.Quick prop_revRevIsOrig ;;
// verbosamente

let _ = Check.Verbose prop_revRevIsOrig ;;

(* Che cosa possiamo concludere? Che la nostra funzione "soddisfa" la
nostra spec (algebra è oracolo), o meglio che ha superato 100 test

Non è una dimostrazione, ma una *validazione*

Fidarsi bene, non fidarsi ....*)

let prop_what (x : int) =
  x < 80
Check.Quick prop_what
Check.Verbose prop_what

// E' tutto un problema di distribuzione -- vedremo dopo come cambiare i parametri

(*
- Proviamo a scrivere una proprietà **falsa**
*)

let revIsOrig (xs:int list) =
  rev xs = xs
let _ = Check.Quick revIsOrig ;;

// SHRINKING ... 

(* FsCheck also shrinks the counter example: it tries to find the
minimal counter example that still fails the property. The counter
example is indeed minimal: the list must have at least two different
elements for the test to fail. FsCheck displays how many times it
found a smaller (in some way) counter example and so proceeded to
shrink further.  *)

// ==> BACK to SLIDES !!

(*
INTERMEZZO: tipo unit

- Il tipo di  Check.Quick è 'a -> unit, che prende un 'a e non ritorna nulla di interessante come valore,
ma segnala un effetto collaterale, qui testing

- unit corrisponde a void in Java. Può essere visto come il caso
zero-ario del cartesiano, cioè il tipo che contiene la nupla con zero
elementi, scritta '()'.

Esempio: input da console
> System.Console.ReadLine :  unit -> string
*)

// Altre proprietà? Append è una operazione monoidale, scriviamole insieme ...
  
  let prop_ApAss (xs:char list,ys,zs)=
    xs@(ys @ zs)=(xs @ ys)@zs

  let _= Check.Quick prop_apAss 

  let prop_ApRI (xs: char list)=
    xs @ []= xs

  let _=Check.Quick prop_ApRI

// TODO

// E' un monoide commutativo?

// TODO

                                                                             
// testiamo una proprietà che collega rev e append

let prop_rev_app (xs : int list, ys) = 
    rev (append (xs , ys)) = append (rev ys, rev xs)

let _ = Check.Quick prop_rev_app


//  Polimorofismo? Viene gestito?

let prop_RevRevp xs =
  rev (rev xs) = xs 

// Si, però ...  Potete scrivere properietà polimorfe, ma in realtà sono istanziate con 'obj'
let prop_RevRevo (xs : obj list) =
  rev ( rev xs) = xs 

// ... e questo "rompe" il type checking poiche' tutto viene cast in obj
let _ = Check.Verbose prop_RevRevo;;

// Quindi tanto vale renderla monoforma, ma attenzione ....

let prop_RevRevf (xs : float list) =
  rev ( rev xs) = xs 

let _ = Check.Quick prop_RevRevf;;

(* nan : float is a special value for "not a number", similarly
infinity : float

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 Testing against a model

Another technique for gaining confidence in some code is to test it
against a model implementation, where one is the reference
implementation and the other the "new" and possibly improved one. We
can tie our implementation of function on lists to the reference
functions in the standard list theory, and, if they behave the same,
we gain confidence that they do the right thing*)

// linear reverse
let bfrev xs =
    let rec rev_aux (xs, acc) =
        match xs with 
        | [] -> acc
        | y ::  ys -> rev_aux (ys, y :: acc)
    rev_aux (xs, [])

// le due reverse si comportano allo "stesso" modo
let prop_rev_qrev (xs :int list) =
  bfrev xs = rev xs

let _ = Check.Quick prop_rev_qrev

// la mia append "vale" la List.append

let prop_app_mapp (xs :unit list, ys) =
  append (xs, ys) = xs @ ys

let _ = Check.Quick prop_app_mapp


// Conditional  checking: da bool a Property

let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered ys

let rec insert (x, xs) = 
    match xs with
    | [] -> [x]
    | c::cs when x <= c -> x::xs 
    | c::cs ->  c::insert (x, cs)
 
let prop_insert (x:int, xs) =
  ordered xs ==> ordered (insert (x, xs))

let _ = Check.Quick prop_insert

// FIX THIS !

let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered (y ::ys)

let prop_insert2 (x:int, xs) =
  ordered xs ==> ordered (insert (x, xs))

let _ = Check.Quick prop_insert2


// proprietà di sorting

// se sorto una lista, è ordinata
let prop_so (xs : int list) =
  List.sort xs |>  ordered

Check.Quick prop_so

// il primo elemento di una lista ordinata è il minimo
let prop_fsm (xs : int list) =
  (List.sort xs |>  List.head) = List.min xs

Check.Quick prop_fsm

// Ah, la lista vuota ... Secondo tentativo:

let prop_bad (xs : int list) =
  if xs <> [] then (List.sort xs |>  List.head) = List.min xs else true

Check.Quick prop_bad

// Questa è una porcheria (resto in bool), ma per fare la cosa giusta e
// usare conditional checking devo usare **lazy evaluation**

(*
Questo non va, per ragioni di type classes che capisco fino a uncerto punto

let prop_fsm2 (xs : int list) =
  xs <> [] ==>  ((List.sort xs |>  List.head) = List.min xs)

   *)

// Per capire cosa succede, un esempio classico

(* Since F# has eager evaluation by default, a property does
more work than necessary: it evaluates the property at the right of
the condition no matter what the outcome of the condition on the
left. While only a performance consideration in the other  example,
this may limit the expressiveness of properties - consider:*) 

let tooEager a =
  a <> 0 ==> (1/a = 1/a)
Check.Quick tooEager

// uso la keyword 'lazy', per ritardare la valutazione del RHS

let moreLazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick moreLazy

// applicato al nosto caso:
let prop_MS2 (xs : int list) =
  xs <> [] ==>
  lazy ((List.sort xs |>  List.head) = List.min xs)

Check.Quick prop_MS2

// ESERCIZIO: scrivere una proprietà simile per List.max

let prop_max (xs : int list)=
  xs <> [] ==> 
  lazy ((List.sort xs |> List.rev |> List.head) = List.max xs);;

// ==> FINIRE LE SLIDES !

// %%%%%%%%%%% GENERATORI %%%%%%%%%%%%%%%%%

(* Anche se non vogliamo far fatica a pensare a proprietà, FsCheck ci
aiuta nel testing semplicemente fornendoci un modo immediato di
produrre dei dati random attraverso la sua nozione di generatori.

Procediamo per esempi, tenendo presente che i generatori sono composizionali
*)

// construiamo dei generatori, usando il modulo Arb

// booleani
let boolGenerator = Arb.generate<bool>

// interi
let intGenerator = Arb.generate<int>

// usiamoli per produrre data ramdom

// generate a list of three ints with a *maximum* size of 10
//  Gen.sample  : (int -> int -> Gen<'a> -> 'a list)
let _ =
  let xs = Gen.sample 10 3 intGenerator
  printf "Original list: %A, reversed list %A\n" xs (List.rev xs)

// let xs = Gen.sample 10 3 intGenerator in  (xs,List.rev xs)
         
let ys = Gen.sample 10 3 intGenerator in (ys,List.map (fun exp -> pown 2 exp) ys)

// generatore di liste
let intlistGenerator = Arb.generate<int list>
Gen.sample 54 3 intlistGenerator

let ys = Gen.sample 20 2 intlistGenerator in (ys,List.map (List.rev) ys)

// Combinare checks

let add x y = x + y 

let commutativeProperty x y = 
    add x y = add y x    

let associativeProperty x y z = 
    add x (add y z) = add (add x y) z    

let leftIdentityProperty x = 
    add x 0 = x

let rightIdentityProperty x = 
    add 0 x = x

type AdditionSpecification =
    static member ``Commutative`` x y = commutativeProperty x y
    static member ``Associative`` x y z = associativeProperty x y z 
    static member ``Left Identity`` x = leftIdentityProperty x 
    static member ``Right Identity`` x = rightIdentityProperty x 

let _ = Check.QuickAll<AdditionSpecification>()


// ora soluzioni a esercizi 
