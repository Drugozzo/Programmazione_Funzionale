#r "FsCheck"
open FsCheck

// %%%%% On Q's generators


// Let's revisit one of our fav examples

let rec insert (x, xs) = 
    match xs with
    | [] -> [x]
    | c::cs when x <= c -> x::xs 
    | c::cs ->  c::insert (x, cs)
    
let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered (y ::ys)

let prop_insert (x:int, xs) =
  ordered xs ==> ordered (insert (x, xs))

do Check.Quick prop_insert

// Arguments exhausted after 59 tests.

(* It is important to be aware of the distribution of test cases: if
   test data is not well distributed then conclusions drawn from the
   test results may be invalid. In particular, the ==> operator can
   skew the distribution of test data badly, since only test data
   which satisfies the given condition is used.

   FsCheck offers some combinator to observe the distribution. Just
put them in the test itself *)

// Counting Trivial Cases: how many tests just consider lists > 1 ?
let insertTrivial (x:int) xs = 
  ordered xs ==> (ordered (insert (x ,xs)))
  |> Prop.trivial (List.length xs <= 1)

do Check.Quick insertTrivial

// Can we actually see what's happening?
//Collecting Data Values: A property may take the form collect
//<expression> <property>

let insertCollect (x:int) xs = 
  ordered xs ==> (ordered (insert (x ,xs)))
      |> Prop.collect (List.length xs)
do Check.Quick insertCollect

// Pretty lousy, innit?

// Another example
let rec remove x = function
    | [] -> []
    | y::ys -> if x=y then remove x ys else y :: (remove x ys)

let rec removeDup xs = 
    match xs with
    | [] -> []
    | y::ys -> y::(removeDup (remove y ys))

let prop_remord (xs : int list) =
  ordered xs ==> ordered (removeDup xs )
    |> Prop.collect (List.length xs)

do Check.Quick prop_remord

(*
There are two (inter-related) issues here:

1. This checks are conditional and have **hard-to-satisfy** premises

  ordered xs ==> P xs

  the likelihood that a list of 3 elements is ordered is 33%, hence
    the reply "Arguments exhausted after 55 tests", meaning that 45
    tests have been discarded because the random instance for xs is
    *not* ordered

2. Of the instances that survive the test, it is no suprise that they
have very short length.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 How to make random testing more covering is a hot research topic in
 FP testing. One can

- drop random testing and shift to *exhautive* testing, or even
  symbolic execution ("narrowing")
    ===> (Lazy)SmallCheck

- Use logic programming techique to improve test generation

   ===> SparseCheck, smart generators in Isabelle/HOL

- Find "deeper" enumeration techniques

 ===> Feat

 Here we just follow the QuickCheck approach, that is, we explain how to write custom made generators

 1. In particular we can write generators so that we can quantify only over (here) ordered lists

 2. We can change probability distribution to suit our needs
   *)



// But wait!  naive fix: increase the number of tests:

let config = {
    Config.Quick with 
        MaxTest = 500
    }

// but check this out
let isSmallerThan80 x = x < 80

// note, you need to call Check.One
do Check.One(config,isSmallerThan80 )

(*
   In, fact generators start with small numbers and gradually
increase them. This is controlled by the StartSize and EndSize
settings. By default, StartSize is 1 and EndSize is 100. So at the end
of the test, the "size" parameter to the generator will be 100.  *)

let config2 = {
    Config.Quick with
        MaxTest = 100
        StartSize = 40
        EndSize = 1000
    }

do Check.One(config2,isSmallerThan80 )

// This does not help with coverage:

do Check.One(config,insertCollect)


// And beyond coverage, you have soundness issues:

let prop_zip_unzip (xs: int list) ys =
  (xs,ys) = (List.zip xs ys |> List.unzip)

do Check.Quick prop_zip_unzip

(* The precondition of List.zip is that both lists have the same
length and so we must constrain the test
*)

let prop_zip_unzipP (xs: int list) ys =
  List.length xs = List.length ys ==> lazy ((xs,ys) = (List.zip xs ys |> List.unzip))
  |> Prop.trivial (List.length xs = 1)

do Check.Quick prop_zip_unzipP

// This is slow : 00:00:05.86, and coverage is bad
//  Arguments exhausted after 26 tests (30% trivial).


(* It's better to write a custom generator.

FsCheck gives you generator for primitve types and then lift them up
to discriminated unions. All these combinators live in the modules Gen
and Arb, where the latter package up generators with shrinkers

Here we start with Gen<'a> and then move to Arb<'a> to express
properties
*)


// Arb.generate<'a> returns the generator of the registered Arbitrary
// instance for the given type 'a

let intGenerator = Arb.generate<int>  

// To see what's going on we use Gen.sample

// First, the usal patch vos VS2010
let _ = Runner.init.Force() |> ignore

let sample size n gn =
  let rec sample i seed samples =
    if i = 0
    then samples else sample (i-1) (Random.stdSplit seed |> snd) (Gen.eval size seed gn :: samples)
  sample n (Random.newSeed()) []



// generate three ints with a maximum size of 1
sample 1 3 intGenerator    

// generate three ints with a maximum size of 100
sample 100 3 intGenerator 

// Then you can lift to compound types

// generate 10 int lists with a maximum size of 5
let intListGenerator = Arb.generate<int list>

sample 5 10 intListGenerator 

// The best thing is that the generator will work with your own user-defined types too!

type Color = Red | Green of int | Blue of bool

let colorGenerator = Arb.generate<Color>

// generate 10 colors with a maximum size of 50
sample 50 10 colorGenerator 


(* Now, we can use some combinators to filter what we want from
generators. Remember, Arbitrary is just like Gen but with shrinking in.

Arb.filter : (('a -> bool) -> Arbitrary<'a> -> Arbitrary<'a>)

   filters the generator and shrinker for a given Arbitrary instance
   to contain only those values that match with the given filter function


Arb.mapFilter : (('a -> 'a) -> ('a -> bool) -> Arbitrary<'a> -> Arbitrary<'a>)

   Arb.mapFilter maps the generator and filter the shrinkers for a
   given Arbitrary instance. Mapping the generator is sometimes
   faster than to filter the negative values.

   *)



// what's wrong with this one?
let prop_max (xs :int list) =
  (List.head (List.sort xs) = List.min xs)


// The usual fix
let prop_max1 (xs :int list) =
  xs <> [] ==> lazy (List.head (List.sort xs) = List.min xs)

do Check.One(config, prop_max1)



// use Prop.forAll :  (Arbitrary<'a> -> ('a -> 'b) -> Property) to quantify here over non empty lists
// Quantified property combinator. Provide a custom test data generator to a property.

// Fisrt define the right generator, for non empty lists, using filter

let notEmpGen =
  Arb.filter (fun x -> x <> []) Arb.from<int list>

// forall xs \in notEmptyGen. List.head (List.sort xs) = List.min xs
  
let prop_maxq (xs :int list) =
  Prop.forAll notEmpGen (fun xs ->
  List.head (List.sort xs) = List.min xs)

do Check.One(config, prop_maxq)


// Let's try a generator for positive integeres using mapFilter, by
// mapping the absolute value abs : int -> int, and keeping the predicate vacuous

let posInt =
  Arb.mapFilter abs (fun x -> true) Arb.from<int>

// I coud also do it by filtering, but it's slower (allegedly)

let posIntF =
  Arb.filter  (fun x -> x >= 0) Arb.from<int>

// let's use it:
  
let factC n =
  let rec fc n  k =
    if n = 0 then (k 1)
       else  fc (n - 1) (fun res -> k (n * res))
  fc n id

let factA n =
  let rec fc n  a =
    if n = 0 then a
       else  fc (n - 1) (n *a)
  fc n 1

// with lazy
let prop_fac n =
   n >=0  ==> lazy (factA n = factC n)

do Check.One(config, prop_fac)
// : Real: 00:00:00.133,

// with mapped gen
let prop_facQ n =
   Prop.forAll  posInt (fun n -> factA n = factC n)
do Check.One(config, prop_facQ)
// Real: 00:00:00.123,

// with filtered gen
let prop_facQF n =
   Prop.forAll  posIntF (fun n -> factA n = factC n)
do Check.One(config, prop_facQF)
// Real: 00:00:00.128,


// bottom line, they are the same here

// Back to coverage: let's define  a generator for ordered lists:

let orderedList =
  Arb.mapFilter List.sort ordered Arb.from<list<int>>

(*why both sort and ordered? Not sure, this is what the manual recommend: the following also work

let orderedListF = Arb.mapFilter  List.sort (fun xs -> true)  Arb.from<list<int>>
  *)
let prop_insertWithArb x =
  Prop.forAll orderedList (fun xs -> ordered(insert (x, xs)))
         
do Check.Quick prop_insertWithArb 

// note: no test exhaustion. Now let's see coverage
let prop_insertWithArbCollect x =
  Prop.forAll orderedList (fun xs -> ordered(insert (x, xs)) |> Prop.collect (List.length xs))
do Check.Quick prop_insertWithArbCollect 


// Let's go back to the zip/unzip problem: 

let sameLen =
  Arb.filter  (fun (xs,ys) -> List.length xs = List.length ys) Arb.from<list<int> * list<int>>


let prop_zip_unzipQ =
  Prop.forAll sameLen (fun (xs,ys ) ->
                       (xs,ys) = (List.zip xs ys |> List.unzip))

do Check.Quick prop_zip_unzipQ

// Less slow, but coverage is 100% !


(*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Controlling data distribution:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sometimes we want to have generators with a *bias*, that is, we give
to some constructor a higher likelihood to be generated. E.g. in a
tree we want more nodes than leaves. In an arithmetic expression more
variables than constants.

We can control the distribution of results using *frequency*
instead. frequency chooses a generator from the list randomly, but
weighs the probability of choosing each alternative by the factor
given.

*)
let boolGen = Arb.generate<bool>  
Gen.sample 1 15 boolGen

let prop_bool (p : bool) q =
  not (p && q) = not p || not q

do Check.Verbose prop_bool
(*
Gen.frequency : seq<int * Gen<'a>> -> Gen<'a>
   Build a generator that generates a value from one of the generators
in the given non-empty seq, with given probabilities.  *)

let genBoolmoreTrueGen =
  Gen.frequency [ (4, gen { return true }); (1, gen { return false })]
Gen.sample 1 15 genBoolmoreTrueGen

(* Suppose now that we want to use the new generator from now on. We
can *register* it and therefore override the default one
*)

type MyGenerators =
    static member  genBoolmoreTrueGen() =
        {new Arbitrary<bool>() with
            override x.Generator =  genBoolmoreTrueGen
            override x.Shrinker t = Seq.empty }

let _ = Arb.register<MyGenerators>()

do Check.Verbose prop_bool
