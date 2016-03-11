(*

More on tail-recursion

Often a tail recursive (iterative) version of a function can be
obtained by adding an additional argment, the accumulator, making it more
space efficient

Recall that a funcion g : t -> t' is an *iteration* of f : t -> t, if
there are predicates p : t -> bool and function h : t -> t' such that

g x = if p x
         then g (f x)
            else h x

Informally, p is the loop condition, h the exiting the loop and f is
the loop body

Example:

factA(n,acc) = if n <> 0 then factA(n-1,n*acc) else acc

f(n,acc) = f(n-1,n*acc)

p(n,acc) = n <> 0

h(n,acc) = acc

So far so good.

The bad news is that adding an accumulator does not alwas turn a
recursive function into an iterative one.

Take the case of binary trees

*)
#r "FsCheck"
open FsCheck


type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a *  BinTree<'a>;;

// the usual recursive function counting nodes
                   
let rec count = function
    | Leaf          -> 0
    | Node(tl,_,tr) -> count tl + count tr + 1;;

// let's put an accumulator:
    
let countA bt =
  let rec cA bt acc =
    match bt with
      | Leaf -> acc
      | Node(l,_,r) -> cA l (cA r (1 + acc))
  cA bt 0


// they are the "same" function
let prop_count (t : int BinTree) =
  count t = countA t

do Check.Quick prop_count

(* but countA is *not* tail recursive according to the definition
above, because you need to recur on the left **and** on the right.

So, stack-wise, we're kind of screwed.

What we can do is to use **continuations**.



This is a very general technique that adds a function as an extra
argument to the given procedure, which describes "the rest of the
computation to be performed"

Continuations are very useful in making the flow of control explicit
and thus the latter flow can be manipulated, as we will see later.

In the tail recursion case, suppose we have a function

  f : t1 -> t2

We add an extra argument

k : t2 → t2

This argument k represents the remaining computation that should be
performed when f v terminates. Such function k is called a
continuation. So,

fC : t1 -> (t2 -> t2) -> t2.


The basic property is that

       f v = fC v id
       
where id is the identity function

It's a general result that any function can be translated
(CPS-converted) to a tail recursive one. However, no free lunch: what
you save in space, you pay on the heap allocating closures.
*)

let id x = x

// the factorial, continuation style:

let factC n =
  let rec fc n  k =
    if n = 0 then (k 1)
       else  fc (n - 1) (fun res -> k (n * res))
  fc n id

(* Let's trace it:

fc 3 id =

  fc 2 (fun x -> id (3 * x)) =
    
  fc 1 (fun y -> (fun x -> id (3 * x)) (2 * y)) =

  fc 0 (fun z ->   (fun y -> (fun x -> id (3 * x)) (2 * y)) (1 * z)) = // base case k 1

  (fun z ->   (fun y -> (fun x -> id (3 * x)) (2 * y)) (1 * z)) 1 = // and now just function application

  (fun y -> (fun x -> id (3 * x)) (2 * y)) (1 * 1) =

  (fun x -> id (3 * x)) (2 * 1 * 1)) =

  id (3 * 2 * 1 *1) =

6
   *)

// It's intersting to compare wrt #time, fact, factA and
// factC. Basically factA is faster, but factC may handle bigger data
// structures (not here, it's just numbers)

// another example: append. It's efficient, but not tail recursive. Let's do it CPS

let appC xs ys =
  let rec aC xs ys k =
    match xs with
      | [] -> k ys
      | x :: xs' -> aC xs' ys (fun r -> k (x :: r))
  aC xs ys id

(*

appC ([1,2], [3,4]) =
⇒ aC ([1,2], [3,4], id)

⇒ aC ([2], [3,4], fun r1 -> (id) (1::r1))

⇒ aC ([], [3,4], fun r2 -> (fun r1 -> (id) (1::r1)) (2::r2))

⇒ (fun r2 -> (fun r1 -> (id) (1::r1)) (2::r2)) [3,4]

⇒ (fun r1 -> (id) (1::r1)) (2::[3,4])

⇒ (fun r1 -> (id) (1::r1)) [2,3,4]

⇒ (id) (1::[2,3,4])

⇒ (id) [1,2,3,4]

⇒ [1,2,3,4]

 *)  

// Back to our tree count

let rec countC t c =
  match t with
  | Leaf          -> c 0
  | Node(tl,n,tr) ->
      countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)));;


let rec genTree xs =
    match xs with
    | [| |]   -> Leaf
    | [| x |] -> Node(Leaf,x,Leaf)
    | _   -> let m = xs.Length / 2
             let xsl = xs.[0..m-1]
             let xm  = xs.[m]
             let xsr = xs.[m+1 ..]
             Node(genTree xsl, xm, genTree xsr);;

let t n = genTree [| 1..n |];;

let t20000000 = t 20000000;;

let cc = countC t20000000  id;;

// changing the control flow

(*

Consider a function that traverses a list and returns the shortest
prefix of the list such that a predicate p is false on the next
element .  If p holds on all elements, we return None to indicate no
such prefix exists.

let ev = prefix (fun x -> x % 2 = 0) [2;4;5;8]

==>   Some [2; 4]

let no = prefix (fun x -> x % 2 = 0) [2;4;2;8]

==> None

let so = prefix (fun x -> x % 2 = 0) [1;2;4;5;8]

==> Some []


   *)

let consO y = function
  None -> None
  | Some ys -> Some (y :: ys)

let rec prefix p xs =
  match xs with
    | [] -> None
    | y :: ys ->
      if p y then consO y (prefix p ys)  else (Some  [])

let ev = prefix (fun x -> x % 2 = 0) [2;4;5;8]
let no = prefix (fun x -> x % 2 = 0) [2;4;2;8]
let so = prefix (fun x -> x % 2 = 0) [1;2;4;5;8]

(*

Note an inefficiency in this function: if p is is true for every
element in the list, consOpt is called on every element after the end
of the list has been reached, passing along None all the way up to be
returned as the overall result.

ex:  prefix (fun x -> x % 2 = 0) [2;4;2;8]

==> consO 2 (cons0 4 (cons0 2 (cons0 8 None)))) ==> None


Instead, we would like to return None immediately once we have reached
the end of the list and found no element on which p is false.

We do this CPS style, where:

- we use as initial continuation a variant of id, i.e.  (fun r -> Some r)

- in the “success” case (we find an element where p is false)
we call the continuation cont on the expected answer ( [] in this
program).

- In the case of a “failure” (we do not find a place where p is false)
we discard the continuation and return directly (with None in this
program)

- 
  *)


let prefixC p xs =
  let rec pC xs k =
    match xs with
    | [] -> None
    | y :: ys ->
      if p y then pC  ys (fun r -> k (y ::r))  else (k  [])
  pC xs (fun r -> Some r)

let fc = prefixC (fun x -> x = 1) [1..10]


(* Another crucial use of continuations is to implement backtracking,
as we show next.   *)



