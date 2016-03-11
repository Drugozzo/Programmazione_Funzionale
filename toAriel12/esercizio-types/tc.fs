type tp = INT | LSTINT

type tm = K of int | Plus of tm * tm | Nil | Cons of tm * tm | Hd of tm | Tl of tm

let rec tpck = function
  K _ -> Some INT
  | Nil -> Some LSTINT
  | Plus(e1,e2) -> 
    match(tpck e1,tpck e2) with
      |(Some(INT),Some(INT)) -> Some(INT)
      | _ -> None
  | Cons(e1,e2) -> 
    match(tpck e1,tpck e2) with
      |(Some(INT),Some(LSTINT)) -> Some(LSTINT)
      | _ -> None
  | Hd e -> 
    match(tpck e) with
      |Some(LSTINT) -> Some(INT)
      | _ -> None
  | Tl e -> 
    match(tpck e) with
      |Some(LSTINT) -> Some(LSTINT)
      |_ -> None
  


let tok = tpck (Hd (Cons(K 3,Nil)))
let ttk = tpck (Tl (Cons(K 3,Nil)))

let twrong = tpck (Hd (Cons(Nil,Nil)))


let rec eval = function
  K n -> K n
  | Nil -> Nil
  | Plus(e1,e2) -> let (K n1) = eval e1
                   let (K n2) = eval e2
                   K(n1 + n2)
  | Cons(e1,e2) -> Cons(eval e1, eval e2)
  | Hd e -> let (Cons (vh, _))  = eval e in vh
  | Tl e -> let (Cons ( _, vt)) = eval e in vt



let eok = eval (Hd (Cons(Plus(K 3,K 9),Nil)))
let etk = eval(Tl (Cons(K 3,Nil)))


