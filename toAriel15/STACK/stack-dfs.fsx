// Depth first using stack operations

#r "stack.dll";;

open Stack;;

let agraph = Map.ofList[  ('a',['b';'C']);  ('b',['d']);  ('C',['d']);  ('d',[])  ]

let atree = Map.ofList [ (1,[2;8])  ; (2,[6])  ; 
                      (8,[5; 4; 12]) ; (6,[]) ; (4,[]) ; (5,[]) ; (12,[])];;


let dfvisitL gr  root = 
 let rec dfs = function
     | [] -> []
     | x :: xs -> let succ_x = Map.find x gr
                  x :: (dfs (succ_x @ xs))                   
 dfs [root];;


// df visit
let dfvisit gr root = 
 let rec dfs ss =
   if isEmpty ss then empty
    else 
        let (x,xs) =   pop ss
        let succ_x = Map.find x gr
        let newss = append succ_x xs
        push x (dfs newss) 
 dfs (push root empty) |> toList;;

let t1 = dfvisit atree  1;;
let t2 = dfvisit agraph  'a';;
    
// with pred
let dfL gr  root pred = 
 let rec dfs = function
     | [] -> []
     | x :: xs -> let succ_x = Map.find x gr
                  if pred x then x :: (dfs (succ_x @ xs)) 
                     else  (dfs (succ_x @ xs))
 dfs [root];;


let df (tree : Map<'a,'a list>)  root pred = 
 let rec df ss = 
     if isEmpty ss then empty
      else 
        let (x,xs) =   pop ss
        let succ_x = Map.find x tree
        let newss = append succ_x xs
        if pred x then push x (df newss) else  (df newss)
 df (push root empty) |> toList;;


let t3 = df atree 1 (fun x -> x % 2 = 0) ;;
