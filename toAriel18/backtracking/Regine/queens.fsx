#r "board"
open BOARD

exception Fail

(* addqueen bd evaluates to bd', where bd' is a complete safe placement
   extending bd, if one exists, and raises Fail otherwise *)
let rec addqueen bd =
        let rec try_place j =
            if j > size bd then
               raise Fail
            else if safe (bd, j) then
               try
                 addqueen (place (bd, j))
               with
                 Fail -> try_place (j+1)
            else
               try_place (j+1)

        if complete bd then
           bd
        else
           try_place 1



let rec addqueenv bd =
        let rec try_place j =
            if j > size bd then
               printf "got to row %d bigger than size, failing\n" j  ; raise Fail
            else if safe (bd, j) then
               try
                 printf "row %d is safe, trying to place it\n" j ; addqueenv (place (bd, j))
               with
                 Fail -> printf " upon failure, trying row %d\n" (j + 1) ; try_place (j+1)
            else
                printf "row %d is NOT safe, trying %d\n" j (j + 1); try_place (j+1)

        if complete bd then
           bd
        else
           printf "(re)starting at row 1\n"; try_place 1


let queens n =
  try
    newb n |>
    addqueenv  |>
    positions |>
    List.rev |>
    printf "solution is %A\n"
  with
    Fail -> printf "no solutions found\n" 

let q1 = queens 1
let q2 = queens 2
let q3 = queens 3
let q4 = queens 4
let q8 = queens 8
let q16 = queens 16


// continuation-based
let rec addqueenc bd fk =
        let rec try_place j =
            if j > size bd then
              fk ()
            else if safe (bd, j) then
                 addqueenc (place (bd, j)) (fun () -> try_place (j+1))
              else
               try_place (j+1)

        if complete bd then
           Some bd
        else
           try_place 1


let queensc n =
  addqueenc (newb n) (fun () -> None)  |>
  Option.get
  |> positions
  |> List.rev |> printf "solution is %A\n"
