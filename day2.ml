type left = A | B | C
type right = X | Y | Z

let readVals () =
  let app = function
        | [a;b] ->
          let lf = match a with
          | "A" -> Some A
          | "B" -> Some B
          | "C" -> Some C
          | _ -> None in
          let rt = match b with
          | "X" -> Some X
          | "Y" -> Some Y
          | "Z" -> Some Z
          | _ -> None in
          Some (lf, rt)
        | _ -> None in
  let rec aux l =
    let x = try Some (read_line ()) with End_of_file -> None  in match x with
    | None -> l
    | Some x -> aux ((app (String.split_on_char ' ' x))::l) in
  aux []

let concrete a = match a with
    | Some (Some a,Some b) -> Some (a,b)
    | _ -> None

let evalp1 a = match a with
    | Some (x,y) -> let v = match x,y with
      | A, X -> 1 + 3
      | B, X -> 1 + 0
      | C, X -> 1 + 6
      | A, Y -> 2 + 6
      | B, Y -> 2 + 3
      | C, Y -> 2 + 0
      | A, Z -> 3 + 0
      | B, Z -> 3 + 6
      | C, Z -> 3 + 3 in v
    | None -> 0

let evalp2 a = match a with
    | Some (x,y) -> let v = match x,y with
      | A, X -> 3 + 0 (*Choose scissors to lose*)
      | B, X -> 1 + 0 (*Choose rock to lose*)
      | C, X -> 2 + 0
      | A, Y -> 1 + 3 (*Choose rock to draw*)
      | B, Y -> 2 + 3
      | C, Y -> 3 + 3
      | A, Z -> 2 + 6 (*Choose paper to win*)
      | B, Z -> 3 + 6
      | C, Z -> 1 + 6 in v
    | None -> 0

let sum l =
  let rec aux acc = function
  | [] -> acc
  | h::t -> aux (h + acc) t in
  aux 0 l

let _ =
  let l = List.map evalp2 (List.map concrete (readVals ())) in
  match l with
  | [] -> ()
  | l -> print_int (sum l)
