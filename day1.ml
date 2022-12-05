let merge l r =
  let rec aux acc l r= match l,r with
  | [],[] -> acc
  | [], r -> acc @ r
  | l, [] -> acc @ l
  | lh::lt, rh::rt ->
    if lh < rh then
      aux (acc @ [lh]) lt r else
      aux (acc @ [rh]) l rt in
  aux [] l r

let splitMiddle l =
  let mid = (List.length l) / 2 in
  let rec aux c l r = match c with
    | 0 -> (l,r)
    | c -> match r with
      | [] -> ([],[])
      | h::t -> aux (pred c) (h::l) t in
    let l,r = aux mid [] l in
    (List.rev l, r)


let rec mergeSort = function
    | [] -> []
    | h::[] -> [h]
    | l ->
      let left, right = splitMiddle l in
      merge (mergeSort left) (mergeSort right)

let readInts () =
  let rec aux l =
    let x = read_int_opt () in
      match x with
      | None -> Some l
      | Some y -> aux (y::l) in
  try aux [] with End_of_file -> None

let sum l =
  let rec aux acc = function
  | [] -> acc
  | h::t -> aux (h + acc) t in
  aux 0 l

let getValues () =
  let rec aux l =
    match readInts () with
    | Some ls -> aux ((sum ls)::l)
    | None -> l in
  aux []

let getMax l =
  let rec aux curr = function
  | [] -> curr
  | h::t -> aux (if h > curr then h else curr) t in
  match l with
  | [] -> None
  | h::t -> Some (aux h t)

let reverse l =
  let rec aux newl = function
  | [] -> newl
  | h::t -> aux (h::newl) t in
  aux [] l

let getNHighest l n =
  let sorted = mergeSort l in
  let rec aux rem vals = function
  | 0 -> vals
  | n -> match rem with
    | [] -> vals
    | h::t -> aux t (h::vals) (n-1) in
  aux (reverse sorted) [] n


let _ =
  let l = getNHighest (getValues ()) 3 in
  match l with
  | [] -> ()
  | l -> print_int (sum l)
