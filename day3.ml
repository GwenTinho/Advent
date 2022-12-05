let readVals () =
  let rec aux l =
    let x = try Some (read_line ()) with End_of_file -> None  in match x with
    | None -> l
    | Some x -> aux (x::l) in
  aux []

let take n l =
  let rec aux i rem acc = if i = 0 then acc, rem else match rem with
  | [] -> acc, []
  | h::t -> aux (i-1) t (h::acc) in
  aux n l []

let groupByN n l =
  let rec aux acc rem = match rem with
  | [] -> acc
  | rem -> let h,t = take n rem in
  aux (h::acc) t in aux [] l

let explode s = List.init (String.length s) (String.get s)

let splitMiddle l =
  let rec aux rem firstH secondH  = match rem with
  | [] -> (firstH, secondH)
  | [a] -> (firstH, secondH)
  | h::t -> match List.rev t with
  | [] -> (firstH, secondH)
  | h2::t2 -> aux (List.rev t2) (h::firstH) (h2::secondH) in
  let a,b = aux l [] [] in
  (List.rev a, b)


let findMatching fh sh =
  let rec aux s l= match s, l with
  | [], _ -> None
  | h::t, [] -> aux t sh
  | h::t, h2::t2 -> if h=h2 then Some h else aux s t2 in
  aux fh sh

let isIn l a= [] != List.filter (fun x -> x = a) l

let findCommon a b c =
  List.filter (fun y -> ((isIn b y) && (isIn c y))) a

let findInBothHalves l =
  let fh, sh = splitMiddle l in
  findMatching fh sh

let convertChar c = match c with
  | None -> None
  | Some c ->
  match Char.code c with
  | c when c >= 97 && c <= 122 -> Some (c - 96)
  | c when c >= 65 && c <= 90 -> Some (c - 64 + 26)
  | _ -> None


let sum l =
  let rec aux acc = function
  | [] -> acc
  | h::t -> aux (h + acc) t in
  aux 0 l

let concrete a = match a with
    | Some a -> a
    | None -> 0

let suffix_char s c = s ^ String.make 1 c

let makeString l =
  List.fold_left suffix_char "" l


let _ =
  let lines = (readVals ()) in
  let grouped = groupByN 3 (List.map explode lines) in
  let aux grp = let [a;b;c] = grp in Some (List.hd (findCommon a b c)) in
  let commons = List.map aux grouped in
  let vals = List.map convertChar commons in
  let vals = List.map concrete vals in
  let sm = sum vals in
  print_int sm


