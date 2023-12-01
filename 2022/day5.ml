

type instruction = MoveLine of string | CrateLine of string | CountLine of string | Empty | Fail

let readVals () =
  let rec aux l =
    let x = try Some (read_line ()) with End_of_file -> None  in match x with
    | None -> l
    | Some x -> aux (x::l) in
  aux []

let string_of_char = String.make 1

let string_at s = string_of_char @@ (String.get s)

let int_of_char_opt c= int_of_string_opt (string_of_char c)

let int_of_string_at_opt s i =try  int_of_char_opt (String.get s 1) with _ -> None

let organize (s:string) = match s with
  | s when s = "" -> Empty
  | s when String.contains s '[' -> CrateLine s
  | s when (int_of_string_at_opt s 1) <> None -> CountLine s
  | s when String.contains s 'm' -> MoveLine s
  | _ -> Fail

let extractCrate s =
  let rec aux vals index rem = match String.sub rem 0 4 with
  | s when String.starts_with " " -> aux vals (index+1) (String.sub rem 4 ((String.length rem) - 4))
  | s when String.starts_with "[" -> aux String.get

let extractCount s = [""]
let extractMove s = [""]

let extract (i:instruction) = match i with
| Fail -> None
| Empty -> None
| CrateLine s -> Some (extractCrate s)
| CountLine s -> Some (extractCount s)
| MoveLine s -> Some (extractMove s)
