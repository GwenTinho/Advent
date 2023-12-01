exception ConversionError of string

let readVals () =
  let rec aux l =
    let x = try Some (read_line ()) with End_of_file -> None  in match x with
    | None -> l
    | Some x -> aux (x::l) in
  aux []

let extract line = List.map (String.split_on_char '-')  (String.split_on_char ',' line)

let convertInts line = List.map (List.map int_of_string) (extract line)

let cmpRanges ranges = match ranges with
| [r1;r2] -> let res = match r1,r2 with
  | [a;b], [c;d] -> (a >= c && b <= d) || (a <= c && b >= d)
  | _,_ -> false in res
| _ -> false


let rec cmpRangesOverlap ranges = match ranges with
| [r1;r2] -> let res = match r1,r2 with
  | [a;b], [c;d] -> if a > c then cmpRangesOverlap (List.rev ranges) else b >= c
  | _,_ -> false in res
| _ -> false

let countInclusions lines =
  let ext = List.map convertInts lines in
  let ft = List.filter cmpRanges ext in
  List.length ft

let countOverlaps lines =
  let ext = List.map convertInts lines in
  let ft = List.filter cmpRangesOverlap ext in
  List.length ft

let _ =
  let lines = readVals () in
  print_int (countOverlaps lines)

