let rec last1 = function
  | [] -> None
  | [x] -> Some x
  | _::t -> last1 t

let () =
  let res = last1 [1;2;3;4;5;6;7] in
  match res with
  | None -> Printf.printf "Empth list\n"
  | Some x -> Printf.printf "%i\n" x

(* if you use the JaneStreet's core , open Core.std, you
   could use more useful method e.g. fold_left...
 *)
