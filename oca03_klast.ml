let rec klast kth = function
  | [] -> None
  | h::t -> if kth == 1 then Some h else klast (kth-1) t;;

let () =
  let res = klast 3 [1;2;3;4;5;6;7] in
  match res with
  | None -> Printf.printf "None."
  | Some x -> Printf.printf "%i\n" x
