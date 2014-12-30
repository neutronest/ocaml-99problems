let rec onelast = function
  | [] -> None
  | [h;h1] -> Some h
  | h::t -> onelast t

let () =
  let res = onelast [1;2;3;4;5;6;7] in
  match res with
  | None -> Printf.printf "Nothing!\n"
  | Some x -> Printf.printf "%i\n" x
