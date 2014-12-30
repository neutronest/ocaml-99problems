let rec my_compress l =
  let rec iter acc = function
    | [] -> acc
    | (h::t) -> if List.length acc = 0
                then iter [h] t (* more expensive here *)
                else
                  if h <> (List.hd acc)
                  then iter (h::acc) t
                  else iter acc t
  in List.rev (iter [] l);;

let () =
  let res = my_compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  List.iter (Printf.printf "%s ") res;
  Printf.printf "\n"
