let rotate_n_num l n =
  let rec iter left_l right_l acc = function
    | [] -> (List.rev right_l) @ (List.rev left_l)
    | (h::t) -> if n > 0 then
                  if acc = n then iter left_l (h::right_l) acc t
                  else iter (h::left_l) right_l (acc+1) t
                else
                  let n' = (List.length l) + n in
                  if acc = n' then iter left_l (h::right_l) acc t
                  else iter (h::left_l) right_l (acc+1) t
  in iter [] [] 0 l;;

let () =
  let res1 = rotate_n_num ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 in
  let res2 = rotate_n_num ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) in
  begin
    List.iter (Printf.printf "%s ") res1;
    Printf.printf "\n\n";

    List.iter (Printf.printf "%s ") res2;
    Printf.printf "\n";
  end;;
