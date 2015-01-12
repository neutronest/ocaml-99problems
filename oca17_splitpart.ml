let split_part l left_len =
  let rec iter acc_l acc_r cur = function
    | [] -> [(List.rev acc_l); (List.rev acc_r)]
    | (h::t) -> if cur > left_len then iter acc_l (h::acc_r) (cur+1) t
                else iter (h::acc_l) acc_r (cur+1) t
  in iter [] [] 1 l;;

let () =
  let res = split_part ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 in
  begin
    List.iter (Printf.printf "%s ") (List.hd res);
    Printf.printf "\n";
    List.iter (Printf.printf "%s ") (List.hd (List.tl res));
    Printf.printf "\n";
  end;;
