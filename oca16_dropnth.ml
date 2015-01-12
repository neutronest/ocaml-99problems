let dropnth l num =
  let rec iter acc cur num = function
    | [] -> acc
    | (h::t) -> if cur = num then iter acc 1 num t
                else iter (h::acc) (cur+1) num t
  in List.rev (iter [] 1 num l);;

let () =
  let res = dropnth  ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 in
  begin
    List.iter (Printf.printf "%s ") res;
    Printf.printf "\n";
  end;;
