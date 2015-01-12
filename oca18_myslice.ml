let my_slice l from_num to_num =
  let rec iter acc cur = function
    | [] -> acc
    | (h::t) -> if cur < from_num then iter acc (cur+1) t
                else if cur >= from_num && cur <= to_num then iter (h::acc) (cur+1) t
                else  iter acc cur []
  in List.rev (iter [] 0 l);;

let () =
  let res = my_slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 in
  begin
    List.iter (Printf.printf "%s ") res;
    Printf.printf "\n";
  end;;
