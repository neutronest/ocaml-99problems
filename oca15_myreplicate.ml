let myreplicate l num =
  let rec iter acc acc_num num = function
    | [] -> acc
    | (h::t) -> match acc_num with
                | 0 -> iter acc num num t
                | _ -> iter (h::acc) (acc_num-1) num (h::t)
  in List.rev (iter [] num num l);;

let () =
  let res = myreplicate ["a";"b";"c"] 3 in
  begin
    List.iter (Printf.printf "%s ") res;
    Printf.printf "\n";
  end
