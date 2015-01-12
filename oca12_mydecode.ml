type 'a term =
  | One of 'a
  | Many of int * 'a;;

let mydecode l =
  let rec iter acc = function
    | [] -> acc
    | (h::t) -> match h with
                | (One x) -> iter (x::acc) t
                | (Many (count, item)) -> (match count with
                                           | 1 -> iter (item::acc) t
                                           | _ -> iter (item::acc) ((Many (count-1, item))::t))
  in List.rev (iter [] l);;

let () =
  let res = mydecode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] in
  begin
    List.iter (Printf.printf "%s ") res;
    Printf.printf "\n";
  end
