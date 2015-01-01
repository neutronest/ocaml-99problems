type 'a node =
  | One of 'a
  | Many of int * 'a ;;

let myencode l =
  let rec iter count item res = function
    | [] -> (match count with
            | 1 -> (One item)::res
            | _ -> (Many (count, item))::res)
    | (h::t) -> if count = 0 then iter 1 h res t
                else
                  if h = item then
                    iter (count+1) h res t
                  else
                    if count = 1 then
                      iter 1 h ((One item)::res) t
                    else iter 1 h ( (Many (count, item))::res) t
  in List.rev (iter 0 "nil" [] l);;

let () =
  let res = myencode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  let rec print_encodelist l =
    match l with
    | [] -> print_endline "\n"
    | h::t ->
       match h with
       | (One x) ->
          begin
            Printf.printf "One ";
            Printf.printf "%s; " x;
            print_encodelist t
          end
       | (Many (count, item)) ->
          begin
            Printf.printf "Many (";
            Printf.printf "%i, "count;
            Printf.printf "%s); " item;
            print_encodelist t;
          end
  in print_encodelist res;;
