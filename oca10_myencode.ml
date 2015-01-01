let myencode l =
  let rec iter count item res = function
    | [] -> (count, item)::res
    | (h::t) -> if count = 0 then iter 1 h res t
                else
                  if h = item then
                    iter (count+1) h res t
                  else
                    if count = 1 then
                      iter 1 h ((1, item)::res) t
                    else iter 1 h ((count, item)::res) t
  in List.rev (iter 0 "a" [] l);;

let () =
  let res = myencode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  let rec print_encodelist l =
    match l with
    | [] -> print_endline "\n end \n"
    | h::t ->
       match h with
       | (count, item) ->
          begin
            Printf.printf "(%i, " count;
            Printf.printf "%s); " item;
            print_encodelist t;
          end
  in print_encodelist res;;
