let rec mylength l =
  let rec iter num = function
    | [] -> num
    | (h::t) -> iter (num+1) t
  in iter 0 l;;

let () =
  let res = mylength [1;2;3;4;5;6;7] in
  Printf.printf "%i\n" res;;
