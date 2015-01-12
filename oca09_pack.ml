let my_pack l =
  let rec iter acc  = function
    | [] -> acc
    | (h::t) -> if List.length acc = 0
                then iter [[h]] t
                else
                  if h = List.hd (List.hd acc)
                  then iter ((h::(List.hd acc))::(List.tl acc)) t
                  else iter ([h]::acc) t
  in List.rev (iter [] l)

let () =
  let res = my_pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
              [["a";"a";"a";"a"]; ["b"]; ["c"; "c"; ]; ["a"; "a"]; ["d"; "d"]; ["e"; "e";"e";"e"; ]] in
  if res then
    Printf.printf "YES! \n";;
