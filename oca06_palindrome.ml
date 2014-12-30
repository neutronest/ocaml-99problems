let rec is_palindrome l =
  l = List.rev l

let () =
  let res = is_palindrome [1;2;3;4;3;2;1] in
  Printf.printf("%b\n") res
