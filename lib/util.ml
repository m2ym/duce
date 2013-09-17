let gensym =
  let counter = ref 0 in
  fun () ->
    let sym = "_g" ^ string_of_int !counter in
    incr counter;
    sym

let rec pp_list ?(sep=", ") pp fmt = function
  | []      -> ()
  | [x]     -> pp fmt x
  | x :: xs -> Format.fprintf fmt "%a%s%a" pp x sep (pp_list pp) xs

let pp_array ?(sep=", ") pp fmt ary =
  for i = 0 to Array.length ary - 1 do
    if i > 0 then Format.pp_print_string fmt sep;
    pp fmt ary.(i)
  done

let rec pp_seq pp fmt = function
  | []      -> ()
  | [x]     -> pp fmt x
  | x :: xs -> Format.fprintf fmt "%a@\n%a" pp x (pp_seq pp) xs
