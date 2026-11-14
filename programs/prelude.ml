let rec fequal x y = x =. y in
let rec fless x y = x <. y in
let rec fispos x = 0.0 <. x in
let rec fisneg x = x <. 0.0 in
let rec fiszero x = x =. 0.0 in
let rec fhalf x = x *. 0.5 in
let rec fsqr x = x *. x in
let rec fneg x = -.x in
let rec print_char c = print_byte c in
(* print (n / p in [0, 9]) and return (n % p). *)
let rec put_digit n p d =
  if n < p
  then (
    print_byte (48 + d);
    n)
  else put_digit (n - p) p (d + 1)
in
(* print (n / p) and return (n % p), where p is a power of 10. *)
let rec put_digits n p =
  if n < p
  then n
  else (
    let r = if p < 1000000000 then put_digits n ((p * 8) + (p * 2)) else n in
    put_digit r p 0)
in
let rec print_int n =
  if n < 0
  then (
    print_byte 45;
    print_int (-n))
  else if n = 0
  then print_byte 48
  else (
    let _ = put_digits n 1 in
    ())
in
let rec or' x y = if x then true else y in
(* read_int, read_float は入力が valid にあることに強く依存している…… *)
let rec read_int u =
  let rec aux in_token acc =
    let byte = read_byte () in
    if
      or'
        (byte = 32 (* ' ' *))
        (or' (byte = 9 (* '\t' *)) (or' (byte = 13 (* '\r' *)) (byte = 10 (* '\n' *))))
    then if in_token then acc else aux false acc
    else if byte = 45 (* '-' *)
    then -aux true acc
    else aux true ((acc * 8) + (acc * 2) + (byte - 48))
  in
  aux false 0
in
let rec read_float u =
  let rec aux in_token exp acc =
    let byte = read_byte () in
    if
      or'
        (byte = 32 (* ' ' *))
        (or' (byte = 9 (* '\t' *)) (or' (byte = 13 (* '\r' *)) (byte = 10 (* '\n' *))))
    then if in_token then acc, exp else aux false (-1) acc
    else if byte = 45 (* '-' *)
    then (
      let (acc', exp') = aux true (-1) acc in
      -acc', exp')
    else if byte = 46
    then (* '.' *)
      aux true 0 acc
    else aux true (if exp >= 0 then exp + 1 else -1) ((acc * 8) + (acc * 2) + (byte - 48))
  in
  let (acc, exp) = aux false (-1) 0 in
  let rec power n = if n = 0 then 1. else 10.0 *. power (n - 1) in
  if exp = -1 then float_of_int acc else float_of_int acc /. power exp
in
let rec cos x = 1.0 -. x*.x *. (0.5 +. x*.x *. (0.041666667 -. x*.x *. 0.001371742))   in
let rec sin x = x *. (1.0 -. x*.x *. (0.166666667 +. x*.x *. 0.008333333)) in
let rec atan x = x *. (1.0 -. x*.x *. (0.333333333 +. x*.x *. (0.2 -. x*.x *. 0.142857143))) in
let rec _dummy_for_infer f = float_of_int (read_int ()) +. read_float () +. f +. cos f +. sin f +. atan f in
