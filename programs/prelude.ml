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
let rec read_int u =
  let f0 = read_byte () in
  let f1 = read_byte () in
  let f2 = read_byte () in
  let f3 = read_byte () in
  (f0 * 16777216) + (f1 * 65536) + (f2 * 256) + f3
in
let rec read_float u = coerce (read_int ()) in
let rec cos x = 1.0 in
let rec sin x = x in
let rec atan x = x in
let rec _dummy_for_infer f = 
  read_float () +. f +. cos f +. sin f +. atan f
in
