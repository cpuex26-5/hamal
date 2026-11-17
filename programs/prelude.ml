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
let rec read_float u = coerce (read_int ())
in
let pi = 3.1415925 in
let pi2 = pi *. 2.0 in
let tau = pi /. 2.0 in
let rec cos x =
  let rec kernel x = 1.0 -. x*.x *. (0.5 -. x*.x *. (0.0416667 -. x*.x *. (0.001388889 -. x*.x*. 0.000024802))) in
  let x = fabs x in
  let x = x -. floor (x /. pi2) *. pi2 in
  let x = if x <. pi then x else pi2 -. x in
  let (coef, x) = if x <. tau then (1.0, x) else (-1.0, pi -. x) in
  coef *. kernel x 
  (* let c = kernel (x /. 2.0) in *)
  (* coef *. (2.0 *. fsqr c -. 1.0) *)
in
let rec sin x = cos (tau -. x) in
let rec tan x = 
  let rec kernel x = x *. (1.0 +. x*.x *. (0.33333333 +. x*.x*. (0.133333333 +. x*.x*. 0.053968254))) in
  let c0 = if x >. 0.0 then 1.0 else -1.0 in
  let x = fabs x in
  let x = x -. (floor (x /. pi)) *. pi in
  let x = if x <. tau then x else x -. pi in
  let c1 = if x >. 0.0 then 1.0 else -1.0 in
  let x = fabs x in
  let t = if  x <. tau /. 2.0 then kernel x else 1.0 /. (kernel (tau -. x)) in
  c0 *. c1 *. t
in
let rec atan x =
  let rec kernel x = x *. (1.0 -. x*.x *. (0.33333333 -. x*.x *. (0.2 -. x*.x *. 0.142857143))) in
  let coef = if x >. 0.0 then 1.0 else -1.0 in
  let x = fabs x in
  let a = if x <. 1.0 then kernel x else tau -. kernel (1.0 /. x) in
  coef *. a
in
let rec _dummy_for_infer f = read_float () +. f in
