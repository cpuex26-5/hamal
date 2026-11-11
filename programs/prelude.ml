let rec fequal x y = x =. y in
let rec fless x y = x <. y in
let rec fispos x = 0.0 <. x in
let rec fisneg x = x <. 0.0 in
let rec fiszero x = x =. 0.0 in
let rec fhalf x = x *. 0.5 in
let rec fsqr x = x *. x in
let rec fneg x = -.x in
let rec print_char c =  print_byte c in
let rec print_int n =  
  let n0 = n / 16777216 in
  let n1 = (n - n0 * 16777216) / 65536 in
  let n2 = (n - n0 * 16777216 - n1 * 65536) / 256 in
  let n3 = n - n0 * 16777216 - n1 * 65536 - n2 * 256 in
  print_byte n0;
  print_byte n1;
  print_byte n2;
  print_byte n3
in
let rec read_int u =
  let f0 = read_byte () in
  let f1 = read_byte () in
  let f2 = read_byte () in
  let f3 = read_byte () in
  f0 * 16777216 + f1 * 65536 + f2 * 256 + f3
in
let rec read_float u = float_of_int (read_int ())
in
let rec cos x = 1.0 in
let rec sin x = x in
let rec atan x = x in
