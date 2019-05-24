(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Value Abstraction: Interval
*)

type t = BOT | V of int_t * int_t
and int_t = INT of int | MINF | PINF

let int_to_string : int_t -> string
= fun a ->
  match a with
  | INT i -> string_of_int i
  | PINF -> "+oo"
  | MINF -> "-oo"

let int_le : int_t -> int_t -> bool
= fun a0 a1 ->
  match (a0, a1) with
  | (MINF, _) -> true
  | (_, PINF) -> true
  | (INT i, INT j) -> i <= j
  | _ -> false

let int_e : int_t -> int_t -> bool
= fun a0 a1 ->
  match (a0, a1) with
  | (MINF, MINF) -> true
  | (PINF, PINF) -> true
  | (INT i, INT j) -> i = j
  | _ -> false

let int_l : int_t -> int_t -> bool = fun a0 a1 -> (int_le a0 a1) && not (int_e a0 a1)

let int_min : int_t -> int_t -> int_t
= fun a0 a1 -> if int_le a0 a1 then a0 else a1

let int_max : int_t -> int_t -> int_t
= fun a0 a1 -> if int_le a0 a1 then a1 else a0

let int_min_widen : int_t -> int_t -> int_t
= fun a0 a1 -> if int_l a0 a1 then MINF else a1

let int_max_widen : int_t -> int_t -> int_t
= fun a0 a1 -> if int_le a0 a1 then a1 else PINF

let int_add : int_t -> int_t -> int_t
= fun a0 a1 ->
  match (a0, a1) with
  | (INT i, INT j) -> INT (i+j)
  | (PINF, MINF) | (MINF, PINF) -> raise (Failure "int_add")
  | (PINF, _) | (_, PINF) -> PINF
  | (MINF, _) | (_, MINF) -> MINF

let bot : t = BOT
let top : t = V (MINF, PINF)

let to_string : t -> string
= fun a ->
  match a with
  | BOT -> "."
  | V (l, u) -> "["^(int_to_string l)^", "^(int_to_string u)^"]"

let is_bot : t -> bool
= fun a ->
  match a with
  | BOT -> true
  | V (l, u) -> l = PINF || u = MINF || not (int_le l u)

let normalize_bot : t -> t
= fun a -> if is_bot a then BOT else a

let le : t -> t -> bool
= fun a0 a1 ->
  let a0 = normalize_bot a0 in
  let a1 = normalize_bot a1 in
  match (a0, a1) with
  | (BOT, _) -> true
  | (_, BOT) -> false
  | (V (l0, u0), V (l1, u1)) -> int_le l1 l0 && int_le u0 u1

let join : t -> t -> t
= fun a0 a1 ->
  let a0 = normalize_bot a0 in
  let a1 = normalize_bot a1 in
  match (a0, a1) with
  | (BOT, _) -> a1
  | (_, BOT) -> a0
  | (V (l0, u0), V (l1, u1)) -> V (int_min l0 l1, int_max u0 u1)  

let widen : t -> t -> t
= fun a0 a1 ->
  let a0 = normalize_bot a0 in
  let a1 = normalize_bot a1 in
  match (a0, a1) with
  | (BOT, _) -> a1
  | (_, BOT) -> a0
  | (V (l0, u0), V (l1, u1)) -> V (int_min_widen l0 l1, int_max_widen u0 u1)  

let meet : t -> t -> t
= fun a0 a1 ->
  let a0 = normalize_bot a0 in
  let a1 = normalize_bot a1 in
  match (a0, a1) with
  | (BOT, _) -> a1
  | (_, BOT) -> a0
  | (V (l0, u0), V (l1, u1)) ->
    normalize_bot (V (int_max l0 l1, int_min u0 u1))

let add : t -> t -> t
= fun a0 a1 ->
  let a0 = normalize_bot a0 in
  let a1 = normalize_bot a1 in
  match (a0, a1) with
  | (BOT, _) | (_, BOT) -> BOT
  | (V (l0, u0), V (l1, u1)) -> V (int_add l0 l1, int_add u0 u1)  

let cst : int -> t
= fun n -> V (INT n, INT n)

(* refines v where v < w *)
let sat_v_less_w : t -> t -> t * t
= fun v w -> (v, v)

(* refines v where w < v *)
let sat_w_less_v : t -> t -> t * t
= fun v w -> (v, v)

(* refines v where v = w *)
let sat_v_eq_w : t -> t -> t * t
= fun v w -> (v, v)
