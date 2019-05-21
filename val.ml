(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Value Abstraction: Sign
*)

type t = BOT | TOP | NNEG | NEG

let bot : t = BOT
let top : t = TOP

let to_string : t -> string
= fun a ->
  match a with
  | BOT -> "."
  | TOP -> "T"
  | NNEG -> "0+"
  | NEG -> "-"

let le : t -> t -> bool
= fun a0 a1 -> a0 = BOT || a1 = TOP || a0 = a1

let is_bot : t -> bool = fun a -> a = BOT

let join : t -> t -> t
= fun a0 a1 ->
  match (a0, a1) with
  | (BOT, _) -> a1
  | (_, BOT) -> a0
  | (NNEG, NNEG) -> NNEG
  | (NEG, NEG) -> NEG
  | _ -> TOP

let add : t -> t -> t
= fun a0 a1 ->
  match (a0, a1) with
  | (BOT, _) | (_, BOT) -> BOT
  | (NNEG, NNEG) -> NNEG
  | (NEG, NEG) -> NEG
  | _ -> TOP

let cst : int -> t
= fun n -> if n < 0 then NEG else NNEG

(* refines v where v < w *)
let sat_v_less_w : t -> t -> t * t
= fun v w -> (v, v)

(* refines v where w < v *)
let sat_w_less_v : t -> t -> t * t
= fun v w -> (v, v)

(* refines v where v = w *)
let sat_v_eq_w : t -> t -> t * t
= fun v w -> (v, v)

