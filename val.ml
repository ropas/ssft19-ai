(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Value Abstraction: Constant
*)

type t = BOT | CST of int | TOP

let bot : t = BOT
let top : t = TOP

let to_string : t -> string
= fun a ->
  match a with
  | BOT -> "."
  | TOP -> "T"
  | CST n -> string_of_int n

let le : t -> t -> bool
= fun a0 a1 -> a0 = BOT || a1 = TOP || a0 = a1

let is_bot : t -> bool = fun a -> a = BOT

let join : t -> t -> t
= fun a0 a1 ->
  match (a0, a1) with
  | (BOT, _) -> a1
  | (_, BOT) -> a0
  | (CST n1, CST n2) -> if n1 = n2 then CST n1 else TOP
  | _ -> TOP

let add : t -> t -> t
= fun a0 a1 ->
  match (a0, a1) with
  | (BOT, _) | (_, BOT) -> BOT
  | (CST n1, CST n2) -> CST (n1+n2)
  | _ -> TOP

let cst : int -> t
= fun n -> CST n 

(* refines v where v < w *)
let sat_v_less_w : t -> t -> t * t
= fun v w -> (v, v)

(* refines v where w < v *)
let sat_w_less_v : t -> t -> t * t
= fun v w -> (v, v)

(* refines v where v = w *)
let sat_v_eq_w : t -> t -> t * t
= fun v w -> (v, v)

