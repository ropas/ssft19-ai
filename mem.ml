(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Abstract Memory
*)

type t = Sill.vid -> Val.t

let bot : t = fun x -> Val.bot

let is_bot : Sill.vid list -> t -> bool
= fun allv m ->
  List.for_all (fun v -> Val.is_bot (m v)) allv

let write : Sill.vid -> Val.t -> t -> t
= fun v a m ->
  fun x -> if x = v then a else m x

let le : Sill.vid list -> t -> t -> bool
= fun allv m1 m2 ->
  List.for_all (fun v -> Val.le (m1 v) (m2 v)) allv

let join : t -> t -> t
= fun m1 m2 ->
  fun x -> Val.join (m1 x) (m2 x)

let widen : t -> t -> t
= fun m1 m2 ->
  fun x -> Val.widen (m1 x) (m2 x)

let pp : Sill.vid list -> t -> unit
= fun allv m ->
  print_string "{ ";
  List.iter
    (fun x ->
      let v = m x in
      if Val.is_bot v then ()
      else print_string (x^":"^(Val.to_string v)^" ")
    ) allv;
  print_endline "}"
      
