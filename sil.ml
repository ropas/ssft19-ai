(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)
  
  Simple Imperative Language
*)

type vid = string
type exp =
  | NUM of int               (* n // int *)
  | VAR of vid               (* v // string *)
  | ADD of exp * exp         (* e + e *)
type bexp =
  | TRUE                     (* true *)
  | FALSE                    (* false *)
  | LESS of exp * exp        (* e < e *)
  | EQ of exp * exp          (* e = e *)
type cmd =
  | SKIP                     (* skip *)
  | SEQ of cmd * cmd         (* c ; c *)
  | ASSIGN of vid * exp      (* v := e *)
  | INPUT of vid             (* input v *)
  | IF of bexp * cmd * cmd   (* if b c c *)
  | WHILE of bexp * cmd      (* while b c *)
  | GOTO of exp              (* goto e *)
type pgm = cmd

let indent = "    "

let rec p_i : int -> unit
= fun i -> if i = 0 then () else (print_string indent; p_i (i-1))

let rec p_s : string -> int -> unit
= fun s i -> p_i i; print_endline s

let rec s_e : exp -> string
= fun e ->
  match e with
  | NUM n -> (string_of_int n)
  | VAR v -> v
  | ADD (e1, e2) -> (s_e e1)^" + "^(s_e e2)

let rec s_b : bexp -> string
= fun b ->
  match b with
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | LESS (e1, e2) -> 
    "("^(s_e e1)^" < "^(s_e e2)^")"
  | EQ (e1, e2) -> 
    "("^(s_e e1)^" = "^(s_e e2)^")"
(*
let rec p_c : cmd -> int -> unit
= fun c i ->
  match c with
  | SKIP -> p_s "SKIP" i
  | SEQ (c1, c2) ->
    p_s "(" i; p_c c1 (i+1); p_s ";" i; p_c c2 (i+1); p_s ")" i
  | ASSIGN (v, e) ->
    p_s (v^" := "^(s_e e)) i
  | IF (b, c1, c2) ->
    p_s ("(IF "^(s_b b)) i; p_c c1 (i+1); p_c c2 (i+1); p_s ")" i
  | WHILE (b, c) ->
    p_s ("(WHILE "^(s_b b)) i; p_c c (i+1); p_s ")" i

let pp : pgm -> unit
= fun p -> p_c p 0
*)
let rec allv_e : exp -> vid list
= fun e -> 
  match e with
  | NUM _ -> []
  | VAR v -> [v]
  | ADD (e1, e2) -> (allv_e e1) @ (allv_e e2)

let rec allv_b : bexp -> vid list
= fun b ->
  match b with
  | TRUE | FALSE -> []
  | LESS (e1, e2) -> (allv_e e1) @ (allv_e e2)
  | EQ (e1, e2) -> (allv_e e1) @ (allv_e e2)

let rec allv_c : cmd -> vid list
= fun c ->
  match c with
  | SKIP -> []
  | SEQ (c1, c2) -> (allv_c c1) @ (allv_c c2)
  | ASSIGN (v, e) -> v::(allv_e e)
  | INPUT v -> [v]
  | IF (b, c1, c2) -> (allv_b b) @ (allv_c c1) @ (allv_c c2)
  | WHILE (b, c) -> (allv_b b) @ (allv_c c)
  | GOTO e -> allv_e e

let get_allv : pgm -> vid list
= fun p -> List.sort_uniq String.compare (allv_c p)


(*

let p0 =
  SEQ(ASSIGN("x", NUM 1),
      SEQ(ASSIGN("y", NUM 2),
          WHILE(LESS(VAR "x", NUM 10),
                ASSIGN("x", ADD(VAR "x", NUM 1)))))

let _ = pp p0

*)


