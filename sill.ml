(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)
  
  Simple Imperative Language, Labelled
*)

type label = int
type vid = Sil.vid
type exp = Sil.exp
type bexp = Sil.bexp
type cmdl = label * cmd
and cmd =
  | SKIP
  | SEQ of cmdl * cmdl
  | ASSIGN of vid * exp
  | INPUT of vid
  | IF of bexp * cmdl * cmdl
  | WHILE of bexp * cmdl
  | GOTO of exp

type pgm = cmdl * label

type nextlabel = 
  | N of label
  | TF of label * label
  | DYN

type next = label -> nextlabel

let rec labelling : Sil.pgm -> label -> pgm
= fun p l0 ->
  match p with
  | Sil.SKIP -> ((l0, SKIP), l0+1)
  | Sil.SEQ (c1, c2) ->
    let (cl1, l1) = labelling c1 (l0+1) in
    let (cl2, l2) = labelling c2 l1 in
    ((l0, SEQ (cl1, cl2)), l2)
  | Sil.ASSIGN (v, e) -> ((l0, ASSIGN (v, e)), l0+1)
  | Sil.INPUT v -> ((l0, INPUT v), l0+1)
  | Sil.IF (b, c1, c2) ->
    let (cl1, l1) = labelling c1 (l0+1) in
    let (cl2, l2) = labelling c2 l1 in
    ((l0, IF (b, cl1, cl2)), l2)
  | Sil.WHILE (b, c) ->
    let (cl, l) = labelling c (l0+1) in
    ((l0, WHILE (b, cl)), l)
  | Sil.GOTO e -> ((l0, GOTO e), l0+1)

let sil2sill : Sil.pgm -> pgm
= fun p -> labelling p 0

let rec pp_cl : cmdl -> int -> unit
= fun (l, c) i ->
  let lstr = (string_of_int l)^": " in
  let lstr_e = " // end of "^(string_of_int l) in
  match c with
  | SKIP -> Sil.p_s (lstr^"SKIP") i
  | SEQ (cl1, cl2) ->
    Sil.p_s (lstr^"(") i; pp_cl cl1 (i+1);
    Sil.p_s ";" i; pp_cl cl2 (i+1);
    Sil.p_s (")"^lstr_e) i
  | ASSIGN (v, e) -> Sil.p_s (lstr^v^" := "^(Sil.s_e e)) i
  | INPUT v -> Sil.p_s (lstr^"INPUT "^v) i
  | IF (b, cl1, cl2) ->
    Sil.p_s (lstr^"(IF "^(Sil.s_b b)) i;
    pp_cl cl1 (i+1); pp_cl cl2 (i+1); Sil.p_s (")"^lstr_e) i
  | WHILE (b, cl) ->
    Sil.p_s (lstr^"(WHILE "^(Sil.s_b b)) i;
    pp_cl cl (i+1); Sil.p_s (")"^lstr_e) i
  | GOTO e -> Sil.p_s (lstr^"GOTO "^(Sil.s_e e)) i

let pp : pgm -> unit
= fun (cl, maxl) ->
  pp_cl cl 0;
  Printf.printf "%d: END (# Labels: %d)\n" maxl (maxl+1)

let get_nextlabel : cmdl -> nextlabel
= fun x -> N (fst x)

let rec collect_next : cmdl -> label -> next -> next
= fun (l, c) lnext n ->
  match c with
  | SEQ (cl1, cl2) ->
    let n' = fun x -> if x = l then get_nextlabel cl1 else n x in
    let n'' = collect_next cl1 (fst cl2) n' in
    collect_next cl2 lnext n'' 
  | IF (_, cl1, cl2) ->
    let n' = fun x -> if x = l then TF (fst cl1, fst cl2) else n x in
    let n'' = collect_next cl1 lnext n' in
    collect_next cl2 lnext n''
  | WHILE (_, cl) ->
    let n' = fun x -> if x = l then TF (fst cl, lnext) else n x in
    collect_next cl l n'
  | GOTO _ -> fun x -> if x = l then DYN else n x
  | _ -> fun x -> if x = l then N lnext else n x

let rec get_next : pgm -> next
= fun (cl, maxl) ->
  collect_next cl maxl (fun x -> DYN)

let next_label : nextlabel -> label
= fun x ->
  match x with
  | N l -> l
  | _ -> raise (Failure "next_label: should be N")

let next_true : nextlabel-> label
= fun x ->
  match x with
  | TF (l, _) -> l
  | _ -> raise (Failure "next_true: should be TF")

let next_false : nextlabel -> label
= fun x ->
  match x with
  | TF (_, l) -> l
  | _ -> raise (Failure "next_true: should be TF")

let cmd_of_label : pgm -> label -> cmd
= fun (cl, _) l0 ->
  let rec find : cmdl -> (cmd option)
  = fun (l, c) ->
    if l = l0 then Some c else
    match c with
    | SEQ (cl1, cl2) ->
      let c1 = find cl1 in
      if c1 = None then find cl2 else c1
    | IF (_, cl1, cl2) ->
      let c1 = find cl1 in
      if c1 = None then find cl2 else c1
    | WHILE (_, cl) -> find cl
    | _ -> None
  in
  match find cl with
  | Some c -> c
  | _ -> raise (Failure "cmd_of_label: cmd not found")

let pp_next : pgm -> next -> unit
= fun (_, maxl) n ->
  let rec iter i =
    if i == maxl then ()
    else begin
      let _ = Printf.printf "%d: " i in
      let _ = match n i with 
      | N l -> Printf.printf "%d\n" l 
      | TF (lt, lf) -> Printf.printf "T %d, F %d\n" lt lf
      | DYN ->  print_endline "DYN" in
      iter (i+1) 
    end
  in
  iter 0 
