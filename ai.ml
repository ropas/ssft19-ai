(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Abstract Interpretation in Transitional Style
*)

type state = Sill.label -> Mem.t (* state abstraction *)
type worklist = Sill.label list 

let empty_state : state = fun x -> Mem.bot

let state_store : state -> Sill.label -> Mem.t -> state
= fun s l m ->
  fun x -> if x = l then m else s x

let pp_state : state -> Sill.label -> (Sill.vid list) -> state -> unit
= fun s maxl allv s_prev ->
  let _ = Printf.printf "-- state:\n" in
  let rec iter i =
    if i > maxl then ()
    else
      let m = s i in
      let m_prev = s_prev i in
      let _ = if Mem.is_bot allv m then () else begin
        let _ = if not (Mem.le allv m m_prev) then
          print_string ">>"
        else
          print_string ".."
        in
        let _ = Printf.printf " %d: " i in
        Mem.pp allv m
      end in
      iter (i+1)
  in
  iter 0

let pp_worklist : Sill.label list -> Sill.label list -> unit
= fun wl wl_new ->
  Printf.printf "-- worklist: ";
  List.iter (fun l -> Printf.printf "%d " l) wl;
  match wl_new with
  | [] -> print_newline ()
  | _ ->
    Printf.printf ">> ";
    List.iter (fun l -> Printf.printf "%d " l) wl_new;
    Printf.printf "<<\n" 

let labels_of_val_itv : Val.t -> Sill.label -> Sill.label list
= fun v maxl ->
  let v = Val.meet v (Val.V (Val.INT 0, Val.INT maxl)) in
  let rec acclst n m = if n > m then [] else n::(acclst (n+1) m) in
  match v with
  | Val.BOT -> []
  | Val.V (Val.INT i, Val.INT j) -> acclst i j
  | _ -> raise (Failure "labels_of_val_itv")

let cond_itv : Sill.bexp -> Mem.t -> (Mem.t * Mem.t)
= fun b m ->
  match b with
  | _ -> (m, m) (* TODO *)

let labels_of_val : Val.t -> Sill.label -> Sill.label list
= labels_of_val_itv

let cond : Sill.bexp -> Mem.t -> (Mem.t * Mem.t)
= cond_itv

let rec expr : Sill.exp -> Mem.t -> Val.t
= fun e m ->
  match e with
  | Sil.NUM n -> Val.cst n
  | Sil.VAR v -> m v
  | Sil.ADD (e1, e2) -> Val.add (expr e1 m) (expr e2 m)

let rec step : Sill.cmdl -> Sill.label -> Sill.next -> Sill.nextlabel -> Mem.t
               -> (Sill.label * Mem.t) list
= fun (l, c) maxl next lnext m ->
  match c with
  | Sill.SKIP -> [ (Sill.next_label lnext, m) ]
  | Sill.SEQ (cl1, cl2) -> step cl1 maxl next (next (fst cl1)) m
  | Sill.ASSIGN (v, e) ->
    [ (Sill.next_label lnext, Mem.write v (expr e m) m) ]
  | Sill.INPUT v -> [ (Sill.next_label lnext, Mem.write v Val.top m) ] 
  | Sill.IF (b, cl1, cl2) ->
    let (mt, mf) = cond b m in
    [ (Sill.next_true lnext, mt);
      (Sill.next_false lnext, mf) ]
  | Sill.WHILE (b, cl) ->
    let (mt, mf) = cond b m in
    [ (Sill.next_true lnext, mt);
      (Sill.next_false lnext, mf) ]
  | Sill.GOTO e ->
    List.map (fun l -> (l, m)) (labels_of_val (expr e m) maxl)

let rec repeat : Sill.pgm -> (Sill.vid list) -> Sill.next -> state
                 -> worklist -> state
= fun p allv next state wlst ->
  let (_, maxl) = p in
  match wlst with
  | [] -> state
  | l::tl -> if l == maxl then repeat p allv next state tl else begin
    let _ = Printf.printf "\n== step on label %d ==\n" l in
    let c = Sill.cmd_of_label p l in
    let m = state l in
    let lnext = next l in
    let posts = step (l, c) maxl next lnext m in 
    let (state_new, wlst_new) = List.fold_left
      (fun (state, wlst) (lnext, post) ->
        let old_post = state lnext in
          if not (Mem.le allv post old_post) then
            let m_updated = 
              if lnext == maxl then Mem.join post old_post else
              match c with
              | Sill.GOTO _ -> Mem.widen post old_post
              | _ -> begin
                match Sill.cmd_of_label p lnext with
                | Sill.WHILE _ -> Mem.widen post old_post
                | _ -> Mem.join post old_post
              end
            in
            (state_store state lnext m_updated, lnext::wlst)
          else
            (state, wlst)
      ) (state, []) posts in
    let _ = print_endline "afterwards," in
    let _ = pp_worklist tl wlst_new in
    let _ = pp_state state_new maxl allv state in
    repeat p allv next state_new (tl@wlst_new)
    end 

let analysis : Sill.pgm -> (Sill.vid list) -> Sill.next -> unit
= fun p allv next -> 
  let (_, maxl) = p in
  let init_state = empty_state in
  let init_worklist = [0] in
  let _ = print_endline "\n== analysis starts with ==" in
  let _ = pp_worklist [] init_worklist in
  let _ = pp_state init_state maxl allv empty_state in 
  let lfp_state = repeat p allv next init_state init_worklist in
  let _ = print_endline "\n==== ANALYSIS RESULT ====" in
  let _ = pp_state lfp_state maxl allv empty_state in
  ()
