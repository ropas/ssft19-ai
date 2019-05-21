(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Main
*)

let input_file = ref "" 
let opt_pponly = ref false

let opts = 
  [("-pponly", Arg.Set opt_pponly, "only pretty-print input pgm")]

let args f = 
  if Sys.file_exists f then input_file := f
  else raise (Arg.Bad (f^": not found."))

let usage = "Usage: "^(Filename.basename Sys.argv.(0))^
  " <options> <file>\nOptions:"

let main () = 
  let _ = Arg.parse opts args usage in
  let ic =
    if !input_file = "" then
      raise (Failure "missing input file")
    else
      open_in !input_file
    in
  let lexbuf = Lexing.from_channel ic in
  let pgm_ = Parser.pgm Lexer.token lexbuf in
  let allv = Sil.get_allv pgm_ in
  let pgm = Sill.sil2sill pgm_ in
  let next = Sill.get_next pgm in

  let _ = print_string "- Program:\n" in 
  let _ = Sill.pp pgm in
  let _ = print_string "\n- Variables used: " in
  let _ = List.iter (fun x -> print_string (x^" ")) allv in
  let _ = print_newline () in
  let _ = print_string "\n- Next:\n" in
  let _ = Sill.pp_next pgm next in
  let _ = print_newline () in
  if !opt_pponly then
    ()
  else
    Ai.analysis pgm allv next

let _ = main ()
