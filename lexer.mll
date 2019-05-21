(*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Lexer
*)

{
open Parser
exception LexError

let keywords = [ ("skip", SKIP); ("input", INPUT); ("if", IF); ("while", WHILE); ("goto", GOTO);
  ("true", TRUE); ("false", FALSE) ]
let keyword_tbl = Hashtbl.create 31
let uncurry f (a,b) = f a b 
let _ = List.iter (uncurry (Hashtbl.add keyword_tbl)) keywords
}


let id  = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let num = ['-']* ['0'-'9']+
let ws  = ['\n' '\r' '\t' ' ']

rule token = parse
  | id as id   { try Hashtbl.find keyword_tbl id with _ -> ID id }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "+"        { PLUS }
  | "<"        { LESS }
  | "="        { EQ }
  | ";"        { SEQ }
  | ":="       { ASSIGN }
  | num as num { INT (int_of_string num) }
  | ws         { token lexbuf }
  | eof        { EOF }
  | "(*"       { comment lexbuf }
  | _          { raise LexError }
 
and comment = parse
  | "*)"       { token lexbuf }
  | eof        { raise LexError }
  | _          { comment lexbuf }
