/*
  SSFT '19,
  Intro to Static Analysis from an Abstract Interpretation Perspective,
  Lab Session
  Jinyung Kim (jykim@ropas.snu.ac.kr)

  Parser
*/

%token <int> INT
%token <string> ID
%token SKIP
%token IF
%token WHILE
%token TRUE 
%token FALSE
%token LPAREN
%token RPAREN
%token PLUS
%token LESS
%token EQ
%token SEQ
%token ASSIGN
%token INPUT
%token GOTO
%token EOF

%start pgm
%type <Sil.pgm> pgm
%type <Sil.cmd> cmd
%type <Sil.bexp> bexp
%type <Sil.exp> exp

%left SEQ
%left PLUS
%nonassoc IF WHILE

%%

pgm:
| cmd EOF { $1 }
;

cmd:
| LPAREN cmd RPAREN { $2 }
| SKIP { Sil.SKIP }
| ID ASSIGN exp { Sil.ASSIGN($1, $3) }
| INPUT ID { Sil.INPUT $2 }
| cmd SEQ cmd { Sil.SEQ($1, $3) }
| IF bexp cmd cmd { Sil.IF($2, $3, $4) }
| WHILE bexp cmd { Sil.WHILE($2, $3) }
| GOTO exp { Sil.GOTO $2 }
;

bexp:
| TRUE { Sil.TRUE }
| FALSE { Sil.FALSE }
| LPAREN bexp RPAREN { $2 }
| exp LESS exp { Sil.LESS($1, $3) }
| exp EQ exp { Sil.EQ($1, $3) }
;

exp:
| INT { Sil.NUM $1 }
| LPAREN exp RPAREN { $2 }
| ID { Sil.VAR $1 }
| exp PLUS exp { Sil.ADD($1, $3) }
;


