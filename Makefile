OBJS=lexer.cmo parser.cmo sil.cmo sill.cmo val.cmo mem.cmo ai.cmo main.cmo

all: $(OBJS)
	ocamlc -o run $(OBJS)

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo .depend

parser.cmo: sil.cmo parser.cmi

parser.ml: sil.cmo parser.mly

lexer.cmo: parser.cmo

.SUFFIXES: .ml .mli .cmo .cmi .mly .mll

.mli.cmi:
	ocamlc -c $<

.ml.cmo:
	ocamlc -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc $<

.mly.mli:
	ocamlyacc $<

.depend:
	ocamldep *.mli *.ml > .depend

.PHONY: clean

include .depend
