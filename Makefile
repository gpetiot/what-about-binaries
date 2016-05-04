
SOURCES = hexa.mli hexa.ml \
	archi.mli archi.ml \
	endian.mli endian.ml \
	machine.mli machine.ml \
	program_header_type.mli program_header_type.ml \
	section_header_type.mli section_header_type.ml \
	elf_header.mli elf_header.ml

EXEC = obaf
EXECGUI = $(EXEC)_gui
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
WITHGRAPHICS=graphics.cma -cclib -lgraphics -cclib -L/usr/X11R6/lib -cclib -lX11
WITHUNIX =unix.cma -cclib -lunix
WITHSTR =str.cma -cclib -lstr
WITHNUMS =nums.cma -cclib -lnums
WITHTHREADS =threads.cma -cclib -lthreads
WITHDBM =dbm.cma -cclib -lmldbm -cclib -lndbm
LIBS=
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

all: $(EXEC)

opt: $(EXEC).opt

$(EXEC): $(SOURCES:.mli=.cmi) $(OBJS) main.cmo
	$(CAMLC) -o $@ $(LIBS) $(OBJS) main.cmo

$(EXEC).opt: $(SOURCES:.mli=.cmi) $(OPTOBJS) main.cmx
	$(CAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(OPTOBJS) main.cmx

%.cmo: %.ml
	$(CAMLC) -c -annot $<

%.cmi: %.mli
	$(CAMLC) -c -annot $<

%.cmx: %.ml
	$(CAMLOPT) -c -annot $<

.PHONY: clean
clean:
	@rm -f *.cm[iox] *.o *.annot *~ .*~ #*#
	@rm -f $(EXEC)
	@rm -f $(EXEC).opt
	@rm -f configure config.log
	@rm -rf autom4te.cache
	@rm -f webgui.byte webgui/webgui.js
	make clean -C tests

.PHONY: gui
gui: $(EXECGUI)

$(EXECGUI): $(SOURCES:.mli=.cmi) $(OBJS) gui/main.ml
	ocamlfind $(CAMLC) -annot -g -package lablgtk2 -linkpkg $(LIBS) $(OBJS) gui/main.ml -o $@

$(EXECGUI).opt: $(SOURCES:.mli=.cmi) $(OPTOBJS) gui/main.ml
	ocamlfind $(CAMLOPT) -annot -g -package lablgtk2 -linkpkg $(LIBS:.cma=.cmxa) $(OPTOBJS) gui/main.ml -o $@

.PHONY: webgui
webgui: webgui/webgui.js

webgui.byte: $(OBJS) webgui.ml
	ocamlfind $(CAMLC) -annot -package js_of_ocaml -syntax camlp4o -package js_of_ocaml.syntax -linkpkg -o $@ $(OBJS) $< webgui.ml

webgui/webgui.js: webgui.byte
	js_of_ocaml $< -o $@

.PHONY: tests
tests:
	make -C tests
