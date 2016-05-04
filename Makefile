
SOURCES = src/hexa.mli src/hexa.ml \
	src/archi.mli src/archi.ml \
	src/endian.mli src/endian.ml \
	src/machine.mli src/machine.ml \
	src/program_header_type.mli src/program_header_type.ml \
	src/section_header_type.mli src/section_header_type.ml \
	src/elf_header.mli src/elf_header.ml

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

$(EXEC): $(SOURCES:.mli=.cmi) $(OBJS) src/main.cmo
	$(CAMLC) -I src -o $@ $(LIBS) $(OBJS) src/main.cmo

$(EXEC).opt: $(SOURCES:.mli=.cmi) $(OPTOBJS) src/main.cmx
	$(CAMLOPT) -I src -o $@ $(LIBS:.cma=.cmxa) $(OPTOBJS) src/main.cmx

%.cmo: %.ml
	$(CAMLC) -I src -c -annot $<

%.cmi: %.mli
	$(CAMLC) -I src -c -annot $<

%.cmx: %.ml
	$(CAMLOPT) -I src -c -annot $<

.PHONY: clean
clean:
	@rm -f src/*.cm[iox] src/*.o src/*.annot src/*~ *~ .*~ #*#
	@rm -f $(EXEC) $(EXEC).opt $(EXECGUI) $(EXECGUI).opt
	@rm -f configure config.log
	@rm -rf autom4te.cache
	@rm -f src/webgui.byte src/webgui/webgui.js
	make clean -C tests

.PHONY: gui
gui: $(EXECGUI)

$(EXECGUI): $(SOURCES:.mli=.cmi) $(OBJS) src/gui/main.ml
	ocamlfind $(CAMLC) -I src -annot -g -package lablgtk2 -linkpkg $(LIBS) $(OBJS) src/gui/main.ml -o $@

$(EXECGUI).opt: $(SOURCES:.mli=.cmi) $(OPTOBJS) src/gui/main.ml
	ocamlfind $(CAMLOPT) -I src -annot -g -package lablgtk2 -linkpkg $(LIBS:.cma=.cmxa) $(OPTOBJS) src/gui/main.ml -o $@

.PHONY: webgui
webgui: src/webgui/webgui.js

src/webgui.byte: $(OBJS) src/webgui.ml
	ocamlfind $(CAMLC) -I src -annot -package js_of_ocaml -syntax camlp4o -package js_of_ocaml.syntax -linkpkg -o $@ $(OBJS) $< src/webgui.ml

src/webgui/webgui.js: src/webgui.byte
	js_of_ocaml $< -o $@

.PHONY: tests
tests:
	make -C tests
