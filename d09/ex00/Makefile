NAME=test-watchover

SOURCES=watchover.ml main.ml
INTERFACES=watchover.mli

OBJS=$(SOURCES:.ml=.cmo)
OPTOBJS=$(SOURCES:.ml=.cmx)

CAMLC=ocamlc
CAMLOPT=ocamlopt
CAMLDEP=ocamldep

LIBS=unix.cma
OPTLIBS=unix.cmxa

.PHONY: all opt byt clean run

all: $(NAME)

$(NAME): opt byt
	ln -sf $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

$(NAME).byt: $(OBJS) .depend
	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS) .depend
	$(CAMLOPT) -o $(NAME).opt $(OPTLIBS) $(OPTOBJS)

%.cmo: %.ml
	$(CAMLC) -c $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmx: %.ml
	$(CAMLOPT) -c $<

.depend: $(SOURCES) $(INTERFACES)
	$(CAMLDEP) $(SOURCES) > .depend

clean:
	rm -f	$(OBJS) $(OPTOBJS) $(SOURCES:.ml=.o) $(SOURCES:.ml=.cmi)
	rm -f $(NAME) $(NAME).byt $(NAME).opt
	rm -f .depend

run: $(NAME)
	./$(NAME)

include .depend
