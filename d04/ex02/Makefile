NAME=card-test

SOURCES=Card.ml main.ml

OBJS=$(SOURCES:.ml=.cmo)
OPTOBJS=$(SOURCES:.ml=.cmx)

CAMLC=ocamlc
CAMLOPT=ocamlopt
CAMLDEP=ocamldep

LIBS=

.PHONY: all opt byt clean run

all: $(NAME)

$(NAME): opt byt .depend
	ln -sf $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(LIBS) $(OPTOBJS)

%.cmo: %.ml
	$(CAMLC) -c $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmx: %.ml
	$(CAMLOPT) -c $<

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

clean:
	rm -f	$(OBJS) $(OPTOBJS) $(SOURCES:.ml=.o) $(SOURCES:.ml=.cmi)
	rm -f $(NAME) $(NAME).byt $(NAME).opt
	rm -f .depend

run: $(NAME)
	./$(NAME)

include .depend
