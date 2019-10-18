NAME=gardening

SOURCES=gardening.ml

OBJS=$(SOURCES:.ml=.cmo)
OPTOBJS=$(SOURCES:.ml=.cmx)

CAMLC=ocamlc
CAMLOPT=ocamlopt
CAMLDEP=ocamldep

LIBS=

.PHONY: all opt byt clean run

all: $(NAME)

$(NAME): opt byt
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

clean:
	rm $(OBJS) $(OPTOBJS) $(NAME) $(NAME).byt $(NAME).opt

run: $(NAME)
	$(NAME)
