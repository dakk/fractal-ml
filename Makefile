$(FRACT): julia_set.ml
	ocamlc -c -I +lablGL lablgl.cmxa lablglut.cmxa julia_set.ml

all: $(FRACT)

clean:
	'rm' *.cmo *.cmi
