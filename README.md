fractal-ml
==========

fractals with ocaml and fsharp



julia_set.ml
------------

Requires lablGL library compiled with +glut.

	ocamlopt -I +lablGL lablgl.cmxa lablglut.cmxa julia_set.ml -o julia_set
	./julia_set
