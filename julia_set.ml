(*
	Copyright (c) 2012, Davide Gessa (dakk)
	All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, are permitted provided 
	that the following conditions are met:

	-	Redistributions of source code must retain the above copyright notice, this list of conditions and the 
		following disclaimer.
	-	Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
		the following disclaimer in the documentation and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
	WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
	PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
	TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
	HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
	NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
	POSSIBILITY OF SUCH DAMAGE.
 *)
(* ocamlopt -I +lablGL lablgl.cmxa lablglut.cmxa julia_set.ml -o julia_set *)


(* Global state *)
let c_values = 
	[
		{	Complex.re=(-0.8);		Complex.im=(0.156)		};
		{	Complex.re=(0.285); 	Complex.im=(0.0)		};
		{	Complex.re=(-0.835); 	Complex.im=(-0.2321)	};
		{	Complex.re=(-0.70176); 	Complex.im=(-0.3842)	};
		{	Complex.re=(0.45); 		Complex.im=(0.1428)		};
		{	Complex.re=(0.285); 	Complex.im=(0.01)		};
	]
;;

let f z = (Complex.mul z z);;

(*
let f z =
	Complex.div 
		(Complex.sub 	(Complex.one) 
						(Complex.div 
							(Complex.pow z { Complex.re=6.0; Complex.im=0.0 }) 
							{ Complex.re=6.0; Complex.im=0.0 })) 
		(Complex.pow 	(Complex.sub z 
							(Complex.div 
								(Complex.pow z { Complex.re=2.0; Complex.im=0.0 }) 
								{ Complex.re=2.0; Complex.im=0.0 })) 
						{ Complex.re=2.0; Complex.im=0.0 })
*)

let c_i = ref 0;;
let c () = (List.nth c_values (!c_i));;
let zoom = ref (1.0, 1.0);;
let zfactor = 1.05;;

let wsize = (500, 500);;



(* Calculate the julia set iteration in a generic point z *)
let julia_calculate c (x,y) imax er =
	let c' = c in
	let z = {Complex.re=x /. (fst !zoom); Complex.im=y /. (snd !zoom)} in
	let rec step i z =
		if Complex.norm(z) > er || i >= imax then i
		else step (i+1) (Complex.add (f z) c')
	in step 0 z
;;


(* Calculate the mandelbrot set iteration in a generic point z *)
let mandelbrot_calculate c (x,y) imax er =
	let z = {Complex.re=x; Complex.im=y} in
	let rec step i z =
		if Complex.norm(z) > er || i >= imax then i
		else step (i+1) (Complex.add (f z) c)
	in step 0 z
;;



let reshape ~w ~h =
	let w = max 1 w and h = max 1 h in
	GlDraw.viewport 0 0 w h;
	GlMat.mode `projection;
	GlMat.load_identity ();
	
	GlMat.mode `modelview
;;


let rec draw_text (x, y) s =
	match String.length s with
	| 0 -> ()
	| n -> 
		GlPix.raster_pos x y ();
		Glut.bitmapCharacter Glut.BITMAP_8_BY_13 (Char.code s.[0]);
		draw_text (x +. 8.0, y) (String.sub s 1 ((String.length s) - 1))
;;
			
		  

let display () =
	GlClear.color (0.0, 0.0, 0.0);
	GlClear.clear [`color];

	(*GlMat.rotate ~angle:90.0 ~x:0.1 ();*)
	GlDraw.begins `points;
	for a = 0 to (fst wsize) - 1 do
		for b = 0 to (snd wsize) - 1 do
			let x = 2.0 *. float a /. float (fst wsize) -. (1.0) in
			let y = 2.0 *. float b /. float (snd wsize) -. (1.0) in

			let i = julia_calculate (c ()) (x,y) 255 2.0 in
			let f i = 0.5 +. 0.5 *. cos(float i *. 0.1) in
			let z =  (float i /. -200.0) in 
			GlDraw.color (0.0, 0.0, f(i + 32));
			GlDraw.vertex ~x ~y () (* ~z () *)
		done;
	done;
	GlDraw.ends ();

	
	(* Draw infos *)
	GlMat.mode `projection;
	GlMat.push ();
	GlMat.load_identity ();

	GlDraw.color (1.0, 1.0, 1.0);
	draw_text (100.0, 100.0) "Julia Set";

	GlMat.mode `modelview;
	GlMat.pop ();
	GlMat.mode `projection;
	GlMat.pop ();

	Glut.swapBuffers ()
;;


let key_handle ~key ~x ~y =
	match key with
	| 27 -> exit 0
	| _ -> ()
;;

let skey_handle ~key ~x ~y =
	match key with
	| Glut.KEY_RIGHT -> c_i := (!c_i + 1) mod (List.length c_values); display ();
	| Glut.KEY_LEFT -> c_i := if !c_i = 0 then (List.length c_values) - 1 else !c_i - 1; display ();
	| Glut.KEY_UP -> zoom := (fst !zoom *. zfactor, snd !zoom *. zfactor); display ();
	| Glut.KEY_DOWN -> zoom := (fst !zoom /. zfactor, snd !zoom /. zfactor); display ();
	| _ -> ()
;;

let () =
	Glut.init Sys.argv;
	Glut.initWindowSize ~w: (fst wsize) ~h: (snd wsize);
	Glut.createWindow ~title: "Julia set";
	Glut.displayFunc ~cb: display;  
	Glut.reshapeFunc ~cb: reshape;
	Glut.keyboardFunc ~cb: key_handle;
	Glut.specialUpFunc ~cb: skey_handle;
	Glut.mainLoop ()
;;
