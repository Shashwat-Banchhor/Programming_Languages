main : main.mll parser.mly cal.ml
	ocamlc -c asix.ml	
	ocamllex main.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli 
	ocamlc -c main.ml
	ocamlc -c parser.ml
	ocamlc -c cal.ml
	ocamlc -o cal asix.cmo main.cmo parser.cmo cal.cmo
