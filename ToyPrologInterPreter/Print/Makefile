main : print_main.mll print_parser.mly print_cal.ml
	ocamlc -c asix.ml	
	ocamllex print_main.mll
	ocamlyacc print_parser.mly
	ocamlc -c print_parser.mli 
	ocamlc -c print_main.ml
	ocamlc -c print_parser.ml
	ocamlc -c print_cal.ml
	ocamlc -o print_cal asix.cmo print_main.cmo print_parser.cmo print_cal.cmo
