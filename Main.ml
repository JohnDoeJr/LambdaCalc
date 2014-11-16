type term = Var of string | Lambda of string * term | Application of term * term;;

let rec print_term t =
	match t with
		| Var v -> print_string v
		| Lambda (v, t) -> print_string("\\" ^ v ^ "."); print_term t
		| Application (t1, t2) -> 
			print_string("("); print_term t1; print_string(")");
			print_string("("); print_term t2; print_string(")");;

let x = Application(Lambda("x", Lambda("y", Lambda("z", Application(Application(Var "x", Var "z"), Application(Var "y", Var "z"))))), Lambda("x", Lambda("y", Var "x")));;
let y = Application(Lambda("x", Lambda("y", Lambda("z", Application(Var "x", Application(Var "z", Application(Var "y", Var "z")))))), Lambda("x", Lambda("y", Var "x")));;
let z = Application(Lambda("x", Application(Var "x", Var "x")), Lambda("x", Application(Var "x", Var "x")));;

let rec substitute t1 var t2 =
	match t1 with
		| Var v -> if Var v = Var var then t2 else Var v
		| Lambda (v, t) -> Lambda (v, substitute t var t2)
		| Application (t11, t12) -> Application (substitute t11 var t2, substitute t12 var t2);;

let rec apply_term t1 t2 =
	match t1 with
		| Var v -> Application (t1, t2)
		| Lambda (v, t) -> substitute t v t2
		| Application (t11, t12) -> apply_term (apply_term t11 t12) t2;;

let rec normalize term =
	print_endline "";
	print_term term;
	match term with
		| Var v -> term
		| Lambda (v, t) -> Lambda (v, normalize t)
		| Application (t1, t2) -> 
			match t1, t2 with
				| Var v1, Var v2 -> Application(Var v1, Var v2)
				| Var v1, t2 -> Application(Var v1, normalize t2)
				| _ -> normalize (apply_term t1 t2);;

print_term(normalize y);;
