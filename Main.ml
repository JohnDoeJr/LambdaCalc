type term = Var of string | Lambda of string * term | Application of term * term;;

let rec termToString t =
	match t with
		| Var v -> v
		| Lambda (v, t) -> "\\" ^ v ^ "." ^ (termToString t)
		| Application (t1, t2) -> "(" ^ (termToString t1) ^ ")(" ^ (termToString t2) ^ ")";;

let rec term_has_var term var =
	match term with
		| Var v -> Var v = Var var
		| Lambda (v, t) -> Var v = Var var || (term_has_var t v)
		| Application (t1, t2) -> (term_has_var t1 var) || (term_has_var t2 var);;

let rec substituteVar term var =
	match term with
		| Var v -> if Var v = Var var then Var (v^"0") else Var v
		| Lambda (v, t) -> if v = var then Lambda(v^"0", substituteVar t var) else Lambda(v, substituteVar t var)
		| Application (t1, t2) -> Application(substituteVar t1 var, substituteVar t2 var);;

let rec substitute t1 var t2 =
	match t1 with
		| Var v -> if Var v = Var var then t2 else Var v
		| Lambda (v, t) -> 
			if term_has_var t2 v 
			then Lambda(v, substitute t var (substituteVar t2 v)) 
			else Lambda(v, substitute t var t2)
		| Application (t11, t12) -> Application (substitute t11 var t2, substitute t12 var t2);;

let rec apply_term t1 t2 =
	match t1 with
		| Var v -> begin
				match t2 with
					| Application (t21, t22) -> Application(Var v, (apply_term t21 t22))
					| t2 -> Application(Var v, t2)
				end
		| Lambda(v, t) -> substitute t v t2
		| Application (t11, t12) ->
			match t11, t12 with
				| Var v1, t12 -> Application(t1, t2)
				| _ -> apply_term (apply_term t11 t12) t2;;

let rec is_term_only_vars term =
	match term with
		| Var v -> true
		| Lambda (v, t) -> false
		| Application (t1, t2) -> (is_term_only_vars t1) && (is_term_only_vars t2);;

let rec normalize term =
	(* print_term term;
	print_endline ""; *)
	match term with
		| Var v -> term
		| Lambda (v, t) -> Lambda (v, normalize t)
		| Application (t1, t2) -> if is_term_only_vars term then term else normalize (apply_term t1 t2);;
		(* | Application (t1, t2) -> normalize (apply_term t1 t2);; *)

(* s = \xyz.x z (y z) *)
let s = Lambda("x", Lambda("y", Lambda("z", Application(Application(Var "x", Var "z"), Application(Var "y", Var "z")))));;
(* k = \xy.x *)
let k = Lambda("x", Lambda("y", Var "x"));;
(* z = (\x.x x)(\x.x x) *)
let z = Application(Lambda("x", Application(Var "x", Var "x")), Lambda("x", Application(Var "x", Var "x")));;
(* sksksk *)
let sksksk = Application(Application(Application(Application(Application(s, k), s), k), s), k);;
(* sk *)
let sk = Application(s, k);;
(* sksksks *)
let sks = Application(Application(Application(Application(sk, s), k), s), k);;
(* (\xyz.zyx)yz(\pq.q) *)
let first = Application(Application(Application(Lambda("x", Lambda("y", Lambda("z", Application(Application(Var "z", Var "y"), Var "x")))), Var "y"), Var "z"), Lambda("p", Lambda("q", Var "q")));;
(* (\yz.zy)(\x.xxx)(\x.xxx)(\y.xxx) *)
let second = Application(Application(
				Lambda("y", Lambda("z", Application(Var "z", Var "y"))), 
				Application(
					Lambda("x", Application(Application(Var "x", Var "x"), Var "x")), 
					Lambda("x", Application(Application(Var "x", Var "x"), Var "x"))
				)
			), Lambda("y", Application(Application(Var "x", Var "x"), Var "x")));;
(* 
0 = \xy.y
sc n = \nxy.x (n x y)

incrementZero = sc 0
*)
let sc = Lambda("n", Lambda("x", Lambda("y", Application(Var "x", Application(Application(Var "n", Var "x"), Var "y")))));;
let zero = Lambda("x", Lambda("y", Var "y"));;

print_endline(termToString(normalize(first)));;
print_endline(termToString(normalize(second)));;
print_endline(termToString(normalize (sks)));;
print_endline(termToString(normalize(Application(sc, zero))));;
(* print_term incrementZero;; *)
