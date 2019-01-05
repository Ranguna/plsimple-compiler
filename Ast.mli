(* melhor arvore do jardim *)
type stmts = stmt list

and varName = string

and stmt =
	| Print of expr
	| IFt of expr * stmts
	| IFte of expr * stmts * stmts
	| Func of varName * varName list * stmts
	| Eval of expr
	
and expr =
	| Define of varName * expr
	| Econst of constant
	| Evar of varName
	| Ebinop of expr * binop * expr
	| Eunop of unop * expr
	| Ecall of varName * expr list
	| Sum of varName * expr * stmts
	| Mul of varName * expr * stmts

and binop =
	| Badd (*adiciona + *)
	| Bsub (*subtrai - *)
	| Bmul (*multiplica * *)
	| Bdiv (*divide / *)
	| Band (* && and *)
	| Bor (* || or *)
	| Bgreater (*maior do que > *)
	| Blesser (*menor do que < *)
	| BgreaterEq (*maior do que >= *)
	| BlesserEq (*menor do que <= *)
	| Beqeq (*igual igual nos ifs == *)
	| Beq (*definir igual = *)
	| Bneq (*diferente de ~= *)
	| Bpow (*elevado ^*)
	| BMod (*modulo %*)
and unop =
	|UDec (*Dec -- *)
	|UInc (*Inc ++ *)
	|Ufact (*Fatorial !*)
	|UNot
and constantMath = 
	|Cfloat of float
	|Cint of int
and constantLogic =
	|Cbool of bool
and constant =
	| MathCst of constantMath
	| LogicCst of constantLogic
		



