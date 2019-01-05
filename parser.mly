(* Tabs with 4 spaces were used *)

%{
	open Ast
%}

%token <Ast.constant> CST
%token <string> VARN
%token LESSER GREATER GREATEREQ LESSEREQ EQ NEQ EQEQ
%token PLUS MINUS TIMES DIV POW FACT DEC INC MOD
%token AND OR NOT
%token IF THEN ELSE DEFINE PRINT
/*		( )		,	{ }		; */
%token LP RP COMMA LB RB SEMICOLON
%token FUNC
%token EOF

%left LESSER GREATER GREATEREQ LESSEREQ NEQ EQEQ
%left OR AND
%right NOT
%left PLUS MINUS
%left TIMES DIV
%left POW MOD
%left FACT DEC INC 
%right EQ

%nonassoc uminus


%start prog

%type <Ast.prog> prog

%%

prog:
	| p = stmts EOF { List.rev p }
;

/* stmts: s = separated_list(SEMICOLON, stmt) { s } */
stmts:
	 i = stmt						{ [i] }
	| l = stmts SEMICOLON i = stmt	{ i :: l }
;

expr:
	 c = CST							{ Econst c }
	| id = VARN							{ Evar id }
	| e1 = expr o = bop e2 = expr		{ Ebinop (e1, o, e2) }
	| e = expr o = uop					{ Eunop (o, e1) }
	| NOT e = expr						{ Eunop (NOT, e) }
	| MINUS e = expr %prec uminus		{ Ebinop (Cst 0, Sub, e) }
	| DEFINE id = VARN EQ e = expr		{ Define (id, e) }
	| LP e = expr RP					{ e }
;

/* created seperatly for readablility */
func: FUNC fname = VARN LP x = separated_list(COMMA, VARN) RP LB s = stmts RB
																{ Func(f, x, s) }
;
stmt:
	 PRINT e = expr												{ Print e }
	| IF e = expr THEN LP s = stmts RP							{ Ift(e, List.rev s) }
	| IF e = expr THEN LP s1 = stmts RP ELSE LP s2 = stmts RP	{ Ifte(e, List.rev s1, List.rev s2) }
	| f = func													{ f }
	| f = VARN LP e = separated_list(COMMA, expr) RP			{ Ecall (f, e) }
	| LP e = expr RP											{ Eval e }
;

%inline bop:
	| PLUS		{ Badd }
	| MINUS		{ Bsub }
	| TIMES		{ Bmul }
	| DIV		{ Bdiv }
	| AND		{ Band } (* && and *)
	| OR		{ Bor } (* || or *)
	| GREATER	{ Bgreater } (*maior do que > *)
	| LESSER 	{ Blesser } (*menor do que < *)
	| GREATEREQ	{ BgreaterEq } (*maior do que >= *)
	| LESSEREQ	{ BlesserEq } (*menor do que <= *)
	| EQEQ		{ Beqeq } (*igual igual nos ifs == *)
	| EQ		{ Beq } (*definir igual = *)
	| NEQ		{ Bneq } (*diferente de ~= *)
	| POW		{ Bpow } (*elevado ^*)
	| MOD		{ Bmod }
;
%inline uop:
	| FACT		{ Ufact } (*Fatorial !*)
	| DEC		{ UDec } (*Dec -- *)
	| INC		{ UInc } (*Inc ++ *)
;
