{
	open Lexing
	open Parser

	exception Lexing_error of char

	let kwd_tbl = 
		[
			"print",PRINT;
			"define",DEFINE;
			"if",IF;
			"then",THEN;
			"else",ELSE;
			"and",AND;
			"or",OR;
			"true", CST (Bool true); 
			"false", CST (Bool false);
			"not", NOT;
			"func", FUNC
		]

	let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

	let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
			lexbuf.lex_curr_p <- {
				pos with pos_lnum = pos.pos_lnum + 1;
				pos_bol = pos.pos_cnum  
			}

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let integer = ['0'-'9']+
let space = [' ' '\t']
(*	| "#" [^'\n']* '\n' { newline lexbuf; token lexbuf } *)
rule token = parse
	| "\n"	 { newline lexbuf; token lexbuf }
	| "#" {comment lexbuf} 
	| space+	{ token lexbuf }
	| ident as id { id_or_kwd id }
	| "+"		{ ADD }
	| "-"		{ SUB }
	| "*"		{ MUL }
	| "/"		{ DIV }
	| "="		{ IGUAL }
	| "("		{ LPARENT }
	| ")"		{ RPARENT }
	| "<"		{MENOR}	 
	| ">"		{MAIOR}
	| integer as s { CST (int_of_string s) }
	| eof		{ EOF }
	| _ as c	{ raise (Lexing_error c) }

and comment = parse
	| "#" {token lexbuf}
	| _ {comment lexbuf}
	| eof { raise (Lexing_error "eof") }


