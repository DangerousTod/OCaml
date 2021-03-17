(* Lexxer reads integers, common operators, strings and logical symbols *)
(*  identifier in inherits  isvoid  let loop  lt  new  of  pool   type  *)
(* main control:  rm -f main_lex main_lex.cmx main_lex.ml main_lex.cmi main_lex.o *)
(* main create:  ocamllex main_lex.mll && ocamlopt -o main_lex main_lex.ml *)


{
let line_number = ref 1
type token = 
	| Integer of int
	| String of string
	| Plus
	| Minus
	| Times
	| Divide
	| Equals
	| At
	| Colon
	| Comma
	| Dot
	| Semi
	| Tilde

	|Lsqb
	|Rsqb

	| Larrow
	| Lbrace
	| Rarrow
	| Rbrace
	| Lparen
	| Rparen
	| Le
	| Re
	| Lt
	| Gt
	| Str
	| Else
	| Not
	| While
	| Then
	| True
	| False
	| Case
	| Class
	| Esac
	| If
	| In
	| Let
	| Of
	| Type
	| Fi

	| Identifier 
	
	| Inherits  
	| Isvoid  

	| Loop  
	| It  
	| New  

	| Pool
	| Vector
	|	Bsl
	| Bsr
	| Tsml
	| MatMul



exception Eof

}

rule token = parse
| [' ' '\t']										{ token lexbuf }
| ['\n']											{ incr line_number ; token lexbuf }
|	['0' - '9'] + as lxm		{ !line_number, Integer(int_of_string lxm) }
| [' ' 'a'-'z' 'A'-'Z' '_']* + as s  { !line_number, String(s) }
|	'+'													{ !line_number, Plus }
| '-'													{ !line_number, Minus }
| '*'													{ !line_number, Times }
| '/'													{ !line_number, Divide }
| '='													{ !line_number, Equals }
| '@'													{ !line_number, At }
| ':'													{ !line_number, Colon }
| ','													{ !line_number, Comma }
| '.'													{ !line_number, Dot }
| ';'													{ !line_number, Semi }
| '~'													{ !line_number, Tilde }
| '['													{ !line_number, Lsqbr }
| ']'													{ !line_number, Rsqbr }
| '{'													{ !line_number, Lbrace }
| '}'													{ !line_number, Rbrace }
| '('													{ !line_number, Lparen }
| ')'													{ !line_number, Rparen }
| '`'													{ !line_number, Str }
| '|'													{ !line_number, Else }
| "else"													{ !line_number, Else }
| '!'													{ !line_number, Not }

| "<-"												{ !line_number, Larrow }
| "->"													{ !line_number, Rarrow }

| "<="										{ !line_number, Le }
| "Le"											{ !line_number, Le }
| "=>"										{ !line_number , Re }
| "Re"										{ !line_number, Re }
| '<'												{ !line_number, Lt }
| "less than"          { !line_number, Lt }
| '>'												{ !line_number, Gt }
| "greater than"				{ !line_number, Gt }
| "<==>"							{ !line_number, While }
| "while"								{ !line_number, While }
| "==>"									{ !line_number, Then }
| "then"								{ !line_number, Then }
| "true"								{ !line_number, True }
| "false"							{ !line_number, False }
| "case"								{ !line_number, Case }
| "class"								{ !line_number, Class }
| "esac"								{ !line_number, Esac }
| "if"								{ !line_number, If }
| "in"								{ !line_number, In }
| "let"								{ !line_number, Let }
| "of"								{ !line_number, Of }
| "type"								{ !line_number, Type }
| "fi"								{ !line_number, Fi }

| "identifier"			{ !line_number, Identifier } 

| "inherits"				{ !line_number,  Inherits }  
| "isvoid"					{ !line_number,  Isvoid }  

| "loop"						{ !line_number,  Loop }  
| "lt" 								{ !line_number,  It }  
| "new"						{ !line_number,  New } 

| "pool"  					{ !line_number, Pool }
| "||"					{ !line_number, Vector }
| "<<"							{ !line_number, Bsl }
| ">>"							{ !line_number, Bsr }
| "[X]"							{ !line_number, Tsml }
| "[M]"							{ !line_number, MatMul }


|	eof												{ raise Eof }

{

let main () = begin
	let outbuf = Buffer.create 255 in
	try
		let filename = Sys.argv.(1) in
		let file_handle = open_in filename in
		let lexbuf = Lexing.from_channel file_handle in
		while true do
			let line, result = token lexbuf in

			Printf.bprintf outbuf "%d\n" line ;
			match result with
			| Plus -> Printf.bprintf outbuf "plus\n"
			| Minus -> Printf.bprintf outbuf "minus\n"
			| Times -> Printf.bprintf outbuf "times\n"
			| Divide -> Printf.bprintf outbuf "divide\n"
			| Equals -> Printf.bprintf outbuf "equals\n"
			| At -> Printf.bprintf outbuf "at\n"
			| Colon -> Printf.bprintf outbuf "colon\n"
			| Comma -> Printf.bprintf outbuf "comma\n"
			| Dot -> Printf.bprintf outbuf "dot\n"
			| Semi -> Printf.bprintf outbuf "semi\n"
			| Tilde -> Printf.bprintf outbuf "tilde\n"
			| Lsqbr -> Printf.bprintf outbuf "lsqbr\n"
			| Rsqbr -> Print.bprintf outbuf "rsqbr\n"
			| Lbrace -> Printf.bprintf outbuf "lbrace\n"
			| Rbrace -> Printf.bprintf outbuf "rbrace\n"
			| Lparen -> Printf.bprintf outbuf "lparen\n"
			| Rparen -> Printf.bprintf outbuf "rparen\n"
			| Str -> Printf.bprintf outbuf "str\n"
			| Else -> Printf.bprintf outbuf "else\n"
			| Not -> Printf.bprintf outbuf "not\n"

			| Larrow -> Printf.bprintf outbuf "left arrow\n"
			| Rarrow -> Printf.bprintf outbuf "right arrow\n"
			| Le -> Printf.bprintf outbuf "left equal\n"
			| Re -> Printf.bprintf outbuf "right equal\n"
			| Lt -> Printf.bprintf outbuf "less than\n"
			| Gt -> Printf.bprintf outbuf "greater than\n"
			| While -> Printf.bprintf outbuf "while\n"
			| Then -> Printf.bprintf outbuf "then\n"

			| True -> Printf.bprintf outbuf "true\n"
			| False -> Printf.bprintf outbuf "false\n"
			| Case -> Printf.bprintf outbuf "case\n"
			| Class -> Printf.bprintf outbuf "class\n"
			| Esac -> Printf.bprintf outbuf "esac\n"
			| If -> Printf.bprintf outbuf "if\n"
			| In -> Printf.bprintf outbuf "in\n"
			| Let -> Printf.bprintf outbuf "let\n"
			| Of -> Printf.bprintf outbuf "of\n"
			| Type -> Printf.bprintf outbuf "type\n"
			| Fi -> Printf.bprintf outbuf "fi\n"

			| Identifier -> Printf.bprintf outbuf "identifier\n"

			| Inherits  -> Printf.bprintf outbuf "inherits\n"
			| Isvoid  -> Printf.bprintf outbuf "isvoid\n"

			| Loop  -> Printf.bprintf outbuf "loop\n"
			| It  -> Printf.bprintf outbuf "lt\n"
			| New  -> Printf.bprintf outbuf "new\n"

			| Pool   -> Printf.bprintf outbuf "pool\n"
			| Vector -> Printf.bprintf outbuf "vector\n"
			| Bsl -> Printf.bprintf outbuf "bit shift left\n"
			| Bsr -> Printf.bprintf outbuf "bit shift right\n"
			| Tsml -> Printf.bprintf outbuf "tensor multiply\n"
			| MatMul -> Printf.bprintf outbuf "matrix multiply\n"

			| String(s) -> Printf.bprintf outbuf "%s\n" s 

			| Integer(i) -> Printf.bprintf outbuf "integer\n%d\n" i
		done
	with	| Eof	-> begin
						let filename = Sys.argv.(1) ^ "-lex" in
						let file_handle = open_out filename in
						Printf.fprintf file_handle "%s" (Buffer.contents outbuf) ;
						close_out file_handle
					end
					| _ -> begin
						Printf.printf "ERROR: %d: Lexer: message\n" !line_number ;
						exit 1
					end
end		;;
main () ;;
}
