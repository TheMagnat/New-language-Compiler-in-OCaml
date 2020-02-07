{
  open Parser ;;

  exception Error of char;;
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = '-'?['0'-'9']+
let identifier = alpha (alpha | digit | '-' | '_')*

rule lex = parse
| eof          		{ Leof }
| [ ' ' '\t' ] 		{ lex lexbuf }
| '\n'         		{ Lexing.new_line lexbuf; lex lexbuf }
| "+"          		{ Lplus }
| "-"          		{ Lminus }
| "*"				{ Lstar }
| "/"				{ Lslash }
| "<"				{ Linf }
| ">"				{ Lsup }
| "==="				{ Leqeq }
| "=="				{ Leq }
| "<="				{ Linfeq }
| ">="				{ Lsupeq }
| "("          		{ Lopar }
| ")"          		{ Lcpar }
| "{"				{ Loacc }
| "}"				{ Lcacc }
| "["				{ Lohook }
| "]"				{ Lchook }
| ","          		{ Lcomma }
| "="				{ Lassign }
| ";"          		{ Lsc }
| "def"				{ Ldef }
| "return"			{ Lreturn }
| "int"				{ Ltype (Num_t) }
| "string"			{ Ltype (Str_t) }
| "void" 			{ Ltype (Void_t) }
| "[int]"			{ Ltype (Pair_t Num_t) }
| "if"            	{ Lif }
| "else"          	{ Lelse }
| "while"			{ Lwhile }
| "print_num"  		{ Lident "print_num" }
| "print_str"  		{ Lident "print_str" }
| "read_num"		{ Lident "read_num" }
| "print_nl"		{ Lident "print_nl" }
| "str_len"			{ Lident "str_len" }
| "pair"       		{ Lident "pair" }
| "head"       		{ Lident "head" }
| "tail"       		{ Lident "tail" }
| "pair_empty"		{ Lident "pair_empty" }
| identifier as s 	{ Lident (s) }
| digit as n  		{ Lnum (int_of_string n) }
| "\""         		{ Lstr (string lexbuf) }
| _ as c       		{ raise (Error c) }

and string = parse
| "\\\"" { "\"" ^ (string lexbuf) }
| "\"" { "" }
| _ as c { (String.make 1 c) ^ (string lexbuf) }
