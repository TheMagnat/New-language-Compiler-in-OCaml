%{

  open Ast ;;
  open Ast.Parsed ;;

%}

%token <int> Lnum
%token <string> Lstr Lident
%token <Ast.type_t> Ltype


%token Lslash Lstar Lminus Lplus Lopar Lcpar Lcomma
%token Linf Lsup Leq Linfeq Lsupeq
%token Leqeq
%token Lohook Lchook Loacc Lcacc
%token Lif Lelse
%token Lwhile
%token Lassign Lsc Leof
%token Ldef
%token Lreturn

%left Lassign

%left Lslash
%left Lstar
%left Lminus
%left Lplus

%left Linf
%left Lsup
%left Leq
%left Linfeq
%left Lsupeq
%left Leqeq


%start prog

%type <Ast.Parsed.prog> prog

%%

prog:
| p = nonempty_list(instr); Leof { p }
;

block:
| Loacc; p = nonempty_list(instr); Lcacc {
  Block { allInstr = p; pos = $startpos }
}
| i = instr {
  Block { allInstr = [ i ] ; pos = $startpos }
}
;

returnBlock:
| Loacc; p = list(instr); r = returnInstr; Lcacc {
  Block { allInstr = p @ [r]; pos = $startpos }
}
| r = returnInstr {
  Block { allInstr = [ r ] ; pos = $startpos }
}
;

instr:
| e = expr; Lsc {
  Instr { expr = e ; pos = $startpos($2) }
}
| g = group {
  Instr { expr = g ; pos = $startpos}
}
| t = Ltype; v = Lident; Lassign; e = expr; Lsc {
  Assign { varType = t ; var = v ; expr = e ; pos = $startpos($3) }
}
| t = Ltype; Ldef; n = Lident; Lopar; a = separated_list(Lcomma, emptyVar); Lcpar; b = returnBlock {
  Func { name = n; param = a; return = t; content = b; pos = $startpos }
}
;

returnInstr:
| Lreturn; e = expr; Lsc {
  Return { expr = e ; pos = $startpos }
}
| Lreturn; Lsc {
  EmptyReturn { pos = $startpos }
}
;

emptyVar:
| t = Ltype ; v = Lident {
  EmptyVar { varType = t; var = v; pos = $startpos }
}
;

group:
| Lif; Lopar; c = expr; Lcpar; t = block; Lelse; e = block {
  Cond { test = c
        ; yes = t
        ; no = e
        ; pos = $startpos }
}
| Lwhile; Lopar; c = expr; Lcpar; b = block {
  While { test = c
        ; block = b
        ; pos = $startpos }
}


expr:
| Lohook; Lchook {
  Nil { pos = $startpos }
}
| Lohook; l = list_of_int; Lchook { l }
| n = Lnum {
  Num { value = n
      ; pos = $startpos }
}
| s = Lstr {
  Str { value = s
      ; pos = $startpos }
}
| f = Lident; Lopar; a = separated_list(Lcomma, expr); Lcpar {
  Call { func = f
       ; args = a
       ; pos = $startpos }
}
| n = Lident {
  Var { name = n
       ; pos = $startpos }
}
| n = Lident; Lassign; e = expr {
  Reassign { var = n; expr = e; pos = $startpos($2) }
}
| a = expr; Lslash; b = expr {
  Call { func = "%divide"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Lstar; b = expr {
  Call { func = "%multiply"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Lminus; b = expr {
  Call { func = "%subtract"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Lplus; b = expr {
  Call { func = "%add"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Linf; b = expr {
  Call { func = "%inf"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Lsup; b = expr {
  Call { func = "%sup"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Leq; b = expr {
  Call { func = "%eq"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Leqeq; b = expr {
  Call { func = "%pair_eq"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Linfeq; b = expr {
  Call { func = "%infeq"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| a = expr; Lsupeq; b = expr {
  Call { func = "%supeq"
       ; args = [ a ; b ]
       ; pos = $startpos($2) }
}
| Lopar; e = expr; Lcpar { e }
;


list_of_int:
| n = expr ; Lcomma; l = list_of_int {
  Call { func = "pair"
       ; args = [ n ; l ]
       ; pos = $startpos }
}
| n = expr {
  Call { func = "pair"
       ; args = [ n ; Nil { pos = $startpos } ]
       ; pos = $startpos }
}
