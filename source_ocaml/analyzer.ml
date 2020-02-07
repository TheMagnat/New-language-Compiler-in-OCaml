open Ast ;;
module P = Ast.Parsed ;;
open Ast.Typed ;;
open Baselib ;;

exception Error of string * Lexing.position ;;

let rec string_of_type t =
  match t with
  | Void_t   -> "void"
  | Num_t    -> "number"
  | Str_t    -> "string"
  | Pair_t p -> "list of " ^ (string_of_type p)
  | Fun_t ft ->
     (if (List.length ft.args) > 1 then "(" else "")
     ^ (String.concat ", " (List.map string_of_type ft.args))
     ^ (if (List.length ft.args) > 1 then ")" else "")
     ^ " -> " ^ (string_of_type ft.ret)
;;

let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_type expected)
                  (string_of_type given),
                pos))
;;

let expr_pos expr =
  match expr with
  | P.Nil e -> e.pos
  | P.Num e  -> e.pos
  | P.Str e  -> e.pos
  | P.Var e -> e.pos
  | P.Reassign e -> e.pos
  | P.Call e -> e.pos
  | P.Cond e -> e.pos
  | P.While e -> e.pos
;;

module Env = Baselib.Env ;;

let rec analyze_expr expr funcEnv varEnv =
  match expr with
  | P.Nil _   -> Nil, Pair_t Num_t
  | P.Num n  -> Num n.value, Num_t
  | P.Str s  -> Str s.value, Str_t
  | P.Var v  ->
    if Env.mem v.name varEnv
    then
      Var v.name, Env.find v.name varEnv
    else
      raise (Error (Printf.sprintf "Variable %s was not declared" v.name, v.pos)) ;
  | P.Reassign r ->
      if not (Env.mem r.var varEnv)
      then
        raise (Error (Printf.sprintf "Variable %s was not declared" r.var, r.pos))
      else
        let ae = analyze_expr r.expr funcEnv varEnv in
        let varType = Env.find r.var varEnv in
        if not (varType = (snd ae))
     
        then errt varType (snd ae) r.pos
     
        else
          Reassign {var = r.var; expr = (fst ae) },
          varType
  | P.Call c -> (
     try
       match Env.find c.func funcEnv with
       | Fun_t ft -> begin
           if not ((List.length c.args) = (List.length ft.args)) then
             raise (Error (Printf.sprintf "function '%s' expects %d arguments but was given %d"
                             c.func (List.length ft.args) (List.length c.args),
                           c.pos))
           else
             let args = List.map2
                          (fun a at ->
                            let aa = analyze_expr a funcEnv varEnv in
                            if not (at = (snd aa)) then errt at (snd aa) c.pos
                            else fst aa)
                          c.args ft.args in
             Call { func = c.func ; args = args }, ft.ret
         end
       | _ -> raise (Error (Printf.sprintf "value '%s' is not a function" c.func, c.pos))
     with Not_found -> raise (Error (Printf.sprintf "undefined function '%s'" c.func, c.pos)) )
  | P.Cond c ->
    let at, tt = analyze_expr c.test funcEnv varEnv in
    if not (tt = Num_t)
    then
      raise (Error (Printf.sprintf "If condition must be a num", expr_pos c.test))
    else
      let ay = analyze_block c.yes funcEnv varEnv Void_t in
      let an = analyze_block c.no funcEnv varEnv Void_t  in
      Cond { test = at ; yes = ay ; no = an }, Void_t
  | P.While w ->
    let at, tt = analyze_expr w.test funcEnv varEnv in
    if not (tt = Num_t)
    then
      raise (Error (Printf.sprintf "While condition must be a num", expr_pos w.test))
    else
      let ab = analyze_block w.block funcEnv varEnv Void_t in
      While { test = at ; block = ab}, Void_t


and analyze_instr instr funcEnviro varEnviro blockReturnType =
  match instr with
  | P.Instr a ->
     let ae = analyze_expr a.expr funcEnviro varEnviro in
     [ Instr { expr = (fst ae) } ], funcEnviro, varEnviro
  | P.Assign a ->
     let ae = analyze_expr a.expr funcEnviro varEnviro in
     if not (a.varType = (snd ae))
     
     then errt a.varType (snd ae) a.pos
     
     else
      if Env.mem a.var funcEnviro
      then
        raise (Error (Printf.sprintf "Variable %s was already declared" a.var, a.pos))
      else
        [ Assign {var = a.var; expr = (fst ae) } ],
        funcEnviro,
        Env.add a.var (snd ae) varEnviro
  | P.Func f ->
    let allParamName, allParamType = (List.fold_left (fun acc element ->
                                                            match element with
                                                            | P.EmptyVar var -> ((fst acc) @ [var.var], (snd acc) @ [var.varType]))
                                     ([], [])
                                     f.param) in
    let newVarEnviro = List.fold_left (fun acc elem ->
                                            match elem with
                                            | P.EmptyVar e -> Env.add e.var e.varType acc)
                                    Env.empty f.param in
    (* On ajoute d'abord à l'environement des fonctions pour pouvoir faire des fonctions récursives *)
    let newFuncEnviro = Env.add f.name (Fun_t {ret = f.return; args = allParamType}) funcEnviro in
    let anylyzedContent = (analyze_block f.content newFuncEnviro newVarEnviro f.return) in
    [ Func { name = f.name; param = allParamName; content = anylyzedContent } ],
    newFuncEnviro,
    varEnviro
  | P.Return r ->
    let ae = analyze_expr r.expr funcEnviro varEnviro in
    if (snd ae) = blockReturnType
    then [ Instr { expr = (fst ae) } ], funcEnviro, varEnviro
    else raise (Error (Printf.sprintf "Return expression of type %s expected, but type %s given" (string_of_type blockReturnType) (string_of_type (snd ae)), r.pos))
  | P.EmptyReturn er ->
    if Void_t = blockReturnType
    then [], funcEnviro, varEnviro
    else raise (Error (Printf.sprintf "Return expression of type %s expected, but type %s given" (string_of_type blockReturnType) (string_of_type Void_t), er.pos))



and analyze_block block funcEnviro varEnviro blockReturnType =
  match block with
  | P.Block b ->
    let aiList = analyze_instruction_list b.allInstr funcEnviro varEnviro blockReturnType in
    Block { allInstr = aiList }

and analyze_instruction_list prog funcEnviro varEnviro blockReturnType =
  match prog with
  | [] ->
      (*Env.iter (fun key value -> Printf.printf "%s : %s\n" key value) enviro;*)
      []
  | instr :: prog ->
     let ai, newFuncEnv, newVarEnviro = analyze_instr instr funcEnviro varEnviro blockReturnType in
     ai @ (analyze_instruction_list prog newFuncEnv newVarEnviro blockReturnType)
;;


(*let rec analyze_instruction_list prog env =
  analyze_expr prog env
;;*)

let analyze parsed =
  analyze_instruction_list parsed Baselib._types_ Env.empty Void_t
;;
