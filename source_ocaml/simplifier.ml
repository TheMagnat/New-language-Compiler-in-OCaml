module T = Ast.Typed ;;
open Ast.Simplified ;;



let uniq = ref 0;;



(*let rec print_simplified_expr expr =
  match expr with
  | Nil -> (Printf.printf "Null\n")
  | Num v -> (Printf.printf "Val : %d\n" v)
  | Data str -> (Printf.printf "Data : %s\n" str)
  | Call truc -> (Printf.printf "Func : %s\n"  truc.func)
;;

let rec print_simplified_instr instr =
  match instr with
  | Instr ins -> print_simplified_expr ins.expr
;;

let rec print_simplified ast =
  match ast with
  | [] -> (Printf.printf "FINI\n")
  | instr :: prog ->
      print_simplified_instr instr;
      print_simplified prog
;;*)

let rec collect_constant_strings ast =
  let rec ccs ast =
    match ast with
    | T.Nil   -> Nil, []
    | T.Num n -> Num n, []
    | T.Var v -> Var v, []
    | T.Str s ->
       incr uniq;
       let lbl = Printf.sprintf "str_%04d" !uniq in
       Data (lbl), [ (lbl, s) ]
    | T.Reassign r ->
       let sr, dr = ccs r.expr in
       Reassign { var = r.var; expr = sr }, dr
    | T.Call c ->
       let args = List.map ccs c.args in
       Call { func = c.func ; args = List.map fst args },
       List.flatten (List.map snd args)
    | T.Cond c ->
      let st, dt = ccs c.test in
      let sy, dy = simplify_block c.yes in
      let sn, dn = simplify_block c.no in
      Cond { test = st; yes = sy; no = sn },
      dt @ dy @ dn
    | T.While w ->
      let st, dt = ccs w.test in
      let sb, db = simplify_block w.block in
      While { test = st; block = sb },
      dt @ db
      
  in
  ccs ast

and simplify_instr instr =
  match instr with
  | T.Instr a ->
     let ae = collect_constant_strings a.expr in
     Instr { expr = (fst ae) }, (snd ae)
  | T.Assign a ->
     let ae = collect_constant_strings a.expr in
     Assign { var = a.var; expr = (fst ae) }, (snd ae)
  | T.Func f ->
    let code, data = (simplify_block f.content) in
    Func { name = f.name; param = f.param; content = code }, data

and simplify_block block =
  match block with
  | T.Block b ->
    let siList = simplify_instruction_list b.allInstr in
    let code = List.map fst siList in
    let data = List.flatten (List.map snd siList) in
    Block { allInstr = code }, data

and simplify_instruction_list ast =
  match ast with
  | [] -> []
  | instr :: prog ->
     let instruData = simplify_instr instr in
     instruData :: (simplify_instruction_list prog)
;;

let simplify ast =
  let pair = simplify_instruction_list ast in
  let code = List.map (fun (a) -> (fst a)) pair in
  let data = (List.fold_left (fun acc a -> acc @ (snd a)) [] pair) in

  (*
  Les instructions sont maintenant une pair, contenant en premier les instruction classique,
  et dans le seconds, les fonctions.
  Cela permettra de les déclarrer avant le main dans le compiler.
  *)
  let instruFunction = List.fold_left (fun acc instru ->
                                                match instru with
                                                | Instr _ -> (fst acc) @ [ instru ], (snd acc)
                                                | Assign _ -> (fst acc) @ [ instru ], (snd acc)
                                                | Func _ -> (fst acc), (snd acc) @ [ instru ]
                                      ) ([], []) code in

  instruFunction, data

;;
