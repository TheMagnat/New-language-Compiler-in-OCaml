open Ast.Simplified ;;
open Ast.Mips ;;

exception Error of string;;


let uniq = ref 0;;


module Env = Baselib.Env ;;

(* helper functions *)

let new_local_env step =
  Env.add "__stepFromFp" step Env.empty
;;



let stack_push regs =
  let rec loop regs =
    if regs = [] then []
    else Sw (List.hd regs, Mem (SP, 4 * ((List.length regs) - 1))) :: (loop (List.tl regs))
  in [ Addi (SP, SP, -4 * List.length regs) ] @ loop regs
;;

let stack_pop regs =
  let rec loop regs =
    if regs = [] then []
    else Lw (List.hd regs, Mem (SP, 4 * ((List.length regs) - 1))) :: (loop (List.tl regs))
  in loop regs @ [ Addi (SP, SP, 4 * List.length regs) ]
;;

let stack_change reg pos =
  [Sw (reg, Mem (FP, pos)) ]
;;

(* Local env Helper *)
let add_local_env varName localEnv =
  (* Si on veut être plus précis, on pourrait bouger le changement de __stepFromFp à chaque modification de SP *)
  let newEnv = Env.add "__stepFromFp" ((Env.find "__stepFromFp" localEnv) - 4) localEnv in
  Env.add varName (Env.find "__stepFromFp" newEnv) newEnv
;;

let link_local_env varName pos localEnv =
  Env.add varName (pos * 4) localEnv
;;


(* compiler *)
let rec compile_and_push expr localEnv env =
  compile_expr expr localEnv env @ stack_push [ V0 ]


and compile_and_change varName expr localEnv env =
  let pos = Env.find varName localEnv in
  compile_expr expr localEnv env @ stack_change V0 pos

and compile_expr expr localEnv env =
  match expr with
  | Nil    -> [ Li (V0, 0) ]
  | Num n  -> [ Li (V0, n) ]
  | Data l -> [ La (V0, Lbl l) ]
  | Var v  ->
    let memPos = Env.find v localEnv in
    [ Lw (V0, Mem (FP, memPos)) ]
  | Reassign r ->
    compile_and_change r.var r.expr localEnv env
  | Call c -> List.flatten (List.map (fun a -> compile_and_push a localEnv env) c.args)
              @ Env.find c.func env
              @ [ Addi (SP, SP, 4 * (List.length c.args)) ]
  | Cond c ->
    incr uniq;
    let lblElse = Printf.sprintf "else_%04d" !uniq in
    let lblEndIf = Printf.sprintf "endIf_%04d" !uniq in
    compile_expr c.test localEnv env
    @ [ Beq (V0, Zero, Lbl lblElse) ]
    @ compile_block c.yes localEnv env
    @ [ B (Lbl lblEndIf)
      ; Label lblElse ]
    @ compile_block c.no localEnv env
    @ [ Label lblEndIf ]
  | While w ->
    incr uniq;
    let lblWhile = Printf.sprintf "while_%04d" !uniq in
    let lblEndWhile = Printf.sprintf "end_while_%04d" !uniq in
    [ Label lblWhile ]
    @ compile_expr w.test localEnv env
    @ [ Beq (V0, Zero, Lbl lblEndWhile) ]
    @ compile_block w.block localEnv env
    @ [ B (Lbl lblWhile) ]
    @ [ Label lblEndWhile ]

and compile_instr instr localEnv env =
  match instr with
  | Instr i ->
     compile_expr i.expr localEnv env, localEnv
  | Assign a ->
     compile_and_push a.expr localEnv env,
     add_local_env a.var localEnv
  | Func _ -> raise (Error (Printf.sprintf "Function in first tab CHELOU"))

and compile_prog prog localEnv env =
  match prog with
  | [] -> []
  | instr :: nextProg ->
     let ai, newLocalEnv = compile_instr instr localEnv env in
     ai @ (compile_prog nextProg newLocalEnv env)

and compile_block block localEnv env =
  let oldStepFromFp = Env.find "__stepFromFp" localEnv in
  let newLocalEnv = Env.map (fun a -> a - (oldStepFromFp - 4)) localEnv in
  let updatedNewLocalEnv = Env.add "__stepFromFp" 0 newLocalEnv in
  match block with
  | Block b ->
    stack_push [ FP ]
    @ [ Move (FP, SP) ]
    @ compile_prog b.allInstr updatedNewLocalEnv env
    @ [ Move (SP, FP) ]
    @ stack_pop [ FP ]
;;

let rec compile_function instr env =
  match instr with
  | Instr _ -> raise (Error (Printf.sprintf "Instr in second tab CHELOU"))
  | Assign _ -> raise (Error (Printf.sprintf "Assign in second tab CHELOU"))
  | Func f -> 
    let newLocalEnv, _ = (List.fold_left (fun acc varName -> (link_local_env varName (snd acc) (fst acc)), ((snd acc) - 1))
                                      (* Pourquoi + 0 ? Car deja on retire 1 car on compte 0 donc on prend longueur - 1,
                                         et on a déjà ajouté ra sur la pile, donc on rajoute 1 ce qui donne 0 *)
                                      ((new_local_env 0), ((List.length f.param) + 0)) 
                                      f.param) in
    (* On ajoute d'abord à l'environement pour pouvoir faire des fonctions récursives *)
    let newEnv = Env.add f.name [ Jal (Lbl f.name) ] env in
    [ Label f.name ]
    @ stack_push [ RA ]
    @ compile_block f.content newLocalEnv newEnv
    @ stack_pop [ RA ]
    @ [ Jr RA ],
    newEnv

;;


let rec compile_prog_function prog env =
  match prog with
  | [] -> [], env
  | instr :: nextProg ->
     let compiledFunc, newEnv = compile_function instr env in
     let compiledNextFunc, finalEnv = (compile_prog_function nextProg newEnv) in
     compiledFunc @ compiledNextFunc, finalEnv
;;

let compile ast =
  let allFunctionCompiled, envWithNewFunction = (compile_prog_function (snd (fst ast)) Baselib._lib_) in
  { data = [ Asciiz ("newline", "\\n") ]
           @ List.map (fun (l,s) -> Asciiz (l, s)) (snd ast)
  ; text = Baselib._implem_
           @ allFunctionCompiled
           @ [ Label "main" ]
           @ stack_push [ RA ]
           @ [ Move (FP, SP)]
           @ compile_prog (fst (fst ast)) (new_local_env 0) envWithNewFunction
           @ [ La (A0, Lbl "newline")
             ; Li (V0, Syscall.print_string)
             ; Syscall 
             ; Move (SP, FP) ]
           @ stack_pop [ RA ]
           @ [ Jr RA ]
  }
;;
