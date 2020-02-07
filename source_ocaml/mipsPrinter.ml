open Ast.Mips ;;

let fmt_reg r =
  match r with
  | SP    -> "$sp"
  | FP    -> "$fp"
  | RA    -> "$ra"
  | Zero  -> "$zero"
  | V0    -> "$v0"
  | A0    -> "$a0"
  | T0    -> "$t0"
  | T1    -> "$t1"
;;

let fmt_loc l =
  match l with
  | Reg r -> fmt_reg r
  | Lbl l -> l
  | Mem (r, o) -> Printf.sprintf "%d(%s)" o (fmt_reg r)
;;

let print_instr out instr =
  let pr = Printf.fprintf in
  match instr with
  | Asciiz (n, s)             -> pr out "%s: .asciiz \"%s\"\n" n s
  | Label n                   -> pr out "%s:\n" n
  | Move (d, r)               -> pr out "  move %s, %s\n" (fmt_reg d) (fmt_reg r)
  | Li (d, i)                 -> pr out "  li %s, %d\n" (fmt_reg d) i
  | La (d, l)                 -> pr out "  la %s, %s\n" (fmt_reg d) (fmt_loc l)
  | Lb (d, l)                 -> pr out "  lb %s, %s\n" (fmt_reg d) (fmt_loc l)
  | Sw (r, l)                 -> pr out "  sw %s, %s\n" (fmt_reg r) (fmt_loc l)
  | Lw (r, l)                 -> pr out "  lw %s, %s\n" (fmt_reg r) (fmt_loc l)
  | Addi (d, r, i)            -> pr out "  addi %s, %s, %d\n" (fmt_reg d) (fmt_reg r) i
  | Add (d, r, s)             -> pr out "  add %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Sub (d, r, s)             -> pr out "  sub %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Mul (d, r, s)             -> pr out "  mul %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Div (d, r, s)             -> pr out "  div %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Sgt (d, r, s)             -> pr out "  sgt %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Slt (d, r, s)             -> pr out "  slt %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Seq (d, r, s)             -> pr out "  seq %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Sge (d, r, s)             -> pr out "  sge %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Sle (d, r, s)             -> pr out "  sle %s, %s, %s\n" (fmt_reg d) (fmt_reg r) (fmt_reg s)
  | Syscall                   -> pr out "  syscall\n"
  | Jal l                     -> pr out "  jal %s\n" (fmt_loc l)
  | Jr r                      -> pr out "  jr %s\n" (fmt_reg r)
  | B l                       -> pr out "  b %s\n" (fmt_loc l)
  | Beq (comp1, comp2, label) -> pr out "  beq %s, %s, %s\n" (fmt_reg comp1) (fmt_reg comp2) (fmt_loc label)

;;

let print_instructions out instrs =
  List.iter (fun i -> print_instr out i) instrs
;;

let print out asm =
  Printf.fprintf out "  .data\n" ;
  print_instructions out asm.data ;
  Printf.fprintf out "\n  .text\n  .globl main\n" ;
  print_instructions out asm.text
;;
