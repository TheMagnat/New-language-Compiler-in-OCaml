open Lexing ;;

type type_t =
  Void_t
| Num_t
| Str_t
| Pair_t of type_t
| Fun_t  of { ret: type_t
            ; args: type_t list }
;;

module Parsed = struct
  

  type prog = instr list

  and block =
  | Block of { allInstr: instr list
             ; pos: position }

  and instr =

  | Instr of { expr: expr
              ; pos: position }
  | Assign of { varType: type_t
              ; var: string
              ; expr: expr
              ; pos: position }
  | Func of { name: string
            ; param: emptyVar list
            ; return: type_t 
            ; content: block
            ; pos: position }
  | Return of { expr: expr
              ; pos: position}
  | EmptyReturn of { pos: position }

  and emptyVar =
  | EmptyVar of { varType: type_t
                ; var: string
                ; pos: position }

  and expr =
    Nil of { pos: position }
  | Num of { value: int
           ; pos: position }
  | Str of { value: string
           ; pos: position }
  | Var of { name: string
           ; pos: Lexing.position }
  | Reassign of { var: string
                ; expr: expr
                ; pos : position }
  | Call of { func: string
            ; args: expr list
            ; pos: position }
  | Cond of { test: expr
            ; yes: block
            ; no: block
            ; pos: Lexing.position }
  | While of { test: expr
             ; block: block
             ; pos: position }
end ;;



module Typed = struct
  type prog = instr list

  and block =
  | Block of { allInstr: instr list }

  and instr =
  | Instr of { expr: expr }
  | Assign of { var: string
              ; expr: expr }

  | Func of { name: string
            ; param: string list
            ; content: block }

  and expr =
    Nil
  | Num  of int
  | Str  of string
  | Var  of string
  | Reassign of { var: string
                ; expr: expr }
  | Call of { func: string
            ; args: expr list }
  | Cond of { test: expr
            ; yes: block
            ; no: block }
  | While of { test: expr
             ; block: block }
end ;;

module Simplified = struct
  type prog = instr list

  and block =
  | Block of { allInstr: instr list }

  and instr =
  | Instr of { expr: expr }
  | Assign of { var: string
              ; expr: expr }
  | Func of { name: string
            ; param: string list
            ; content: block }

  and expr =
    Nil
  | Num  of int
  | Data of string
  | Var  of string
  | Reassign of { var: string
                ; expr: expr }
  | Call of { func: string
            ; args: expr list }
  | Cond of { test: expr
            ; yes: block
            ; no: block }
  | While of { test: expr
             ; block: block }
end ;;





module Mips = struct
  type reg =
    SP
  | FP
  | RA
  | Zero
  | V0
  | A0
  | T0
  | T1

  type loc =
    Reg of reg
  | Lbl of string
  | Mem of reg * int

  type instr =
    Asciiz of string * string
  | Label  of string
  | Li     of reg * int
  | La     of reg * loc
  | Lb     of reg * loc
  | Addi   of reg * reg * int
  | Add    of reg * reg * reg
  | Sub    of reg * reg * reg
  | Mul    of reg * reg * reg
  | Div    of reg * reg * reg
  | Sgt    of reg * reg * reg
  | Slt    of reg * reg * reg
  | Seq    of reg * reg * reg
  | Sge    of reg * reg * reg
  | Sle    of reg * reg * reg
  | Sw     of reg * loc
  | Lw     of reg * loc
  | Move   of reg * reg
  | Syscall
  | Jal    of loc
  | Jr     of reg
  | B      of loc
  | Beq    of reg * reg * loc

  type asm = { data: instr list ; text: instr list }

  module Syscall = struct
    let print_int = 1
    let print_string = 4
    let read_int = 5
    let sbrk = 9
  end
end ;;
