open Ast ;;
open Ast.Mips ;;

module Env = Map.Make(String) ;;

let _types_ =
  List.fold_left
    (fun e builtin -> Env.add (fst builtin) (snd builtin) e)
    Env.empty [ "%add", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%subtract", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%multiply", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%divide", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%inf", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%sup", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%eq", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%infeq", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "%supeq", Fun_t { ret = Num_t
                              ; args = [ Num_t ; Num_t ] }
              ; "print_num", Fun_t { ret = Void_t
                                   ; args = [ Num_t ] }
              ; "print_str", Fun_t { ret = Void_t
                                   ; args = [ Str_t  ] }
              ; "print_nl", Fun_t { ret = Void_t
                                   ; args = [] }
              ; "read_num", Fun_t { ret = Num_t
                                  ; args = [] }
              ; "str_len", Fun_t { ret = Num_t
                                 ; args = [ Str_t ] }
              ; "pair", Fun_t { ret = Pair_t Num_t
                              ; args = [ Num_t ; Pair_t (Num_t) ] }
              ; "pair_empty", Fun_t { ret = Num_t
                                    ; args = [ Pair_t Num_t ] }
              ; "%pair_eq", Fun_t { ret = Num_t
                                 ; args = [ Pair_t Num_t ; Pair_t Num_t ] }
              ; "head", Fun_t { ret = Num_t
                              ; args = [ Pair_t (Num_t) ] }
              ; "tail", Fun_t { ret = Pair_t (Num_t)
                              ; args = [ Pair_t (Num_t) ] }
    ]
;;

let _lib_ =
  List.fold_left
    (fun e builtin -> Env.add (fst builtin) (snd builtin) e)
    Env.empty [ "%add", [ Lw (T0, Mem (SP, 4))
                        ; Lw (T1, Mem (SP, 0))
                        ; Add (V0, T0, T1) ]
              ; "%subtract",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Sub (V0, T0, T1) ]
              ; "%multiply",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Mul (V0, T0, T1) ]
              ; "%divide",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Div (V0, T0, T1) ]
              ; "%inf",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Slt (V0, T0, T1) ]
              ; "%sup",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Sgt (V0, T0, T1) ]
              ; "%eq",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Seq (V0, T0, T1) ]
              ; "%infeq",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Sle (V0, T0, T1) ]
              ; "%supeq",  [ Lw (T0, Mem (SP, 4))
                              ; Lw (T1, Mem (SP, 0))
                              ; Sge (V0, T0, T1) ]
              ; "print_num", [ Lw (A0, Mem (SP, 0))
                             ; Li (V0, Syscall.print_int)
                             ; Syscall ]
              ; "print_str", [ Lw (A0, Mem (SP, 0))
                             ; Li (V0, Syscall.print_string)
                             ; Syscall ]
              ; "print_nl", [ La (A0, Lbl "newline")
                             ; Li (V0, Syscall.print_string)
                             ; Syscall ]
              ; "read_num", [ Li (V0, Syscall.read_int)
                            ; Syscall ]
              ; "str_len", [ Jal (Lbl "_str_len") ]
              ; "pair", [ Jal (Lbl "_pair") ]
              ; "pair_empty", [ Jal (Lbl "_pair_empty") ]
              ; "%pair_eq", [ Lw (T0, Mem (SP, 4))
                           ; Lw (T1, Mem (SP, 0))
                           ; Seq (V0, T0, T1) ]
              ; "head", [ Lw (T0, Mem (SP, 0))
                        ; Lw (V0, Mem (T0, -4)) ]
              ; "tail", [ Lw (T0, Mem (SP, 0))
                        ; Lw (V0, Mem (T0, 0)) ]
    ]
;;

let _implem_ =
  [ Label "_pair"
  ; Li (A0, 8)
  ; Li (V0, Syscall.sbrk)
  ; Syscall
  ; Lw (T0, Mem (SP, 4))
  ; Sw (T0, Mem (V0, -4))
  ; Lw (T0, Mem (SP, 0))
  ; Sw (T0, Mem (V0, 0))
  ; Jr RA
  ]
  @
  [ Label "_pair_empty"
  ; Lw (T0, Mem (SP, 0))
  ; Seq (V0, T0, Zero)
  ; Jr RA
  ]
  @
  [ Label "_str_len"
  ; Li (V0, 0)
  ; Lw (A0, Mem (SP, 0))
  ; Label "_str_len_loop"
  ; Lb (T0, Mem (A0, 0))
  ; Beq (T0, Zero, Lbl "_str_len_end_loop")
  ; Addi (V0, V0, 1)
  ; Addi (A0, A0, 1)
  ; B (Lbl "_str_len_loop")
  ; Label "_str_len_end_loop"
  ; Jr RA
  ]
;;
