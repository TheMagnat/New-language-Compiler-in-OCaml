
open Ast.Mips ;;

(*
	Dans ce fichier, on peut passer par dessus la liste d'instruction mips
	et les changer pour faire des optimisation. Je n'ai pas été très inspiré
	mais on peut facilement en rajouter
*)

let addi_matched reg1 reg2 value nextList =
	
	let nextInstr = List.hd nextList in
	let newNextList = List.tl nextList in


	match nextInstr with
	| Addi (reg3, reg4, value2) ->
		if reg1 = reg2 && reg1 = reg4
		then	if (value + value2) = 0
				then
					if reg3 = reg4
					(*Si
						addi A A 1
						addi A A -1
					  Deviens
					  	<rien>
					*)
					then [], newNextList
					(*Si
						addi A A 1
						addi B A -1
					  Deviens
					  	move B A
					*)
					else [ Move(reg3, reg4) ], newNextList
				else
					(*Si
						addi A A 1
						addi B A 1
					  Deviens
					  	addi B A 2
					*)
					[ Addi(reg1, reg1, value + value2) ], newNextList
		(* Si ca ne match pas, on laisse commme c'etait *)	
		else [ Addi(reg1, reg2, value) ; Addi(reg3, reg4, value2) ], newNextList
	| wildcard -> [ Addi(reg1, reg2, value) ; wildcard ], newNextList



let optimise_instruction instr nextList =
    match instr with
    | Addi (reg1, reg2, value) -> addi_matched reg1 reg2 value nextList
    | wildcard -> [ wildcard ], nextList
;;


let rec optimise_text asm =
	match asm with
	| [] -> []
	| instr :: nextList ->
		let pastList, newNextList = optimise_instruction instr nextList in
		pastList @ (optimise_text newNextList)

;;

let optimise asm =
	let newText = optimise_text asm.text in

	{ data = asm.data
	; text = newText 
	}
;;