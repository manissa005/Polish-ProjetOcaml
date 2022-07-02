open CreateTypes;;

(**************************************************************AFFICHAGE********************************************************)

(* impression d'un opérateur sur le terminal*)
(* print_op : op -> unit*)
let print_op op =
  match op with
  | Add -> print_string "+ "
  | Sub -> print_string "- "
  | Mul -> print_string "* "
  | Div -> print_string "/ "
  | Mod -> print_string "% " 

(* impression d'une expression sur le terminal*)
(* print_expr : expr -> unit*)
let rec print_expr expr =
  match expr with
  | Num(a) ->  print_int a
  | Var(v) -> print_string v
  | Op(op,exp1,exp2) -> print_op op ; print_expr exp1; print_string " "; print_expr exp2;;

(* impression d'un opérateur de comparaison sur le terminal*)
(* print_comp : comp -> unit*)
let print_comp comp = 
  match comp with
  |Eq -> print_string " = "
  |Ne -> print_string " <> "
  |Lt -> print_string " < "
  |Le -> print_string " <= "
  |Gt -> print_string " > "
  |Ge -> print_string " >= " 
;;

(* impression d'une condition sur le terminal*)
(* print_cond : cond -> unit*)
let print_cond cond = 
  match cond with
  |(exp1,comp,exp2) -> print_expr exp1 ; print_comp comp ; print_expr exp2 
;; 

(* impression des indentation sur le terminal*)
(* print_indentation : int -> unit*)
let rec print_indentation indent = 
  match indent with 
  | 0 -> print_string ""
  | _ -> print_string " " ; print_indentation (indent-1);;

(* impression d'un block sur le terminal*)
(* print_block : block -> int -> unit*)
let rec print_block block ind0=
  match block with 
  |[] -> print_string ""
  |(pos,instr)::res -> print_instr instr  ind0; print_block res ind0

(* impression d'une instruction et son indentation sur le terminal*)
(* print_instr : instr -> int -> unit*)
and print_instr instr indent=
print_indentation indent ;
match instr with
  | Set(var,exp) -> print_string (var^" := ") ; print_expr exp ; print_string "\n" 
  | Read(var) -> print_string ("READ "^var^"\n")
  | Print(exp) -> print_string "PRINT "; print_expr exp ; print_string "\n"
  | If(cond,block1,block2) -> if block2<>[] then 
      (print_string "IF " ; print_cond cond ; print_string "\n" ;print_block block1 (indent+2);
      print_indentation indent ; print_string "ELSE\n" ; print_block block2 (indent+2) )
      else (print_string "IF " ; print_cond cond ; print_string "\n" ;print_block block1 (indent+2) )
  |While(cond,block) ->  print_string "WHILE "; print_cond cond ; print_string "\n" ; print_block block (indent+2)
;;

(* impression d'un programme polish sur le terminal*)
(* print_program : program -> int -> unit*)
let print_program prog ind0=
  print_block prog ind0 ;;
