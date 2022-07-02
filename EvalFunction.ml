open CreateTypes;;
(**************************************************************EVALUATION********************************************************)

(* verification d'indentation , si l'indentation est incorrecte on renvoie un couple de boolean false 
et le numéro de la ligne mal indentée*)
(* verify_indentation : position*string list -> int -> boolean*int *)
let rec verify_indentation list ind0 =
  match list with
  |[] -> (true,0)
  |(pos,line) ::ls -> let indent = indentation_string line 0 in
      if indent <> ind0 then (false,pos)
      else 
        let list_words = line_split_on_words line  in
        match list_words with
        |[] -> (true,0)
        |fst_word:: li  when (fst_word="IF" || fst_word="ELSE" || fst_word = "WHILE") -> 
            if (fst (recupere_ss_block ls [] ind0) = []) then
              begin
                print_string("ERREUR : format incorrect de l'instruction à la ligne "^string_of_int pos ^ " !! Block vide !!\n" );
                exit 1;
              end
            else
              let (fst,snd) = recupere_ss_block ls [] ind0  in
              let (verify1,pos1) = verify_indentation fst (ind0+2) in
              let (verify2,pos2) = verify_indentation snd ind0 in
              if (not verify1) then (false,pos1)
              else (verify2,pos2)
        |_->verify_indentation ls ind0
;;

(*eval_expression : expr -> Hashtbl.t -> int*)
let rec eval_expression expr env pos=
  match expr with
  |Num (i) -> i
  |Var(i) ->if Hashtbl.mem env i then Hashtbl.find env i 
            else begin 
              prerr_string ("ATTENTION : la variable ( "^ i ^" ) n'est pas initialisée à la ligne "^string_of_int pos ^" !\n");
            exit 1 end
  |Op (op,expr1,expr2) -> 
      match op with
      |Add -> (eval_expression expr1 env pos) + (eval_expression expr2 env pos)
      |Sub -> (eval_expression expr1 env pos) - (eval_expression expr2 env pos)
      |Div -> if (eval_expression expr2 env pos =0) then begin 
      prerr_string ("ERREUR : impossible de déviser par 0  à la ligne "^string_of_int pos ^" !\n") ; exit 1 end
          else (eval_expression expr1 env pos) / eval_expression expr2 env pos
      |Mod -> if (eval_expression expr2 env pos) > 0 then (eval_expression expr1 env pos) mod (eval_expression expr2 env pos)
          else begin prerr_string ("ERREUR : impossible de déviser par 0 à la ligne "^string_of_int pos ^" !\n") ; exit 1 end
      |Mul -> (eval_expression expr1 env pos) * (eval_expression expr2 env pos)
;;

(*eval_condition: renvoie true si la condition est vérifiée false si non *)
(* eval_condition : cond-> Hashtbl.t -> boolean*)
let eval_condition cond env pos= 
  let (expr1,comp,expr2)= cond in
  let eval_expr1 = eval_expression expr1 env pos in
  let eval_expr2= eval_expression expr2 env pos in
  match comp with
  | Eq -> if eval_expr1 = eval_expr2 then true else false
  | Ne -> if eval_expr1 <> eval_expr2 then true else false
  | Lt -> if eval_expr1 < eval_expr2 then true else false
  | Le -> if eval_expr1 <= eval_expr2 then true else false
  | Gt -> if eval_expr1 > eval_expr2 then true else false
  | Ge -> if eval_expr1 >= eval_expr2 then true else false
;;

(* eval_block : block -> Hashtbl.t -> unit *)
let rec eval_block block env =
  match block with
  |[]->()
  |(pos,instr)::block_res-> eval_instruction instr env pos ; eval_block block_res env
and
  eval_instruction instr env pos=
  match instr with
  |Print(expr) -> Printf.printf " %d \n"   (eval_expression expr env pos)
  |Read(i)-> Printf.printf "%s ?" i ;  Hashtbl.add env i (read_int())
  |Set(i,expr)-> Hashtbl.add env i (eval_expression expr env pos)
  |If(cond,block1,block2) -> if eval_condition cond env  pos then eval_block block1 env
      else eval_block block2 env
  |While(cond,block) -> while (eval_condition cond env pos) do eval_block block env; done

;;  
(* evaluation d'un program *)
let eval_program program env =
  eval_block program env
;; 