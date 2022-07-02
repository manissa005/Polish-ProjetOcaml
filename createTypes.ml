(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
  | Eq (* = *)
  | Ne (* Not equal, <> *)
  | Lt (* Less than, < *)
  | Le (* Less or equal, <= *)
  | Gt (* Greater than, > *)
  | Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block

(******************************************************CREATION DE TYPES********************************************************)
(* prend un string et renvoie le nombre d'indentations dans ce string*)
(*indentation_string : string -> int -> int*)
let rec indentation_string line ind=
  match line with
  |""->ind
  |_-> if (line.[0]=' ') then indentation_string (String.sub line 1 ((String.length line)-1)) (ind+1) else ind
;;

(* on enlève tous les espaces et on ne récupère que les mots du string donné*)
(*line_split_on_words : string -> string list*)
let line_split_on_words line=
  let i= indentation_string line 0 in 
  let l= String.sub line i ((String.length line)-i) in
  match l with
  |"" -> []
  |_->String.split_on_char ' ' l
;;

let create_position num =
  if(num<1) then failwith "numéro de ligne erroné"
  else (num : position);; 

let create_variable var=
  (var : name)
;; 

  
(*fonction qui teste si un string est un entier représenté en string ou pas*) 
(* test_int : int -> boolean*)
let test_int x = 
  try int_of_string x |> ignore; true
  with Failure _ -> false;;

(* create_expr : elle prend une list de string et crée une expression à partir de cette liste*)
(* create_expr -> string list -> position -> expr*(string list) *)
let rec create_expr expr pos =
  match expr with
  | [] -> begin print_string ("ERREUR : format incorrect de l'expression à la ligne "^
            string_of_int pos^" !\n"); exit 1; end
  |op :: exp_rest -> begin
    match op with
    | "+" -> let (e1,res1)=create_expr exp_rest pos in let(e2,res2)=create_expr res1 pos in (Op(Add,e1,e2),res2)
    | "-" -> let (e1,res1)=create_expr exp_rest pos in let(e2,res2)=create_expr res1 pos in (Op(Sub,e1,e2),res2)
    | "*" -> let (e1,res1)=create_expr exp_rest pos in let(e2,res2)=create_expr res1 pos in (Op(Mul,e1,e2),res2)
    | "/" -> let (e1,res1)=create_expr exp_rest pos in let(e2,res2)=create_expr res1 pos in (Op(Div,e1,e2),res2)
    | "%" -> let (e1,res1)=create_expr exp_rest pos in let(e2,res2)=create_expr res1 pos in (Op(Mod,e1,e2),res2)
    |_-> if (test_int op) then (Num(int_of_string op),exp_rest)
         else (Var(op),exp_rest)
  end
;;

(*fonction pour créer un opérateur de comparaison a partir d'un string représentant l'op*)
(*create_comp : string -> comp*)
let create_comp comp pos =
  match comp with 
  |"=" -> Eq
  |"<>" -> Ne
  |"<" ->Lt
  |"<="->Le
  |">"->Gt
  |">="-> Ge
  |_->failwith ("ERREUR : opérateur de comparaison incorrecte à la ligne "^string_of_int pos)
;;

(* vérifier si a est un opérateur de comparaison *)
(* is_op_comp : string -> boolean*)
let is_op_comp a=
  match a with 
  |"=" -> true
  |"<>" -> true
  |"<" -> true
  |"<="-> true
  |">"->true
  |">="-> true
  |_ ->false
;;

(* on récupère la première expression d'une condition sous forme d'une liste de string*)  
(* recupere_expr1 : string list -> string list -> string list*)
let rec recupere_expr1 l l1 =
  match l with 
  |[]-> []
  |[x]-> if (is_op_comp x) then l1 else [x]@l1
  |x::ls-> if (is_op_comp x) then l1 else [x]@(recupere_expr1 ls l1) 
;;

(* create_cond prend une condition : string*list  et renvoie expr*comp*expr => la première expression 
de la condition*le comparateur*la deuxième expression de la condition *)
(*create_cond : string list -> expr*comp*expr*)
let create_cond cond pos=
  let expr1 = recupere_expr1 cond [] in
  let rec aux l expr1=
    match l with 
    |[]-> begin print_string ("ERREUR : format incorrect de la condition à la ligne "^string_of_int pos^"\n"); exit 1; end
    |x::ls-> if (is_op_comp x) then (fst (create_expr expr1 pos),create_comp x pos,fst (create_expr ls pos)) else aux ls expr1
  in
  aux cond expr1
;;


(* recupere_ss_block : renvoie le sous block d'une boucle if, else ou while*)
(* recupere_ss_block : position*string list -> position*string list -> int -> position*string list *)
let rec recupere_ss_block list_line list_sous_block ind0=
  match list_line with
  |[]->(list_sous_block,list_line)
  |(pos,line)::ls -> let ind1 = indentation_string line 0 in
      if ind0<ind1 then  recupere_ss_block ls  (list_sous_block@[(pos,line)]) ind0
      else (list_sous_block,list_line)
;; 


 (*create_instr : string list -> string list -> position -> instr*)
let rec create_instr instruction res pos=
  let instr = line_split_on_words instruction in  
  match instr with
  | []  -> begin print_string ("ERREUR : format incorrect de l'instruction à la ligne "^string_of_int pos^"\n"); exit 1; end
  | "READ"::l -> if l<>[] then (Read(create_variable(List.hd l)),res) 
                 else begin print_string ("ERREUR : format incorrect de l'instruction à la ligne "^string_of_int pos^
                " ! Ajoutez une variables à lire\n"); exit 1; end
  | "PRINT"::l ->if l<>[] then (Print(fst (create_expr l pos)),res)
                 else begin print_string ("ERREUR : format incorrect de l'instruction à la ligne "^string_of_int pos^
                " ! Ajoutez une expression arithmétique à imprimer\n"); exit 1; end
  | var::":="::l-> let expr = fst (create_expr l pos) in
      let v = create_variable var in
      (Set(v,expr),res)
  |"WHILE" :: l -> let cond = create_cond l pos in
      let indent = indentation_string instruction 0 in
      let (bloc, reste) = recupere_ss_block res [] indent in 
      let (bloc_while,reste') = create_block bloc [] in 
      (While(cond,bloc_while),reste)
  |"IF"::l -> let cond = create_cond l pos in
      let indent = indentation_string instruction 0 in
      let (bloc1, reste1) = recupere_ss_block res [] indent in 
      let (bloc_if,reste') = create_block bloc1 [] in 
      if (reste1 == []) then 
        (If(cond,bloc_if,[]),reste1)
      else let ligne_els= snd (List.hd reste1) in
        let elsse = line_split_on_words ligne_els in
        if (List.hd elsse = "ELSE") then 
          let (bloc2,reste2) = recupere_ss_block (List.tl reste1) [] indent in
          let (bloc_else,reste'') = create_block bloc2 [] in 
          (If(cond,bloc_if,bloc_else),reste2)
        else
          (If(cond,bloc_if,[]),reste1)
|_ ->begin print_string ("ERREUR : format incorrect de l'instruction à la ligne "^string_of_int pos^"\n"); exit 1; end

(* create_block : position*string list -> position*string -> block *)
and create_block block res = 
  match block with
  | [] -> (List.rev res, block)
  | (position,ligne)::l ->
      let (instr, reste) = create_instr ligne l position in
      create_block reste ((position,instr)::res)
;;


(*create_program : position*string list -> program*)        
let create_program list_lines=
  match list_lines with
  |[]->begin print_string "ERREUR : Programme vide\n"; exit 1; end 
  |_ -> let (list,res)=create_block list_lines [] in list
;;
