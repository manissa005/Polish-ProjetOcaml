open CreateTypes ;;
(*********************************************VARIABLES NON INITIALISEES***********************************)

module Names = Set.Make(String)

(* fonctions pour affichage de la premiere ligne de la commande --vars , avec toutes les variables 
du programme polish dans un ordre quelconque et sans redondances*)


(* fonction qui renvoie un ensemble de toutes les variables apparaissant dans une expression, sans répétition*)
(* get_list_variables_expr : expr -> Names.t -> Names.t *)
let rec get_variables_expr expr set1=
  match expr with
  |Num(i)-> Names.empty 
  |Var(a) -> Names.add a set1
  |Op(op,expr1,expr2) -> Names.union (get_variables_expr expr1 set1) (get_variables_expr expr2 set1)
;;

(* fonction qui renvoie un ensemble de toutes les variables apparaissant dans une condition, sans répétition*)
(* get_list_variables_cond : cond -> Names.t -> Names.t *)
let  get_variables_cond cond set=
  let (expr1,comp,expr2)=cond in
  Names.union (get_variables_expr expr1 set) (get_variables_expr expr2 set)
;;

(* fonction qui renvoie un ensemble de toutes les variables apparaissant dans un block, sans répétiton*)
(* get_list_variables_block : block -> Names.t -> Names.t *)
let rec get_variables_block block set=
  match block with
  |[]->Names.empty
  |(pos,instr)::block_res -> Names.union (get_variables_instr instr set) (get_variables_block block_res set)
and
(* fonction qui renvoie un ensemble de toutes les variables apparaissant dans une instruction, sans répétiton*)
(* get_list_variables_instr : instr -> Names.t -> Names.t *)
get_variables_instr instr set=
  match instr with
  |Print(expr) -> get_variables_expr expr set
  |Read(i) -> Names.add i set
  |Set(i,expr) -> Names.union (Names.add i set) (get_variables_expr expr set)
  |If(cond,block1,block2)-> 
Names.union (Names.union (get_variables_cond cond set) (get_variables_block block1 set)) (get_variables_block block2 set)
  |While(cond,block) ->Names.union (get_variables_cond cond set) (get_variables_block block set)
;;

(* fonction qui renvoie un ensemble de toutes les variables d'un programme polish et sans redondances*)
(* get_list_variables_prog : program -> Names.t -> Names.t *)
let get_variables_prog prog set=
  get_variables_block prog set
;;

(*fonction d'affichage de tous les éléments d'un ensemble*)
(*print_set : Names.t -> unit*)
let print_set s = 
  Names.iter (fun a -> print_string a; print_string " ") s
;;



(***********************************************************************************************************)

(* fonctions pour affichage de la deuxième ligne de la commande --vars, pour les variables non-initialisées*)

(*get_vars_non_init_expr: renvoie un couple d'ensembles de variables initialisées et non initialisées d'une expression*)
(*init : est l'ensemble de variables initialisées*)
(*inotInit : est l'ensemble de variables non initialisées*)
(*get_vars_non_init_expr : expr -> Names.t -> Names.t -> Names.t*Names.t *)
let rec get_vars_non_init_expr expr notInit init   =
  match expr with
  |Num(i) ->(notInit,init) 
  |Var(i) -> if not(Names.mem i init) then (Names.add i notInit,init) else (notInit,init) 
  |Op(op,expr1,expr2) -> let set1= fst (get_vars_non_init_expr expr1 notInit init ) 
                          and set2= fst (get_vars_non_init_expr expr2 notInit init ) in (Names.union set1 set2,init)
;;  
(*get_vars_non_init_cond : renvoie l'union des deux ensembles des variables non initialisées
 des deux expressions d'une condition*)
(*get_vars_non_init_cond : cond -> Names.t -> Names.t -> Names.t*)
let get_vars_non_init_cond cond notInit init =
  let (expr1,comp,expr2)=cond in
  let e1= fst (get_vars_non_init_expr expr1 notInit init) in
  let e2= fst (get_vars_non_init_expr expr2 notInit init) in Names.union e1 e2
  ;;
(* get_vars_non_init_block: renvoie l'ensemble des variables non-initialisées d'un block*)
(*get_vars_non_init_block : block -> Names.t -> Names.t -> Names.t*)
let rec get_vars_non_init_block block result init= 
  match block with
  |[] -> result
  |(pos,instr)::block_rest -> let (set1,set2) = get_vars_non_init_instr instr result (Names.empty) init  in
                              get_vars_non_init_block block_rest (Names.union set1 result) set2

(* get_vars_non_init_instr :renvoie un couple d'ensembles de variables initialisées et non initialisées 
d'une instruction*)
(*get_vars_non_init_instr : instr -> Names.t -> Names.t -> Names.t -> Names.t*Names.t *)
and get_vars_non_init_instr instr result notInit init = 
  match instr with
  |Read (i) -> (notInit,Names.add i init)
  |Print(expr) -> get_vars_non_init_expr expr notInit init
  |Set(i,expr) -> get_vars_non_init_expr expr notInit (Names.add i init)
  |If(cond,block1,block2) ->let varsCond= get_vars_non_init_cond cond notInit init in
                            let varsBlock1 = get_vars_non_init_block block1 result init in 
                            let varsBlock2 = get_vars_non_init_block block2 result init in
                            (Names.union varsCond (Names.union varsBlock1 varsBlock2),init)
  |While(cond,block) ->let varsCond= get_vars_non_init_cond cond notInit init in
                       let varsBlock=get_vars_non_init_block block result init in
                      (Names.union varsCond varsBlock,init)
;;

(* get_vars_non_init_prog: renvoie l'ensemble des variables non-initialisées d'un programme*)
(*get_vars_non_init_prog : program -> Names.t -> Names.t -> Names.t*)
let get_vars_non_init_prog prog res init=
  get_vars_non_init_block prog res init
;;
     