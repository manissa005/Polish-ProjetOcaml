open CreateTypes;;

type sign = Neg | Zero | Pos | Error

module SigneTbl = Map.Make(String) 

(* vérifie si l'élément a figure dans la liste l et retourne un boolean*)
(*isInList : a' -> a' list -> boolean*)
let rec isInList a l =
  match l with
  | [] -> false
  | w::ls -> w=a || isInList a ls
;;


(* list_sans_doublons : supprime les doublons d'une liste donnée en paramètres et la retourne*)
(*list_sans_doublons: a' list -> a' list -> a' list*)
let rec list_sans_doublons list res=
  match list with
  |[] -> res
  |x::ls -> if isInList x res then list_sans_doublons ls res else list_sans_doublons ls (res@[x])
;;



(* renvoie le signe de l'entier i*)
(*signInt : int -> sign*)
let signInt i=
  if i=0 then [Zero]
  else if i>0 then [Pos]
  else if i<0 then [Neg]
  else [Error]
;;


(* signAdd : retourne la liste de signes de l'addition de deux variables selon leurs signes sign1+sign2 *)
(*signAdd : sign -> sign -> sign list*)
let signAdd sign1 sign2=
  match sign1 with
  |Pos -> begin match sign2 with
          |Pos ->[Pos]
          |Neg -> [Neg;Zero;Pos]
          |Zero -> [Pos]
          |_-> [Error]
        end
  |Neg -> begin match sign2 with
          |Pos ->[Neg;Zero;Pos]
          |Neg -> [Neg]
          |Zero -> [Neg]
          |_-> [Error]
        end
  |Zero ->[sign2]
  |Error ->[Error]
;;
(*signAddList: retourne la liste de signes à partir de deux listes données, lors d'une addition
de deux expressions*)
(*signAddList: sign list -> sign list -> sign list*)
let rec signAddList list1 list2=
  match list1 with
  |[] -> []
  |x::l1 ->begin match list2 with
            |[]->[]
            |y::l2 ->list_sans_doublons ((signAdd x y)@(signAddList [x] l2)@(signAddList l1 list2)) []
          end
;;

(* signSub : retourne la liste de signes de la soustraction de deux variables 
selon leurs signe; sign1-sign2 *)
(* signSub : sign -> sign -> sign list*)
let signSub sign1 sign2 =
  match sign1 with
  |Pos -> begin match sign2 with
          |Pos ->[Neg;Zero;Pos]
          |Neg -> [Pos]
          |Zero -> [Pos]
          |_-> [Error]
        end
  |Neg -> begin match sign2 with
          |Pos ->[Neg]
          |Neg -> [Neg;Zero;Pos]
          |Zero -> [Neg]
          |_-> [Error]
        end
  |Zero ->begin match sign2 with
          |Pos -> [Neg]
          |Neg -> [Pos]
          |Zero -> [Zero]
          |_-> [Error]
        end
  |Error ->[Error]
;;

(*signSubList: retourne la liste de signes à partir de deux listes données, lors d'une soustraction
de deux expressions*)
(*signSubList: sign list -> sign list -> sign list*)
let rec signSubList list1 list2=
  match list1 with
  |[] -> []
  |x::l1 ->begin match list2 with
            |[]->[]
            |y::l2 ->list_sans_doublons ((signSub x y)@(signSubList [x] l2)@(signSubList l1 list2)) []
          end
;;

(* signMul : retourne la liste de signes de la multiplication de deux variables 
selon leurs signe; sign1 * sign2 *)
(* signMul : sign -> sign -> sign list*)
let signMul sign1 sign2=
  match sign1 with
  |Pos -> begin match sign2 with
          |Pos ->[Pos]
          |Neg -> [Neg]
          |Zero -> [Zero]
          |_-> [Error]
        end
  |Neg -> begin match sign2 with
          |Pos ->[Neg]
          |Neg -> [Pos]
          |Zero -> [Zero]
          |_-> [Error]
        end
  |Zero ->[Zero]
  |Error -> begin match sign2 with
            |Zero -> [Zero]
            |_->[Error]
          end
;;

(*signMulList: retourne la liste de signes à partir de deux listes données, lors d'une multiplication
de deux expressions*)
(*signMulList: sign list -> sign list -> sign list*)
let rec signMulList list1 list2=
  match list1 with
  |[] -> []
  |x::l1 ->begin match list2 with
            |[]->[]
            |y::l2 ->list_sans_doublons ((signMul x y)@(signMulList [x] l2)@(signMulList l1 list2)) []
          end
;;

(* signDiv : retourne la liste de signes de la division de deux variables 
selon leurs signes; sign1 / sign2 *)
(* signDiv : sign -> sign -> sign list*)
let signDiv sign1 sign2=
  match sign2 with
  |Zero -> [Error]
  |_->signMul sign1 sign2
;;

(*signDivList: retourne la liste de signes à partir de deux listes données, lors d'une division
de deux expressions*)
(*signDivList: sign list -> sign list -> sign list*)
let rec signDivList list1 list2=
  match list1 with
  |[] -> []
  |x::l1 ->begin match list2 with
            |[]->[]
            |y::l2 ->list_sans_doublons ((signDiv x y)@(signDivList [x] l2)@(signDivList l1 list2)) []
          end
;;

(* signMod : retourne la liste de signes du modulo de deux variables 
selon leurs signes; sign1 mod sign2 *)
(* signMod : sign -> sign -> sign list*)
let signMod sign1 sign2=
  match sign1 with
  |Pos -> begin match sign2 with
          |Pos ->[Zero;Pos]
          |Neg -> [Neg;Zero;Pos]
          |_->[Error]
        end
  |Neg -> begin match sign2 with
          |Pos ->[Neg;Zero;Pos]
          |Neg -> [Neg;Zero;Pos]
          |_->[Error]
        end
  |Zero ->[Zero]
  |Error -> [Error]
;;

(*signModList: retourne la liste de signes à partir de deux listes données, lors d'un modulo
de deux expressions*)
(*signModList: sign list -> sign list -> sign list*)
let signModList list1 list2=
  match list1 with
  |[] -> []
  |x::l1 ->begin match list2 with
            |[]->[]
            |y::l2 ->list_sans_doublons ((signDiv x y)@(signDivList [x] l2)@(signDivList l1 list2)) []
          end
;;


(* sign_expr: retourne l'ensembles de listes des signes des variables d'une expression*)
(*sign_expr : expr -> SigneTbl.t -> position -> SigneTbl.t *)
let rec sign_expr expr env pos=
  match expr with
  |Num(i) -> signInt i
  |Var(i) ->if SigneTbl.mem i env then SigneTbl.find i env
            else begin 
              prerr_string ("ATTENTION : la variable ( "^ i ^" ) n'est pas initialisée à la ligne "^
              string_of_int pos ^" !\n");
            exit 1 end
  |Op(op,expr1,expr2) -> begin match op with
                        |Add -> signAddList (sign_expr expr1 env pos) (sign_expr expr2 env pos)
                        |Sub-> signSubList (sign_expr expr1 env pos) (sign_expr expr2 env pos)
                        |Mul -> signMulList (sign_expr expr1 env pos) (sign_expr expr2 env pos)
                        |Div -> signDivList (sign_expr expr1 env pos) (sign_expr expr2 env pos)
                        |Mod -> signModList (sign_expr expr1 env pos) (sign_expr expr2 env pos)
                      end
;;

(* notEqual_sign: retourne false si sign1==sign2 true sinon*)
(*notEqual_sign : sign -> sign -> boolean *)
let notEqual_sign sign1 sign2=
  if sign1== sign2 then false 
  else if sign1==Error || sign2==Error then false
  else true
;;


(* lessThen_sign :retourne true si sign1 < sign2 false sinon*)
(* lessThen_sign : sign -> sign -> boolean *)
let lessThen_sign sign1 sign2=
  match sign1 with
  |Neg -> if sign2!= Error then true else false
  |Zero -> if sign2=Pos then true else false
  |Pos-> if sign2=Pos then true else false
  |Error-> false
;;

(* greatThen_sign :retourne true si sign1 > sign2 false sinon*)
(* greatThen_sign : sign -> sign -> boolean *)
let greatThen_sign sign1 sign2=
  match sign1 with
  |Neg -> if sign2=Neg then true else false
  |Zero -> if sign2=Neg then true else false
  |Pos-> if sign2!=Error then true else false
  |Error-> false
;;

(* equi_sign_exp : retourne true si deux listes de signes peuvent etre égales pour comparer
si deux expressions peuvent etre égales selon leurs signes, false sinon*)
(*equi_sign_exp: sign list -> sign list -> boolean*)
let rec equi_sign_exp list1 list2=
  match list1 with
  |[]->false
  |s::ls -> if isInList s list2 then true else equi_sign_exp ls list2
;;

(* notEqual_exp_sign: sign list -> sign list -> sign list*)
let rec notEqual_exp_sign list1 list2 listRes=
  match list1 with 
  |[] -> listRes
  |sign::ls ->notEqual_exp_sign ls list2 (list_sans_doublons ((List.map (notEqual_sign sign) list2)@listRes) [])
;;


(* on compare tous les signe des deux listes un par un et on renvoie une liste de boolean*)
(* lessThen_exp_sign: sign list -> sign list -> boolean list *)
let rec lessThen_exp_sign list1 list2 listRes=
  match list1 with
  |[]->listRes
  |sign::ls -> lessThen_exp_sign ls list2 (list_sans_doublons ((List.map (lessThen_sign sign) list2)@listRes) [])
;;

(* greatThen_exp_sign : sign list -> sign list -> boolean list *)
let rec greatThen_exp_sign list1 list2 listRes=
  match list1 with
  |[]->listRes
  |sign::ls -> greatThen_exp_sign ls list2 (list_sans_doublons ((List.map (greatThen_sign sign) list2)@listRes) [])
;;

(* sign_cond: renvoie true si la condition peut etre satisfaites selon les deux expressions 
de la condition, false sinon *)
(*sign_cond : expr -> comp -> expr -> SIgneTbl.t -> position*)
let rec sign_cond expr1 comp expr2 env pos=
  match comp with
  |Eq -> equi_sign_exp (sign_expr expr1 env pos) (sign_expr expr2 env pos)
  |Ne -> let listVerif =notEqual_exp_sign (sign_expr expr1 env pos) (sign_expr expr2 env pos) [] in
         if isInList true listVerif then true else false
  |Lt -> let listVerif =lessThen_exp_sign (sign_expr expr1 env pos) (sign_expr expr2 env pos) [] in
         if isInList true listVerif then true else false
  |Le -> if (sign_cond expr1 Eq expr2 env pos) then true 
         else sign_cond expr1 Lt expr2 env pos
  |Gt -> let listVerif =greatThen_exp_sign (sign_expr expr1 env pos) (sign_expr expr2 env pos) [] in
         if isInList true listVerif then true else false
  |Ge -> if (sign_cond expr1 Eq expr2 env pos) then true 
         else sign_cond expr1 Gt expr2 env pos
;;


(* fonctions utilisées dans sign_block*)

(*compRev : retourne l'inverse d'un comparateur*)
(*compRev : comp -> comp*)
let compRev comp=
  match comp with
  |Eq -> Ne
  |Ne -> Eq
  |Lt -> Ge
  |Le -> Gt
  |Gt -> Le
  |Ge -> Lt
;;

(* join_env : fait un merge de deux environnement*)
(* join_env : -> SigneTbl.t -> SigneTbl.t -> SigneTbl.t*)
let join_env env1 env2=
  SigneTbl.merge 
  (fun key list1 list2 -> match (list1,list2) with
    |(Some(li1), Some(li2))->Some(list_sans_doublons (li1@li2) [])
    |(None,Some(li2))->Some(li2)
    |(Some(li1),None)->Some(li1)
    |_->None
  ) env1 env2
;;

(* verify_equal_List : renvoie true si li1 et li2 ont les memes élément*)
(*verify_equal_List : a' list -> a' list -> boolean*)
let verify_equal_List li1 li2=
  let rec aux_equal_list l1 l2=
    match l1 with
    |[] -> true
    |x::ls -> if (isInList x l2) then aux_equal_list ls l2 else false
    in aux_equal_list li1 li2
;;


(*sign_block : renvoie un couple d'environnemnt des listes de signes de chaque variable 
et une liste de position où une division par zero risque de se produire*)
(* sign_block -> block -> SigneTbl.t -> position list -> SigneTbl.t * position list *)
let rec sign_block block env listERR=
  match block with
  |[]->(env,listERR)
  |(pos,instr)::block_res -> let (env,listERR)=sign_instr instr env pos listERR in
   (sign_block block_res env listERR)

and 
(*sign_instr : instr -> SigneTbl.t -> position -> position list -> SigneTbl.t * position list *)
sign_instr instr env pos listERR=
  match instr with
  |Read(i) -> ((SigneTbl.add i [Neg;Zero;Pos] env),listERR)
  |Print(expr)->if (isInList Error (sign_expr expr env pos)) then (env,(listERR@[pos]))  
                else (env,listERR)
  |Set(i,expr) -> if (isInList Error (sign_expr expr env pos)) then
                ((SigneTbl.add i (sign_expr expr env pos) env),(pos::listERR))
                else ((SigneTbl.add i (sign_expr expr env pos) env),listERR)
  |If(cond,block1,block2)->let newEnv= env in let (exp1,comp,exp2)=cond in
                    let (if_sign,listERR)= if(sign_cond exp1 comp exp2 env pos) then sign_block block1 env listERR
                    else (env,listERR) in
                    let (else_sign,listERR)= if(sign_cond exp1 (compRev comp) exp2 env pos) then sign_block block2 env listERR
                    else (env,listERR) in
                    ((join_env (join_env if_sign else_sign ) newEnv),listERR)
  |While(cond,block)->points_fixes listERR (aux_sign_while cond block pos listERR) env
                    
and aux_sign_while cond block pos listERR env =
  let (exp1,comp,exp2)=cond in
  let newEnv=env in
  if (sign_cond exp1 comp exp2 env pos) then ((join_env (fst (sign_block block env listERR)) newEnv),listERR)
  else (env,listERR)

(*calcul des points fixes d'une boucle while*)
and points_fixes listERR fonc l =
    let l1= fst (fonc l )in
    if SigneTbl.equal verify_equal_List l l1 then (l,listERR) else points_fixes listERR fonc l1 
;;

(* affichage d'un signe*)
(*printSign : sign -> unit*)
let printSign sign=
  match sign with 
  |Pos->print_string "Pos "
  |Zero ->print_string "Zero "
  |Neg->print_string "Neg "
  |Error ->print_string "Error "
;;

(* affichage de tous les éléments d'une liste*)
(* printList: a' list -> unit*)
let rec printList l=
  match l with
  |[]->print_string ""
  |x::ls -> printSign x; printList ls
;;

(* affichage d'une liste de signes*)
let print_list_sign var listSign=
  print_string (var ^" :: "); printList listSign; print_string "\n"
;;

(* affichage de la liste des positions de la division par zero *)
let rec print_list_ERR list=
  match list with
  |[]-> print_string ""
  |x::ls -> print_string ("DIVBYZERO at line "^string_of_int x ^"\n") ; print_list_ERR ls
;;

(* affichage du message "SAFE" s'il n'y a aucune division par zero dans le programme*)
let print_ERROR_List list=
  if list=[] then print_string "SAFE \n"
  else print_list_ERR list
  ;;





