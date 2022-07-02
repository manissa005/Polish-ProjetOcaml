open CreateTypes ;;
(*************************************************SIMPLIFICATION***************************************)
(*fonction qui prend une expression en paramètre, la simplifie selon ce qui est demandé dans le projet 
et renvoie une expression simplifié *)
(*simpl_expression : expr->expr *)
let rec simpl_expression expr=
  match expr with
  |Num(i) -> Num(i)
  |Var(v) -> Var(v)
  |Op (op,expr1,expr2)->
  match (expr1,expr2) with 
  |(Num(a),Num(b)) -> begin match op with
                      |Add -> Num(a+b)
                      |Sub -> Num(a-b)
                      |Mul -> Num(a*b)
                      |Div -> if b<> 0 then Num(a/b) else expr
                      |Mod -> if a=0 && b=0  then expr else Num(a mod b)
                      end
  |(Var(a),Var(b))-> if a=b then 
                        if op=Div then Num(1) 
                        else if op=Mod || op=Sub then Num(0)
                        else expr
                     else expr
  |(Num(a),Var(i))-> begin match op with
                     |Add -> if a=0 then Var(i) else expr
                     |Mul -> if a=0 then Num(0) else if a=1 then Var(i) else expr
                     |Div -> if a=0 then Num(0) else expr
                     |Mod -> if a=0 then Num(0) else expr
                     |Sub -> expr 
                    end 
  |(Var(i),Num(a))-> if op=Add || op=Sub then if a=0 then Var(i) else expr
                     else if op=Mul then if a=0 then Num(0) else if a=1 then Var(i) else expr
                     else if op=Div then if a<>0 then if a=1 then Var(i) else expr else expr 
                     else  if a=1 then Num(0) else  expr
  |(Num(a),exp)->begin match op with
                |Add -> if a=0 then simpl_expression exp else expr
                |Mul -> if a=0 then Num(0) else if a=1 then simpl_expression exp else expr
                |Div -> if a=0 then Num(0) else expr
                |Mod -> if a=0 then Num(0) else expr
                |Sub -> expr 
                end 
  |(exp,Num(a))->begin match op with
                |Add -> if a=0 then simpl_expression exp else expr
                |Mul -> if a=0 then Num(0) else if a=1 then simpl_expression exp else expr
                |Div -> if a=0 then Num(0) else expr
                |Mod -> if a=0 then Num(0) else expr
                |Sub -> if a=0 then  simpl_expression exp else expr
                end
  |(Var(i),exp)-> simplifie_exp_var exp (Var(i)) op
  |(exp,Var(i))-> simplifie_exp_var exp (Var(i)) op
  |_-> simpl_expression (Op(op,simpl_expression expr1, simpl_expression expr2))
  
  (*après la simplification si on obtient une expression de la forme (op,exp1,exp2) on n'applique plus
  la récursion pour ne pas tomber dans une boucle infinie *)
and simplifie_exp_var exp var op=
  let exp_simpl = simpl_expression exp in
  match exp_simpl with
  |Op(opp,exp1,exp2)->Op(op, var,  exp_simpl)
  |_->simpl_expression (Op(op, var,  exp_simpl))
;;

(*fonction pour simplifier une condition *)
(*simpl_condition : cond -> cond*)
let simpl_condition cond =
  let (expr1,comp,expr2)=cond in
  (simpl_expression expr1,comp,simpl_expression expr2)
;;

(*fonction pour simplifier un block et un instruction mutuellement *)
(*simpl_block : block -> block *)
let rec simpl_block block =
  match block with
  |[] -> []
  |(pos,instr)::block_res -> (pos,simpl_instruction instr )::simpl_block block_res
and 
(*simpl_instruction : instr -> instr *)
simpl_instruction instr =
  match instr with
  |Print(expr) -> Print(simpl_expression expr)
  |Read(i) -> Read(i)
  |Set(i,expr) -> Set(i,simpl_expression expr)
  |If(cond,block1,block2)-> If(simpl_condition cond, simpl_block block1, simpl_block block2)
  |While(cond,block) -> While(simpl_condition cond, simpl_block block)
;;
(*simpl_program : program -> program  *)
let simpl_program program=
  simpl_block program
;;

(*elimine le code mort d'un block et renvoie un block propre *)
(*code_mort_block : block -> block*)
let rec code_mort_block  block = 
  match block with
  |[]->[]
  |(pos,instr)::block_res -> (code_mort_instr instr pos)@ code_mort_block block_res
and
(*elimine le code mort d'une instruction if ou while et renvoie un block propre  *)
(*code_mort_instr : instr -> position -> block*)
code_mort_instr instr pos = 
  match instr with
  |If(cond,block1,block2)-> verify_cond cond block1 block2 instr pos
  |While(cond,block) -> verify_cond cond block [] instr pos
  |_->[(pos,instr)]
and 
(*fonction auxiliaire qui vérifie si les conditions de la boucle If/While sont vraies ou fausses
et elimine ensuite le code mort dans les block de ces boucles *)
(*verify_cond : cond -> block -> block -> instr -> position -> block*)
verify_cond cond block1 block2 instr pos =
  let (exp1,comp,exp2)=cond in
  match (exp1,exp2) with
  |(Num(a),Num(b))-> if comp=Eq then if a=b then code_mort_block block1 else code_mort_block block2
                     else if comp=Ne then if a<>b then code_mort_block block1 else code_mort_block block2
                     else if comp=Lt then if a<b then code_mort_block block1 else code_mort_block block2
                     else if comp=Le then if a<=b then code_mort_block block1 else code_mort_block block2
                     else if comp=Gt then if a>b then code_mort_block block1 else code_mort_block block2
                     else if a>=b then code_mort_block block1 else code_mort_block block2
  |(Var(a),Var(b)) ->if a=b then if comp=Eq then code_mort_block block1 else code_mort_block block2
                     else false_cond instr pos
  |_->false_cond instr pos

and
(*fonction auxiliaire qui va éliminer le code mort dans une boucle if
ou while dans le cas où la condition de cette boucle n'est pas vérifiée *)
(*false_cond : instr -> pos -> block*)
false_cond instr pos= 
match instr with
|If(cond,b1,b2) ->[(pos,If(cond,code_mort_block b1,code_mort_block b2))]
|While(cond,b) ->[(pos,While(cond,code_mort_block b))]
|_->[(pos,instr)]
;;

(*élimine le code mort d'un program *)
(*code_mort_prog : program -> program *)
let code_mort_prog prog =
  code_mort_block prog 
;;

