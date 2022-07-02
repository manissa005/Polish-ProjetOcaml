open ReprintFunction;;
open CreateTypes;;
open EvalFunction;;
open Simplification ;;
open InitVariables ;;
open Sign;;

(*read_file: fonction pour lire un fichier polish ligne par ligne, en ignorant les commentaires du 
programme ainsi que les sauts de lignes, elle renvoie une liste de couples de position et de la ligne 
à cette position*)
(*read_file : string -> position*String list *)
let read_file filename =
  let file = open_in filename  in
  let rec aux  k lines=
    try
      let ligne=input_line file in
      match ligne with
      |"\n" ->aux (k+1) lines
      |_-> let l=line_split_on_words ligne in if l=[] then aux (k+1) lines else 
            match (List.hd l) with
            |"COMMENT"->aux (k+1) lines
            |_->aux (k+1) (lines@[(k,ligne)])
    with End_of_file ->
      close_in file;
      lines ; in
  aux 1 []
;;

(*read_polish : lit le contenu d'un fichier polish à partir d'un chemin donnée vers le fichier, et retourne
le programme polish correspendant si aucune erreur d'indentation ou autre n'est présente, sinon échoue avec
une erreur*)
(*read_polish : string -> program*)
let read_polish (filename:string) : program = 
  let list = read_file filename in
  let(fst,snd)=verify_indentation list 0 in
  if fst then   create_program (read_file filename)
  else begin
    print_string ("ERREUR : erreur d'indentation à la ligne "^string_of_int snd^"\n");
    exit 1; end
;;

(* réaffichage d'un programe polish*)
let print_polish (p:program): unit = 
  print_program p 0 
;;
(* evaluation d'un programme polish*)
let eval_polish (p:program) : unit = 
  let env = Hashtbl.create 20 in eval_program p env
;;
(* simplification du code d'un programme polish et élimination du code mort*)
let simpl_polish (p:program)=
  code_mort_prog (simpl_program p)
;;

(*affichage de toutes les variables d'un programme polish ainsi que les variables non initialisées 
du programme, biensur après la simplification de ce dernier*)
let vars_polish (p:program) : unit =
  print_set (get_variables_prog p Names.empty) ;
  print_string "\n" ;
  print_set (get_vars_non_init_prog p Names.empty Names.empty);
  print_string "\n"
;; 

let sign_polish (p:program):unit=
  let (res,list) =sign_block p SigneTbl.empty [] in 
  SigneTbl.iter (print_list_sign) res;
  print_string "\n";
  print_ERROR_List list
;;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage : Polish\n";
  print_string "USAGE:\n 
  * Pour reafficher votre programme polish :
  ./run --reprint [chemin vers le fichier contenant votre programme]\n
  * Pour évaluer votre programme polish : 
  ./run --eval [chemin vers le fichier contenant votre programme]\n
  * Pour simplifier et éliminer le code mort de votre programme : 
  ./run --simpl [chemin vers le fichier contenant votre programme]\n
  * Pour afficher toutes les variables de votre programme, ainsi que les variables non-initiaisées :
   ./run --vars [chemin vers le fichier contenant votre programme]\n
  * Pour afficher les signes possibles de chaque variable du programme, à la fin de l'exécution : 
  ./run --sign [chemin vers le fichier contenant votre programme]\n"
  ;;
  let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] ->print_polish (read_polish (file))
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | [|_;"--simpl";file|] ->  print_polish (simpl_polish (read_polish file))
  | [|_;"--vars";file|] -> vars_polish (read_polish file)
  | [|_;"--sign";file|] -> sign_polish (read_polish file)
  | _ -> usage ()
          

(* lancement de ce main *)
let () = main ()
