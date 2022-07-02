# Analyse statique d'un mini-langage : Polish #

## Réalisé par : ##
* SMAINI Khalida - ID Gitlab : smaini - N° étudiant : 21964953
* BOUDJABOUT Manissa - ID Gitlab : boudjabo   N° étudiant : 22018741

## Fonctionnalités ##

Dans notre projet nous avons implémenté 6 fonctionnalités du langage polish, qui sont les suivantes :

1. **Lecture d'un fichier polish :**

on lit un fichier contenant un programme polish ligne par ligne avec la fonction **read_file** et on le traduit ensuite en syntaxe abstraite, selon les types indiqués dans le sujet, avec la fonction **create_program** ainsi que toutes les fonctions auxiliaires définies dans le module **createTypes.ml**. Bien évidemment si l'indentation du programme ou le format des types ne sont pas respectée, la lecture échoue en renvoyant un message d'erreur indiquant le type d'erreur et sa position dans le programme.

2. **Reprint :** 

Cette fonctionnalité permet à l'utilisateur de réafficher son programme polish sur le terminal en partant de la syntaxe abstraite obtenue à l'étape de la lecture du fichier polish. Pour cela, nous avons implémenté des fonction d'affichage dans le module **reprintFunction.ml** qui servent à afficher le programme tout en respectant l'indentation et en ignorant les commentaires et les sauts de lignes.

3. **Evaluation d'un programme polish :**

L'utilisateur a la possibilité d'executer son programme et l'évaluer instruction par instruction. Cette évaluation permet de lire un entier sur le terminal, évaluer toutes les expressions arithmétiques (addition, soustraction, multiplication, division et modulo) selon les valeurs données par l'utilisateur ou obtenues lors de l'évaluation donc toutes les variables du programme seront remplacées par leurs valeurs entieres. Cette évaluation permet également de vérifier si la condition d'une boucle if ou while est satisfaite, et cette boucle n'est éxécutée que dans le cas ou la condition est vraie. Les résultats sont affichés sur le terminal que s'il y a une instruction "PRINT variable". Si la syntaxe de programmation polish n'est pas respectée ou s'il y a des variables non-initialisée, l'évaluation échoue avec un message d'erreur indiquant sa position dans le programme. Les fonctions qui assurent l'évaluation sont implémentées dans **EvalFunction.ml** .

4. **Simplification d'un programme polish et élimination du code mort :**

Dans un programme polish, on peut trouver des expressions arithmétiques comme : * var 0 , + 0 expr ,   * 1 var .... etc. Cette fonctionnalité nous permet de simplifier toutes ces expressions.

Les expressions portant sur deux nombres sont simplifiées par le résultat de leur calcul. Si une expression est multipliée ou divisée par 1 elle sera remplacée seulement par l'expression elle meme, une opération de multiplication par zero est remplacée par zero.L'addition d'une expression à zero est remplacée par l'expression. Toutes ces simplification sont propagées autant que possible grace aux fonctions implémentées dans la premiere partie du fichier **simplification.ml**

Si, après la simplification, on obtient des conditions comparant deux constantes ou une meme variable, on pourra donc savoir si la condition est satisfaite ou non. Dans le cas où la condition d'une boucle if est satisfaite on remplace entierement cette boucle par son code, sinon par le code de sa boucle else si elle existe. Pareil pour une boucle while, si la condition n'est pas vérifiée la boucle est directement supprimée. On appele ça l'élimination du code mort qui se fait grace aux fonctions implémentées dans la deuxième partie du fichier **simplification.ml**

5. **Les variables initialisées et non-initialisées d'un programme Polish :**

Dans cette partie, on récupères d'abord toutes les variables figurant dans un programme polish donné, qu'on va afficher ensuite sur une première ligne, sans redondance. Ensuite, on récupère toutes les variables non-initialisées pouvant etre accédées avant leur première écriture et on les affiches sur une deuxième ligne, sans répétition. Les fonctions pour implémenter cette partie sont dans le fichier **initVariables.ml** .

6. **Les signes possibles de toutes les variables d'un programme Polish :**

Cette fonctionnalité permet d'afficher une ligne par variables d'un programme donné, en associant à chaque variable tous les signes possibles qu'elle peut prendre à la fin de l'execution du programme: Neg pour négatif, Pos pour positif, Zero pour zero, Error si une erreur risque de se produire due à une division par zero. Les fonctions pour implémenter cette partie sont dans le fichier **sign.ml** .

## Compilation et exécution du projet : ##

- Cloner notre dépot git du projet.
- Se placer dans le répertoire " pf5-projet/ "
- Lancer la commande **make** pour compiler le projet via dune.
- Pour Réafficher un programme polish, lancer la commande   **./run --reprint [ le chemin vers le fichier contenant le programme ]**
- Pour évaluer un programme polish, lancer la commande : **./run --eval [chemin vers le fichier contenant votre programme]**
- Pour simplifier et éliminer le code mort d'un programme polish, lancer la commande : **./run --simpl [chemin vers le fichier contenant votre programme]**
- Pour afficher toutes les variables de votre programme, ainsi que les variables non-initiaisées, lancer la commande : **/run --vars [chemin vers le fichier contenant votre programme]**
- Pour afficher les signes possibles de chaque variable du programme à la fin de l'exécution, ainsi qu'un message indiquant l'existance d'une division par zero ou non, lancer la commande : **./run --reprint [chemin vers le fichier contenant votre programme]**

## Découpage modulaire du code et le traitement assuré par chaque module: ##
Voici une brève description du role des modules utilisés, pour plus de précisions, nous vous invitons à lire les commentaires sur chaque fonction du module.
- **Polish.ml :** module principale pour lancer le projet

    - Lire un fichier polish ligne par ligne et le traduire en syntaxe abstraite avec les fonction (read_polish et read_file)
    - Réafficher un programme polish avec l'option --reprint grace à  (print_polish)
    - Evaluer un programme polish avec l'option --eval grace à la fonction (eval_polish)
    - Simplifier et éliminer le code mort d'un programe avec l'option --simpl grace à (simpl_polish)
    - Afficher toutes les variables d'un programme ainsi que les variables non initialisées avec l'option --vars grace à la fonction (vars_polish)
    - Afficher les signes possibles de chaque variable du proggramme avec l'option --sign grace à la fonction (sign_polish)
    - Si l'option entrée par l'utilisateur est diiférente de celles citées en dessus, un manuel d'utilisation est affiché grace à la fonction (usage)

- **createTypes.ml :** traduction d'un programme polish en syntaxe abstraite

    - Définition des types à utiliser
    - **create_expr :** Traduction d'une expression représentée par une liste de string en une expression du langage polish donc (op * expr * expr) || (Num i) || (Var i) a.
    - **create_comp :** Traduction d'un comparateur <>,=,<,>,<=,>= en un comparateur en syntaxe abstraite 
    - **create_cond :** Une condition sous forme d'une liste de string est traduite en une condition sous la forme expr * comp * expr.
    - **create_instr :** Une instructions sous forme d'une liste de string est traduite en une instruction de la syntaxe abstraite du langage polish.
    - **create_block :** Un block sous forme de liste de couple (position,instruction) est traduit en un program 

- **reprintFunction.ml :** Réaffichage d'un programme polish sur le terminal en ignorant les commentaires et les sauts de lignes. Après avoir lu un programme sous forme (position, instruction) on transforme chaque type du programme en string qui sera affiché sur le terminal.

- **EvalFunction.ml :** Vérification de l'indentation d'un programme polish; Evaluation d'un programme polish et affichage des résultats en cas de "PRINT" dans le programme.

- **simplification.ml :** simplification et élimination du code mort d'un proramme polish selon les critères de la section 1.4.1 du projet.

- **initVariables.ml :** Ce module est composé de deux partie de code. La première partie c'est pour récupérer un ensemble de toutes les variables d'un programme polish; la deuxième partie c'est pour récupérer un ensemble des variables pouvant etre accédées avant leur première écriture.

- **Sign.ml :** Affichage de toutes les variables d'un programme polish, une variable par ligne suivie de tous les signes possibles (Neg,Zero,Pos,Error) qu'elle peut avoir à la fin de l'exécution du programme. Une dernière ligne est affichée pour indiquer s'il y a une division par zero risque de se produire ou non.


## Organisation du travail : ##

Les taches ont été réparties de la manière suivante entre les membres de l'équipe :

- Rendu 1 :

    **SMAINI Khalida :** 

    1)- Fonctions Create (Fichier createTypes.ml) : 

    * indentation_string : pour récupérer l'indentation d'un string
    * line_split_on_words : décomposer un string en liste de mots .
    * create_comp,is_op_comp, recupere_expr1 et create_cond : créer une condition
    * recupere_ss_block : retourne le sous block d'un if , else ou while
   * create_program : créer un programme polish

    2)- Fonctions Reprint (Fichier EvalFunction.ml) :

    * Implémentation de verify_indentation pour vérifier si l'indentation d'un programme est respectée.
    * eval_expression : pour évaluer une expression arithmétique
    * eval_condition : évaluer une condition à vrai ou faux
    * eval_block : évaluer un block d'instructions
    * eval_instruction : évaluer une instruction
    * eval_program : évaluer un programme polish

    3)- Fonctions Polish (Fichier polish.ml) :

    * eval_polish : pour évaluer un programme polish
    * read_file et read_polish pour la lecture d'un fichier contenant un programme polish

     **BOUDJABOUT Manissa :**

    1)- Fonctions Create (Fichier createTypes.ml) : 

    * Implémentation de la fonction test_int qui teste si un String représente un Int (utilisé sans la fonction create_expr)
    * La fonction create_position
    * La fonction create_variable
    * create_expr (création d'une expression)
    * create_instr (création d'une instruction)
    * create_block (création d'un block)

    2)- Fonctions Reprint (Fichier reprintFunction.ml) :
    
    * print_op (utilisée dans print_expr)
    * print_expr (impression d'une expression)
    * print_comp (utilisée dans print_cond)
    * print_cond (impression d'une condition)
    * print_indentation (impression des indentations)
    * print_block (impression d'un block)
    * print_instr (impression d'une instruction)
    * print_program(impression d'un program, fait appel a print_block)

    3)- Fonctions Polish (Fichier polish.ml) :

    * print_polish (impression de tout le programme polish)
    * La fonction usage() (Documentation brève de notre mini language Polish pour l'utilisateur)

- Rendu 2 :

    **SMAINI Khalida :** 

    1)- Fonctions de simplifications (Fichier simplification.ml) : 

    * simpl_expression , simplifie_exp_var : pour simplifier une expression
    * simpl_condition : pour simplifier une condition
    * simpl_block : simplifier un block d'instruction
    * simpl_instr : simplifier une instruction
    * simpl_program : simplifier un programme polish
    * code_mort_block : éliminer le code mort d'un block
    * code_mort_instr : éliminer le code mort d'une instruction
    * verify_cond : vérifie si une condition est vraie, sinon élimine le code mort de son block
    * false_cond : élimine le code mort d'une boucle si sa condition est fausse
    * code_mort_prog : élimine le code mort d'un programme polish

    2)- Fonctions de Sign (Fichier sign.ml)
    
    * notEqual_sign vérifie si deux signes ne sont pas équivalents
    * equi_sign_exp , notEqual_exp_sign , lessThen_exp_sign, greatThen_exp_sign : comparer deux listes de signes selon le comparateur (Eq,Ne,Lt,Le,Gt,Ge)
    * sign_cond : vérifier si deux listes de signes satisfont une condition
    * fonction auxiliare : compREv 
    * join_env : merger deux environnement
    * verify_equal_List : vérifier l'égalité de deux listes
    * sign_block : renvoie l'ensemble de listes de signes des variables d'un block
    * sign_instr : renvoie l'ensemble listes de signes des variables d'une instruction
    * aux_sign_while : ensemble de liste de signes d'une boucle while
    * points_fixes : points fixes d'une boucle while
    * printSign, printList , print_list_sign : afficher l'ensemble de liste des signes des variables
    * print_list_ERR , print_ERROR_List : afficher "SAFE" ou "DIVBYZERO" selon les cas

    3)- Fonctions Polish (Fichier polish.ml) :
    * ajout de sign_polish : pour afficher les signes des variables d'un programme
    * ajout de simpl_polish : pour simplifier un programme
    * Modification Main.


    **BOUDJABOUT Manissa :**

    1)- Fonctions variables non initialisées (Fichier initVariables.ml) :

    * get_variables_expr : récupérer toutes les variables d'une expression sans redondance
    * get_variables_cond: récuperer les variables d'une condition
    * get_variables_block: récuperer les variables d'un block
    * get_variables_instr: récuperer les variables d'une instruction
    * get_variables_prog et print_set  :récuperer les variables d'un program et les afficher
    * get_vars_non_init_expr: récuperer les variables non initialisée d'une expression sans redondance
    * get_vars_non_init_cond: récuperer les variables non initialisée d'une condition
    * get_vars_non_init_block: récuperer les variables non initialisée d'un block
    * get_vars_non_init_instr: récuperer les variables non initialisée d'une instruction
    * get_vars_non_init_prog: récuperer les variables non initialisée d'un programme

    2)- Fonctions de Sign (Fichier sign.ml)
    
    * Fonctions auxiliaires : list_sans_doublons , isInList , signInt
    * signAdd , signAddList: signe d'une opération d'addition
    * signSub , signSubList : signe d'une soustraction
    * signMul , signMulList : signe d'une multiplication
    * signDiv , signDivList : signe d'une division
    * signMod , signModList : signe d'un modulo
    * sign_expr : signe d'une expression
    * lessThen_sign , greatThen_sign : comparaison entre deux signes

    3)- Fonctions Polish (Fichier polish.ml) :
    * vars_polish : pour afficher les variables d'un programmes ainsi que celles non initialisées.
    * Modification Main
