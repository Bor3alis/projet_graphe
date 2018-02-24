open Skeleton.Skeleton
open Skeleton.Skeleton.Display
#use"projet.ml";;
#use"skeltest.ml";;
#use"affichageGraphes.ml";;



(******** fonctions utiles pour les tests ******************)
let get_succ g s = (fold_succ (fun s2 resq -> s2::resq) g s []);;

(* get_succ_execpt_a
 * sémantique : renvoit la liste de tous les successeurs excepté a
 * précondition : a est successeur de s
 * *)

let rec get_succ_except_a ls a =
    match ls with
    |[] -> []
    |t::q -> if (t = a) then q
             else
                 t::(get_succ_except_a q a);;

(* equals_listes
 * sémantique : teste si 2 listes sont égales
 * précondition : les 2 listes ont les successeurs rangés dans le même ordre*)
let rec equal_listes l1 l2 =
    match l1,l2 with
    |[],[] -> true
    |[],_ -> false
    |_,[] -> false
    |t1::q1, t2::q2 -> (t1==t2) && (equal_listes q1 q2);;

(* affiche_sommets
 * sémantique : affiche la liste de sommets *)
let affiche_sommets l =
    List.map (fun v -> Vertex.indice (V.label v)) l;;


(************************************************************)

(* 1 Operation sur les graphes *)

print_string "============================= TEST ASSOCIATE  =======================\n" ;;

Mark.get obj1_1v1 = 0;;
Mark.get obj1_1v2 = 0;;

associate obj1_1v1 obj1_1v2;;

Mark.get obj1_1v1 = 2;;
Mark.get obj1_1v2 = 1;;

print_string "============================= TEST SEPARATE  =======================\n";

separate obj1_1v1 obj1_1v2;;

Mark.get obj1_1v1 = 0;;
Mark.get obj1_1v2 = 0;;

print_string "============================= TEST CONTRACT  =======================\n";;


let lv = contract obj1_1 obj1_1v1 obj1_1v2;;
let lv2 = contract obj1_2 obj1_2v1 obj1_2v3;;

let l1 = get_succ_except_a lv obj1_1v1 ;;
let l2 = get_succ_except_a lv2 obj1_2v1;;

affiche_sommets l1;;
affiche_sommets lv;;

affiche_sommets l2;;
affiche_sommets lv2;;

(*equal_listes l1 lv;;
equal_listes l2 lv2;;*)

print_string "============================= TEST INSERT  =======================\n";;

let l3 = ordered_succ obj1_1 obj1_1v1;;
affiche_sommets l3;;

insert obj1_1 obj1_1v1 obj1_1v2 lv;;
insert obj1_2 obj1_2v1 obj1_2v3 lv2;;

let l3_1 = ordered_succ obj1_1 obj1_1v1;;
affiche_sommets l3_1;;

(*let l4 = ordered_succ obj1_1 obj1_1v2;;
affiche_sommets l4;;*)





