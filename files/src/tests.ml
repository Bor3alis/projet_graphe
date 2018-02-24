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




let rec rm_doublon l = function

    |[] -> l
    |h::t -> if(List.mem h l) then rm_doublon l t else rm_doublon (h::l) t;;

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

(**********************************************)
print_string "---------------------------------\n";;

let a = ordered_succ obj1_1 obj1_1v1 in
    affiche_sommets a;;
let b = ordered_succ obj1_1 obj1_1v2 in
    affiche_sommets b;;

let lc1 = contract obj1_1 obj1_1v1 obj1_1v2;;

let res = ordered_succ obj1_1 obj1_1v1 in
    affiche_sommets res;;

print_string "---------------------------------\n";;

(**********************************************)
print_string "---------------------------------\n";;

let a = ordered_succ obj1_2 obj1_2v1 in
       affiche_sommets a;;
let b = ordered_succ obj1_2 obj1_2v3 in
        affiche_sommets b;;

let lc2 = contract obj1_2 obj1_2v1 obj1_2v3;;

let res = ordered_succ obj1_2 obj1_2v1 in
        affiche_sommets res;;

affiche_sommets l2;;
affiche_sommets lres2;;

print_string "---------------------------------\n";;

(**********************************************)

let a = ordered_succ obj1_3 obj1_3v2 in
    affiche_sommets a;;
let b = ordered_succ obj1_3 obj1_3v6 in
        affiche_sommets b;;


(*let lres3 = let l = get_succ_except_a l31 obj1_3v6 in
            let l2 = get_succ_except_a l32 obj1_3v2 in
            l@l2;; *)

let lc3 = contract obj1_3 obj1_3v2 obj1_3v6;;
let res = ordered_succ obj1_3 obj1_3v2 in
    affiche_sommets res;;

print_string "---------------------------------\n";;

print_string "============================= TEST INSERT  =======================\n";;

print_string "---------------------------------\n";;
let a = ordered_succ obj1_1 obj1_1v1 in
affiche_sommets a;;
affiche_sommets lc1;;

insert obj1_1 obj1_1v1 obj1_1v2 lc1;;

let b = ordered_succ obj1_1 obj1_1v1 in
affiche_sommets b;;
print_string "---------------------------------\n";;


let a = ordered_succ obj1_2 obj1_2v1 in
affiche_sommets a;;
affiche_sommets lc2;;

insert obj1_2 obj1_2v1 obj1_2v3 lc2;;

let b = ordered_succ obj1_2 obj1_2v1 in
affiche_sommets b;;
print_string "---------------------------------\n";;





