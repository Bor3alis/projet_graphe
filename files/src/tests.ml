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
 * sémantique : affiche la liste des sommets *)
let affiche_sommets l =
    List.map (fun v -> Vertex.indice (V.label v)) l;;

(* affiche_arête
 * sémantique : affiche la liste des arêtes *)
let affiche_arete l =
    List.map (fun (v1,v2) -> (Vertex.indice (V.label v1), Vertex.indice (V.label v2))) l;;



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

(*let a = ordered_succ obj1_1 obj1_1v1 in
    affiche_sommets a;;
let b = ordered_succ obj1_1 obj1_1v2 in
    affiche_sommets b;;

let lc1 = contract obj1_1 obj1_1v1 obj1_1v2;;

let res = ordered_succ obj1_1 obj1_1v1 in
    affiche_sommets res;;*)

print_string "---------------------------------\n";;

(**********************************************)
print_string "---------------------------------\n";;

(*let a = ordered_succ obj1_2 obj1_2v1 in
       affiche_sommets a;;
let b = ordered_succ obj1_2 obj1_2v3 in
        affiche_sommets b;;

let lc2 = contract obj1_2 obj1_2v1 obj1_2v3;;

let res = ordered_succ obj1_2 obj1_2v1 in
        affiche_sommets res;;

(*affiche_sommets l2;;
affiche_sommets lres2;;*) 

let c = insert obj1_2 obj1_2v1 obj1_2v3 lc2;;
print_string "---------------------------------\n";;

let a = ordered_succ obj1_2 obj1_2v1 in
       affiche_sommets a;;
let b = ordered_succ obj1_2 obj1_2v3 in
        affiche_sommets b;;*)
(**********************************************)
(*
let a = ordered_succ obj1_3 obj1_3v2 in
    affiche_sommets a;;
let b = ordered_succ obj1_3 obj1_3v6 in
        affiche_sommets b;;


(*let lres3 = let l = get_succ_except_a l31 obj1_3v6 in
            let l2 = get_succ_except_a l32 obj1_3v2 in
            l@l2;; *)

let lc3 = contract obj1_3 obj1_3v2 obj1_3v6;;
affiche_sommets lc3;;
let res = ordered_succ obj1_3 obj1_3v2 in
    affiche_sommets res;;
    *)
print_string "---------------------------------\n";;

print_string "============================= TEST INSERT  =======================\n";;

print_string "---------------------------------\n";;
(*let a = ordered_succ obj1_1 obj1_1v1 in
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
*)


print_string "============================= TEST EQUALS  =======================\n" ;;
(* Dans ces test, on ne vérifie que le booléen renvoyé par la fonction equals et non
la liste des associations de noeuds *)
(* tests sur l'objet 1 *)
print_string "---------------------------------\n";;
let (b1, l1) = equals obj1_1 obj1_1v5 obj1_2 obj1_2v4 in b1;;
let (b2, l2) = equals obj1_1 obj1_1v5 obj1_3 obj1_3v3 in not b2;;
let (b3, l3) = equals obj1_2 obj1_2v4 obj1_3 obj1_3v3 in not b3;;

(* tests sur l'objet 2 *)
let (b6, l6) = equals obj2_1 obj2_1v3 obj2_2 obj2_2v1 in not b6;;

print_string "---------------------------------\n";;
(* tests entre objet 1 et objet 2 *)
let (b4, l4) = equals obj1_1 obj1_1v5 obj2_1 obj2_1v3 in  not b4;;
let (b5, l5) = equals obj2_2 obj2_2v1 obj1_3 obj1_3v3 in not b5 ;;




print_string "============================= TEST DISTANCE  =======================\n" ;;
(* Dans ces test, on ne vérifie que le nombre c d'éditions nécéssaires pour passer d'un 
graphe à l'autre *)
(* tests de c *)
(* tests sur l'objet 1 *)
print_string "---------------------------------\n";;
let (c1, l01, l11, l21) = distance obj1_1 obj1_1v5 obj1_2 obj1_2v4 in c1 = 0;;
let (c2, l02, l12, l22) = distance obj1_1 obj1_1v5 obj1_3 obj1_3v3 in c2 = 2;;
let (c3, l03, l13, l23) = distance obj1_2 obj1_2v4 obj1_3 obj1_3v3 in c3 = 2;;

(* tests sur l'objet 2 *)
let (c4, l04, l14, l24) = distance obj2_1 obj2_1v3 obj2_2 obj2_2v1 in c4 = 2;;

print_string "---------------------------------\n";;
(* tests entre objet 1 et objet 2 *)
let (c5, l05, l15, l25) = distance obj1_1 obj1_1v5 obj2_1 obj2_1v3 in  c5 <> 0 (* TODO *);;
let (c6, l06, l16, l26) = distance obj2_2 obj2_2v1 obj1_3 obj1_3v3 in  c6 <> 0 (* TODO *);;

(* tests de l0  : TODO *)
(* tests de l1  : TODO *)
(* tests de l2  : TODO *)



equals obj1_1 obj1_1v5 obj1_3 obj1_3v3;;

let (c,l0,l1,l2) = distance obj1_1 obj1_1v5 obj1_3 obj1_3v3;;
affiche_arete l0;;
affiche_arete l1;;
affiche_arete l2;;


