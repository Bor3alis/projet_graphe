(* Marine Médard Claire Brocherieux Groupe B *)

open Skeleton.Skeleton;;
#use"skeltest.ml";;

(*Section 1*)

let associate v1 v2 = 
	Mark.set v1 (Vertex.indice (V.label v2));
	Mark.set v2 (Vertex.indice (V.label v1));;

let separate v1 v2 = 
	Mark.set v1 0;
	Mark.set v2 0;;

(* fct contract pour un graphe non orienté *)
let contract g o a = 
	let liste_voisins = fold_succ (fun v liste_voisins-> if (v <> o) then
				(remove_edge g a v;
				add_edge g o v;
				v::liste_voisins)
			    else
				liste_voisins 
		  ) g a []  in
	remove_edge g o a;
	remove_vertex g a;
	liste_voisins;;

(* fct cherche_voisin et insert pour un graphe non orienté *)
let rec cherche_voisin_a g o a la v =
	match la with
	|[] -> ()
	|t::reste -> if ( t = v ) then
			 (add_edge g a v;
			 remove_edge g o v;
			 cherche_voisin_a g o a reste v)
		     else
			 cherche_voisin_a g o a reste v
		      ;;

let insert g o a la = 
	add_vertex g a;
	iter_succ (cherche_voisin_a g o a la) g o;
	add_edge g o a;;


(*Section 2*)

(* fct unmarked : a une liste de sommets associe la liste des sommets non marqués de cette liste *)

let rec unmarked liste_sommets =
	match liste_sommets with
	|[] -> []
	|a::reste -> if (Mark.get a) = 0 then
			a::(unmarked reste)
		     else
			unmarked reste ;;


let rec equals_aux g1 v1 g2 v2 =
	let s1 = unmarked (ordered_succ g1 v1) in
	let s2 = unmarked (ordered_succ g2 v2) in
	match s1,s2 with
	|[],[] -> (true,[])
	|[],_ -> (false,[])
	|_,[] -> (false,[])
	|a1::reste1, a2::reste2 -> associate a1 a2;
				   let (bh,lh) = equals_aux g1 a1 g2 a2 in 
				   let (bq,lq) = equals_aux g1 v1 g2 v2 in
				   (separate a1 a2;
				   let lf = lh@lq in
				   (bh && bq, (a1,a2)::lf))


    and equals g1 v1 g2 v2 = 
	associate v1 v2;
	let (b,lr) = equals_aux g1 v1 g2 v2 in
	let l = (v1,v2)::lr in
	separate v1 v2;
	(b,l);;
	


(*Section 3*)

let rec distance_aux g1 v1 g2 v2 = 
    let s1 =  unmarked (ordered_succ g1 v1) in
    let s2 =  unmarked (ordered_succ g2 v2) in

    match s1, s2 with
    |[],[] -> (0,[],[],[])
    |[],t::q -> let lv = contract g2 v2 t in
                let (c,l0,l1,l2) = distance_aux g1 v1 g2 v2 in
                insert g2 v2 t lv;
                (1,l0,l1,(v2,t)::l2);

    |t::q,[] -> let lv = contract g1 v1 t in
                let (c,l0,l1,l2) = distance_aux g1 v1 g2 v2 in
                insert g1 v1 t lv;
                (1,l0,(v1,t)::l1,l2);

    |t1::q1,t2::q2 -> associate t1 t2;
                      let (c1,l01,l11,l21) = distance_aux g1 t1 g2 t2 in
                      let (c2,l02,l12,l22) = distance_aux g1 v1 g2 v2 in
                        separate t1 t2;
                      let lf = l01@l02 in
                      let lf1 = l11@l12 in
                      let lf2 = l21@l22 in
                      (c1+c2,(t1,t2)::lf,lf1,lf2);;


 

let distance g1 v1 g2 v2 = 
    associate v1 v2;
    let (c,l0,l1,l2) = distance_aux g1 v1 g2 v2 in
    let lf = (v1,v2)::l0 in
    separate v1 v2;
    (c,lf,l1,l2);;



let distance_opti g1 v1 g2 v2 = (0,[],[],[]);;


