open Skeleton.Skeleton;;
(* ------------------- Fonctions d'affichage -------------------*)

(* affiche_liste : (_,_,_,_,_,_) format6                        *)
 (*  		 -> (Format.formatter -> 'a -> unit)            *)
(*   		 -> Format.formatter -> 'a list -> unit         *)
(* affiche les elements d'une liste avec la fonction f,         *)
(* separes par la chaine de formatage sep                       *)
let rec affiche_liste ~sep:sep f fmt =
  function
  | []   -> ()
  | [t]  -> Format.fprintf fmt "%a"
    	    		   f t
  | t::q -> Format.fprintf fmt "%a%(%)%a"
    	    		   f t
			   sep
			   (affiche_liste ~sep f) q;;

(* affiche_sommet : (_,_,_,_,_,_) format6                       *)
(*                -> Format.formatter -> V.t -> unit            *)
(* affiche le label d'un sommet v d'un graphe,                  *)
(* suivi d'une chaine de formatage trm                          *)
let affiche_sommet ~trm:trm fmt v =
  Format.fprintf fmt "%a" (vertex_name v) trm;;

(* affiche_arete : (_,_,_,_,_,_) format6                        *)
(*               -> Format.formatter -> E.t -> unit             *)
(* affiche les sommets d'une arete e d'un graphe,               *)
(* suivi d'une chaine de formatage trm                          *)
let affiche_arete ~trm:trm fmt e =
  Format.fprintf fmt "%a -- %i -- > %a%(%)"
  		 (affiche_sommet ~trm:"") (E.src e)
		 (E.label e)
		 (affiche_sommet ~trm:"") (E.dst e)
		 trm;;

(* affiche un sommet v d'un graphe g et ses aretes sortantes,   *)
(* chacun suivi de la chaine de formatage trm                   *)
let affiche_sommet_et_aretes ~trm:trm g fmt v =
  Format.fprintf fmt "%a:%(%)  @[<v0>%a@]%(%)"
  		 (affiche_sommet ~trm:"") v
		 trm
		 (affiche_liste ~sep:"@," (affiche_arete ~trm:""))
		 	        (succ_e g v)
		 trm;;

(* affiche les sommets et les aretes sortantes d'un graphe g    *)
let affiche_graphe ~trm:trm fmt g =
  Format.fprintf fmt "{@[<v0>@,%a@]}"
  		 (affiche_liste ~sep:"" (affiche_sommet_et_aretes ~trm:"@," g))
		 		(fold_vertex (fun v q -> v::q) g []);;

(*---------------------- Installation --------------------------*)
(* installe les procédures d'affichage de sommets et de graphes *)
(* dans l'interprète Ocaml                                      *)

let affiche_sommet_ocaml = affiche_sommet ~trm:"";;
#install_printer affiche_sommet_ocaml;;
let affiche_arete_ocaml = affiche_arete ~trm:"";;
#install_printer affiche_arete_ocaml;;
let affiche_graphe_ocaml = affiche_graphe ~trm:"";;
#install_printer affiche_graphe_ocaml;;
