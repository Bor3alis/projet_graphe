(*Section 1*)

(* entrees :
    - sommet v1 appartenant à un premier graphe
    - sommet v2 appartenant à un second graphe
   sorties :
   specifs : 
    - marquage du sommet v1 avec le label de v2
    - marquage du sommet v2 avec le label de v1
   *)
val associate : Skeleton.vertex -> Skeleton.vertex -> unit

(* entrees:
    - sommet v1 appartenant à un premier graphe
    - sommet v2 appartenant à un second graphe
   sorties :
   specifs : 
    - attribution de la marque 0 à v1 et v2
   *)
val separate : Skeleton.vertex -> Skeleton.vertex -> unit

(* entrees :
    - graphe g
    - sommet o appartenant à g
    - sommet a (voisin de o) appartenant à g
   sorties :
    - liste des voisins de a, excepté o
   specifs :
    - attribue les voisins de a à o
    - retire l'arête (o,a)
    - retire a du graphe
   *)
val contract : Skeleton.t -> Skeleton.vertex -> Skeleton.vertex -> Skeleton.vertex list

(* entrees :
    - graphe g
    - sommet o appartenant à g
    - sommet a
    - liste des voisins la à attribuer à a
   sorties :
   specifs :
    - insère a dans le graphe
    - retire les arêtes (o,n), pour tout n de la
    - insère les arêtes (a,n), pour tout n de la
    - insère l'arête (o,a)
   *)
val insert : Skeleton.t -> Skeleton.vertex -> Skeleton.vertex -> Skeleton.vertex list -> unit


(*Section 2*)

(* entrees :
    - graphe g1
    - sommet v1 du graphe g1
    - graphe g2
    - sommet v2 du graphe g2
   sorties :
    - couple (b,l), avec :
      - b un booléen indiquant l'égalité entre les arbres
      - l une liste de couples de sommets mis en correspondance
   specifs :
    - implémentation de l'algorithme 1
*)
val equals_aux : Skeleton.t -> Skeleton.vertex -> Skeleton.t -> Skeleton.vertex -> (bool*(Skeleton.vertex*Skeleton.vertex list))

(* entrees :
    - graphe g1
    - sommet v1 du graphe g1
    - graphe g2
    - sommet v2 du graphe g2
   sorties :
    - couple (b,l), avec :
      - b un booléen indiquant l'égalité entre les arbres
      - l une liste de couples de sommets mis en correspondance
   specifs :
    - implémentation de l'algorithme 2
*)
val equals : Skeleton.t -> Skeleton.vertex -> Skeleton.t -> Skeleton.vertex -> (bool*(Skeleton.vertex*Skeleton.vertex list))


(*Section 3*)

(* entrees :
    - graphe g1
    - sommet v1 du graphe g1
    - graphe g2
    - sommet v2 du graphe g2
   sorties :
    - quadruplet (b,l0,l1,l2), avec :
      - c un entier donnant la distance d'édition entre les graphes
      - l0 une liste de couples de sommets mis en correspondance
      - l1 une liste de couples de sommets contractés dans le graphe g1
      - l2 une liste de couples de sommets contractés dans le graphe g2
   specifs :
    - fonction auxiliaire pour l'estimation de la distance d'édition entre g1 et g2
*)
val distance_aux : Skeleton.t -> Skeleton.vertex -> Skeleton.t -> Skeleton.vertex -> (bool*(Skeleton.vertex*Skeleton.vertex list)*(Skeleton.vertex*Skeleton.vertex list)*(Skeleton.vertex*Skeleton.vertex list))

(* entrees :
    - graphe g1
    - sommet v1 du graphe g1
    - graphe g2
    - sommet v2 du graphe g2
   sorties :
    - quadruplet (b,l0,l1,l2), avec :
      - c un entier donnant la distance d'édition entre les graphes
      - l0 une liste de couples de sommets mis en correspondance
      - l1 une liste de couples de sommets contractés dans le graphe g1
      - l2 une liste de couples de sommets contractés dans le graphe g2
   specifs :
    - estimation de la distance d'édition entre g1 et g2
*)
val distance : Skeleton.t -> Skeleton.vertex -> Skeleton.t -> Skeleton.vertex -> (bool*(Skeleton.vertex*Skeleton.vertex list)*(Skeleton.vertex*Skeleton.vertex list)*(Skeleton.vertex*Skeleton.vertex list))


(*Section 4*)

(* entrees :
    - graphe g1
    - sommet v1 du graphe g1
    - graphe g2
    - sommet v2 du graphe g2
   sorties :
    - quadruplet (b,l0,l1,l2), avec :
      - c un entier donnant la distance d'édition entre les graphes
      - l0 une liste de couples de sommets mis en correspondance
      - l1 une liste de couples de sommets contractés dans le graphe g1
      - l2 une liste de couples de sommets contractés dans le graphe g2
   specifs :
    - estimation de la distance d'édition entre g1 et g2
    - plus rapide que distance
*)
val distance_opti : Skeleton.t -> Skeleton.vertex -> Skeleton.t -> Skeleton.vertex -> (bool*(Skeleton.vertex*Skeleton.vertex list)*(Skeleton.vertex*Skeleton.vertex list)*(Skeleton.vertex*Skeleton.vertex list))
