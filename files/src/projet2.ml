open Skeleton.Skeleton
(*open Graph.Pack.Graph;;*)


(* 1 Opération sur les graphes *)

let associate s1 s2 =
    Mark.set s1 (Vertex.indice (V.label s2));
    Mark.set s2 (Vertex.indice (V.label s1)) ;;


let separate s1 s2 =
    Mark.set s1 0;
    Mark.set s2 0;
;;


let voisins_aux a o g =
    remove_edge g o a ;
    remove_edge g a o;
    (fold_succ (fun s2 resq -> s2::resq) g a []);;


let voisins a o g =
    let gcopy = copy g in 
    voisins_aux a o g;;
    

let rec contract_aux a o lv g =
    match lv with
    |[] -> g
    |t::q -> remove_edge g a t;
             remove_edge g t a;
             add_edge g o t;
             contract_aux a o q g;;


let contract g a o =
    let lv = voisins a o g in
    let gbis = contract_aux a o lv g in
    remove_vertex g a;
    gbis;;


let unmarked ls =
    List.fold_right (fun t qt -> if(Mark.get t = 0) then t::qt else qt) ls [];;


(* 2 Test d'égalité entre deux arbres *)

let rec 

    equals_aux g1 v1 g2 v2 =
    let s1 = unmarked (Skeleton.ordered_succ g1 v1) in
    let s2 = unmarked (Skeleton.ordered_succ g2 v2) in
    
    match s1, s2 with
    |[],[] -> (true,[])
    |[],_ -> (false,[])
    |_, [] -> (false,[])
    |_,_ -> let h1 = List.hd s1 in
            let h2 = List.hd s2 in

            associate h1 h2;

            let (bh,lh) = equals_aux g1 h1 g2 h2 in
            let (bq,lq) = equals_aux g1 v1 g2 v2 in

            separate h1 h2 ;

            let lf = lh@lq in
            (bh&&bq,(h1,h2)::lf)

        and
        
        equals g1 v1 g2 v2 =
            let (b,lr) = equals_aux g1 v1 g2 v2 in
            let l = (v1,v2)::lr in
            separate v1 v2;
            (b,l);;









(************ TESTS *************************)
(*let g1bis = contract g1 vv2 vv3 ;;

dot_output g1bis "g1bis.dot";;*)





