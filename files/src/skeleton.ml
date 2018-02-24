module Skeleton = 
struct
	module Vertex = struct
	  type t = int*(float*float)*float
	  let compare : t -> t -> int = Pervasives.compare
	  let hash = Hashtbl.hash
	  let equal = (=)
	  let default = (0,(0.0,0.0),0.0)
	  let indice ((ind,ctr,rad) : int*(float*float)*float) = ind
	  let center ((ind,ctr,rad) : int*(float*float)*float) = ctr
	  let radius ((ind,ctr,rad) : int*(float*float)*float) = rad
	  let stringindice ((ind,ctr,rad) : int*(float*float)*float) = string_of_int ind
	  let stringcenter ((ind,ctr,rad) : int*(float*float)*float) = "("^(string_of_float (fst ctr))^", "^(string_of_float (snd ctr))^")"
	  let stringradius ((ind,ctr,rad) : int*(float*float)*float) = string_of_float rad
	end

	module Edge = struct
	  type t = int
	  let compare : t -> t -> int = Pervasives.compare
	  let hash = Hashtbl.hash
	  let equal = (=)
	  let default = 0
	end

	include Graph.Imperative.Graph.AbstractLabeled(Vertex)(Edge)
	
	module Display = struct
		include Graph.Imperative.Graph.AbstractLabeled(Vertex)(Edge)
		let vertex_name v = string_of_int (Vertex.indice (V.label v))
		let graph_attributes _ = []
		let default_vertex_attributes _ = []
		let vertex_attributes v = (`Label ((Vertex.stringindice (V.label v))^" : "^(Vertex.stringcenter (V.label v))))::
								  []
		let default_edge_attributes _ = []
		let edge_attributes e = []
		let get_subgraph _ = None
	end
	
	module Dot = Graph.Graphviz.Dot(Display)
	module Neato = Graph.Graphviz.Neato(Display)

	let dot_output g f =
		let oc = open_out f in
		if is_directed then Dot.output_graph oc g else Neato.output_graph oc g;
		close_out oc;;

    let createv lab = V.create lab;;
	
	let ordered_succ g v =
		let l_succ = succ g v in
		let ang (x1,y1) (x2,y2) =
			let angi = atan2 (-1.0*.(y2-.y1)) (-1.0*.(x2-.x1)) in
			if angi < 0. then
				angi +. 2.0*.3.14159
			else
				angi in
		let l_ang_succ = List.map (fun ve -> (ang (Vertex.center (V.label ve)) (Vertex.center (V.label v)),ve)) l_succ in
		let cmpfun v1 v2 =
			let diff = (fst v2) -. (fst v1) in
			if diff > 0. then 1
			else if diff < 0. then -1
			else 0 in
		let l_ang_succ_sort = List.sort cmpfun l_ang_succ in
		let l_succ_sort = List.map (fun (_,vi) -> vi) l_ang_succ_sort in
		let marked = List.fold_right (fun vi b -> b || ((Mark.get vi) <> 0)) l_succ_sort false in
		if marked then
			let rec get_mark_begin l =
				match l with
				| [] -> ([],[])
				| t::q ->
						if ((Mark.get t) <> 0) then 
							let (l1,l2) = get_mark_begin q in
							(t::l1,l2)
						else
							([],l) in
			let (lt1,lt2) = get_mark_begin l_succ_sort in
			let ltmp1 = lt2@lt1 in
			let rec get_unmark_begin l =
				match l with
				| [] -> ([],[])
				| t::q ->
						if ((Mark.get t) = 0) then 
							let (l1,l2) = get_unmark_begin q in
							(t::l1,l2)
						else
							([],l) in
			let (ls1,ls2) = get_unmark_begin ltmp1 in
			ls2@ls1
		else
			l_succ_sort;;
end

