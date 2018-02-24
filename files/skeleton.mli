open Graph
module Skeleton :
sig
	module Vertex :
	sig
	  type t
	  val compare : t -> t -> int
	  val equal : t -> t -> bool
	  val default : t
	  val indice : t -> int
	  val center : t -> float*float
	  val radius : t -> float
	  val stringindice : t -> string
	  val stringcenter : t -> string
	  val stringradius : t -> string
	end

	module Edge :
	sig
		type t
		val compare : t -> t -> int
		val equal : t -> t -> bool
		val default : t
    end

    include (module type of Graph.Imperative.Graph.AbstractLabeled(Vertex)(Edge))
    
	module Display :
    sig
		val graph_attributes : 'a -> 'b list
	end

    val createv : (int*(float*float)*float) -> V.t
	val ordered_succ : t -> V.t -> V.t list
end

