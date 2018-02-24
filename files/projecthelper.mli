open Skeleton
module Projecthelper :
sig
	val associate : Skeleton.vertex -> Skeleton.vertex -> unit
	val separate : Skeleton.vertex -> Skeleton.vertex -> unit
	val contract : Skeleton.t -> Skeleton.vertex -> Skeleton.vertex -> Skeleton.vertex list
	val insert : Skeleton.t -> Skeleton.vertex -> Skeleton.vertex -> Skeleton.vertex list -> unit
end
