module type MTreeContext =
  sig
    type data
    type digest
    val hash : data -> digest
    val concat_hashes : digest -> digest -> digest
    val equal : digest -> digest -> bool
    val to_string : digest -> string
  end
module type MerkleTree =
  functor (Ctx : MTreeContext) ->
    sig
      type t
      type proof
      val mtree_of_txs : Ctx.data list -> t
      val witness : Ctx.data -> t -> proof
      val proof : Ctx.data -> proof -> t -> bool
      val equal : t -> t -> bool
      val to_string : t -> string
    end
module MerkleTree :
  functor (Ctx : MTreeContext) ->
    sig
      type mtree = Leaf of Ctx.digest | Node of Ctx.digest * mtree * mtree
      type t = Empty | Mtree of mtree
      type direction = L | R
      type proof = (Ctx.digest * direction) list
      val get_hash : mtree -> Ctx.digest
      val mtree_of_txs : Ctx.data list -> t
      val witness : Ctx.data -> t -> (Ctx.digest * direction) list
      val proof : Ctx.data -> (Ctx.digest * direction) list -> t -> bool
      val equal : t -> t -> bool
      val to_string : t -> string
    end
