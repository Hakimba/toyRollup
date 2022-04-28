module Rollup :
  sig
    type ledger = (string, int) Hashtbl.t

    (**
      This module is the context needed by the MerkleTree functor, provide the hash library    
    **)
    module LedgerCtx :
      sig
        type data = string * int
        type digest = Sha256.t
        val hash : string * int -> Sha256.t
        val to_string : Sha256.t -> string
        val hash_for_concat : string -> Sha256.t
        val concat_hashes : Sha256.t -> Sha256.t -> Sha256.t
        val equal : Sha256.t -> Sha256.t -> bool
      end
    module LedgerMerkleTree :
      sig
        type mtree =
          Merkle.MerkleTree(LedgerCtx).mtree =
            Leaf of Sha256.t
          | Node of Sha256.t * mtree * mtree
        type t = Merkle.MerkleTree(LedgerCtx).t = Empty | Mtree of mtree
        type direction = Merkle.MerkleTree(LedgerCtx).direction = L | R
        type proof = (Sha256.t * direction) list
        val get_hash : mtree -> Sha256.t
        val mtree_of_txs : LedgerCtx.data list -> t
        val witness : LedgerCtx.data -> t -> (Sha256.t * direction) list
        val proof :
          LedgerCtx.data -> (Sha256.t * direction) list -> t -> bool
        val equal : t -> t -> bool
        val to_string : t -> string
      end
    type context = {
      ledger : ledger;
      id : int;
      txs : Transaction.transaction list;
      level : int;
    }
    val create : unit -> context
    val get_id : context -> int
    val get_current_level : context -> int
    val get_participants : context -> string Seq.t
    val get_nb_participants : context -> int

    (**[get_state context] merklization of the ledger contained in a rollup**)
    val get_state : context -> LedgerMerkleTree.t
  end
