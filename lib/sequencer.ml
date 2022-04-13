module type TxsStorage = sig
  type 'a t
  val create : 'a t
  val add : 'a -> 'a t -> 'a t
end

module TxsStorage  : TxsStorage = struct
  type 'a t = 'a list
  let create = []
  let add tx txs = tx :: txs
end

module type Sequencer = functor (Storage : TxsStorage) ->
  sig
    type context
    val make : string -> int -> int -> context
    val get_port : context -> int
    val get_associated_rollup : context -> int
    val get_name : context -> string
    val batch : 'a Storage.t -> 'a Storage.t
  end

module Sequencer (Storage : TxsStorage) = struct
  type context = {
    name : string;
    port : int;
    associated_rollup : int;
  }
  let get_port ctx = ctx.name
  let get_associated_rollup ctx = ctx.associated_rollup
  let get_name ctx = ctx.name
  let batch (txs : 'a Storage.t) = txs
end