module type TXSSTORAGE = sig
  type 'a t
  val create : 'a t
  val add : 'a -> 'a t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
end

module TxsStorage  : TXSSTORAGE = struct
  type 'a t = 'a list
  let create = []
  let add tx txs = tx :: txs
  let filter = List.filter
end

module type SEQUENCER = functor (Storage : TXSSTORAGE) ->
  sig
    type context
    val make : string ref -> int ref -> int ref -> context
    val get_port : context -> int
    val get_associated_rollup : context -> int
    val get_name : context -> string
    val batch : 'a Storage.t -> 'a Storage.t
  end

module Sequencer (Storage : TXSSTORAGE) = struct
  type context = {
    name : string ref;
    port : int ref;
    associated_rollup : int ref;
  }
  let make n p r = {name=n;port=p;associated_rollup=r}
  let get_port ctx = !(ctx.name)
  let get_associated_rollup ctx = !(ctx.associated_rollup)
  let get_name ctx = !(ctx.name)

  let add_transaction tx txs = TxsStorage.add tx txs

  let batch (txs : 'a Storage.t) = txs
end