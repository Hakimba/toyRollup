module type TXSSTORAGE =
  sig
    type 'a t
    val create : 'a t
    val add : 'a -> 'a t -> 'a t
    val filter : ('a -> bool) -> 'a t -> 'a t
  end
module TxsStorage : TXSSTORAGE
module type SEQUENCER =
  functor (Storage : TXSSTORAGE) ->
    sig
      type context
      val make : string ref -> int ref -> int ref -> context
      val get_port : context -> int
      val get_associated_rollup : context -> int
      val get_name : context -> string
      val batch : 'a Storage.t -> 'a Storage.t
    end
module Sequencer :
  functor (Storage : TXSSTORAGE) ->
    sig
      type context = {
        name : string ref;
        port : int ref;
        associated_rollup : int ref;
      }
      val make : string ref -> int ref -> int ref -> context
      val get_port : context -> string
      val get_associated_rollup : context -> int
      val get_name : context -> string
      val add_transaction : 'a -> 'a TxsStorage.t -> 'a TxsStorage.t
      val batch : 'a Storage.t -> 'a Storage.t
    end
