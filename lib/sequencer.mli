module type TxsStorage =
  sig type 'a t val create : 'a t val add : 'a -> 'a t -> 'a t end
module TxsStorage : TxsStorage
module type Sequencer =
  functor (Storage : TxsStorage) ->
    sig
      type context
      val make : string -> int -> int -> context
      val get_port : context -> int
      val get_associated_rollup : context -> int
      val get_name : context -> string
      val batch : 'a Storage.t -> 'a Storage.t
      (**[batch txs] pour l'instant un batch ce seras un sous ensemble de la structure contenant un 
    certain nombre de transaction qui auront été choisi selon un critère**)
    end
module Sequencer :
  functor (Storage : TxsStorage) ->
    sig
      type context = { name : string; port : int; associated_rollup : int; }
      val get_port : context -> string
      val get_associated_rollup : context -> int
      val get_name : context -> string
      val batch : 'a Storage.t -> 'a Storage.t
    end
