module type TxsStorage = sig
  type 'a t
  val create : 'a t
  val add : 'a -> 'a t -> 'a t
  val batch : 'a t -> 'a t
  (**[batch txs] pour l'instant un batch ce seras un sous ensemble de la structure contenant un 
  certain nombre de transaction qui auront été choisi selon un critère**)
end

module TxsStorage  : TxsStorage = struct
  type 'a t = 'a list
  let create = []
  let add tx txs = tx :: txs
  let batch txs = txs
end
