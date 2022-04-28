open Merkle
open Transaction

module RefutationGame = struct
  type dispute_result =
  | Dispute_at of int
  | No_dispute

  module TransactionCtx = struct
    type data = transaction
    type digest = Sha256.t
    let hash (tx : transaction) =
      tx.sender ^ (string_of_int tx.amount) ^ tx.receiver |> Sha256.string
    let to_string = Sha256.to_hex
    let hash_for_concat = Sha256.string
    let concat_hashes h1 h2 = hash_for_concat (String.concat "" [to_string h1;to_string h2])
    let equal h1 h2 = (to_string h1) = (to_string h2)
  end

  module LedgerMerkleTree = MerkleTree(TransactionCtx)

  let merklized txs = List.map TransactionCtx.hash txs
  let find_dispute (txs : transaction list) (txs' : transaction list) =
    let _ = merklized txs in
    let _ = merklized txs' in
    (*démarrer le jeu*)
    ()
end

