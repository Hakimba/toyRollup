open Merkle
module Rollup = struct
  type ledger = (string, int) Hashtbl.t
  
  module LedgerCtx = struct
    type data = (string * int)
    type digest = Sha256.t
    let hash el =
      let (name,balance) = el in
      name ^ (string_of_int balance) |> Sha256.string
    let to_string = Sha256.to_hex
    let hash_for_concat = Sha256.string
    let concat_hashes h1 h2 = hash_for_concat (String.concat "" [to_string h1;to_string h2])
    let equal h1 h2 = (to_string h1) = (to_string h2)
  end

  module LedgerMerkleTree = MerkleTree(LedgerCtx)
  
  type context = {
    ledger : ledger;
    id : int;
    txs : Transaction.transaction list;
    level : int
  }
  let create =
    let cnt = ref 0 in
    fun () -> 
      let ledger = Hashtbl.create 50 in
      let id = !cnt in
      let txs = [] in
      incr cnt;
      {ledger=ledger;id=id;txs=txs;level=1}

  let get_id {ledger = _;id=id;txs = _;level = _} = id
  let get_current_level {ledger = _;id=_;txs = _;level = lvl} = lvl

  let get_participants {ledger = l;id=_;txs = _;level = _} =
    Hashtbl.to_seq_keys l

  let get_nb_participants {ledger = l;id=_;txs = _;level = _} =
    Hashtbl.length l

  let get_state {ledger = l;id=_;txs = _;level = _} =
    let list_ledger = List.of_seq (Hashtbl.to_seq l) in
    LedgerMerkleTree.mtree_of_txs list_ledger

end