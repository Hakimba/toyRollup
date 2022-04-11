module Rollup = struct
  type ledger = (string, int) Hashtbl.t

  type transaction = {
    sender : string;
    amount : int;
    receiver : string
  }
  type context = {
    ledger : ledger;
    id : int;
    txs : transaction list;
    level : int
  }
  let create =
    let cnt = ref 0 in
    fun () -> 
      let ledger = Hashtbl.create 50 in
      let id = !cnt in
      let txs = [] in
      incr cnt;
      {ledger=ledger;id=id;txs=txs;level=0}

  let get_id {ledger = _;id=id;txs = _;level = _} = id
  let get_current_level {ledger = _;id=_;txs = _;level = lvl} = lvl

  let get_participants {ledger = l;id=_;txs = _;level = _} =
    Hashtbl.to_seq_keys l

  let get_nb_participants {ledger = l;id=_;txs = _;level = _} =
    Hashtbl.length l
end