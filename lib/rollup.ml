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
    txs : transaction list
  }
  let create =
    let cnt = ref 0 in
    fun () -> 
      let ledger = Hashtbl.create 50 in
      let id = !cnt in
      let txs = [] in
      incr cnt;
      {ledger=ledger;id=id;txs=txs}

  let get_id {ledger = _;id=id;txs = _} = id
end