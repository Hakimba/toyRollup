module Rollup = struct
  module Ledger = Map.Make(String)
  let default_balance = 0
  type context = {
    ledgers : int Ledger.t;
    id : int;
    txs : (string * int * string) list
  }
  let make =
    let cnt = ref 0 in
    fun () -> 
      let ledgers = Ledger.add "genesis_ledger" default_balance Ledger.empty in
      let id = !cnt in
      let txs = ["genesis",0,"genesis"] in
      incr cnt;
      {ledgers=ledgers;id=id;txs=txs}

  let get_id {ledgers = _;id=id;txs = _} = id
end