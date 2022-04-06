module Rollup :
  sig
    type ledger = (string, int) Hashtbl.t
    type transaction = { sender : string; amount : int; receiver : string; }
    type context = { ledger : ledger; id : int; txs : transaction list; }
    val create : unit -> context
    val get_id : context -> int
  end
