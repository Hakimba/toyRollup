module Rollup :
  sig
    type ledger = (string, int) Hashtbl.t
    type transaction = { sender : string; amount : int; receiver : string; }
    type context = {
      ledger : ledger;
      id : int;
      txs : transaction list;
      level : int;
    }
    val create : unit -> context
    val get_id : context -> int
    val get_current_level : context -> int
    val get_participants : context -> string Seq.t
    val get_nb_participants : context -> int
  end
