type transaction = {
    sender : string;
    amount : int;
    receiver : string
  }[@@deriving yojson]

let make_transaction s a r = {sender=s;amount=a;receiver=r}