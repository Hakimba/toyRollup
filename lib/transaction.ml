type transaction = {
    sender : string;
    amount : int;
    receiver : string
  }[@@deriving yojson]