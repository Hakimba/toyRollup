open Dream
open Rollup
let port = ref 8080

(*gestion des options/arguments pour lancer le serveur*)
let spec_list = ["-port", Arg.Set_int port, "Specify the port of the server, 8080 by default"]
let anon_fun p = port := int_of_string p
let usage = "server [-port]"


(* LAYER 1 STUFF *)
let default_balance = 500
let ledger_l1 = Hashtbl.create 50
let rollups = Hashtbl.create 50
let sequencers = Hashtbl.create 50

exception Ledger_not_found
exception Rollup_not_found
exception Insufficient_balance

let deposit ledger account amount =
  if Hashtbl.mem ledger account then
    let curr_value = Hashtbl.find ledger account in
    Hashtbl.replace ledger account (curr_value + amount)
  else
    raise Ledger_not_found
let withdraw ledger account amount =
  if Hashtbl.mem ledger account then
    let balance = Hashtbl.find ledger account in
    if balance >= amount then
      Hashtbl.replace ledger account (balance - amount)
    else
      raise Insufficient_balance
  else
    raise Ledger_not_found
(*FIN LAYER1 STUFF*)

type fund_object = {
  rollup_id_fund : int;
  amount : int;
  identity : string;
} [@@deriving yojson]

type put_object = {
  rollup_id_put : int;
  sender : string;
  amount : int;
  receiver : string;
} [@@deriving yojson]

type registration_object = {
  sequencer_identity : string;
  sequencer_port : int;
  targeted_rollup : int;
} [@@deriving yojson]


let _ =
  Arg.parse spec_list (fun _ -> ()) usage;
  log "Server started on ..%i" !port;
  Dream.run ~port:!port ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router [
    Dream.post "/registration"
      (fun request ->
        let%lwt body = Dream.body request in
        let registration_object =
          body |> Yojson.Basic.from_string
        in

        let open Yojson.Basic.Util in
        let name_sequencer = registration_object |> member "sequencer_identity" |> to_string in
        let port_sequencer = registration_object |> member "sequencer_port" |> to_int in
        let rollup_id = registration_object |> member "targeted_rollup" |> to_int in
        if Hashtbl.mem rollups rollup_id then
          if Hashtbl.mem sequencers name_sequencer then
            (*peut etre paramétré les messages de réponse aussi*)
            Dream.html (Printf.sprintf "Séquenceur déja enregistré : %s, affecté au rollup : %i, sur le port: %i" name_sequencer rollup_id port_sequencer)
          else (
            Hashtbl.add sequencers name_sequencer (port_sequencer,rollup_id);
            Dream.html (Printf.sprintf "Séquenceur %s, port %i enregistré et affecté au rollup %i" name_sequencer port_sequencer rollup_id))
        else
          Dream.html (Printf.sprintf "This rollup doesn't exist")
      )
    ;
    Dream.post "/ledger/new"
      (fun request ->
        let%lwt body = Dream.body request in
        if Hashtbl.mem ledger_l1 body then
          Dream.html (Printf.sprintf "%s's entry in the ledger already exist" body)
        else
          (Hashtbl.add ledger_l1 body default_balance;
          Dream.html (Printf.sprintf "%s's entry in the ledger created with a balance of : %i" body default_balance)))
    ;
    Dream.post "/rollup/new"
      (fun _ ->
        let new_rollup = Rollup.create () in
        let new_rollup_id = Rollup.get_id new_rollup in
        Hashtbl.add rollups new_rollup_id new_rollup;
        Dream.html (Printf.sprintf "New rollup context initialized, rollup_id : %i" new_rollup_id))
    ;
    Dream.post "rollup/fund"
      (fun request ->
        let%lwt body = Dream.body request in
        let fund_object =
          body
          |> Yojson.Basic.from_string
        in
        let open Yojson.Basic.Util in 
        let rollup_id = fund_object |> member "rollup_id_fund" |> to_int in
        let amount = fund_object |> member "amount" |> to_int in
        let identity = fund_object |> member "identity" |> to_string in

        if Hashtbl.mem rollups rollup_id then
          let curr_rollup = Hashtbl.find rollups rollup_id in
          let curr_rollup_ledger = curr_rollup.ledger in
          match withdraw ledger_l1 identity amount with
            | exception Insufficient_balance -> 
              Dream.html (Printf.sprintf "%s's doesn't have enough money to deposit this amount" identity)
            | exception Ledger_not_found -> 
              Dream.html (Printf.sprintf "%s doesn't have an entry in the ledger of L1" identity)
            | _ ->
              match deposit curr_rollup_ledger identity amount with
                | exception Ledger_not_found -> (**If user doesn't have an entry in the ledger of the rollup, i create it**)
                  Hashtbl.add curr_rollup_ledger identity amount;
                  Dream.html (Printf.sprintf "%s's wallet created on the rollup %i with a balance of : %i" identity rollup_id amount)
                | _ -> Dream.html (Printf.sprintf "%itz added to the %s's wallet in the rollup : %i" amount identity rollup_id)
        else
          Dream.html (Printf.sprintf "This rollup doesn't exist"))
    ;
    Dream.post "rollup/put"
      (fun request ->
        let%lwt body = Dream.body request in
        let put_object =
          body
          |> Yojson.Basic.from_string
        in
        let open Yojson.Basic.Util in 
        let rollup_id = put_object |> member "rollup_id_put" |> to_int in
        let sender = put_object |> member "sender" |> to_string in
        let amount = put_object |> member "amount" |> to_int in
        let receiver = put_object |> member "receiver" |> to_string in
        
        if Hashtbl.mem rollups rollup_id then
          let curr_rollup = Hashtbl.find rollups rollup_id in
          let curr_rollup_ledger = curr_rollup.ledger in
          match withdraw curr_rollup_ledger sender amount with
            | exception Insufficient_balance -> 
              Dream.html (Printf.sprintf "%s's doesn't have enough money to deposit this amount" sender)
            | exception Ledger_not_found -> 
              Dream.html (Printf.sprintf "%s doesn't have an entry in the rollup" sender)
            | _ ->
              match deposit curr_rollup_ledger receiver amount with
                | exception Ledger_not_found -> (**If user doesn't have an entry in the ledger of the rollup, i create it**)
                  Hashtbl.add curr_rollup_ledger receiver amount;
                  Dream.html (Printf.sprintf "%s's doesn't have money on this rollup, fund first please" receiver)
                | _ -> Dream.html (Printf.sprintf "transaction done, sender : %s, amount : %itz receiver : %s, in the rollup : %i" sender amount receiver rollup_id)
        else
          Dream.html (Printf.sprintf "This rollup doesn't exist"))
    ;
    Dream.get "/ledger/:identity"
      (fun request ->
        let id = Dream.param request "identity" in
        if Hashtbl.mem ledger_l1 id then
          Dream.html (Printf.sprintf "Balance of %s : %i" id (Hashtbl.find ledger_l1 id))
        else
          Dream.html (Printf.sprintf "%s's has no entry in the ledger" id)
        )
    ;
    Dream.get "/rollup/:id/ledger/:identity"
      (fun request ->
        let rollup_id = Dream.param request "id" |> int_of_string in
        let identity = Dream.param request "identity" in
        if Hashtbl.mem rollups rollup_id then
          let curr_rollup = Hashtbl.find rollups rollup_id in
          let curr_rollup_ledger = curr_rollup.ledger in
          if Hashtbl.mem curr_rollup_ledger identity then
            let balance = Hashtbl.find curr_rollup_ledger identity in
            Dream.html ((Printf.sprintf "Balance de %s : %i") identity balance)
          else
            Dream.html (Printf.sprintf "Aucun ledger n'est associé a %s" identity)
        else
          Dream.html (Printf.sprintf "This rollup doesn't exist"))
    ;
    Dream.get "/rollup/:rollup_id/level"
      (fun request ->
        let rollup_id = Dream.param request "rollup_id" |> int_of_string in
        if Hashtbl.mem rollups rollup_id then
          let level = Rollup.get_current_level (Hashtbl.find rollups rollup_id) in
          Dream.html (Printf.sprintf "Rollup %i current level : %i" rollup_id level)
        else
          Dream.html (Printf.sprintf "This rollup doesn't exist")
      )
    ;
    Dream.get "/rollup/:rollup_id/participants"
    (fun request ->
      let rollup_id = Dream.param request "rollup_id" |> int_of_string in
      if Hashtbl.mem rollups rollup_id then
        let rollup = Hashtbl.find rollups rollup_id in
        let participants = Rollup.get_participants rollup in
        let n = Rollup.get_nb_participants rollup in
        let buff = Buffer.create (n*2) in
        Seq.iter (fun name -> Buffer.add_string buff name; Buffer.add_char buff '\n') participants;
        Dream.html (Printf.sprintf "Participant in the rollup %i : \n%s" rollup_id (Buffer.contents buff))
      else
        Dream.html (Printf.sprintf "This rollup doesn't exist")
    )
    ;
    Dream.get "/sequencers"
    (fun _ ->
      let iterator = fun name (port,rollup_id) agreg -> 
        let str = Printf.sprintf "Séquenceur : %s, port : %i, rollup monitoré : %i" name port rollup_id in
        str ^ agreg in 
      Dream.html ( Hashtbl.fold iterator sequencers "")
    )
  ]