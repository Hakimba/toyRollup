open Dream
open Rollup
let port = ref 8080

(*gestion des options/arguments pour lancer le serveur*)
let spec_list = ["-port", Arg.Set_int port, "Specify the port of the server, 8000 by default"]
let anon_fun p = port := int_of_string p
let usage = "server [-port]"

(*Tout les ledger seront initialisé avec une balance de 1000*)
let default_balance = 1000

module Ledgers = Map.Make(String)
module Rollups = Map.Make(Int)
let ledgers = ref (Ledgers.add "genesis_ledger" default_balance Ledgers.empty)
let genesis_rollup = Rollup.make ()

let rollups = ref (Rollups.add (Rollup.get_id genesis_rollup) genesis_rollup Rollups.empty)

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

(*
  Architecture :

    -Représenter le Layer 1 par une notion de ledger associé a des identités
    -Un contexte dédié aux rollups (qui maintient toutes les opérations reçues) et représente le Layer 2
    -Une notion de temps qui passe (pas comprit)
*)

(*
  Routes du serveur a implémenter : 

  Celle données par Xavier :
    GET 
      ledger IDENTITY [DONE]
    
    POST
      rollup/new [DONE]
      rollup/fund ROLLUP_ID AMOUNT IDENTITY [DONE]
      rollup/put ROLLUP_ID IDENTITY AMOUNT IDENTITY_RECEIVER
        -> c'est une transaction, donc faudrais l'enregistrer dans le txs du rollup en question
      batch/send ROLLUP_ID IDENTITY BALANCES_UPDATES (pourquoi faire le balances_updates ?)
    
  Ceux que j'infère : 

    POST
      ledger/new IDENTITY [DONE] -> créer un ledger associé a IDENTITY, valeur par défaut : 100
    GET
      rollup/ledger ROLLUP_ID IDENTITY [DONE] -> regarder la balance d'un ledger d'un rollup
*)

let _ =
  Arg.parse spec_list anon_fun usage;
  log "Server started on ..%i" !port;
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.post "/ledger/new" 
      (fun request ->
        let%lwt body = Dream.body request in
        if Ledgers.mem body !ledgers then
          Dream.html (Printf.sprintf "Le ledger associé a %s existe déja" body)
        else
          (ledgers := Ledgers.add body default_balance !ledgers;
          Dream.html (Printf.sprintf "Le ledger associé a %s a été créer" body)))
    ;
    Dream.post "/rollup/new"
      (fun _ ->
        let new_rollup = Rollup.make () in
        let new_rollup_id = Rollup.get_id new_rollup in
        rollups := Rollups.add new_rollup_id new_rollup !rollups;
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

        (if Rollups.mem rollup_id !rollups then
          let curr_rollup = Rollups.find rollup_id !rollups in
          let curr_rollup_ledgers = curr_rollup.ledgers in
          let curr_balance = 
            if Ledgers.mem identity curr_rollup_ledgers then
              Ledgers.find identity curr_rollup_ledgers
            else
              0 in
          let new_ledgers = Ledgers.add identity (curr_balance + amount) curr_rollup_ledgers in
          let new_context = {Rollup.ledgers=new_ledgers;id=curr_rollup.id;txs=curr_rollup.txs} in
          rollups := Rollups.add rollup_id new_context !rollups;
          Dream.html (Printf.sprintf "%itz added to the %s's ledger in the rollup : %i" amount identity rollup_id)  
        else
          Dream.html "bad rollup_id, this rollup doesn't exist"))
    ;
    Dream.post "rollup/put"
      (fun request ->
        let%lwt body = Dream.body request in
        let put_object =
          body
          |> Yojson.Safe.from_string
          |> put_object_of_yojson
        in
        let rollup_id = `Int put_object.rollup_id_put |> Yojson.Safe.to_string |> int_of_string in
        let sender = `String put_object.sender |> Yojson.Safe.to_string in
        let amount =  `Int put_object.amount |> Yojson.to_string |> int_of_string in
        let receiver = `String put_object.receiver |> Yojson.to_string in
        
        if Rollups.mem rollup_id !rollups then
          let curr_rollup = Rollups.find rollup_id !rollups in
          let curr_rollup_ledger = curr_rollup.ledgers in
          if Ledgers.mem sender curr_rollup_ledger then
            if Ledgers.mem receiver curr_rollup_ledger then
              let sender_balance = Ledgers.find sender curr_rollup_ledger in
              let receiver_balance = Ledgers.find receiver curr_rollup_ledger in
              let withdraw = sender_balance - amount in
              if withdraw < 0 then
                Dream.html ((Printf.sprintf "%s's has not enough money, current balance at : %i") sender sender_balance)
              else 
                let new_ledger1 = Ledgers.add sender withdraw curr_rollup_ledger in
                let new_ledger2 = Ledgers.add receiver (amount + receiver_balance) new_ledger1 in 
                let new_context = {Rollup.ledgers=new_ledger2;id=curr_rollup.id;txs=curr_rollup.txs} in
                rollups := Rollups.add rollup_id new_context !rollups;
                Dream.html (Printf.sprintf ("Transaction : %s send %i to %s") sender amount receiver)
            else
              Dream.html (Printf.sprintf "Aucun ledger n'est associé a %s" receiver)
          else
            Dream.html (Printf.sprintf "Aucun ledger n'est associé a %s" sender)

        else
          Dream.html "bad rollup_id, this rollup doesn't exist"
        )
    ;
    Dream.get "/ledger/:identity"
      (fun request ->
        let id = Dream.param request "identity" in
        if Ledgers.mem id !ledgers then
          Dream.html (Printf.sprintf "Balance de %s : %i" id (Ledgers.find id !ledgers))
        else
          Dream.html (Printf.sprintf "Aucun ledger n'est associé a %s" id)
        )
    ;
    Dream.get "/rollup/:id/:identity"
      (fun request ->
        let rollup_id = Dream.param request "id" |> int_of_string in
        let identity = Dream.param request "identity" in
        if Rollups.mem rollup_id !rollups then
          let curr_rollup = Rollups.find rollup_id !rollups in
          let curr_rollup_ledgers = curr_rollup.ledgers in
          if Ledgers.mem identity curr_rollup_ledgers then
            let balance = Ledgers.find identity curr_rollup_ledgers in
            Dream.html ((Printf.sprintf "Balance de %s : %i") identity balance)
          else
            Dream.html (Printf.sprintf "Aucun ledger n'est associé a %s" identity)
        else
          Dream.html "bad rollup_id, this rollup doesn't exist")
  ]