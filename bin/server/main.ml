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
  rollup_id : int;
  amount : int;
  identity : string;
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
      rollup/new
      rollup/fund ROLLUP_ID AMOUNT IDENTITY
      rollup/put ROLLUP_ID IDENTITY AMOUNT IDENTITY_RECEIVER
      batch/send ROLLUP_ID IDENTITY BALANCES_UPDATES (pourquoi faire le balances_updates ?)
    
  Ceux que j'infère : 

    POST
      ledger/new IDENTITY [DONE] -> créer un ledger associé a IDENTITY, valeur par défaut : 100
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
          |> Yojson.Safe.from_string
          |> fund_object_of_yojson
        in
        let rollup_id = `Int fund_object.rollup_id |> Yojson.Safe.to_string in
        let amount = `Int fund_object.amount |> Yojson.Safe.to_string in
        let sender = `String fund_object.identity |> Yojson.Safe.to_string in 
        
        Dream.html (Printf.sprintf "rollup_id : %s\namount : %s\nsender : %s" rollup_id amount sender))
    ;
    (**Dream.post "rollup/put"
      (fun request ->
        )
    ;**)
    Dream.get "/ledger/:identity"
      (fun request ->
        let id = Dream.param request "identity" in
        if Ledgers.mem id !ledgers then
          Dream.html (Printf.sprintf "Balance de %s : %i" id (Ledgers.find id !ledgers))
        else
          Dream.html (Printf.sprintf "Aucun ledger n'est associé a %s" id)
        )
  ]