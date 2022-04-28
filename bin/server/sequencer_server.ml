open Dream
open Lwt
open Cohttp_lwt_unix
open Rlp.Sequencer
open Rlp.Transaction
let port = ref 8000
let identity = ref ""
let rollup_id = ref 0 

(*by default*)

(*on stocke la transaction et le niveau courant dans un couple, pour pouvoir filtrer sur le niveau quand on feras un batch*)
let txs_storage = ref (TxsStorage.add ({sender="genesis";amount=0;receiver="genesis"},0) (TxsStorage.create))
module Sqc = Sequencer(TxsStorage)
let sequencer_setting = Sqc.make identity port rollup_id

let spec_list = [
  "-identity", Arg.Set_string identity, "Your entry name in the ledger of the rollup";
  "-rollup-id", Arg.Set_int rollup_id, "The rollup your want to monitor";
  "-port", Arg.Set_int port, "Specify the port of the server, 8000 by default"
]

let usage = "[-identity] [-rollup-id] [-port]"
let get_request addr f = Client.get (Uri.of_string addr) >>= f
let post_request addr f body = Client.post ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string addr) >>= f

(**Au lancement du séquenceur, on s'enregistre sur le serveur principal (celui qui gère le L1 et les rollups)**)
let registration_addr = "http://localhost:8080/registration"
let registration_body identity port rollup = 
  Printf.sprintf {|{"sequencer_identity" : "%s", "sequencer_port" : %i, "targeted_rollup" : %i}|} identity port rollup

let basic_response ctx =
  let (resp,body) = ctx in
  let _ = resp |> Response.status in
  body |> Cohttp_lwt.Body.to_string

let registration () =
  let body = registration_body !identity !port !rollup_id in 
  Lwt_main.run (post_request registration_addr basic_response body)

let _ =
  Arg.parse spec_list (fun _ -> ()) usage;
  log "Sequencer started on %i" !port;
  let _ = Dream.log "%s\n" (registration ()) in
  Dream.run ~port:!port ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router [
    Dream.post "/sequencer/transaction"
    (fun request ->
      let%lwt body = Dream.body request in
      let transaction_object =
        body
        |> Yojson.Basic.from_string
      in
      let open Yojson.Basic.Util in
      let sender = transaction_object |> member "sender" |> to_string in
      let amount = transaction_object |> member "amount" |> to_int in
      let receiver = transaction_object |> member "receiver" |> to_string in
      let associated_rollup = Sqc.get_associated_rollup sequencer_setting in

      (*pour les appels aux endpoints qui permettent de verifier l'existence du sender/receiver, et la balance du sender*)
      let url_sender = Printf.sprintf "http://localhost:8080/rollup/%i/%s" associated_rollup sender in
      let url_receiver = Printf.sprintf "http://localhost:8080/rollup/%i/%s" associated_rollup receiver in
      let url_balance_sender = 
        (Printf.sprintf "http://localhost:8080/rollup/%i/balance/%s" 
        associated_rollup sender) in
      
      (*pour l'appel a l'endpoint d'envoie de transaction au serveur rollup*)
      let url_send_tx = "http://localhost:8080/rollup/put" in
      let body_send_tx = 
        Printf.sprintf {|{"rollup_id_put" : "%i", "sender" : %s, "amount" : %i, "receiver":%s}|} associated_rollup sender amount receiver in

      let url_get_level = Printf.sprintf "http://localhost:8080/rollup/%i/level" associated_rollup in

      let%lwt body_s = get_request url_sender basic_response in
      let%lwt body_r = get_request url_receiver basic_response in
      let exist_s = bool_of_string body_s in
      let exist_r = bool_of_string body_r in

      if exist_s && exist_r then
        let%lwt balance = get_request url_balance_sender basic_response in
        let%lwt level = get_request url_get_level basic_response in
        let balance = int_of_string balance in
        let level = int_of_string level in
        if balance >= amount then
          (*la partie sequenceur, on enregistre la transaction*)
          let tx = make_transaction sender amount receiver in
          txs_storage := Sqc.add_transaction (tx,level) !txs_storage;

          (*la partie rollup, on envoie la transaction au serveur rollup en utilisant l'endpoint adequat et on renvoie
             la reponse de l'endpoint*)
          let%lwt body_send_tx = post_request url_send_tx basic_response body_send_tx in
          Dream.html body_send_tx
        else
          Dream.html (Printf.sprintf "%s's balance not enough" sender)
      else
        Dream.html (Printf.sprintf "both sender and receiver have to be participant on rollup %i" (Sqc.get_associated_rollup sequencer_setting))
    )
    ;
    Dream.get "/sequencer/batch"
    (fun _ ->
      let url = (Printf.sprintf "http://localhost:8080/rollup/%i/level" (Sqc.get_associated_rollup sequencer_setting)) in
      let%lwt body = get_request url basic_response in
      let level = int_of_string body in
      (*pour l'instant, un batch = list des transactions selon le level courant*)
      let _ = TxsStorage.filter (fun (_,lvl) -> level = lvl) (!txs_storage) in
      (*que faire du batch ?*)
      Dream.html "unsupported yet"

    )
  ]