open Dream
open Lwt
open Cohttp_lwt_unix

let port = ref 8000
let identity = ref ""
let rollup_id = ref 0 
let spec_list = [
  "-identity", Arg.Set_string identity, "Your entry name in the ledger of the rollup";
  "-rollup-id", Arg.Set_int rollup_id, "The rollup your want to monitor";
  "-port", Arg.Set_int port, "Specify the port of the server, 8000 by default"
]

let usage = "[-identity] [-rollup-id] [-port]"

let send_transaction addr = Client.get (Uri.of_string addr) >>=
        fun (_, body) ->
          body |> Cohttp_lwt.Body.to_string >|= fun body -> Printf.sprintf "balance : %s" body
let run_transaction b = Lwt_main.run b

let _ =
  Arg.parse spec_list (fun _ -> ()) usage;
  log "Sequencer started on %i" !port;
  Dream.run ~port:!port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/sequencer"
      (fun _ -> 
        Dream.html (run_transaction (send_transaction "http://localhost:8080/ledger/hakim"))
      )
    ;
    Dream.get "/test"
    (fun _ -> Dream.html "hey\n")
    (*TODO : lire les quelques ressources que C2 m'as donné*)
    (*TODO : trouver comment envoyer des requetes http le plus simplement possible pour lancer une communication entre sequenceur et rollup*)
    (*TODO : determiner le modèle du sequenceur (ecrire les types etc meme si c'est incomplet au début)*)
  ]