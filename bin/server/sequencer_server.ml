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
let get_request addr f = Client.get (Uri.of_string addr) >>= f
let post_request addr f body = Client.post ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string addr) >>= f

(**Au lancement du séquenceur, on s'enregistre sur le serveur principal (celui qui gère le L1 et les rollups)**)
let registration_addr = "http://localhost:8080/registration"
let registration_body identity port rollup = 
  Printf.sprintf {|{"sequencer_identity" : "%s", "sequencer_port" : %i, "targeted_rollup" : %i}|} identity port rollup

let registration_response ctx =
  let (resp,body) = ctx in
  let _ = resp |> Response.status in 
  body |> Cohttp_lwt.Body.to_string

let registration () =
  let body = registration_body !identity !port !rollup_id in 
  Lwt_main.run (post_request registration_addr registration_response body)


let _ =
  Arg.parse spec_list (fun _ -> ()) usage;

  log "Sequencer started on %i" !port;
  let _ = Dream.log "%s\n" (registration ()) in
  Dream.run ~port:!port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/sequencer"
      (fun _ -> 
        Dream.html "sequencer"
      )
  ]