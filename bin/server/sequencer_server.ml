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

let registration_addr = "http://localhost:8080/registration"
let registration_body identity port = Printf.sprintf {|{"sequencer_identity" : %s, "sequencer_port" : %i}|} identity port

let registration_response ctx =
  let (resp,body) = ctx in
  let _ = resp |> Response.status in 
  body |> Cohttp_lwt.Body.to_string


let _ =
  Arg.parse spec_list (fun _ -> ()) usage;
  log "Sequencer started on %i" !port;
  let _ = registration_body !identity !port in 
  let resp = Lwt_main.run (get_request registration_addr registration_response) in 
  let _ = Dream.log "%s\n" resp in (**remplacer toute cette merde par des middleware serais plus malin, je pense hein**)
  Dream.run ~port:!port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/sequencer"
      (fun _ -> 
        Dream.html "sequencer"
      )
  ]