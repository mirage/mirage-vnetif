open Lwt

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let netmask = Ipaddr.V4.of_string_exn "255.255.255.0" 
let gw = Ipaddr.V4.of_string_exn "192.168.56.1" 

module Main (C: V1_LWT.CONSOLE) = struct

  module Stack = struct
    module B = Basic_backend.Make
    module V = Vnetif.Make(B)
    module E = Ethif.Make(V)
    module I = Ipv4.Make(E)
    module U = Udp.Make(I)
    module T = Tcp.Flow.Make(I)(OS.Time)(Clock)(Random)
    module S = Tcpip_stack_direct.Make(C)(OS.Time)(Random)(V)(E)(I)(U)(T)
    include S
  end

  let or_error name fn t =
    fn t
    >>= function
        | `Error e -> fail (Failure ("Error starting " ^ name))
        | `Ok t -> return t 

  let accept c flow =
    let ip, port = Stack.TCPV4.get_dest flow in
    C.log_s c (green "Accepted connection from %s:%d" (Ipaddr.V4.to_string ip) port) >>= fun () ->
    Stack.TCPV4.close flow >>= fun () ->
    C.log_s c (green "Connection closed")

  let create_stack c backend ip =
    or_error "backend" Stack.V.connect backend >>= fun netif ->
    C.log_s c (blue "Connected to backend with mac %s" (Macaddr.to_string (Stack.V.mac netif))) >>= fun () ->
    or_error "ethif" Stack.E.connect netif >>= fun ethif ->
    or_error "ipv4" Stack.I.connect ethif >>= fun ipv4 ->
    or_error "udpv4" Stack.U.connect ipv4 >>= fun udpv4 ->
    or_error "tcpv4" Stack.T.connect ipv4 >>= fun tcpv4 ->
    let config = {
        V1_LWT.name = "stack";
        V1_LWT.console = c; 
        V1_LWT.interface = netif;
        V1_LWT.mode = `IPv4 (Ipaddr.V4.of_string_exn ip, netmask, [gw]);
    } in
    or_error "stack" (Stack.connect config ethif ipv4 udpv4) tcpv4
  
  let start c =
    let backend = Stack.B.create in
    Lwt.choose [
        (create_stack c backend "192.168.56.99" >>= fun s1 ->
        Stack.listen_tcpv4 s1 ~port:80 (fun f -> accept c f);
        Stack.listen s1) ;

        (OS.Time.sleep 3.0 >>= fun () ->
        create_stack c backend "192.168.56.98" >>= fun s2 ->
        or_error "connect" (Stack.TCPV4.create_connection (Stack.tcpv4 s2)) ((Ipaddr.V4.of_string_exn "192.168.56.99"), 80) >>= fun flow ->
        C.log_s c (yellow "Connected to other end.. closing") >>= fun () ->
        Stack.TCPV4.close flow >>= fun () ->
        Lwt.return_unit) ]

end
