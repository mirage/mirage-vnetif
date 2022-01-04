(*
 * Copyright (c) 2015-20 Magnus Skjegstad <magnus@skjegstad.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

module Stack(B: Vnetif.BACKEND) = struct
  module V = Vnetif_stack.Vnetif_stack(B)(Mirage_random_test)(Time)(Mclock)
  include V
end

let connect_test_lwt _ () =
  let module Backend = Basic_backend.Make in
  let module Stack = Stack(Backend) in
  let backend = Backend.create ~use_async_readers:true ~yield:Lwt.pause () in

  let test_msg = "This is a connect test. ABCDEFGHIJKLMNOPQRSTUVWXYZ" in

  let or_error name fn t =
    fn t >>= function
    | Error e -> Alcotest.failf "%s: %s" name (Format.asprintf "%a" Stack.V4.TCPV4.pp_error e)
    | Ok t    -> Lwt.return t
  in

  let accept client_l flow expected =
    or_error "read" Stack.V4.TCPV4.read flow >>= function
    | `Eof -> Alcotest.failf "eof while reading from socket"
    | `Data data ->
        let recv_str = Cstruct.to_string data in
        Alcotest.(check string) "server and client strings matched" expected recv_str;
        Lwt_mutex.unlock client_l;
        Lwt.return_unit
  in

  let client_ip = Ipaddr.V4.Prefix.of_address_string_exn "10.0.0.10/24" in
  let server_ip = Ipaddr.V4.Prefix.of_address_string_exn "10.0.0.11/24" in

  let timeout_in_s = 5 in

  (* mutex to signal success from server to client *)
  let accept_l = Lwt_mutex.create () in
  (* mutex to signal client that server is listening *)
  let listen_l = Lwt_mutex.create () in
  (Lwt_mutex.with_lock accept_l (fun _ ->
    Lwt_mutex.with_lock listen_l (fun _ ->
      Lwt.pick [
          (* Cancellation timer *)
          (Time.sleep_ns (Duration.of_sec timeout_in_s) >>= fun () ->
          Alcotest.failf "timeout: test timed out after %d seconds" timeout_in_s);

          (* Server side *)
          (Stack.create_stack_ipv4 ~ip:server_ip ~unlock_on_listen:listen_l backend >>= fun s1 ->
          Stack.V4.listen_tcpv4 s1 ~port:80 (fun f -> accept accept_l f test_msg);
          Stack.V4.listen s1 >>= fun () ->
          Alcotest.failf "server: listen should never exit");

          (* Client side *)
          Lwt_mutex.lock listen_l >>= fun () -> (* wait for server to unlock with call to listen *)
          Stack.create_stack_ipv4 ~ip:client_ip backend >>= fun s2 ->
          or_error "connect" (Stack.V4.TCPV4.create_connection (Stack.V4.tcpv4 s2)) (snd server_ip, 80) >>= fun flow ->
          Stack.V4.TCPV4.write flow (Cstruct.of_string test_msg) >>= (function
              | Ok () -> Lwt.return_unit
              | Error e -> Alcotest.failf "write: %s" (Format.asprintf "%a" Stack.V4.TCPV4.pp_write_error e))
          >>= fun () ->
          Stack.V4.TCPV4.close flow >>= fun () ->
          Lwt_mutex.lock accept_l (* wait for accept to unlock *)
      ]
    )
  )
 )

let () =
  let rand_seed = 0 in
  Random.init rand_seed;
  Printf.printf "Testing with rand_seed %d\n" rand_seed;

  (*Mirage_random_test.initialize();*)

  Lwt_main.run @@
  Alcotest_lwt.run "mirage-vnetif" [
      ("stack.v4",
          [ Alcotest_lwt.test_case "connect" `Quick connect_test_lwt ]
      )
  ]
