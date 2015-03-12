(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
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
open Lwt

module Make = struct
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t
    type error = [ `Unknown of string | `Disconnected | `Unimplemented ]

    type t = {
        mutable last_id : int;
        mutable call_counter : int;
        listeners : (int, buffer -> unit io) Hashtbl.t;
        macs : (int, macaddr) Hashtbl.t;
    }

    let make_mac id =
        (* TODO Do something more clever here.*)
        let base_mac = [| 0 ; 0x50 ; 0x2a ; 0x16 ; 0x6d ; id |] in
        Macaddr.make_local (Array.get base_mac)

    let create =
        {last_id = 0; 
         call_counter = 0;
         listeners = Hashtbl.create 7; 
         macs = Hashtbl.create 7}

    let register t =
        t.last_id <- t.last_id + 1;
        Hashtbl.add t.macs t.last_id (make_mac t.last_id);
        (`Ok t.last_id)

    let unregister t id =
        Hashtbl.remove t.macs id;
        Hashtbl.remove t.listeners id;
        Lwt.return_unit

    let mac t id =
        Hashtbl.find t.macs id

    let set_listen_fn t id fn =
        Hashtbl.replace t.listeners id fn

    let write_bytes t id buffer =
        let keys = 
            Hashtbl.fold (fun k v lst -> k::lst) t.listeners [] 
        in
        let send t src dst =
            if src != dst then
            begin
                t.call_counter <- t.call_counter + 1;
                let call_counter = t.call_counter in
                let fn = (Hashtbl.find t.listeners dst) in
                    Lwt.async (fun f -> 
                        Printf.printf "Bridge: Calling fn %d -> %d (%d,len=%d)\n%!" src dst call_counter (String.length buffer);
                        fn (Cstruct.of_string buffer) >>= fun () ->
                        Printf.printf "Bridge: Exit from fn %d -> %d (%d)\n%!" src dst call_counter; 
                        Lwt.return_unit);
                    Lwt.return_unit
            end else
                Lwt.return_unit
        in
        (Lwt_list.iter_s (send t id) keys)

    let write t id buffer =
        write_bytes t id (Cstruct.to_string buffer)

    let writev t id buffers =
        (* assemble list of buffers into one buffer before sending *)
        let total_len = List.fold_left (fun a b -> a + Cstruct.len b) 0 buffers in
        let total_buf = Cstruct.create total_len in
        let check_len = List.fold_left (fun current_pos part_buf -> 
                                            let part_len = Cstruct.len part_buf in 
                                            Cstruct.blit part_buf 0 total_buf current_pos part_len;
                                            current_pos + part_len) 0 buffers 
        in
        assert(check_len == total_len);
        write t id total_buf

end

