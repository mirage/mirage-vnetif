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
        use_async_readers : bool;
        listener_callback : ((buffer -> unit io) -> buffer -> unit io);
        listeners : (int, buffer -> unit io) Hashtbl.t;
        macs : (int, macaddr) Hashtbl.t;
    }

    let make_mac id =
        (* TODO Do something more clever here.*)
        let base_mac = [| 0 ; 0x50 ; 0x2a ; 0x16 ; 0x6d ; id |] in
        Macaddr.make_local (Array.get base_mac)

    let create ?(use_async_readers=false) () =
        if use_async_readers then
            {last_id = 0; 
             call_counter = 0;
             listeners = Hashtbl.create 7; 
             macs = Hashtbl.create 7;
             use_async_readers;
             listener_callback = (fun f buffer -> Lwt.async (fun () -> f buffer); Lwt.return_unit)}
        else
            {last_id = 0; 
             call_counter = 0;
             listeners = Hashtbl.create 7; 
             macs = Hashtbl.create 7;
             use_async_readers;
             listener_callback = (fun f buffer -> (f buffer))}

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

    let buffer_copy src =
        let len = Cstruct.len src in
        let dst = Cstruct.create len in
        Cstruct.blit src 0 dst 0 len;
        dst

    let write_copy t id buffer =
        let keys = 
            Hashtbl.fold (fun k v lst -> k::lst) t.listeners [] 
        in
        let send t src dst =
            if src != dst then
            begin
                t.call_counter <- t.call_counter + 1;
                let fn = (Hashtbl.find t.listeners dst) in
                t.listener_callback fn (buffer_copy buffer)
            end else
                Lwt.return_unit
        in
        (Lwt_list.iter_p (send t id) keys) >>= fun () ->
        if t.use_async_readers then
            Lwt.pause () (* yield to other threads *)
        else
            Lwt.return_unit

    let write t id buffer =
        write_copy t id buffer

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
        write_copy t id total_buf

end

