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

module Make = struct
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t
    type error = [ `Unknown of string | `Disconnected | `Unimplemented ]

    type t = {
        mutable last_id : int;
        listeners : (int, buffer -> unit io) Hashtbl.t;
        macs : (int, macaddr) Hashtbl.t;
    }

    let make_mac id =
        (* TODO Do something more clever here.*)
        let base_mac = [| 0 ; 0x50 ; 0x2a ; 0x16 ; 0x6d ; id |] in
        Macaddr.make_local (Array.get base_mac)

    let create =
        {last_id = 0; 
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

    let write t id buffer =
        let keys = 
            Hashtbl.fold (fun k v lst -> k::lst) t [] 
        in
        let send t buffer src dst =
            if src != dst then
                let fn = (Hashtbl.find t dst) in
                    fn buffer
            else
                Lwt.return_unit
        in
        (Lwt_list.iter_s (send t buffer id) keys)

    (*Hashtbl.iter (send id) t.listeners;
    Lwt.return_unit*)

end
