(*
 * Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2015 Magnus Skjegstad <magnus@v0.no>
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

module type BACKEND = sig
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t
    type t
    type error = [ `Unknown of string | `Disconnected | `Unimplemented ]

    val register : t -> [ `Ok of id | `Error of error ]
    val unregister : t -> id -> unit io
    val mac : t -> id -> macaddr
    val write : t -> id -> buffer -> unit io
    val set_listen_fn : t -> id -> (buffer -> unit io) -> unit
end


(** Dummy interface for software bridge. *)
module Make(B : BACKEND) : sig
    include V1.NETWORK
    with type 'a io = 'a Lwt.t
     and type     page_aligned_buffer = Io_page.t
     and type     buffer = Cstruct.t
     and type     id = B.id
     and type     macaddr = Macaddr.t

    val connect : B.t -> [`Ok of t | `Error of error] io
end

