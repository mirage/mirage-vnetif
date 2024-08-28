(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org>
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

open Mirage_net

module type BACKEND = sig
  type t

  val register : t -> (int, Net.error) result
  val unregister : t -> int -> unit Lwt.t
  val mac : t -> int -> Macaddr.t
  val write : t -> int -> size:int -> (Cstruct.t -> int) -> (unit, Net.error) result Lwt.t
  val set_listen_fn : t -> int -> (Cstruct.t -> unit Lwt.t) -> unit
  val unregister_and_flush : t -> int -> unit Lwt.t
end


(** Dummy interface for software bridge. *)
module Make(B : BACKEND) : sig
  include Mirage_net.S
  val connect : ?size_limit:int -> ?flush_on_disconnect:bool -> ?monitor_fn:(Cstruct.t -> unit Lwt.t) -> ?unlock_on_listen:Lwt_mutex.t -> B.t -> t Lwt.t
  val disconnect : t -> unit Lwt.t
end
