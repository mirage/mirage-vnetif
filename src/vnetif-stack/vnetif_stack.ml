(*
 * Copyright (c) 2015-2020 Magnus Skjegstad <magnus@skjegstad.com>
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

module type Vnetif_stack =
sig
  type backend
  module V4V6 : Tcpip.Stack.V4V6
  module Backend : Vnetif.BACKEND

  (** Create a new IPv4 stack connected to an existing backend *)
  val create_stack_ipv4 : cidr:Ipaddr.V4.Prefix.t ->
    ?gateway:Ipaddr.V4.t -> ?mtu:int -> ?monitor_fn:(Cstruct.t -> unit Lwt.t) ->
    ?unlock_on_listen:Lwt_mutex.t ->
    backend -> V4V6.t Lwt.t
end

module Vnetif_stack (B : Vnetif.BACKEND)(R : Mirage_random.S)(Time : Mirage_time.S)(Mclock : Mirage_clock.MCLOCK):
          Vnetif_stack with type backend = B.t =
struct
  type backend = B.t

  module Backend = B
  module V = Vnetif.Make(Backend)
  module E = Ethernet.Make(V)
  module A = Arp.Make(E)(Time)
  module Ip4 = Static_ipv4.Make(R)(Mclock)(E)(A)
  module Icmp = Icmpv4.Make(Ip4)
  module Ip6 = Ipv6.Make(V)(E)(R)(Time)(Mclock)
  module Ip = Tcpip_stack_direct.IPV4V6(Ip4)(Ip6)
  module U = Udp.Make(Ip)(R)
  module T = Tcp.Flow.Make(Ip)(Time)(Mclock)(R)
  module V4V6 = Tcpip_stack_direct.MakeV4V6(Time)(R)(V)(E)(A)(Ip)(Icmp)(U)(T)

  let create_stack_ipv4 ~cidr ?gateway ?mtu ?monitor_fn ?unlock_on_listen backend =
    V.connect ?size_limit:mtu ?monitor_fn ?unlock_on_listen backend >>= fun netif ->
    E.connect netif >>= fun ethif ->
    A.connect ethif >>= fun arp ->
    Ip4.connect ~cidr ?gateway ethif arp >>= fun ipv4 ->
    Icmp.connect ipv4 >>= fun icmp ->
    Ip6.connect ~no_init:true netif ethif >>= fun ipv6 ->
    Ip.connect ~ipv4_only:true ~ipv6_only:false ipv4 ipv6 >>= fun ip ->
    U.connect ip >>= fun udp ->
    T.connect ip >>= fun tcp ->
    V4V6.connect netif ethif arp ip icmp udp tcp
end

(*module Vnetif_stack_unix(B: Vnetif.BACKEND)(R : Mirage_random.S):
  Vnetif_stack with type backend = B.t =
struct
  module X = Vnetif_stack(B)(R)(Time)(Mclock)
  include X
end*)
