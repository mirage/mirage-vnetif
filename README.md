# mirage-vnetif -- Virtual network interface and software switch for Mirage

Provides the module `Vnetif` which can be used as a replacement for the regular `Netif` implementation in Xen and Unix. Stacks built using `Vnetif` are connected to a software switch that allows the stacks to communicate as if they were connected to the same LAN.

An example of a unikernel that communicates with itself over `Vnetif` can be seen [here](https://github.com/mirage/mirage-vnetif/blob/master/examples/connect/unikernel.ml). An iperf-like performance test is available [here](https://github.com/mirage/mirage-vnetif/tree/master/examples/iperf_self). The examples can be compiled for Unix and Xen and do not need access to a real network interface.

## Install

```
opam install mirage-vnetif
```

## Getting started

First, construct a TCP/IP stack based on `vnetif`:

```ocaml
  module S = struct
    module B = Basic_backend.Make
    module V = Vnetif.Make(B)
    module E = Ethif.Make(V)
    module I = Ipv4.Make(E)(Clock)(OS.Time)
    module U = Udp.Make(I)
    module T = Tcp.Flow.Make(I)(OS.Time)(Clock)(Random)
    module S = Tcpip_stack_direct.Make(C)(OS.Time)(Random)(V)(E)(I)(U)(T)
    include S
  end
```

Since we don't have the mirage-tool to help us we have to construct the stack manually. This code would usually be generated in `main.ml` by `mirage configure --xen/unix`.

```ocaml
let or_error name fn t =
    fn t
    >>= function
        | `Error e -> fail (Failure ("Error starting " ^ name))
        | `Ok t -> return t

let create_stack c backend ip netmask gw =
    or_error "backend" S.V.connect backend >>= fun netif ->
    or_error "ethif" S.E.connect netif >>= fun ethif ->
    or_error "ipv4" S.I.connect ethif >>= fun ipv4 ->
    or_error "udpv4" S.U.connect ipv4 >>= fun udpv4 ->
    or_error "tcpv4" S.T.connect ipv4 >>= fun tcpv4 ->
    let config = {
        Mirage_types_lwt.name = "stack";
        Mirage_types_lwt.console = c;
        Mirage_types_lwt.interface = netif;
        Mirage_types_lwt.mode = `IPv4 (ip, netmask, gw);
    } in
    or_error "stack" (S.connect config ethif ipv4 udpv4) tcpv4

```


We can now create multiple stacks that talk over the same backend. `Basic_backend.create` accepts two optional parameters:
- `use_async_readers` makes the `write` calls non-blocking. This is necessary to use Vnetif with the Mirage TCP/IP stack.
- `yield` specifies the yield function to use in non-blocking mode. In a unikernel this is typically `OS.Time.sleep 0.0`, but in a Unix process `Lwt_main.yield ()` can be used instead.

```ocaml

let () =

    (* create async backend with OS.Time.sleep 0.0 as yield *)
    let backend = Basic_backend.create ~use_async_readers:true
        ~yield:(fun() -> OS.Time.sleep 0.0 ) () in

    let netmask = Ipaddr.V4.of_string_exn "255.255.255.0"  in
    let gw = Ipaddr.V4.of_string_exn "10.0.0.1" in

    let server_ip = Ipaddr.V4.of_string_exn "10.0.0.100" in
    create_stack c backend server_ip netmask [gw] >>= fun server_stack ->

    let client_ip = Ipaddr.V4.of_string_exn "10.0.0.101" in
    create_stack c backend server_ip netmask [gw] >>= fun client_stack ->
```

The stacks can now be used as regular Mirage TCP/IP stacks, e.g.:

```ocaml
S.listen_tcpv4 server_stack ~port:80 (fun f -> ...);
S.listen s1
```

## Build examples
```
mirage configure --xen/--unix
make
```

## Releasing mirage-vnetif

Please ensure that tcpip works with your proposed changes and release:

```
git clone https://github.com/mirage/mirage-tcpip.git
cd mirage-tcpip ; ln -s ../mirage-vnetif .
cd mirage-tcpip ; dune runtest
```
