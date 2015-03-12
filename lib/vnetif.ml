open Lwt
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

module Make (B : BACKEND) = struct
  type page_aligned_buffer = Io_page.t
  type buffer = B.buffer
  type error = [ `Disconnected | `Unimplemented | `Unknown of string ]
  type macaddr = B.macaddr
  type +'a io = 'a Lwt.t
  type id = B.id

  type stats = {
    mutable rx_bytes : int64;
    mutable rx_pkts : int32;
    mutable tx_bytes : int64;
    mutable tx_pkts : int32;
  }

  type t = {
    id : B.id;
    backend : B.t;
    mutable wake_listener : unit Lwt.u option;
    stats : stats;
  }

  let id t =
    t.id

  let connect backend = 
      match (B.register backend) with
      | `Error e -> Lwt.return (`Error e)
      | `Ok id -> 
          let stats = { rx_bytes = 0L ; rx_pkts = 0l; tx_bytes = 0L; tx_pkts = 0l } in
          let t = { id; backend; stats; wake_listener=None } in
          Lwt.return (`Ok t)

  let disconnect t =
      B.unregister t.backend t.id >>= fun () ->
      match t.wake_listener with
      | None -> Lwt.return_unit
      | Some e -> (Lwt.wakeup e ()); Lwt.return_unit

  let write t buffer =
    t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int (Cstruct.len buffer));
    t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
    B.write t.backend t.id buffer

  let writev t buffers = 
    Lwt_list.iter_s (fun f -> write t f) buffers

  let listen t fn =
    let listener t fn buf =
        t.stats.rx_bytes <- Int64.add (Int64.of_int (Cstruct.len buf)) (t.stats.rx_bytes);
        t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
        fn buf
    in 
    B.set_listen_fn t.backend t.id (listener t fn);
    let task, waker = MProf.Trace.named_task "Netif.listen" in
    t.wake_listener <- (Some waker);
    task

  let mac t =
    B.mac t.backend t.id

  let get_stats_counters t =
    t.stats

  let reset_stats_counters t = 
    t.stats.rx_bytes <- 0L;
    t.stats.rx_pkts  <- 0l;
    t.stats.tx_bytes <- 0L;
    t.stats.tx_pkts  <- 0l

end
