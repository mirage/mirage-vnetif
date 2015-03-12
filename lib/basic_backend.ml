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
        let send src dst fn =
            if src != dst then
                Lwt.ignore_result (fn buffer)
        in
    Hashtbl.iter (send id) t.listeners;
    Lwt.return_unit

end
