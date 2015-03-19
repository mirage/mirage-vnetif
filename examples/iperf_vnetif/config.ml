open Mirage

let main = foreign "Iperf_vnetif.Main" (console @-> job)
let tracing = mprof_trace ~size:1000000 ()
let platform =
    match get_mode () with
        | `Xen -> "xen"
        | _ -> "unix"

let () =
    add_to_ocamlfind_libraries [ 
        "mirage-vnetif" ; 
        "mirage-net-" ^ platform ; 
        "mirage-" ^ platform; 
        "mirage-clock-" ^ platform;
        "tcpip.stack-direct" ; 
        "mirage-types" ];
  register "unikernel" ~tracing [
    main $ default_console
  ]
