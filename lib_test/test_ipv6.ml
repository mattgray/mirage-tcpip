module Time = Vnetif_common.Time
module B = Vnetif_backends.Basic
module V = Vnetif.Make(B)
module E = Ethif.Make(V)

module Ipv6 = Ipv6.Make(E)(Time)(Mclock)
module Udp = Udp.Make(Ipv6)(Stdlibrandom)
module Tcp_unmarshal = Tcp.Tcp_packet.Unmarshal
module Tcp_packet = Tcp.Tcp_packet
module Tcp = Tcp.Flow.Make(Ipv6)(Time)(Mclock)(Stdlibrandom)
open Lwt.Infix
open Common

let ip =
  let module M = struct
    type t = Ipaddr.V6.t
    let pp = Ipaddr.V6.pp_hum
    let equal p q = (Ipaddr.V6.compare p q) = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

type stack = {
  backend : B.t;
  netif : V.t;
  ethif : E.t;
  ip : Ipv6.t;
  udp : Udp.t;
  tcp : Tcp.t;
}

let get_stack backend address =
  let ip = [address] in
  let netmask = [Ipaddr.V6.Prefix.make 24 address] in
  let gateways = [] in
  Mclock.connect () >>= fun clock ->
  V.connect backend >>= fun netif ->
  E.connect netif >>= fun ethif ->
  Ipv6.connect ~ip ~netmask ~gateways ethif clock >>= fun ip ->
  Udp.connect ip >>= fun udp ->
  Tcp.connect ip clock >>= fun tcp ->
    Lwt.return { backend; netif; ethif; ip; udp; tcp }

let noop = fun ~src:_ ~dst:_ _ -> Lwt.return_unit

let listen ?(tcp = (fun dst -> None)) ?(udp = noop) ?(default = noop) stack =
  V.listen stack.netif
    ( E.input stack.ethif
      ~arpv4:(fun _ -> Lwt.return_unit)
      ~ipv4:(fun _ -> Lwt.return_unit)
      ~ipv6:(
        Ipv6.input stack.ip
          ~tcp:(Tcp.input stack.tcp ~listeners:tcp)
          ~udp:udp
          ~default:(fun ~proto:_ -> default))) >>= fun _ -> Lwt.return_unit

let udp_message = Cstruct.of_string "hello on UDP over IPv6"

let check_for_one_udp_packet netif ~src ~dst buf =
  Alcotest.(check ip) "sender address" (Ipaddr.V6.of_string_exn "fd75:bca1:e007:d22b::23") src;
  Alcotest.(check ip) "receiver address" (Ipaddr.V6.of_string_exn "fd75:bca1:e007:d22b::45") dst;
  (match Udp_packet.Unmarshal.of_cstruct buf with
  | Ok (_, payload) ->
    Alcotest.(check Common.cstruct) "payload is correct" udp_message payload
  | Error m -> Alcotest.fail m);
  (*after receiving 1 packet, disconnect stack so test can continue*)
  V.disconnect netif

let pass_udp_traffic () =
  let sender_address = Ipaddr.V6.of_string_exn "fd75:bca1:e007:d22b::23" in
  let receiver_address = Ipaddr.V6.of_string_exn "fd75:bca1:e007:d22b::45" in
  let backend = B.create () in
  get_stack backend sender_address >>= fun sender ->
  get_stack backend receiver_address >>= fun receiver ->
  Lwt.pick [
    listen receiver ~udp:(check_for_one_udp_packet receiver.netif);
    listen sender;
    (* Duration.of_ms 500 makes this test fail - why? *)
    Time.sleep_ns (Duration.of_ms 1000) >>= fun () ->
      Udp.write sender.udp ~dst:receiver_address ~dst_port:789 udp_message
      >|= Rresult.R.get_ok >>= fun () ->
      Time.sleep_ns (Duration.of_ms 3000) >>= fun () ->
      Alcotest.fail "UDP packet should have been received";
  ]

let tcp_message = Cstruct.of_string "hello on TCP over IPv6"

let check_for_tcp_message netif dst =
  let check_flow flow =
    Tcp.read flow >>= function
    | Error e -> Logs.debug (fun f -> f "Error reading data from established connection: %a" Tcp.pp_error e); Lwt.return_unit
    | Ok `Eof -> Logs.debug (fun f -> f "Closing connection!"); Lwt.return_unit
    | Ok (`Data data) -> Logs.debug (fun f -> f "rese"); Alcotest.(check cstruct) "tcp message" tcp_message data; Lwt.return_unit in
  match dst with
  | 789 -> Some check_flow
  | _ -> None

let pass_tcp_traffic () =
  let sender_address = Ipaddr.V6.of_string_exn "fd75:bca1:e007:d22b::67" in
  let receiver_address = Ipaddr.V6.of_string_exn "fd75:bca1:e007:d22b::89" in
  let backend = B.create () in
  get_stack backend sender_address >>= fun sender ->
  get_stack backend receiver_address >>= fun receiver ->
  Lwt.pick [
    listen receiver ~tcp:(check_for_tcp_message receiver.netif);
    listen sender;
    Time.sleep_ns (Duration.of_ms 3000) >>= fun () ->
    Logs.info (fun f -> f "Sending TCP message now");
    let conn = Tcp.create_connection sender.tcp in
    or_error "connect" conn (receiver_address, 789) >>= fun flow ->
    Logs.info (fun f -> f "Connection made, writing some data");
    Tcp.write flow tcp_message >>= function
    | Error `Closed -> Alcotest.fail "write flow was closed"
    | Error e -> Alcotest.fail "error in TCP write"
    | Ok () -> Time.sleep_ns (Duration.of_ms 10000) >>= fun () ->
    Alcotest.fail "TCP message should have been received";
  ]

let suite = [
  "Send a UDP packet from one IPV6 stack and check it is received by another", `Quick, pass_udp_traffic;
  "Send a TCP message from one IPV6 stack and check it is received by another", `Quick, pass_tcp_traffic;
]
