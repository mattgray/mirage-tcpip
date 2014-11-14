(*
 * Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS l SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Make (E : V2_LWT.ETHIF) (T : V2_LWT.TIME) (C : V2.CLOCK) = struct
  type ethif    = E.t
  type 'a io    = 'a Lwt.t
  type buffer   = Cstruct.t
  type ipaddr   = Ipaddr.V6.t
  type callback = src:ipaddr -> dst:ipaddr -> buffer -> unit Lwt.t

  type t =
    { ethif : E.t;
      mutable state : Ipv6_core.state;
      mutable nc : Ipv6_core.nb_info Ipv6_core.IpMap.t }

  type error =
    [ `Unimplemented
    | `Unknown of string ]

  let id { ethif } = ethif

  let rec tick state =
    Printf.printf "Ticking...\n%!";
    run state @@ Ipv6_core.tick ~st:state.state ~nc:state.nc ~now:(Ipv6_core.Time.of_float @@ C.time ())

  and run state (st, nc, acts) =
    state.state <- st;
    state.nc <- nc;
    Lwt_list.iter_s begin function
      | Ipv6_core.SetTimer dt ->
        let dt = Ipv6_core.Time.Span.to_float dt in
        Printf.printf "Setting up a timer in %.1fs\n%!" dt;
        Lwt.ignore_result (T.sleep @@ dt >>= fun () -> tick state);
        Lwt.return_unit
      | Ipv6_core.SendPacket pkt ->
        E.writev state.ethif pkt
    end acts

  let writev state ~dst ~proto datav =
    let now = Ipv6_core.Time.of_float @@ C.time () in
    let src = Ipv6_core.select_source_address state.state in
    let nc, acts = Ipv6_core.output ~now ~st:state.state ~nc:state.nc ~src ~dst ~proto datav in
    run state (state.state, nc, acts)

  let input ~tcp ~udp ~default state buf =
    let r, pkts_timers =
      Ipv6_core.handle_packet ~now:(Ipv6_core.Time.of_float @@ C.time ()) ~st:state.state ~nc:state.nc buf
    in
    run state pkts_timers >>= fun () ->
    match r with
    | `None -> Lwt.return_unit
    | `Tcp (src, dst, pkt) -> tcp ~src ~dst pkt
    | `Udp (src, dst, pkt) -> udp ~src ~dst pkt
    | `Default (proto, src, dst, pkt) -> default ~proto ~src ~dst pkt

  let connect ethif =
    let state = {state = Ipv6_core.create (E.mac ethif); nc = Ipv6_core.IpMap.empty; ethif} in
    T.sleep 10.0 >>= fun () ->
    Printf.printf "Starting\n%!";
    run state @@
    Ipv6_core.add_ip ~now:(Ipv6_core.Time.of_float @@ C.time ()) ~st:state.state ~nc:state.nc
      (Ipv6_core.Macaddr.link_local_addr (E.mac ethif)) >>= fun () ->
    Lwt.return (`Ok state)

  let disconnect _ = (* TODO *)
    Lwt.return_unit

  let add_ipv6 state ip =
    run state @@ Ipv6_core.add_ip ~now:(Ipv6_core.Time.of_float @@ C.time ()) ~st:state.state ~nc:state.nc ip

  let get_ipv6 state =
    let rec loop = function
      | [] -> []
      | (_, Ipv6_core.TENTATIVE _) :: rest -> loop rest
      | (ip, (Ipv6_core.PREFERRED _ | Ipv6_core.DEPRECATED _)) :: rest -> ip :: loop rest
    in
    loop state.state.Ipv6_core.my_ips

  let checksum = Ipv6_core.checksum

  let get_source state ~dst =
    Ipv6_core.select_source_address state.state (* FIXME dst *)
end
