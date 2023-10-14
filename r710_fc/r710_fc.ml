(* Copyright © 2022 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: GPL-3.0 *)

open Async
open Core

let hex_of_int = Printf.sprintf "0x%.2x"

let cmd ?(fan = 255) percentage =
  let percentage = Int.min percentage 100 in
  [ "raw"; "0x30"; "0x30"; "0x02"; hex_of_int fan; hex_of_int percentage ]
;;

let run args =
  Process.run_exn ~prog:"ipmitool" ~args ()
  >>| fun v ->
  assert (String.equal v "\n");
  ()
;;

let enable () = run [ "raw"; "0x30"; "0x30"; "0x01"; "0x00" ]
let disable () = run [ "raw"; "0x30"; "0x30"; "0x01"; "0x01" ]

let set ?fan percentage =
  [%log.global.debug "setting fan speed" (fan : int option) (percentage : int)];
  run (cmd ?fan percentage)
;;
