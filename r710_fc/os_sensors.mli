(* Copyright © 2022 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: GPL-3.0 *)

(** Parses sensors command output. Returns a list of tuples with CPU ID and list of tuples
    with core ID and temperature reading. *)
val parse_json_string : string -> (int * (int * float) list) list

(** Executes sensors command and returns parsed output. *)
val get : unit -> (int * (int * float) list) list Async.Deferred.t

(** Executes sensors command and returns average CPU temperature. *)
val get_avg : unit -> (int * float) list Async.Deferred.t

(** Executes sensors command and returns maximum CPU core temperature. *)
val get_max : unit -> (int * float) list Async.Deferred.t
