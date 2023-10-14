(* Copyright © 2022 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: GPL-3.0 *)

(** Disable manual fan control. *)
val disable : unit -> unit Async.Deferred.t

(** Enable manual fan control. *)
val enable : unit -> unit Async.Deferred.t

(** Set fan speed percentage. *)
val set : ?fan:int -> int -> unit Async.Deferred.t
