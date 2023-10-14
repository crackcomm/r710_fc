(* Copyright © 2022-2023 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: GPL-3.0 *)

open Core
open Async

let min_speed, max_speed = 5, 100
let max_temp = 70.
let period = 5
let interval = 5.0
let min_change, max_change = -3, 5

(* Global array. *)
let cpu_fans_speed = [| 0; 0 |]

(* Application. *)

let clamp_change = Int.clamp_exn ~min:min_change ~max:max_change

let min_max_speed temp =
  let temp = Float.(round_up temp + 1.) in
  if Float.(temp >= 80.)
  then 100, 100
  else if Float.(temp >= 75.)
  then 60, 100
  else if Float.(temp >= 70.)
  then 25, 60
  else if Float.(temp >= 60.)
  then 15, 30
  else if Float.(temp >= 40.)
  then 10, 15
  else 5, 10
;;

let initial_fan_speed temp =
  let temp = Float.(round_up temp + 1.) in
  if Float.(temp >= 80.)
  then 100
  else if Float.(temp >= 75.)
  then 60
  else if Float.(temp >= 70.)
  then 25
  else if Float.(temp >= 60.)
  then 15
  else if Float.(temp >= 40.)
  then 10
  else 5
;;

let clamp_speed = Int.clamp_exn ~min:min_speed ~max:max_speed

let clamp_speed_temp ~temp speed =
  let speed = clamp_speed speed in
  let min, max = min_max_speed temp in
  Int.clamp_exn ~min ~max speed
;;

let period_k period = Float.(2.0 / (1. + of_int period))

let calc_emas ~k ~emas temps =
  List.map2_exn emas temps ~f:(fun (_, ema) (core, temp) ->
    core, Float.((k * temp) + ((1.0 - k) * ema)))
;;

let fans_of_cpu cpu =
  match cpu with
  | 0 -> [ 0; 1 ]
  | 1 -> [ 3; 4 ]
  | _ -> failwithf "unrecognized cpu %d" cpu ()
;;

let middle_fans = [ 2 ]

let set_fans_speed fans speed =
  List.map ~f:(fun fan -> R710_fc.set ~fan speed) fans |> Deferred.all_unit
;;

let set_cpu_fans ~cpu speed =
  let current = Array.get cpu_fans_speed cpu in
  if Int.equal current speed
  then Deferred.return false
  else (
    let fans = fans_of_cpu cpu in
    set_fans_speed fans speed
    >>| fun () ->
    Array.set cpu_fans_speed cpu speed;
    true)
;;

let set_speeds speeds =
  let sum = List.fold ~init:0 ~f:(fun sum (_, speed) -> sum + speed) speeds in
  let avg = sum / List.length speeds in
  List.map ~f:(fun (cpu, speed) -> set_cpu_fans ~cpu speed) speeds
  |> Deferred.all
  >>= fun changes ->
  let changed = List.exists changes ~f:(Bool.equal true) in
  if changed then set_fans_speed middle_fans avg else Deferred.unit
;;

let update_cpu_fans changes =
  List.map
    ~f:(fun (cpu, (temp, change)) ->
      let current = Array.get cpu_fans_speed cpu in
      let speed = clamp_speed_temp ~temp (current + change) in
      cpu, speed)
    changes
  |> set_speeds
;;

let set_initial_fans_speed temps =
  List.map ~f:(fun (cpu, temp) -> cpu, initial_fan_speed temp) temps |> set_speeds
;;

let update_fans_speed ~ema temps =
  List.map2_exn
    ~f:(fun (_, ema) (cpu, temp) ->
      let ema_delta = Float.(temp - ema) in
      let delta = Float.(temp - max_temp) in
      let change =
        if Float.is_negative delta
        then Float.(ema_delta |> round |> to_int)
        else Float.(delta + ema_delta |> round |> to_int)
      in
      let change = clamp_change change in
      cpu, (temp, change))
    ema
    temps
  |> update_cpu_fans
;;

let main () =
  Os_sensors.get_max ()
  >>= fun temps ->
  (* Enable only if we got temperatures. *)
  R710_fc.enable ()
  >>= fun () ->
  set_initial_fans_speed temps
  >>= fun () ->
  let k = period_k period in
  let emas = ref temps in
  Clock.every' (Time_float.Span.of_sec interval) (fun () ->
    Os_sensors.get_max ()
    >>= fun temps ->
    let prev = !emas in
    let current = calc_emas ~k ~emas:prev temps in
    emas := current;
    update_fans_speed ~ema:current temps
    >>| fun () ->
    (* Just printing values. *)
    List.iter2_exn current temps ~f:(fun (_, ema) (core, temp) ->
      let temp = Float.round_up temp in
      let increase = Float.(temp - ema) in
      Core.Printf.printf
        "Core %d (ema%d: %.1f°C -> temp: %.1f°C = %+.1f°C) "
        core
        period
        ema
        temp
        increase);
    Core.Printf.printf "Fans: %s" (Sexp.to_string [%sexp (cpu_fans_speed : int array)]);
    Core.print_endline "";
    Out_channel.flush Out_channel.stdout);
  Deferred.never ()
;;

let install_handlers () =
  Signal.handle Signal.terminating ~f:(fun signal ->
    Core.Printf.printf "Received signal %s\n" (Signal.to_string signal);
    don't_wait_for
      (R710_fc.disable ()
       >>= fun () ->
       print_endline "Disabled manual fan control";
       exit 0))
;;

let main () =
  Log.Global.set_level `Info;
  install_handlers ();
  Monitor.protect main ~finally:(fun () -> R710_fc.disable ())
;;

let () = Thread_safe.block_on_async_exn main
