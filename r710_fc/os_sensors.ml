(* Copyright © 2022 Łukasz Kurowski. All rights reserved.
   SPDX-License-Identifier: GPL-3.0 *)

open Async
open Core

let parse_core_id cpu_id =
  let name, id = String.lsplit2_exn ~on:' ' cpu_id in
  if not (String.equal name "Core") then failwithf "invalid sensor core name %s" cpu_id ();
  Int.of_string id
;;

let parse_core core_name values =
  let values =
    match values with
    | `Assoc values -> values
    | _ -> failwith "invalid sensors core values output format"
  in
  let core = parse_core_id core_name in
  let values =
    List.map
      ~f:(fun (name, value) ->
        let value =
          match value with
          | `Float v -> v
          | _ -> failwith "invalid sensors core reading"
        in
        let _, typ = String.lsplit2_exn ~on:'_' name in
        let typ =
          match typ with
          | "input" -> `Input
          | "max" -> `Max
          | "crit" -> `Crit
          | "crit_alarm" -> `Crit_alarm
          | other -> failwithf "unrecognized sensors core reading: %s" other ()
        in
        typ, value)
      values
  in
  core, values
;;

let parse_core_temp core_name values =
  let core, values = parse_core core_name values in
  let _, temp =
    List.find_exn
      ~f:(fun (typ, _) ->
        match typ with
        | `Input -> true
        | _ -> false)
      values
  in
  core, temp
;;

let parse_cpu_id cpu_id =
  let split = String.split ~on:'-' cpu_id in
  let name = List.nth_exn split 0 in
  if not (String.equal name "coretemp") then failwithf "unrecognized name %s" name ();
  List.nth_exn split 2 |> Int.of_string
;;

let parse_cpu cpu_id values =
  let cpu_id = parse_cpu_id cpu_id in
  let values =
    match values with
    | `Assoc values -> values
    | _ -> failwith "invalid sensors cpu output format"
  in
  let values =
    List.filter ~f:(fun (name, _) -> not (String.equal name "Adapter")) values
    |> List.map ~f:(fun (name, values) -> parse_core_temp name values)
  in
  cpu_id, values
;;

let parse_assoc v = List.map ~f:(fun (cpu_id, values) -> parse_cpu cpu_id values) v

let parse_json_string v =
  let json = Yojson.Safe.from_string v in
  match json with
  | `Assoc v -> parse_assoc v
  | _ -> failwithf "invalid sensors json output: %s" v ()
;;

let get () = Process.run_exn ~prog:"sensors" ~args:[ "-j" ] () >>| parse_json_string

let calc_avgs readings =
  List.map
    ~f:(fun (cpu, cores) ->
      let sum = List.fold ~init:Float.zero ~f:(fun sum (_, temp) -> sum +. temp) cores in
      let avg = Float.(sum / of_int (List.length cores)) in
      cpu, avg)
    readings
;;

let get_avg () = get () >>| calc_avgs

let find_max readings =
  List.map
    ~f:(fun (cpu, cores) ->
      let temps = List.map ~f:snd cores in
      let max = List.max_elt ~compare:Float.compare temps in
      cpu, Option.value_exn max)
    readings
;;

let get_max () = get () >>| find_max

let%expect_test "cpu sensors" =
  let input =
    {|{
   "coretemp-isa-0000":{
      "Adapter": "ISA adapter",
      "Core 0":{
         "temp2_input": 25.000,
         "temp2_max": 80.000,
         "temp2_crit": 96.000,
         "temp2_crit_alarm": 0.000
      },
      "Core 1":{
         "temp3_input": 27.000,
         "temp3_max": 80.000,
         "temp3_crit": 96.000,
         "temp3_crit_alarm": 0.000
      }
   },
   "coretemp-isa-0001":{
      "Adapter": "ISA adapter",
      "Core 0":{
         "temp2_input": 26.000,
         "temp2_max": 80.000,
         "temp2_crit": 96.000,
         "temp2_crit_alarm": 0.000
      },
      "Core 1":{
         "temp3_input": 24.000,
         "temp3_max": 80.000,
         "temp3_crit": 96.000,
         "temp3_crit_alarm": 0.000
      }
   }
}|}
  in
  let temps = parse_json_string input in
  print_s [%sexp (temps : (int * (int * float) list) list)];
  [%expect {| ((0 ((0 25) (1 27))) (1 ((0 26) (1 24)))) |}];
  let avgs = calc_avgs temps in
  print_s [%sexp (avgs : (int * float) list)];
  [%expect {| ((0 26) (1 25)) |}];
  let max = find_max temps in
  print_s [%sexp (max : (int * float) list)];
  [%expect {| ((0 27) (1 26)) |}]
;;
