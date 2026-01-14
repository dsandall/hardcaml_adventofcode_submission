open! Core
open! Core_unix
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Zero_dial = Hardcaml_advent_d1.Zero_dial
module Harness = Cyclesim_harness.Make (Zero_dial.I) (Zero_dial.O)

let ( <--. ) = Bits.( <--. )

(* Parse a single line like "L68" or "R48" *)
let parse_line line =
  let n = String.sub line ~pos:1 ~len:(String.length line - 1) |> Int.of_string in
  match line.[0] with
  | 'L' -> -n
  | 'R' -> n
  | c -> failwith (Printf.sprintf "Invalid line start: %c" c)
;;

let sample_input_values =
  let cwd = Core_unix.getcwd () in
  (* i'm not happy with this filepath either, but it works for now*)
  let filepath = Filename.concat cwd "../../../../../test/input.txt" in
  In_channel.read_lines filepath |> List.map ~f:parse_line
;;

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n =
    inputs.data_in <--. n;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter sample_input_values ~f:(fun x -> feed_input x);
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.zero_count.valid)) do
    cycle ()
  done;
  let zero_count = Bits.to_unsigned_int !(outputs.zero_count.value) in
  print_s [%message "Result" (zero_count : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
(* let waves_config = Waves_config.no_waves *)

let waves_config =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Zero_dial.hierarchical simple_testbench;
  [%expect
    {|
    (Result (zero_count 992))
    Saved waves to /tmp/test_zero_dial_ml_Simple_test__optionally_saving_waveforms_to_disk.hardcamlwaveform
    |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "range_finder*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Zero_dial.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    (Result (zero_count 992))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │                            ││────────────┬───────────────────────────────────────────────│
    │range_finder$_state         ││ 0          │1                                              │
    │                            ││────────────┴───────────────────────────────────────────────│
    │                            ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───│
    │range_finder$current_positio││ 0          │50 │79     │20     │28     │78     │51     │91 │
    │                            ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───│
    │range_finder$i$clear        ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │range_finder$i$clock        ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────│
    │range_finder$i$data_in      ││ 0          │29     │41     │8      │50     │4069   │40     │
    │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────│
    │range_finder$i$data_in_valid││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │range_finder$i$finish       ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │range_finder$i$start        ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │                            ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │range_finder$little_sum     ││ 0          │79 │108│120│61 │28 │36 │78 │128│51 │24 │91 │131│
    │                            ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │                            ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │range_finder$next_pos       ││ 0          │79 │0  │20 │0  │28 │0  │78 │0  │51 │0  │91 │0  │
    │                            ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │range_finder$o$zero_count$va││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │range_finder$o$zero_count$va││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │range_finder$zeros          ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
