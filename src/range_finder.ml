(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal

let num_bits = 16

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits num_bits]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      zero_count : 'a With_valid.t [@bits num_bits]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    (* Note that the state machine defaults to initializing to the first state *)
    State_machine.create (module States) spec
  in
  (*expose & name the state for waveforms*)
  let%hw _state = sm.current in
  let%hw_var current_position = Variable.reg spec ~width:num_bits in
  (* We don't need to name the range here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
  let%hw_var little_sum = Variable.wire ~default:(zero num_bits) () in
  compile [ little_sum <-- current_position.value +: data_in ];
  let%hw_var next_pos = Variable.wire ~default:(zero num_bits) () in
  compile
    [ when_
        data_in_valid
        [ when_
            (little_sum.value <+. 0)
            [ next_pos <-- little_sum.value +: of_int_trunc ~width:num_bits 100 ]
        ; when_
            (little_sum.value >+. 99)
            [ next_pos <-- little_sum.value -: of_int_trunc ~width:num_bits 100 ]
        ; when_
            (little_sum.value >=+. 0 &: (little_sum.value <=+. 99))
            [ next_pos <-- little_sum.value ]
        ]
    ];
  let%hw_var zeros = Variable.reg spec ~width:num_bits in
  let zeros_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start (*if start, init vals*)
                [ current_position <-- of_int_trunc ~width:num_bits 50
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ current_position <-- next_pos.value
                ; when_
                    (next_pos.value ==: of_int_trunc ~width:num_bits 0)
                    [ zeros <-- zeros.value +:. 1 ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ zeros_valid <-- vdd; when_ finish [ sm.set_next Accepting_inputs ] ]
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { zero_count = { value = zeros.value; valid = zeros_valid.value } }
;;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"range_finder" create
;;
