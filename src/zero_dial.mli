(** An example design that tracks position based on input thresholds and counts zero
    crossings. *)

open! Core
open! Hardcaml

val num_bits : int

(*_ The module interface exports the same I/O records. Note that the widths don't need to
    be specified in the interface. *)
module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { zero_count : 'a With_valid.t } [@@deriving hardcaml]
end

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
