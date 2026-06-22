(* prng.sig

   Seedable, deterministic, byte-identical pseudo-random number generators.

   All generators are pure: `next` maps a state to an output word and the
   successor state, so streams are reproducible and referentially transparent.
   All arithmetic is on `Word64.word` (masked), giving identical streams on
   MLton and Poly/ML.

   `splitmix64`, `xoshiro256**`, and `pcg32` reproduce their published
   reference output vectors exactly. *)

signature RANDOM =
sig
  type state
  type word = Word64.word

  (* Seed a generator from a single 64-bit value. *)
  val seed : Word64.word -> state

  (* Core step: an output word and the next state. *)
  val next : state -> word * state

  (* Derived helpers (all pure: thread the returned state). *)

  (* A real in [0, 1) using the top 53 bits. *)
  val real01 : state -> real * state

  (* A boolean (top bit of the output word). *)
  val bool : state -> bool * state

  (* An int uniformly in [lo, hi] (inclusive). Requires lo <= hi.
     Uses rejection sampling so the distribution is unbiased. *)
  val intRange : int * int -> state -> int * state

  (* The first n output words as a list (and the resulting state). *)
  val words : int -> state -> word list * state

  (* Fisher-Yates shuffle of a list; a permutation of the input. *)
  val shuffle : 'a list -> state -> 'a list * state
end
