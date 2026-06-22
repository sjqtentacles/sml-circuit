(* prng.sml

   Three byte-identical PRNGs behind RANDOM: SplitMix64, Xoshiro256**, Pcg32.

   The derived helpers (real01, bool, intRange, words, shuffle) are identical
   across all three generators, so they are produced once by the RandomCore
   functor from a generator's `seed` and `next`. *)

structure W = Word64

(* Core: a generator just needs a state type, a seeder, and a stepper. *)
signature RANDOM_CORE =
sig
  type state
  val seed : Word64.word -> state
  val next : state -> Word64.word * state
end

functor RandomCore (Core : RANDOM_CORE) : RANDOM =
struct
  type state = Core.state
  type word = Word64.word

  val seed = Core.seed
  val next = Core.next

  (* Top 53 bits -> a real in [0,1). 2^53 = 9007199254740992. *)
  fun real01 s =
    let
      val (w, s') = next s
      val top53 = W.>> (w, 0w11)
      val r = Real.fromLargeInt (W.toLargeInt top53) / 9007199254740992.0
    in
      (r, s')
    end

  fun bool s =
    let val (w, s') = next s
    in (W.andb (W.>> (w, 0w63), 0w1) = 0w1, s') end

  (* Unbiased [lo, hi] via rejection sampling on a power-of-two-free range. *)
  fun intRange (lo, hi) s =
    if lo > hi then raise Domain
    else if lo = hi then (lo, s)
    else
      let
        (* span = hi - lo + 1, as Word64; safe since ints fit in 63 bits *)
        val span = W.fromInt (hi - lo) + 0w1
        (* largest multiple of span that fits; reject above it *)
        val limit = W.- (0w0, W.mod (W.- (0w0, span), span))  (* = floor(2^64/span)*span *)
        fun loop s =
          let val (w, s') = next s
          in if span <> 0w0 andalso W.> (w, W.- (limit, 0w1))
             then loop s'
             else (lo + W.toInt (W.mod (w, span)), s')
          end
      in
        loop s
      end

  fun words n s =
    let
      fun loop (0, acc, s) = (List.rev acc, s)
        | loop (k, acc, s) =
            let val (w, s') = next s in loop (k - 1, w :: acc, s') end
    in
      if n < 0 then raise Domain else loop (n, [], s)
    end

  fun shuffle xs s =
    let
      val arr = Array.fromList xs
      val n = Array.length arr
      fun swap (i, j) =
        let val tmp = Array.sub (arr, i)
        in Array.update (arr, i, Array.sub (arr, j));
           Array.update (arr, j, tmp) end
      fun loop (i, s) =
        if i <= 0 then s
        else
          let
            val (j, s') = intRange (0, i) s
          in
            swap (i, j); loop (i - 1, s')
          end
      val s' = loop (n - 1, s)
    in
      (Array.foldr (op ::) [] arr, s')
    end
end

(* ---- SplitMix64 ----
   Reference: Steele/Lea/Flood. Golden gamma 0x9E3779B97F4A7C15. *)
structure SplitMix64 :> RANDOM =
  RandomCore (
    struct
      type state = Word64.word
      fun seed w = w
      fun next z0 =
        let
          val z1 = W.+ (z0, 0wx9E3779B97F4A7C15)
          val z2 = W.* (W.xorb (z1, W.>> (z1, 0w30)), 0wxBF58476D1CE4E5B9)
          val z3 = W.* (W.xorb (z2, W.>> (z2, 0w27)), 0wx94D049BB133111EB)
          val z4 = W.xorb (z3, W.>> (z3, 0w31))
        in
          (z4, z1)
        end
    end)

(* ---- xoshiro256** ----
   Reference: Blackman & Vigna. State is four words, seeded via SplitMix64
   (the canonical seeding procedure). *)
structure Xoshiro256ss :> RANDOM =
  RandomCore (
    struct
      type state = Word64.word * Word64.word * Word64.word * Word64.word
      fun rotl (x, k) = W.orb (W.<< (x, k), W.>> (x, Word.- (0w64, k)))
      fun seed w =
        let
          (* fill four words from a SplitMix64 stream *)
          fun sm z =
            let
              val z1 = W.+ (z, 0wx9E3779B97F4A7C15)
              val a = W.* (W.xorb (z1, W.>> (z1, 0w30)), 0wxBF58476D1CE4E5B9)
              val b = W.* (W.xorb (a, W.>> (a, 0w27)), 0wx94D049BB133111EB)
              val c = W.xorb (b, W.>> (b, 0w31))
            in (c, z1) end
          val (s0, z1) = sm w
          val (s1, z2) = sm z1
          val (s2, z3) = sm z2
          val (s3, _)  = sm z3
        in (s0, s1, s2, s3) end
      fun next (s0, s1, s2, s3) =
        let
          val result = W.* (rotl (W.* (s1, 0w5), 0w7), 0w9)
          val t = W.<< (s1, 0w17)
          val s2' = W.xorb (s2, s0)
          val s3' = W.xorb (s3, s1)
          val s1' = W.xorb (s1, s2')
          val s0' = W.xorb (s0, s3')
          val s2'' = W.xorb (s2', t)
          val s3'' = rotl (s3', 0w45)
        in
          (result, (s0', s1', s2'', s3''))
        end
    end)

(* ---- pcg32 ----
   Reference: O'Neill, PCG XSH-RR 64/32. Output is 32-bit, zero-extended into
   a Word64 word so it shares the RANDOM interface. *)
structure Pcg32 :> RANDOM =
  RandomCore (
    struct
      (* state = (state, inc); inc must be odd *)
      type state = Word64.word * Word64.word
      val mult = 0wx5851F42D4C957F2D : Word64.word
      (* Canonical reference stream selector seq = 54 -> inc = (54<<1)|1. *)
      val defaultSeq = 0w54 : Word64.word
      fun incOf seq = W.orb (W.<< (seq, 0w1), 0w1)

      fun step (st, inc) = (W.+ (W.* (st, mult), inc), inc)

      fun seedSeq (seedw, seq) =
        let
          val inc = incOf seq
          (* canonical pcg32_srandom_r: state=0; step; state+=seed; step *)
          val (s1, _) = step (0w0, inc)
          val s2 = W.+ (s1, seedw)
          val (s3, _) = step (s2, inc)
        in (s3, inc) end

      fun seed seedw = seedSeq (seedw, defaultSeq)

      fun output st =
        let
          val xorshifted =
            W.andb (W.>> (W.xorb (W.>> (st, 0w18), st), 0w27), 0wxFFFFFFFF)
          val rot = W.toInt (W.>> (st, 0w59))
          val rotw = Word.fromInt rot
          val r32 =
            W.andb
              (W.orb (W.>> (xorshifted, rotw),
                      W.<< (xorshifted, Word.andb (Word.- (0w32, rotw), 0w31))),
               0wxFFFFFFFF)
        in r32 end

      fun next (st, inc) =
        let
          val out = output st
          val (st', _) = step (st, inc)
        in (out, (st', inc)) end
    end)

structure Prng =
struct
  structure SplitMix64 = SplitMix64
  structure Xoshiro256ss = Xoshiro256ss
  structure Pcg32 = Pcg32
end
