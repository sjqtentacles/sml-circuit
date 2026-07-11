(* demo.sml - drives the four sml-circuit resilience primitives through a
   deterministic scenario: a circuit breaker tripping and recovering, retry
   backoff delays from a fixed seed, a token-bucket rate limiter, and a
   bulkhead's acquire/release accounting. Deterministic: identical output on
   every run and both compilers. *)

structure CB = Circuit.CircuitBreaker
structure RT = Circuit.Retry
structure RL = Circuit.RateLimiter
structure BH = Circuit.Bulkhead

fun roleToString CB.Closed = "Closed"
  | roleToString (CB.Open t) = "Open(" ^ Int.toString t ^ ")"
  | roleToString CB.HalfOpen = "HalfOpen"

fun actionToString CB.Allow = "Allow"
  | actionToString CB.Reject = "Reject"
  | actionToString CB.Probe = "Probe"

fun fmtReal x =
  let val x' = if Real.== (x, 0.0) then 0.0 else x
  in Real.fmt (StringCvt.FIX (SOME 1)) x' end

val () = print "CircuitBreaker (failureThreshold=3, resetTimeout=2):\n"
val cbCfg = { failureThreshold = 3, resetTimeout = 2 }
val cbOutcomes = [CB.Failure, CB.Failure, CB.Failure, CB.Failure, CB.Failure, CB.Success]
fun runBreaker (i, s, []) = ()
  | runBreaker (i, s, outc :: rest) =
      let
        val (s', act) = CB.step (s, outc)
        val () = print ("  tick " ^ Int.toString i ^ ": role=" ^ roleToString (CB.role s')
                        ^ " action=" ^ actionToString act ^ "\n")
      in runBreaker (i + 1, s', rest) end
val () = runBreaker (1, CB.init cbCfg, cbOutcomes)

val () = print "\nRetry backoff (baseDelayMs=100, maxDelayMs=2000, jitterFactor=0.5):\n"
val rp = { maxAttempts = 4, baseDelayMs = 100, maxDelayMs = 2000, jitterFactor = 0.5 }
fun runRetry (i, seed) =
  if i > #maxAttempts rp then ()
  else
    let
      val (ms, seed') = RT.nextDelay (rp, i, seed)
      val () = print ("  attempt " ^ Int.toString i ^ ": delay = " ^ Int.toString ms ^ "ms\n")
    in runRetry (i + 1, seed') end
val () = runRetry (1, 0w12345 : Word64.word)
val () = print ("  exhausted(4)  = " ^ Bool.toString (RT.exhausted (rp, 4)) ^ "\n")

val () = print "\nRateLimiter token bucket (capacity=5, refillPerTick=1.0):\n"
val bucket0 = RL.initBucket { capacity = 5, refillPerTick = 1.0 }
val (bucket1, g1) = RL.step (bucket0, 0, 3)
val () = print ("  tick 0 request 3  -> granted " ^ Int.toString g1
                ^ ", available " ^ fmtReal (RL.available bucket1) ^ "\n")
val (bucket2, g2) = RL.step (bucket1, 2, 2)
val () = print ("  tick 2 request 2  -> granted " ^ Int.toString g2
                ^ ", available " ^ fmtReal (RL.available bucket2) ^ "\n")
val (bucket3, g3) = RL.step (bucket2, 5, 10)
val () = print ("  tick 5 request 10 -> granted " ^ Int.toString g3
                ^ ", available " ^ fmtReal (RL.available bucket3) ^ "\n")

val () = print "\nBulkhead (2 slots):\n"
val bh0 = BH.init 2
val (bh1, tokA) = valOf (BH.acquire bh0)
val (bh2, tokB) = valOf (BH.acquire bh1)
val () = print ("  acquired tokens " ^ Int.toString tokA ^ "," ^ Int.toString tokB
                ^ "; active=" ^ Int.toString (BH.active bh2) ^ "/" ^ Int.toString (BH.max bh2) ^ "\n")
val () = print ("  acquire when full -> "
                ^ (case BH.acquire bh2 of NONE => "NONE" | SOME _ => "SOME") ^ "\n")
val bh3 = BH.release (bh2, tokA)
val (bh4, tokC) = valOf (BH.acquire bh3)
val () = print ("  release " ^ Int.toString tokA ^ ", then acquire -> token " ^ Int.toString tokC
                ^ "; active=" ^ Int.toString (BH.active bh4) ^ "\n")
