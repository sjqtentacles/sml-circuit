(* circuit.sig

   Resilience primitives as pure state machines over a logical clock.

   Every primitive here is a pure function of (state, input) -> (state,
   output): no I/O, no wall-clock, no threads. Time is supplied as discrete
   integer ticks by the caller, which makes traces deterministic and
   byte-identical across MLton and Poly/ML.

   The four modules are:

     - CircuitBreaker: a Closed/Open/HalfOpen state machine that rejects
       calls while Open and probes with a single trial when HalfOpen.
     - Retry: exponential backoff with SplitMix jitter (from sml-prng),
       capped at `maxDelayMs`.
     - RateLimiter: a token-bucket limiter and a sliding-window variant;
       `step` applies a batch of requested permits at a given tick.
     - Bulkhead: a fixed-size concurrency slot pool with `acquire` /
       `release` returning a token used to release the slot later. *)

signature CIRCUIT =
sig

  (* ============ CircuitBreaker ============ *)

  structure CircuitBreaker :
  sig
    datatype role = Closed | Open of int | HalfOpen
    datatype outcome = Success | Failure
    datatype action = Allow | Reject | Probe

    type config =
      { failureThreshold : int   (* consecutive failures before opening  *)
      , resetTimeout     : int   (* ticks the breaker stays Open         *)
      }

    (* Opaque state: carries the role, the consecutive-failure count, and
       the logical-clock bookkeeping needed by `step`. *)
    type state

    val init    : config -> state
    val config  : state -> config
    val role    : state -> role

    (* Pure step. Each call advances the internal logical clock by one tick.
       `Closed + Success` resets the failure count; `Closed + Failure`
       opens once the threshold is reached. An Open breaker whose elapsed
       time since opening has reached `resetTimeout` transitions to
       HalfOpen and returns Probe; otherwise it stays Open and returns
       Reject. `HalfOpen + Success` -> Closed; `HalfOpen + Failure` ->
       Open at the current tick. *)
    val step    : state * outcome -> state * action

    (* Variant that advances the clock to the supplied tick (absolute)
       before applying the outcome. Useful when callers batch outcomes at
       non-uniform ticks. *)
    val stepAt  : state * int * outcome -> state * action

    val isOpen       : state -> bool
    val isClosed     : state -> bool
    val isHalfOpen   : state -> bool
  end

  (* ============ Retry ============ *)

  structure Retry :
  sig
    type policy =
      { maxAttempts  : int      (* total attempts, including the first   *)
      , baseDelayMs  : int      (* delay for attempt 1 = baseDelayMs     *)
      , maxDelayMs   : int      (* cap; effective delay never exceeds it *)
      , jitterFactor : real     (* in [0,1]; fraction of delay to jitter *)
      }

    (* `nextDelay (policy, attempt, seed)` returns the delay (ms) before
       retrying after the given attempt (1-based) along with the updated
       PRNG seed. Attempt 0 is the initial call (delay = 0). Attempts
       beyond `maxAttempts` return 0 (caller should stop).

       The base delay is `baseDelayMs * 2^(attempt-1)` (full exponential),
       capped at `maxDelayMs`; uniform jitter in
       `[delay * (1 - jitterFactor), delay]` is then applied via
       sml-prng SplitMix64. *)
    val nextDelay : policy * int * Word64.word -> int * Word64.word

    (* True iff `attempt` is the last allowed attempt. *)
    val exhausted : policy * int -> bool
  end

  (* ============ RateLimiter ============ *)

  structure RateLimiter :
  sig
    (* Token bucket: capacity `capacity`, refilled at `refillPerTick`
       tokens per logical tick. Requests at a tick consume tokens; the
       limiter grants as many as it can (up to available) and returns the
       count. *)
    type bucket
    type bucketCfg = { capacity : int, refillPerTick : real }

    val initBucket : bucketCfg -> bucket
    (* `step (b, tick, requested)` refills the bucket for elapsed ticks
       since the last call, then grants `min(requested, available)`
       tokens. Returns (newBucket, granted). *)
    val step : bucket * int * int -> bucket * int
    val available : bucket -> real

    (* Sliding-window counter: counts requests in the last `windowTicks`
       ticks, capped at `maxRequests` per window. *)
    type window
    type windowCfg = { windowTicks : int, maxRequests : int }
    val initWindow : windowCfg -> window
    (* `step (w, tick, requested)` records `requested` at `tick` (after
       evicting entries older than `tick - windowTicks`), and grants
       `min(requested, maxRequests - countInWindow)`. Returns (newWindow,
       granted). *)
    val stepWindow : window * int * int -> window * int
    val countInWindow : window -> int
  end

  (* ============ Bulkhead ============ *)

  structure Bulkhead :
  sig
    type state
    type token = int   (* slot index, used only to release the slot *)

    val init : int -> state                (* max concurrent slots *)
    val max  : state -> int
    val active : state -> int

    (* `acquire s` returns SOME (s', token)` if a slot is free, else NONE. *)
    val acquire : state -> (state * token) option
    (* `release (s, token)` frees the slot. Token must have been acquired
       and not already released. *)
    val release : state * token -> state
  end
end
