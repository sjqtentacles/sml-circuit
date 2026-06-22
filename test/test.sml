(* Tests for sml-circuit.

   All transitions are pure over a logical clock and a seeded PRNG, so the
   suite is byte-identical on MLton and Poly/ML. *)

structure CircuitTests =
struct
  open Harness

  (* Friendly infix for action equality display. *)
  fun actionToStr Circuit.CircuitBreaker.Allow = "Allow"
    | actionToStr Circuit.CircuitBreaker.Reject = "Reject"
    | actionToStr Circuit.CircuitBreaker.Probe = "Probe"

  fun checkAction name (expected, actual) =
    checkBool name (true, expected = actual)

  fun run () =
    let
      open Circuit.CircuitBreaker
      val () = section "CircuitBreaker"

      val cfg = { failureThreshold = 3, resetTimeout = 5 }
      val cb0 = Circuit.CircuitBreaker.init cfg

      (* Closed + Success: stays Closed, action Allow. *)
      val (cb1, a1) = Circuit.CircuitBreaker.step (cb0, Success)
      val () = checkAction "closed+success = Allow" (Allow, a1)
      val () = checkBool "still Closed" (true, isClosed cb1)

      (* Two failures: still Closed (threshold = 3). *)
      val (cb2, a2) = Circuit.CircuitBreaker.step (cb1, Failure)
      val () = checkAction "closed+fail#1 = Allow" (Allow, a2)
      val () = checkBool "still Closed after 1 fail" (true, isClosed cb2)
      val (cb3, a3) = Circuit.CircuitBreaker.step (cb2, Failure)
      val () = checkAction "closed+fail#2 = Allow" (Allow, a3)
      val () = checkBool "still Closed after 2 fails" (true, isClosed cb3)

      (* Third failure opens the breaker. *)
      val (cb4, a4) = Circuit.CircuitBreaker.step (cb3, Failure)
      val () = checkAction "closed+fail#3 = Reject" (Reject, a4)
      val () = checkBool "now Open" (true, isOpen cb4)

      (* While Open: every outcome rejected. *)
      val (cb5, a5) = Circuit.CircuitBreaker.step (cb4, Success)
      val () = checkAction "open+success = Reject" (Reject, a5)
      val () = checkBool "still Open" (true, isOpen cb5)

      (* Advance past resetTimeout: Open -> HalfOpen, action Probe.
         cb4 opened at tick 4. resetTimeout = 5. At tick 9: 9-4 = 5 >= 5
         -> HalfOpen on the step that reaches tick 9. *)
      val (cb6, a6) = Circuit.CircuitBreaker.step (cb5, Success)
      val () = checkAction "open@6 still Reject" (Reject, a6)
      val (cb7, a7) = Circuit.CircuitBreaker.step (cb6, Success)
      val () = checkAction "open@7 still Reject" (Reject, a7)
      val (cb8, a8) = Circuit.CircuitBreaker.step (cb7, Success)
      val () = checkAction "open@8 still Reject" (Reject, a8)
      (* tick 9: 9 - 4 = 5 >= 5 -> HalfOpen + Probe. *)
      val (cb9, a9) = Circuit.CircuitBreaker.step (cb8, Success)
      val () = checkAction "open@9 -> Probe" (Probe, a9)
      val () = checkBool "now HalfOpen" (true, isHalfOpen cb9)

      (* HalfOpen + Success -> Closed. (cb9 is HalfOpen; the outcome is the
         result of the probe call.) *)
      val (cb11, a11) = Circuit.CircuitBreaker.step (cb9, Success)
      val () = checkAction "halfopen+success = Allow" (Allow, a11)
      val () = checkBool "now Closed" (true, isClosed cb11)

      (* HalfOpen + Failure -> Open. Feed a Failure to a fresh HalfOpen. *)
      val (cbH2, aH2) = Circuit.CircuitBreaker.step (cb9, Failure)
      val () = checkAction "halfopen+fail = Reject" (Reject, aH2)
      val () = checkBool "now Open again" (true, isOpen cbH2)

      (* stepAt variant: jump straight to a tick past the timeout. *)
      val cbJump = Circuit.CircuitBreaker.init cfg
      fun failStep s = #1 (Circuit.CircuitBreaker.step (s, Failure))
      val cbOpen = failStep (failStep (failStep cbJump))
      val () = checkBool "folded to Open" (true, isOpen cbOpen)
      (* Jump far ahead -> Probe on the first outcome. *)
      val (_, aJ) = Circuit.CircuitBreaker.stepAt (cbOpen, 1000, Success)
      val () = checkAction "stepAt far future = Probe" (Probe, aJ)

      val () = section "Retry"

      (* Policy: 3 attempts, base 100ms, max 1000ms, 0 jitter (deterministic). *)
      val rp = { maxAttempts = 3, baseDelayMs = 100, maxDelayMs = 1000,
                 jitterFactor = 0.0 }
      val (d0, _) = Circuit.Retry.nextDelay (rp, 0, 0w1)
      val () = checkInt "attempt 0 delay = 0" (0, d0)
      val (d1, _) = Circuit.Retry.nextDelay (rp, 1, 0w1)
      val () = checkInt "attempt 1 delay = 100" (100, d1)
      val (d2, _) = Circuit.Retry.nextDelay (rp, 2, 0w1)
      val () = checkInt "attempt 2 delay = 200" (200, d2)
      val (d3, _) = Circuit.Retry.nextDelay (rp, 3, 0w1)
      val () = checkInt "attempt 3 delay = 400" (400, d3)
      (* Beyond maxAttempts: 0. *)
      val (d4, _) = Circuit.Retry.nextDelay (rp, 4, 0w1)
      val () = checkInt "attempt 4 (exhausted) delay = 0" (0, d4)

      (* Cap: baseDelayMs * 2^(attempt-1) exceeds maxDelayMs. *)
      val rp2 = { maxAttempts = 5, baseDelayMs = 1000, maxDelayMs = 2000,
                  jitterFactor = 0.0 }
      val (c2, _) = Circuit.Retry.nextDelay (rp2, 2, 0w1)
      val () = checkInt "attempt 2 capped = 2000" (2000, c2)

      (* Exhausted predicate. *)
      val () = checkBool "exhausted at 3" (true, Circuit.Retry.exhausted (rp, 3))
      val () = checkBool "not exhausted at 2" (false, Circuit.Retry.exhausted (rp, 2))

      (* Jitter: with factor 1.0, delay is in [0, base]. For attempt 1,
         base = 100, so delay is in [0, 100]. Verify it's in range for a
         few seeds. *)
      val rpj = { maxAttempts = 3, baseDelayMs = 100, maxDelayMs = 1000,
                  jitterFactor = 1.0 }
      fun inRange (lo, hi, x) = x >= lo andalso x <= hi
      val (j1, _) = Circuit.Retry.nextDelay (rpj, 1, 0w42)
      val (j2, _) = Circuit.Retry.nextDelay (rpj, 1, 0w99)
      val (j3, _) = Circuit.Retry.nextDelay (rpj, 1, 0w12345)
      val () = checkBool "jitter attempt 1 seed 42 in [0,100]"
        (true, inRange (0, 100, j1))
      val () = checkBool "jitter attempt 1 seed 99 in [0,100]"
        (true, inRange (0, 100, j2))
      val () = checkBool "jitter attempt 1 seed 12345 in [0,100]"
        (true, inRange (0, 100, j3))

      (* Seeded determinism: same seed -> same delay. *)
      val (dA, _) = Circuit.Retry.nextDelay (rpj, 1, 0w7)
      val (dB, _) = Circuit.Retry.nextDelay (rpj, 1, 0w7)
      val () = checkInt "same seed -> same delay" (dA, dB)

      val () = section "RateLimiter (token bucket)"

      (* Capacity 10, refill 1.0/tick. *)
      val bc = { capacity = 10, refillPerTick = 1.0 }
      val b0 = Circuit.RateLimiter.initBucket bc
      val () = checkBool "initial tokens = 10" (true,
        Real.== (Circuit.RateLimiter.available b0, 10.0))

      (* Request 7 at tick 0 -> granted 7, 3 left. *)
      val (b1, g1) = Circuit.RateLimiter.step (b0, 0, 7)
      val () = checkInt "granted 7 of 7" (7, g1)
      val () = checkBool "3 tokens left" (true,
        Real.== (Circuit.RateLimiter.available b1, 3.0))

      (* Request 5 at tick 0 (no refill, same tick): only 3 available. *)
      val (b2, g2) = Circuit.RateLimiter.step (b1, 0, 5)
      val () = checkInt "granted 3 of 5 (no refill)" (3, g2)
      val () = checkBool "0 tokens left" (true,
        Real.== (Circuit.RateLimiter.available b2, 0.0))

      (* At tick 5: 5 ticks elapsed * 1.0 = 5 tokens refilled (capped at 10). *)
      val (b3, g3) = Circuit.RateLimiter.step (b2, 5, 4)
      val () = checkInt "granted 4 after refill" (4, g3)
      val () = checkBool "1 token left" (true,
        Real.== (Circuit.RateLimiter.available b3, 1.0))

      (* Big jump: caps at capacity. *)
      val (b4, g4) = Circuit.RateLimiter.step (b3, 100, 10)
      val () = checkInt "granted 10 after big jump" (10, g4)
      val () = checkBool "0 tokens left" (true,
        Real.== (Circuit.RateLimiter.available b4, 0.0))

      val () = section "RateLimiter (sliding window)"

      val wc = { windowTicks = 5, maxRequests = 10 }
      val w0 = Circuit.RateLimiter.initWindow wc
      val () = checkInt "empty window count = 0"
        (0, Circuit.RateLimiter.countInWindow w0)

      (* Request 6 at tick 0: all granted (room = 10). *)
      val (w1, g1) = Circuit.RateLimiter.stepWindow (w0, 0, 6)
      val () = checkInt "granted 6 of 6" (6, g1)
      val () = checkInt "window count = 6" (6, Circuit.RateLimiter.countInWindow w1)

      (* Request 6 more at tick 1: only 4 room. *)
      val (w2, g2) = Circuit.RateLimiter.stepWindow (w1, 1, 6)
      val () = checkInt "granted 4 of 6 (room 4)" (4, g2)
      val () = checkInt "window count = 10" (10, Circuit.RateLimiter.countInWindow w2)

      (* Request 5 at tick 2: 0 room (already at cap). *)
      val (w3, g3) = Circuit.RateLimiter.stepWindow (w2, 2, 5)
      val () = checkInt "granted 0 (at cap)" (0, g3)
      val () = checkInt "window count still 10" (10, Circuit.RateLimiter.countInWindow w3)

      (* At tick 6: events at tick 0 and 1 are evicted (cutoff = 6 - 5 = 1;
         entries with tick > 1 are kept). Events: (2,0)->evicted? No, (2,0)
         was never added (granted 0). Events list: [(2,0_kept_no), (1,4),
         (0,6)] -- but we only store non-zero grants. So events =
         [(1,4),(0,6)]. cutoff=1: keep tick>1 -> []. count=0. Request 10
         granted fully. *)
      val (w4, g4) = Circuit.RateLimiter.stepWindow (w3, 6, 10)
      val () = checkInt "after window slide, granted 10" (10, g4)
      val () = checkInt "window count = 10" (10, Circuit.RateLimiter.countInWindow w4)

      val () = section "Bulkhead"

      val bk0 = Circuit.Bulkhead.init 2
      val () = checkInt "max slots = 2" (2, Circuit.Bulkhead.max bk0)
      val () = checkInt "active = 0" (0, Circuit.Bulkhead.active bk0)

      val (bk1, t1) =
        case Circuit.Bulkhead.acquire bk0 of
            SOME st => st
          | NONE => (bk0, 0)
      val () = checkInt "active = 1" (1, Circuit.Bulkhead.active bk1)

      val (bk2, t2) =
        case Circuit.Bulkhead.acquire bk1 of
            SOME st => st
          | NONE => (bk1, 0)
      val () = checkInt "active = 2" (2, Circuit.Bulkhead.active bk2)

      (* Full: acquire returns NONE. *)
      val r3 = Circuit.Bulkhead.acquire bk2
      val () = checkBool "acquire full = NONE" (true, not (Option.isSome r3))

      (* Release t1, then acquire succeeds. *)
      val bk3 = Circuit.Bulkhead.release (bk2, t1)
      val () = checkInt "after release active = 1" (1, Circuit.Bulkhead.active bk3)
      val (bk4, _) =
        case Circuit.Bulkhead.acquire bk3 of
            SOME st => st
          | NONE => (bk3, 0)
      val () = checkInt "after re-acquire active = 2" (2, Circuit.Bulkhead.active bk4)
      val _ = t2  (* silence unused warning *)
    in
      ()
    end
end
