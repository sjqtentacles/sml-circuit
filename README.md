# sml-circuit

[![CI](https://github.com/sjqtentacles/sml-circuit/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-circuit/actions/workflows/ci.yml)

Resilience primitives as pure state machines over a logical clock for
Standard ML: circuit breaker, retry with jitter, rate limiter, and
bulkhead. No I/O, no wall clock, no threads -- every transition is a pure
function of `(state, input) -> (state, output)`, and time is supplied as
discrete integer ticks by the caller.

Part of the `sjqtentacles` monorepo of SML libraries. It builds on
[`sml-prng`](https://github.com/sjqtentacles/sml-prng) (vendored), whose
SplitMix64 drives retry jitter, so a fixed seed reproduces the entire
backoff sequence byte-for-byte.

## Features

- **CircuitBreaker** -- `Closed | Open of int | HalfOpen` state machine.
  `step : state * outcome -> state * action` where `outcome = Success |
  Failure` and `action = Allow | Reject | Probe`. Configurable
  `failureThreshold` and `resetTimeout` (in logical ticks). An Open
  breaker whose elapsed time has passed the reset timeout transitions to
  HalfOpen and returns `Probe` for one trial call; the next outcome
  closes or re-opens the breaker. `stepAt` lets callers batch outcomes at
  absolute ticks.
- **Retry** -- exponential backoff (`baseDelayMs * 2^(attempt-1)`, capped
  at `maxDelayMs`) with uniform SplitMix64 jitter in
  `[delay * (1 - jitterFactor), delay]`. `nextDelay : policy * attempt *
  seed -> delay * seed` is pure and seeded.
- **RateLimiter** -- token bucket (`step` refills per elapsed tick, grants
  `min(requested, available)`) and sliding-window counter (evicts entries
  older than `windowTicks`, grants up to `maxRequests` per window).
- **Bulkhead** -- fixed-size concurrency slot pool. `acquire` returns
  `SOME (state, token)` or `NONE`; `release` restores the slot.

## Status

Complete and tested. The primitives are the consensus-style "pure state +
logical clock" shape; wiring them to real I/O (network calls, timers) is
the caller's job.

## Dependencies

- `sml-prng` (vendored at `lib/github.com/sjqtentacles/sml-prng/`)
- Standard ML Basis only -- no FFI, no threads.

## Portability

Pure Standard ML. Verified on **MLton** and **Poly/ML**, with identical,
deterministic output across both.

## Usage

```sml
(* CircuitBreaker: open after 3 failures, reset after 5 ticks. *)
val cfg = { failureThreshold = 3, resetTimeout = 5 }
val cb0 = Circuit.CircuitBreaker.init cfg
val (cb1, a1) = Circuit.CircuitBreaker.step (cb0, Circuit.CircuitBreaker.Success)
(* a1 = Allow; cb1 still Closed *)

(* Retry: 3 attempts, 100ms base, 1000ms cap, 50% jitter. *)
val rp = { maxAttempts = 3, baseDelayMs = 100, maxDelayMs = 1000,
           jitterFactor = 0.5 }
val (delay, seed') = Circuit.Retry.nextDelay (rp, 1, 0w42)   (* ~100ms *)

(* RateLimiter: 10 tokens, 1.0/tick refill. *)
val b = Circuit.RateLimiter.initBucket { capacity = 10, refillPerTick = 1.0 }
val (b', granted) = Circuit.RateLimiter.step (b, 0, 7)        (* granted = 7 *)

(* Bulkhead: 2 concurrent slots. *)
val bk = Circuit.Bulkhead.init 2
val (bk', tok) = valOf (Circuit.Bulkhead.acquire bk)
val bk'' = Circuit.Bulkhead.release (bk', tok)
```

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make clean
```

## Test coverage

- CircuitBreaker: Closed accumulates failures and opens at the threshold;
  Open rejects until `resetTimeout` elapses; Open -> HalfOpen returns
  `Probe`; HalfOpen + Success -> Closed; HalfOpen + Failure -> Open;
  `stepAt` jumps straight past the timeout.
- Retry: exponential sequence (100, 200, 400) with 0 jitter; cap at
  `maxDelayMs`; `exhausted` predicate; jitter stays in
  `[0, base]` for factor 1.0 across seeds; same seed -> same delay.
- RateLimiter: token-bucket refill across ticks, cap at capacity, partial
  grant when insufficient; sliding-window eviction and `maxRequests` cap.
- Bulkhead: acquire fills slots, returns `NONE` when full, release
  restores a slot.

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-circuit
smlpkg sync
```

Then reference the library basis from your own `.mlb`:

```
lib/github.com/sjqtentacles/sml-circuit/sml-circuit.mlb
```

For Poly/ML, `use` the sources listed in `sources.mlb` in order (the
vendored `sml-prng` first, then `circuit.sig` and `circuit.sml`).

## License

MIT. See [LICENSE](LICENSE).
