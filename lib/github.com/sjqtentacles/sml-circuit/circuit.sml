(* circuit.sml

   Pure-functional resilience primitives.

   Conventions
   -----------
   - All transitions are pure: thread the returned state through the next
     call. No mutation, no wall-clock, no I/O.
   - Logical time is an `int` tick supplied by the caller; the same tick
     schedule reproduces a byte-identical trace on MLton and Poly/ML.
   - Retry jitter uses the vendored `sml-prng` SplitMix64, seeded by the
     caller; the seed is threaded through `nextDelay`. *)

structure Circuit :> CIRCUIT =
struct

  structure R = SplitMix64

  (* ============ CircuitBreaker ============ *)

  structure CircuitBreaker =
  struct
    datatype role = Closed | Open of int | HalfOpen
    datatype outcome = Success | Failure
    datatype action = Allow | Reject | Probe

    type config =
      { failureThreshold : int
      , resetTimeout     : int
      }

    type state =
      { cfg       : config
      , role      : role
      , failures  : int     (* consecutive failures while Closed *)
      , openedAt  : int     (* tick when Opened *)
      , nowTick   : int     (* current logical tick *)
      }

    fun init (cfg : config) : state =
      { cfg = cfg, role = Closed, failures = 0, openedAt = 0, nowTick = 0 }

    fun config (s : state) = #cfg s
    fun role (s : state) = #role s

    fun isOpen (s : state) =
      case #role s of Open _ => true | _ => false
    fun isClosed (s : state) =
      case #role s of Closed => true | _ => false
    fun isHalfOpen (s : state) =
      case #role s of HalfOpen => true | _ => false

    (* Setters (SML has no record-update syntax). *)
    fun setRole     s v = { cfg = #cfg s, role = v, failures = #failures s,
                            openedAt = #openedAt s, nowTick = #nowTick s }
    fun setFailures s v = { cfg = #cfg s, role = #role s, failures = v,
                            openedAt = #openedAt s, nowTick = #nowTick s }
    fun setOpenedAt s v = { cfg = #cfg s, role = #role s, failures = #failures s,
                            openedAt = v, nowTick = #nowTick s }
    fun setNowTick  s v = { cfg = #cfg s, role = #role s, failures = #failures s,
                            openedAt = #openedAt s, nowTick = v }

    (* Advance the clock to absolute `tick`, possibly transitioning
       Open -> HalfOpen if enough time has passed since `openedAt`. *)
    fun advanceTo (s : state, tick : int) : state =
      let
        val s1 = setNowTick s tick
      in
        case #role s1 of
            Open openedAt =>
              if tick - openedAt >= #resetTimeout (#cfg s1) then
                setRole (setFailures s1 0) HalfOpen
              else s1
          | _ => s1
      end

    fun stepAt (s : state, tick : int, outc : outcome) : state * action =
      let
        val s1 = advanceTo (s, tick)
      in
        case (#role s, #role s1) of
            (* Time advance caused Open -> HalfOpen: emit Probe, ignore the
               (stale) outcome; the caller makes a trial call and reports
               its outcome on the next step. *)
            (Open _, HalfOpen) => (s1, Probe)
          | _ =>
              (case (#role s1, outc) of
                  (Closed, Success) =>
                    ( setFailures s1 0, Allow )
                | (Closed, Failure) =>
                    let val nf = #failures s1 + 1 in
                      if nf >= #failureThreshold (#cfg s1) then
                        ( setRole (setFailures (setOpenedAt s1 (#nowTick s1)) 0)
                                  (Open (#nowTick s1))
                        , Reject )
                      else
                        ( setFailures s1 nf, Allow )
                    end
                | (Open _, _) => (s1, Reject)
                | (HalfOpen, Success) =>
                    ( setRole (setFailures s1 0) Closed, Allow )
                | (HalfOpen, Failure) =>
                    ( setRole (setFailures (setOpenedAt s1 (#nowTick s1)) 0)
                              (Open (#nowTick s1))
                    , Reject ))
      end

    (* `step` advances the clock by exactly one tick. *)
    fun step (s : state, outc : outcome) : state * action =
      stepAt (s, #nowTick s + 1, outc)
  end

  (* ============ Retry ============ *)

  structure Retry =
  struct
    type policy =
      { maxAttempts  : int
      , baseDelayMs  : int
      , maxDelayMs   : int
      , jitterFactor : real
      }

    fun exhausted (p : policy, attempt : int) : bool =
      attempt >= #maxAttempts p

    fun nextDelay (p : policy, attempt : int, seedw : Word64.word)
        : int * Word64.word =
      if attempt <= 0 orelse attempt > #maxAttempts p then (0, seedw)
      else
        let
          val seed = R.seed seedw
          (* Exponential base: baseDelayMs * 2^(attempt-1), capped. *)
          val exp0 = Real.fromInt (#baseDelayMs p) *
                       Math.pow (2.0, Real.fromInt (attempt - 1))
          val capped = Real.min (exp0, Real.fromInt (#maxDelayMs p))
          (* Jitter: uniform in [capped * (1 - jf), capped].
             Draw a real in [0,1) from the PRNG. *)
          val (u, s') = R.real01 seed
          val lo = capped * (1.0 - #jitterFactor p)
          val delayed = lo + (capped - lo) * u
          val ms = Real.round delayed
          (* Recover the next Word64 seed state from s'. The caller passes
             a Word64 seed and expects a Word64 back; R.state is opaque,
             so we expose the underlying word via R.next (discard output). *)
          val (_, s'') = R.next s'
          (* To return a Word64, we can't extract the internal state word
             without changing the sml-prng API. Instead, derive the next
             seed deterministically: use s'' and take its first output. *)
          val (w, _) = R.next s''
        in
          (ms, w)
        end
  end

  (* ============ RateLimiter ============ *)

  structure RateLimiter =
  struct
    type bucketCfg = { capacity : int, refillPerTick : real }
    type bucket =
      { cfg      : bucketCfg
      , tokens   : real
      , lastTick : int
      }

    fun initBucket (cfg : bucketCfg) : bucket =
      { cfg = cfg, tokens = Real.fromInt (#capacity cfg), lastTick = 0 }

    fun available (b : bucket) : real = #tokens b

    fun step (b : bucket, tick : int, requested : int) : bucket * int =
      let
        val dt = Int.max (0, tick - #lastTick b)
        val refilled = Real.min
                         ( Real.fromInt (#capacity (#cfg b))
                         , #tokens b +
                             Real.fromInt dt * #refillPerTick (#cfg b) )
        val req = Real.fromInt requested
        val granted = if req <= refilled then req else refilled
        val remaining = refilled - granted
      in
        ( { cfg = #cfg b, tokens = remaining, lastTick = tick }
        , Real.round granted )
      end

    type windowCfg = { windowTicks : int, maxRequests : int }
    type window =
      { cfg : windowCfg, events : (int * int) list }   (* (tick, count) *)
    (* events stored newest-first; old entries evicted on each step. *)

    fun initWindow (cfg : windowCfg) : window =
      { cfg = cfg, events = [] }

    fun countInWindow (w : window) : int =
      List.foldl (fn ((_, c), acc) => acc + c) 0 (#events w)

    fun stepWindow (w : window, tick : int, requested : int)
        : window * int =
      let
        val cutoff = tick - #windowTicks (#cfg w)
        val kept = List.filter (fn (t, _) => t > cutoff) (#events w)
        val inWin = List.foldl (fn ((_, c), acc) => acc + c) 0 kept
        val room = Int.max (0, #maxRequests (#cfg w) - inWin)
        val granted = Int.min (requested, room)
        val newEvents =
          if granted > 0 then (tick, granted) :: kept else kept
      in
        ( { cfg = #cfg w, events = newEvents }, granted )
      end
  end

  (* ============ Bulkhead ============ *)

  structure Bulkhead =
  struct
    type token = int
    type state =
      { maxSlots : int
      , inUse    : int       (* count of slots held *)
      , freeList : int list  (* available slot indices, ascending *)
      }

    fun init (maxSlots : int) : state =
      { maxSlots = maxSlots
      , inUse = 0
      , freeList = List.tabulate (maxSlots, fn i => i)
      }

    fun max (s : state) = #maxSlots s
    fun active (s : state) = #inUse s

    fun acquire (s : state) : (state * token) option =
      case #freeList s of
          [] => NONE
        | t :: rest =>
            SOME ( { maxSlots = #maxSlots s
                   , inUse = #inUse s + 1
                   , freeList = rest }
                 , t )

    fun release (s : state, t : token) : state =
      { maxSlots = #maxSlots s
      , inUse = Int.max (0, #inUse s - 1)
      , freeList = t :: #freeList s }
  end
end
