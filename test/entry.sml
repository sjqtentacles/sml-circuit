fun runAllSuites () =
  ( Harness.reset ()
  ; CircuitTests.run ()
  ; Harness.run () )

fun main () =
  OS.Process.exit
    (if runAllSuites () then OS.Process.success else OS.Process.failure)
