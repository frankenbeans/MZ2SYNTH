# TODO for MZ2SYNTH

## SHORT TERM
- Update README to reflect the new support for
  double precision output format in addition to
  single.
- NB NB NB: There is stupidity in the way that the oscillator accumulator
  indices are calculated during linear interpolation.  The way to fix this:
  (a) initialize oscillator accumulators to 1.0_RKIND or random values between
      1 and N_TIC_PER_CYCLE.  At the moment, the things can be be 0, which
      is the main source of the stupidity.
  (b) In the update procedure, there is code using INT(...) + 1 with a MIN
      function - that should be changed to NINT(...)
  Check carefully - currently the code is SAFE but potentially not accurate,
  but it needs to be both.  I can't do this now - too many other commitments,
  but it should be the top priority for the next improvement.
- In verbose mode, put elementary clipping stats at the end of the run.
- Lanczos approximation is not quite right as written.  Should use
  sinc(x)=(sin x)/x and GPCOEF last line should be GPCOEF=SINC(PI*Q).
  This is just cosmetic as the expressions come to the same thing
  after substitution.

## MEDIUM TERM
- Write proper documentation (manual page + tutorial information)

## LONG TERM
- Support Intel Fortran on Windows
- Establish a FIFO for interprocess communication that will allow at least the
  rate of advance to be changed during execution to allow for limited live
  performance.  ALTERNATIVELY, a means of putting column-number based triggers
  in command-line options with one-shot or retriggerable behaviour.
