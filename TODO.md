# TODO for MZ2SYNTH

## SHORT TERM
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
