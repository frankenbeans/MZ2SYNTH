# TODO for MZ2SYNTH

## SHORT TERM
- Audit code and do a Big Tidy.
- Improve makefiles - don't really need four...

## MEDIUM TERM
- Write proper documentation (manual page, technical report + tutorial information)

## LONG TERM
- Support Intel Fortran on Windows [DEBATING WITH SELF: dump IFX in favour of LFortran?]
- Establish a FIFO for interprocess communication that will allow at least the
  rate of advance to be changed during execution to allow for limited live
  performance.  ALTERNATIVELY, a means of putting column-number based triggers
  in command-line options with one-shot or retriggerable behaviour.
