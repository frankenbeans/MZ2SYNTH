# TODO for MZ2SYNTH

## SHORT TERM

- MZ2SYNTH compiles on Win32 but the program performs abominably slowly.  The compiler
  used was GFortran 2.10.0 (msys2-base-i686-20210705.sfx).
- Test properly on native machine with Win64 to compare performance with my development
  system
- in verbose mode, put elementary clipping stats at the end of the run.

## MEDIUM TERM
- Tidy up source code
  - lots of styles, lots of header formats, etc.
- Improve byte-swapping routines in mzaufile.f95 - arguably they should be optimized for
  use with a little-endian machine since a PC is probably the likeliest platform.
- Write proper documentation (manual page + tutorial information)
- Supply template files and examples [*** DONE: 2 examples provided ***]
- Support Intel Fortran

## LONG TERM

- Establish a FIFO for interprocess communication that will allow at least the
  rate of advance to be changed during execution to allow for limited live
  performance.  ALTERNATIVELY, a means of putting column-number based triggers in
  command-line options with one-shot or retriggerable behaviour.

- [*** DONE, but test on RPI is still to be done ***]
  Linear interpolation within wavetables has been implemented, and this works well
  enough on a PC, but clips in which many oscillators are active at any given moment can
  lead to latency when generating audio in real-time.  Need to find ways to improve
  performance further in order to support small platforms like Raspberry Pi.
  
- Support GPU computation (LOW priority)
