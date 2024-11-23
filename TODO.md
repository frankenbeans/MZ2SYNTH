# TODO for MZ2SYNTH

## SHORT TERM
- MZ2SYNTH compiles on Win32 but cannot successfully allocate MZOSC wavetable arrays, and the
  program performs abominably slowly.  The compiler used was GFortran 2.10.0 (msys2-base-i686-20210705.sfx).  
  - Test properly on native machine with Win64 to compare performance with my development system
  - Provide a Makefile.gfortran.w32 which defines CONST_DWWW as 2**15, which will at least give an executable
    that generates output, however glacial the pace.
  - let input and output filenames '-' represent the standard input and output streams, IF it is possible to
    do so within F2003 without using extensions.
  - in verbose mode, put elementary clipping stats at the end of the run.

## MEDIUM TERM
- Tidy up source code
  - lots of styles, lots of header formats, etc.
- Write proper documentation (manual page + tutorial information)
- Supply template files and examples [*** DONE: 1 example ***]
- Support Intel Fortran

## LONG TERM
- Establish a FIFO for interprocess communication that will allow at least the
  rate of advance to be changed during execution to allow for limited live
  performance.
- In MZOSC, the sine-wave oscillator is profligate its use of RAM.  Unlike the other voices,
  where in principle the number of Fourier components present change through the octaves,
  the sine wave oscillator could consist of only one element that can be sampled at all
  frequencies.  This could have been done long ago, but I forgot about it...
- Also in MZOSC, look at the possibility of changing the way that wavetables are done to save space.
  This will allow small platforms (e.g., Raspberry Pi) to be supported in future.
  
  Avenues to consider:
  - Make one wavetable for every OCTAVE (nyquist = max frequency / 2) instead of for every TONE
  - Make wavetables substantially narrower and use a sensible interpolation scheme instead of a
    brute lookup.
  - Currently the mechanism for changing form RKIND=8 to RKIND=4 is broken because of the F77 code
    that is used to initialize oscillators in MZOSC.  Eliminating the F77 code and just putting those
    functions in the MZOSC module will fix the problem.
