# TODO for MZ2SYNTH

## IMMEDIATE
- [*** DONE ***] Test repository contents to see that it is complete and builds without warnings.
  - NB:  original local copy already builds with no warnings
- [*** DONE ***] Write basic usage instructions
- Make public, warts and all

## MEDIUM TERM
- Tidy up source code
  - lots of styles, lots of header formats, etc.
  - Fortran 2008 required to build, see if can modify to allow Fortran 2003 only
- Write proper documentation (manual page + tutorial information)
- Supply template files and examples

## LONG TERM
- [*** DONE ***] Re-imagine the way in which output is created currently.  The main problem is
  that output files are created in one go at the end of execuation so that live
  performance is impossible.  The problems are going to be in
  - totally changing the file-creation mechanism in the MZAUFILE module
  - finding a smarter way to use functions in MZNORM for soft clipping to prevent
    clipping in output
- [*** DONE *** ] Look at the mechanisms in mzpnl - these are a bodge at the current time and
  are probably much more processor intensive than necessary.
- Establish a FIFO for interprocess communication that will allow at least the
  rate of advance to be changed during execution to allow for limited live
  performance.
- In MZOSC, the sine-wave oscillator is profligate its use of RAM.  Unlike the other voices,
  where in principle the number of Fourier components present change through the octaves,
  the sine wave oscillator could consist of only one element that can be sampled at all
  frequencies.  This could have been done long ago, but I forgot about it...
