# TODO for MZ2SYNTH

## SHORT TERM

- Support Intel Fortran on Windows
- in verbose mode, put elementary clipping stats at the end of the run.
- Provide debugging output for the wavetables (each in a separate file)
- There is horrible code in the "destructor" code in MZ2Pnl.f90 - it directly deallocates the red, green and blue channel data pointers but these actually point into the FImage record's data arrays.  The deallocation should be done simply by closing the image.  Only the luminance channel data needs to be deallocated directly.

## MEDIUM TERM
- Write proper documentation (manual page + tutorial information)
- Supply template files and examples [*** DONE: 2 examples provided ***]

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
  
