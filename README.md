# README for MZ2SYNTH

Copyright (c) 2025 by E. Lamprecht.  All rights reserved.  See
LICENSE.md for licensing terms and TODO.md for notes on the current
development status.

# MANUAL PAGE

## NAME

> mz2 [option]... <input_file>

## DESCRIPTION

MZ2SYNTH is a wavetable synthesizer inspired by the ANS synthesizer built
by Yevgeny Murzin in various versions up to 1958, and currently housed
in the Glinka State Central Museum of Musical Culture in Moscow
(https://en.wikipedia.org/wiki/ANS_synthesizer).

MZ2SYNTH models 720 oscillators spanning 10 octaves, with oscillator
no. 294 set up with a fundamental frequency of 440 Hz and all others
calibrated to this using equal temperament.  Each oscillator has four
channels, these generating sine, square, sawtooth and triangle waves.
The wave-table is initialized in such a manner that, at the desired
audio sampling frequency, no aliasing will occur.  In practice, this
also means that oscillators with fundamental frequencies above the
Nyquist frequency corresponding to the desired audio sampling
frequency would be silent.

Input takes the form of a PPM graphics file with 720 rows and as many
columns as desired.  MZ2SYNTH begins at the leftmost column and
advances, column by column at the desired rate.  By default, the
amplitude of the sine, square, sawtooth and triangle wave channels is
multiplied by the red, green, blue and luminance channel values at
each instant within the run, but the specific channels can be linked
as desired.  A simple bumpless transfer mechanism is implemented so
that oscillator channels are rapidly but smoothly turned on or off.

Each of the sine, square, sawtooth or triangle waves can be linked to
the (r)ed, (g)reen, (b)lue, (l)uminance values of the graphic file
pixels, or else (m)uted.  See option -c below for syntax.

Output takes the form of a stereo 32-bit floating point PCM Sun audio
file.

The following options are available (case insensitive):
|Short form| Long form                | Description                                     |
|----------|--------------------------|-------------------------------------------------|
|-h        | -help                    | Show help screen and exit                       |
|-v        | -verbose                 | Toggle verbose textual output mode              |
|-d        | -debug                   | Toggle debugging output mode                    |
|-p        | -dynamic-compression     | Toggle dynamic compression on                   |
|-w        | -overwrite               | Toggle audio file overwrite mode                |
|-x        | -fixed-phase             | Toggle on fixed-phase mode (slower, accurate)   |
|-z        | -zero-phase              | Toggle on zero-phase mode (changes beat freq.s) |
|-a cps    | -advance cps             | Set advance rate in columns per second          |
|-c XXXX   | -channel selct XXXX      | X in (R,G,B,L,M)                                |
|-m mul    | -volume-multiplier mul   | Multiply raw volume by factor mul \>  0         |
|-o ofn    | -output-file ofn         | Set output file name to ofn                     |
|-r ftr    | -transition ftr          | Set transition TC (as fraction of <s>) to ftr   |
|-s spr    | -sampling-rate spr       | Set sampling-rate in c.p.s                      |

## NOTE

### DEFAULTS

- Input file: input.ppm
- Output file: output.au
- Channels:  sine:square:sawtooth:triangle = R:G:B:L
- Volume multiplier:  1.0
- Transition fraction: 0.333 (also the maximum)
- Sampling rate: 44100 c.p.s.
- Fixed phase:  off
- Zero phase:   off (i.e., phases are randomized at start by default)

### USAGE EXAMPLES

- Generate file **track01.au** from input **track01.ppm**, advancing
  at 12 columns per second, at a sampling rate of 48000 c.p.s.  Enable dynamic
  compression, overwriting of output if it exists and verbose textual
  output.

  ``mz2 -verbose -overwrite -output-file track01.au
        -dynamic-compression -sampling-rate 48000
        track01.ppm``

- As above, but play real-time through the SOX *play* program with
  some effects, via a named pipe called *fifo*

  - In one terminal enter:
    
  ``mkfifo fifo``

  ``play --buffer 16384 fifo reverb deemph``

  - In a separate terminal enter:
    
  ``mz2 -verbose -overwrite -output-file fifo -dynamic-compression
   -sampling-rate 48000 track01.ppm``

  The *play* process will block until mz2 starts generating output.
  
  **WARNING:** This synthesizer can generate very loud and very high-pitched
  sounds.  Playing unknown inputs through powerful speakers or earphones
  can result in pain or hearing loss, so set your volumes low to start with,
  and beware of using volume multipliers larger than 0.01 or so until you
  become familiar with the audio that your input files generate.
