# README for MZ2SYNTH

Copyright (c) 2025 by E. Lamprecht.  All rights reserved.  See
LICENSE.md for licensing terms and TODO.md for notes on the current
development status.

# MANUAL PAGE

## NAME

> mz2 [option]... <input_file>

## DESCRIPTION

MZ2SYNTH is a wavetable synthesizer inspired by the ANS synthesizer
built by Yevgeny Murzin in various versions up to 1958, and currently
housed in the Glinka State Central Museum of Musical Culture in Moscow
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
advances, column by column at the desired rate. Pitch goes from bass
to treble from the bottom row of the image (as it would appear on
screen) to the top. By default, the amplitude of the sine, square,
sawtooth and triangle wave channels is multiplied by the red, green,
blue and luminance channel values at each instant within the run, but
the specific channels can be linked as desired.  A simple bumpless
transfer mechanism is implemented so that oscillator channels are
rapidly but smoothly turned on or off.

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
|-r ftr    | -transition ftr          | Set transition TC (as frac. of (spr*1s)) to ftr |
|-s spr    | -sampling-rate spr       | Set sampling-rate in c.p.s                      |

## NOTE

### DEFAULTS

|Parameter               |Default                                                       |
|------------------------|--------------------------------------------------------------|
|Input file name         |input.ppm                                                     |
|Output file name        |output.au                                                     |
|Advance rate            |10 columns per second                                         |
|Channels                |sine:square:sawtooth:triangle = R:G:B:L ( as for -c RGBL)     |
|Volume multiplier       |0.1                                                           |
|Transition fraction     |0.01 (of the number of samples in one second)                 |
|Sampling rate           |44100 c.p.s.                                                  |
|Fixed phase mode        |off (i.e., phase relationships between oscillators not fixed) |
|Zero phase initially    |off (i.e., phases are randomized at start by default)         |

### USAGE EXAMPLES

- Generate file **track01.au** from input **track01.ppm**, advancing
  at 12 columns per second, at a sampling rate of 48000 c.p.s.  Enable dynamic
  compression, overwriting of output if it exists and verbose textual
  output.

  >``mz2 -verbose -overwrite -advance 12 -output-file track01.au
        -dynamic-compression -sampling-rate 48000
        track01.ppm``

- As above, but play real-time through the SOX *play* program with
  some effects, via a named pipe called *fifo*

  - In one terminal enter:
    
  >``mkfifo fifo``

  >``play --buffer 16384 fifo reverb deemph``

  - In a separate terminal enter:
    
  >``mz2 -verbose -overwrite -advance 12 -output-file fifo
        -dynamic-compression -sampling-rate 48000 track01.ppm``

  The *play* process will block until mz2 starts generating output.
  
  **WARNING:** This synthesizer can generate very loud and very high-pitched
  sounds.  Playing unknown inputs through powerful speakers or earphones
  can result in pain or hearing loss, so set your volumes low to start with,
  and beware of using volume multipliers larger than 0.01 or so until you
  become familiar with the audio that your input files generate.

  **NOTE:** Intel Fortran (ifx (IFX) 2025.0.4 20241205) executables cannot
  write to named pipes as noted above, for reasons currently not known.
  There is a work-around:
  
  - In a terminal, start mz2 as explained in the second step  above, but
    writing to an ordinary disk file instead of a FIFO
    
   >``mz2 -verbose -overwrite -advance 12 -output-file output.au
    -dynamic-compression -sampling-rate 48000 track01.ppm``

  - After file generation commences, enter the following command in a separate
    terminal to play the file in real-time:

  >``cat output.au | play - reverb deemph``

# INSTALLATION GUIDE

MZ2SYNTH is written in standard Fortran 2003.  It should therefore
work on any platform for which a recent version of GCC exists.  I
shall assume that you have GCC installed on your system.

For simplicity I shall assume that we are working in our home directory.

(1)  Let us first clone the Github repository:

`git clone https://github.com/frankenbeans/MZ2SYNTH.git`

(2)  Next, we will change directory to the **MZSYNTH/SOURCE** subdirectory
     and build our executable using **make**:

`cd MZ2SYNTH/SOURCE/`
`make -f Makefile.gfortran all clean`

(3) Now that our executable is compiled, we will go to the
**MZSYNTH/EXAMPLES** subdirectory from where we are now, and run the
executable from there without arguments just to check that it works.

`cd ../EXAMPLES`
`../SOURCE/mz2`

We should see a help screen like this:

> MZ2SYNTH 0.1/2025-02-16
> Copyright (C) by E. Lamprecht.   All rights reserved.
> 
> SYNOPSIS:  mz2 [options...] [input_filename]
> 
> OPTIONS:
>   -h|-help                     Print help-screen and halt processing    [off]
>   -v|-verbose                  Toggle verbose text output mode          [off]
>   -w|-overwrite                Toggle overwrite output mode             [off]
>   -d|-debug                    Toggle debugging output mode             [off]
> ...
>

(4) We will now produce an audio file called **example-02.au** from
the graphic image file **example-02.ppm**, advancing at 4 columns per
second with fixed phase relationships and an audio sampling rate of
48000 c.p.s, with verbose text output mode on (as a sort of progress
indicator).  We want to link the sine wave voice to the luminance
channel of the input image and mute the other voices.

`../SOURCE/mz2 -verbose -advance 4 -fixed-phase -sampling-rate 48000
-output-file example-02.au -channel-select LMMM example-02.ppm`

**NOTE** that the command above runs on continuously without a line-break.

We will see output that looks like this:
```
*INF (Mz2Syn_CmdLine): -VERBOSE : VERBOSE MODE ON
*INF (Mz2Syn_CmdLine): -ADVANCE : SET ADVANCE RATE
*INF (Mz2Syn_CmdLine): ADVANCE RATE =   4.00000     COL/S
*INF (Mz2Syn_CmdLine): -FIXED-PHASE : FIXED PHASE MODE ON
*INF (Mz2Syn_CmdLine): -SAMPLING-RATE : SET SAMPLING RATE
*INF (Mz2Syn_CmdLine): SAMPLING RATE =         48000
*INF (Mz2Syn_CmdLine): -OUTPUT-FILE : SET OUTPUT FILE
*INF (Mz2Syn_CmdLine): OUTPUT FILE IS example-02.au
*INF (Mz2Syn_CmdLine): -CHANNEL-SELECT : SET CHANNEL DEFINITIONS
*INF (Mz2Syn_CmdLine): CHANNEL SELECTOR = LMMM
*INF (Mz2Syn_CmdLine): INPUT FILE IS example-02.ppm
*INF (OscBank_Init): Initializing oscillator bank
*INF (OscBank_Init): Oscillator accumulators will be randomized
*INF (OscBank_Init): Oscillator accumulators initialized
*INF (OscBank_Init): Initializing wavetables
*INF (OscBank_Init): Wavetables initialized
*INF (OscBank_Init): Setting up oscillator banks
*INF (OscBank_Init): Done!
*INF (Mz2Pnl_Load): Reading input file example-02.ppm...
*INF (Mz2Pnl_Load): Done!
*INF (Mz2Pnl_Load): Executing flip across horizontal line...
*INF (Mz2Pnl_Load): Done!
*INF (Mz2Pnl_Load): Associated sine oscillators with lum channel
*INF (Mz2Pnl_Load): Sqwv oscillators are muted
*INF (Mz2Pnl_Load): Swth oscillators are muted
*INF (Mz2Pnl_Load): Trng oscillators are muted
*INF (Mz2Pnl_Load): Memory allocation and initialization ...
*INF (Mz2Pnl_Load): Done!
*INF (Au_WrtHdr): File example-02.au opened in mode NEW on unit 22
*INF (Au_WrtHdr): Hardware is little-endian so samples will be converted
*INF (Au_WrtHdr): Audio file header write status is 0
*INF (Mz2Syn_Init): Number of samples to generate: 15360000 (Num channels=  2 )
*INF (Mz2_Generate): SMPL=    48000 ; %DONE = 0.31250    ; TIME/S=  1.0000    
*INF (Mz2_Generate): SMPL=    96000 ; %DONE = 0.62500    ; TIME/S=  2.0000    
*INF (Mz2_Generate): SMPL=   144000 ; %DONE = 0.93750    ; TIME/S=  3.0000
...
*INF (Au_Close): Buffer flushed into unit 22
*INF (Au_Close): Closed file unit 22
*INF (Au_Close): Status of operation is 0
*INF (Au_Close): Buffer deallocated
*INF (Mz2Pnl_Clear): Closed graphic input file.
*INF (Mz2Pnl_Clear): Panel memory deallocated and structure cleared.
*INF (OscBank_Clear): Oscillator banks deallocated cleared
*INF (Mz2Synth):  -*- END OF STATEMENTS -*- 
```

This will take a few minutes.

(5) We can now use the SoX (Sound eXchange) system to convert the Sun
Audio output file to an Ogg Vorbis audio file, while at the same time
applying an 8 kHz low-pass filter, a chorus and a reverb.

`sox example-02.au example-02.ogg lowpass -1 8000 chorus 0.7 0.9 55
0.4 0.25 2 -t reverb 75 75 100 deemph`

(6) You can please the executable, **mz2**, from the SOURCES folder,
somewhere in your executable search path if you like, e.g., in
/usr/local/bin, which will save a bit of palavering if you use
mz2synth a lot.  Just remember to update it if you recompile!

**NOTE:** User **scruss** has kindly provided good information for
  information for MACOS users.  Herewith, a summary:

If [Homebrew](https://brew.sh/)'s `gcc@13` package is not installed,
the included binary fails.

After installing Homebrew, the package can be installed using:
```
brew install gcc@13
```
the included binary executable will run as expected.
