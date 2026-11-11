*** README.txt ***

This is the home of the MZ2SYNTH binary executables, built nightly by
GitHub runners.

LINUX: Should run with default install
MACOS: Homebrew's gcc@13 package
WIN64: Requires Mingw-w64 runtime

I have not tested these executables beyond starting them in -help mode
in the action script.  Please report problems in the Issues
discussion.

Please note that the Makefiles for GFortran and IFX respectively use
-march=native and -xHost.  This means that if you build them on your
system, the executables will be specifically targeted to your hardware
to the extent that GFortran and IFX will do this, which is good.  However,
the executables actually in this folder have been targeted to whatever the
emulated hardware set up for the GitHub runners may be, which may not be
the same as your system, which would be bad...

Owners of current computers will probably not have problems, but if 
you get weird errors (e.g., "illegal instruction") then try rebuilding 
on your machine to get a fresh executable tailored to your own computer.
