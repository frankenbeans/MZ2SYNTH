* ======================================================================
* WVECMP.F - FOURIER SYNTHESIS OF COMMON MUSICAL SYNTHESIZER WAVEFORMS
* COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
* ======================================================================

#define PPI    (4.0D0*ATAN(1.0D0))
#define MXNCMP (128)

* ======================================================================
*     FUNCTIONS WVFxxx(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
*
*     Fill the array OUTPUT (double precision) - dimensioned from 1 to
*     IZOUTP (integer) - with a single cycle of all Fourier components
*     of waveform xxx, having a frequency of IFWAVE (double precision) 
*     in C.P.S., up to the Nyquist frequency corresponding to a sampling
*     frequency of IFSMPL (double precision) in C.P.S. .
*
*        xxx   Waveform
*        ---   --------
*        SIN   Sine wave
*        SQR   Square wave
*        SAW   Sawtooth wave
*        TRI   Triangle wave
* ----------------------------------------------------------------------
      SUBROUTINE WVFSIN(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION IFWAVE
      DOUBLE PRECISION IFSMPL
      INTEGER          IZOUTP
      DOUBLE PRECISION OUTPUT(1:IZOUTP)
*     --- PARAMETER ---
      INTEGER          MXNITR
      PARAMETER(MXNITR=1)
*     --- EXTERNALS ---
      DOUBLE PRECISION FGNSIN
      EXTERNAL         FGNSIN
*     --- EXE CODE ---
      CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSIN,MXNITR)
*     --- END CODE ---
      END
* ----------------------------------------------------------------------
      SUBROUTINE WVFSQR(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION IFWAVE
      DOUBLE PRECISION IFSMPL
      INTEGER          IZOUTP
      DOUBLE PRECISION OUTPUT(1:IZOUTP)
*     --- PARAMETER ---
      INTEGER          MXNITR
      PARAMETER(MXNITR=MXNCMP)
*     --- EXTERNALS ---
      DOUBLE PRECISION FGNSQR
      EXTERNAL         FGNSQR
*     --- EXE CODE ---
      CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSQR,MXNITR)
*     --- END CODE ---
      END
* ----------------------------------------------------------------------
      SUBROUTINE WVFSAW(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION IFWAVE
      DOUBLE PRECISION IFSMPL
      INTEGER          IZOUTP
      DOUBLE PRECISION OUTPUT(1:IZOUTP)
*     --- PARAMETER ---
      INTEGER          MXNITR
      PARAMETER(MXNITR=MXNCMP)
*     --- VARIABLES ---
      DOUBLE PRECISION FGNSAW
      EXTERNAL         FGNSAW
*     --- EXE CODE ---
      CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSAW,MXNITR)
*     --- END CODE ---
      END
* ----------------------------------------------------------------------
      SUBROUTINE WVFTRI(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION IFWAVE
      DOUBLE PRECISION IFSMPL
      INTEGER          IZOUTP
      DOUBLE PRECISION OUTPUT(1:IZOUTP)
*     --- PARAMETER ---
      INTEGER          MXNITR
      PARAMETER(MXNITR=MXNCMP)
*     --- EXTERNALS ---
      DOUBLE PRECISION FGNTRI
      EXTERNAL         FGNTRI
*     --- EXE CODE ---
      CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNTRI,MXNITR)      
*     --- END CODE ---
      END
* =======================================================================
      FUNCTION OMEGA(F)
*     Calculates angular frequency OMEGA for F in C.P.S.
      IMPLICIT NONE
      DOUBLE PRECISION OMEGA
*     --- DUMMIES ---
      DOUBLE PRECISION F
*     --- EXE CODE ---
      OMEGA=2.0D0*PPI*F
*     --- END CODE ---
      END
* -----------------------------------------------------------------------
      FUNCTION GPCOEF(GK,GMK)
*     Calculates low-pass filter coefficient to reduce the amplitudes
*     of the high-frequency Fourier components as these approach the
*     Nyquist Frequency.  GK corresponds to the Kth Fourier component
*     that this coefficient will be used with, and GMK is the maximum
*     K that we will reach before exceeding the Nyquist Frequency.
*
*     REFERENCE:  "Bandlimited Wavetable Synthesis"
*     http://hackmeopen.com/2010/11/bandlimited-wavetable-synthesis/      
      IMPLICIT NONE
      DOUBLE PRECISION GPCOEF
*     --- DUMMIES ---
      INTEGER GK,GMK
*     --- EXE CODE ---
      GPCOEF=COS(DBLE(GK-1)*PPI/(2D0*DBLE(GMK)))**2
*     --- END CODE ---      
      END
* -----------------------------------------------------------------------      
      SUBROUTINE WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,WVFUNC,MXNITR)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION IFWAVE
      DOUBLE PRECISION IFSMPL
      INTEGER          IZOUTP
      DOUBLE PRECISION OUTPUT(1:IZOUTP)
      DOUBLE PRECISION WVFUNC
      INTEGER          MXNITR
      EXTERNAL         WVFUNC   
*     --- VARIABLES ---
      INTEGER          J
      INTEGER          K,MAXK
      DOUBLE PRECISION FCFREQ
      DOUBLE PRECISION T
      DOUBLE PRECISION ABSAMP
*     --- EXTERNALS ---
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION GPCOEF
*     --- EXE CODE ---
*     INITIALIZE VARIABLES TO ZERO
      K=0
      DO 5 J=1,IZOUTP
         OUTPUT(J)=0D0
 5    CONTINUE
*     ..................................................................
*     FCFREQ is the number of cycles of the waveform that must appear
*     in the array OUTPUT(1:IZOUTP).  Normally, this should be set to
*     UNITY.
*     ..................................................................
      FCFREQ=1D0
*     ..................................................................
*     For a sample rate IFSMPL, the Nyquist frequency is 0.5*IFSMPL,
*     and therefore no Fourier component should appear with a frequency
*     greater than 0.5*IFSMPL.  Since WVF000 fills OUTPUT with exactly
*     one cycle, 0.5*IFSMPL/IFWAVE is the frequency (in units of IFWAVE)
*     of the highest frequency Fourier component that should appear in
*     OUTPUT.
*     ..................................................................
      MAXK=MIN(INT(0.5D0*IFSMPL/IFWAVE),MXNITR)
*     DO FOURIER SUMMATION FOR THIS WAVEFORM
 10   K=K+1
      IF (K.GT.MAXK) GOTO 25
      DO 20 J=1,IZOUTP
         T=DBLE(J-1)/DBLE(IZOUTP)
         OUTPUT(J)=OUTPUT(J)+GPCOEF(K,MAXK)*WVFUNC(OMEGA(FCFREQ),T,K)
 20   CONTINUE
      GOTO 10
 25   CONTINUE
*     ..................................................................
*     Find amplitude of output waveform section and normalize to UNITY.
      ABSAMP=0D0
      DO 30 J=1,IZOUTP
         IF (ABS(OUTPUT(J)).GT.ABSAMP) ABSAMP=ABS(OUTPUT(J))
 30   CONTINUE
*
      IF (ABSAMP.EQ.0) GOTO 999
      DO 40 J=1,IZOUTP         
         OUTPUT(J)=OUTPUT(J)/ABSAMP         
 40   CONTINUE
*     ..................................................................
*     --- END CODE ---
999   END
* ======================================================================
      FUNCTION FGNSIN(OMEGA,T,K)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION FGNSIN
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION T
      INTEGER          K
*     --- EXE CODE ---
      IF (K.EQ.1) THEN
         FGNSIN=SIN(DBLE(K)*OMEGA*T)
      ELSE
         FGNSIN=0D0
      END IF
*     --- END CODE ---
      END
* ----------------------------------------------------------------------
      FUNCTION FGNSQR(OMEGA,T,K)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION FGNSQR
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION T
      INTEGER          K
*     --- EXE CODE ---
      IF (MOD(K,2).EQ.0) THEN
         FGNSQR=0D0
      ELSE
         FGNSQR=SIN(DBLE(K)*OMEGA*T)/DBLE(K)
      END IF
*     --- END CODE ---
      END    
* ----------------------------------------------------------------------
      FUNCTION FGNSAW(OMEGA,T,K)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION FGNSAW
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION T
      INTEGER          K
*     --- EXE CODE ---
      FGNSAW=-SIN(DBLE(K)*OMEGA*T)/DBLE(K)
*     --- END CODE ---
      END    
* ----------------------------------------------------------------------
      FUNCTION FGNTRI(OMEGA,T,K)
      IMPLICIT NONE
*     --- DUMMIES ---
      DOUBLE PRECISION FGNTRI
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION T
      INTEGER          K
*      --- EXE CODE ---
      IF (MOD(K,2).EQ.0) THEN
         FGNTRI=0D0
      ELSE
         FGNTRI=(-1D0)**(DBLE(K-1)/2D0)/(DBLE(K)**2)*SIN(K*OMEGA*T)
      END IF
*     --- END CODE ---
      END
* ======================================================================
