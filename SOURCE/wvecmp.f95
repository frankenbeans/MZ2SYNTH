! ==========================================================================
! WVECMP.F95 - FOURIER SYNTHESIS OF COMMON MUSICAL SYNTHESIZER WAVEFORMS
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ==========================================================================

#define PPI    (4.0_RKIND*ATAN(1.0_RKIND))
#define MXNCMP (128)

MODULE WveCmp
  USE Constant
  IMPLICIT NONE
  
CONTAINS
  
  ! ========================================================================
  !     FUNCTIONS WVFxxx(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
  !
  !     Fill the array OUTPUT (double precision) - dimensioned from 1 to
  !     IZOUTP (integer) - with a single cycle of all Fourier components
  !     of waveform xxx, having a frequency of IFWAVE (double precision) 
  !     in C.P.S., up to the Nyquist frequency corresponding to a sampling
  !     frequency of IFSMPL (double precision) in C.P.S. .
  !
  !        xxx   Waveform
  !        ---   --------
  !        SIN   Sine wave
  !        SQR   Square wave
  !        SAW   Sawtooth wave
  !        TRI   Triangle wave
  ! ------------------------------------------------------------------------
  PURE SUBROUTINE WVFSIN(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: IFWAVE
    REAL(KIND=RKIND) :: IFSMPL
    INTEGER          :: IZOUTP
    REAL(KIND=RKIND) :: OUTPUT(1:IZOUTP)
    INTENT(IN)    :: IFWAVE,IFSMPL,IZOUTP
    INTENT(INOUT) :: OUTPUT
    ! --- PARAMETER ---
    INTEGER,PARAMETER :: MXNITR=1
    ! --- EXE CODE ---
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSIN,MXNITR)
    ! --- END CODE ---
  END SUBROUTINE WVFSIN
! --------------------------------------------------------------------------
  PURE SUBROUTINE WVFSQR(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: IFWAVE
    REAL(KIND=RKIND) :: IFSMPL
    INTEGER          :: IZOUTP
    REAL(KIND=RKIND) :: OUTPUT(1:IZOUTP)
    INTENT(IN)    :: IFWAVE,IFSMPL,IZOUTP
    INTENT(INOUT) :: OUTPUT
    ! --- PARAMETER ---
    INTEGER,PARAMETER :: MXNITR=MXNCMP
    ! --- EXE CODE ---
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSQR,MXNITR)
    ! --- END CODE ---
  END SUBROUTINE WVFSQR
! --------------------------------------------------------------------------
  PURE SUBROUTINE WVFSAW(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: IFWAVE
    REAL(KIND=RKIND) :: IFSMPL
    INTEGER          :: IZOUTP
    REAL(KIND=RKIND) :: OUTPUT(1:IZOUTP)
    INTENT(IN)    :: IFWAVE,IFSMPL,IZOUTP
    INTENT(INOUT) :: OUTPUT
    ! --- PARAMETER ---
    INTEGER,PARAMETER :: MXNITR=MXNCMP
    ! --- EXE CODE ---
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSAW,MXNITR)
    ! --- END CODE ---
  END SUBROUTINE WVFSAW
! --------------------------------------------------------------------------
  PURE SUBROUTINE WVFTRI(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: IFWAVE
    REAL(KIND=RKIND) :: IFSMPL
    INTEGER          :: IZOUTP
    REAL(KIND=RKIND) :: OUTPUT(1:IZOUTP)
    INTENT(IN)    :: IFWAVE,IFSMPL,IZOUTP
    INTENT(INOUT) :: OUTPUT
    ! --- PARAMETER ---
    INTEGER,PARAMETER :: MXNITR=MXNCMP
    ! --- EXE CODE ---
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNTRI,MXNITR)      
    ! --- END CODE ---
  END SUBROUTINE WVFTRI
! ==========================================================================
  PURE FUNCTION OMEGA(F)
    ! Calculates angular frequency OMEGA for F in C.P.S.
    IMPLICIT NONE
    REAL(KIND=RKIND) :: OMEGA
    ! --- DUMMIES ---
    REAL(KIND=RKIND),INTENT(IN) :: F
    ! --- EXE CODE ---
    OMEGA=2*PPI*F
    ! --- END CODE ---
  END FUNCTION OMEGA
! --------------------------------------------------------------------------
  PURE FUNCTION GPCOEF(GK,GMK)
    ! Calculates low-pass filter coefficient to reduce the amplitudes
    ! of the high-frequency Fourier components as these approach the
    ! Nyquist Frequency.  GK corresponds to the Kth Fourier component
    ! that this coefficient will be used with, and GMK is the maximum
    ! K that we will reach before exceeding the Nyquist Frequency.
    !
    ! REFERENCE:  "Bandlimited Wavetable Synthesis"
    ! http://hackmeopen.com/2010/11/bandlimited-wavetable-synthesis/
    IMPLICIT NONE
    REAL(KIND=RKIND) :: GPCOEF
    ! --- DUMMIES ---
    INTEGER,INTENT(IN) :: GK,GMK
    ! --- EXE CODE ---
    GPCOEF=COS(REAL(GK-1,RKIND)*PPI/(2*REAL(GMK,RKIND)))**2
    ! --- END CODE ---      
  END FUNCTION GPCOEF
! --------------------------------------------------------------------------
  PURE SUBROUTINE WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,WVFUNC,MXNITR)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: IFWAVE
    REAL(KIND=RKIND) :: IFSMPL
    INTEGER          :: IZOUTP
    REAL(KIND=RKIND) :: OUTPUT(1:IZOUTP)
    INTERFACE
       PURE FUNCTION WVFUNC(OMEGA,T,K)
         USE Constant
         IMPLICIT NONE
         REAL(KIND=RKIND) :: WVFUNC
         ! --- DUMMIES ---
         REAL(KIND=RKIND) :: OMEGA
         REAL(KIND=RKIND) :: T
         INTEGER          :: K
         INTENT(IN) :: OMEGA,T,K
       END FUNCTION WVFUNC
    END INTERFACE
    INTEGER          :: MXNITR
    INTENT(IN)       :: IFWAVE,IFSMPL,IZOUTP,MXNITR
    INTENT(INOUT)    :: OUTPUT
    ! --- VARIABLES ---
    INTEGER          :: J
    INTEGER          :: K,MAXK
    REAL(KIND=RKIND) :: FCFREQ
    REAL(KIND=RKIND) :: T
    REAL(KIND=RKIND) :: ABSAMP
    ! --- EXE CODE ---
    ! INITIALIZE VARIABLES TO ZERO
    OUTPUT=0
    ! ....................................................................
    ! FCFREQ is the number of cycles of the waveform that must appear
    ! in the array OUTPUT(1:IZOUTP).  Normally, this should be set to
    ! UNITY.
    ! ....................................................................
    FCFREQ=1
    ! ....................................................................
    ! For a sample rate IFSMPL, the Nyquist frequency is 0.5*IFSMPL,
    ! and therefore no Fourier component should appear with a frequency
    ! greater than 0.5*IFSMPL.  Since WVF000 fills OUTPUT with exactly
    ! one cycle, 0.5*IFSMPL/IFWAVE is the frequency (in units of IFWAVE)
    ! of the highest frequency Fourier component that should appear in
    ! OUTPUT.
    ! ....................................................................
    MAXK=MIN(INT(IFSMPL/(2*IFWAVE)),MXNITR)
    ! DO FOURIER SUMMATION FOR THIS WAVEFORM
    DO K=1,MAXK
       DO J=1,IZOUTP
          T=REAL(J-1,RKIND)/REAL(IZOUTP,RKIND)
          OUTPUT(J)=OUTPUT(J)+GPCOEF(K,MAXK)*WVFUNC(OMEGA(FCFREQ),T,K)
       END DO
    END DO
    ! ....................................................................
    ! Find amplitude of output waveform section and normalize to UNITY.
    ABSAMP=MAXVAL(ABS(OUTPUT))
    IF (ABSAMP.NE.0) OUTPUT=OUTPUT/ABSAMP
    ! ....................................................................
    ! --- END CODE ---
  END SUBROUTINE WVF000
! ==========================================================================
  PURE FUNCTION FGNSIN(OMEGA,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNSIN
    REAL(KIND=RKIND) :: OMEGA
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMEGA,T,K
    ! --- EXE CODE ---
    IF (K.EQ.1) THEN
       FGNSIN=SIN(REAL(K,RKIND)*OMEGA*T)
    ELSE
       FGNSIN=0
    END IF
    ! --- END CODE ---
  END FUNCTION FGNSIN
! --------------------------------------------------------------------------
  PURE FUNCTION FGNSQR(OMEGA,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNSQR
    REAL(KIND=RKIND) :: OMEGA
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMEGA,T,K
    ! --- EXE CODE ---
    IF (MOD(K,2).EQ.0) THEN
       FGNSQR=0
    ELSE
       FGNSQR=SIN(REAL(K,RKIND)*OMEGA*T)/REAL(K,RKIND)
    END IF
    ! --- END CODE ---
  END FUNCTION FGNSQR
! --------------------------------------------------------------------------
  PURE FUNCTION FGNSAW(OMEGA,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNSAW
    REAL(KIND=RKIND) :: OMEGA
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMEGA,T,K
    ! --- EXE CODE ---
    FGNSAW=-SIN(REAL(K,RKIND)*OMEGA*T)/REAL(K,RKIND)
    ! --- END CODE ---
  END FUNCTION FGNSAW
! --------------------------------------------------------------------------
  PURE FUNCTION FGNTRI(OMEGA,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNTRI
    REAL(KIND=RKIND) :: OMEGA
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMEGA,T,K
    ! --- EXE CODE ---
    IF (MOD(K,2).EQ.0) THEN
       FGNTRI=0
    ELSE
       ! FORTRAN-77 VERSION WAS:
       !   FGNTRI=(-1D0)**(DBLE(K-1)/2D0)/(DBLE(K)**2)*SIN(K*OMEGA*T)
       FGNTRI=((-1.0_RKIND)**(REAL(K-1,RKIND)/2.0_RKIND))*(SIN(K*OMEGA*T)) &
              / (REAL(K,RKIND)**2)
    END IF
    ! --- END CODE ---
  END FUNCTION FGNTRI
! --------------------------------------------------------------------------
  END MODULE WveCmp
! ==========================================================================
