! ------------------------------------------------------------------------------
! WVECMP.F90
!
! FOURIER SYNTHESIS OF COMMON MUSICAL SYNTHESIZER WAVEFORMS
!
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ------------------------------------------------------------------------------

MODULE WveCmp
  USE Constant
  IMPLICIT NONE

  INTEGER,PARAMETER :: MXNCMP=128
  
CONTAINS
  
  ! ============================================================================
  !     FUNCTIONS Wvfxxx(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
  !
  !     Fill the array OUTPUT (double precision) - dimensioned from 1 to
  !     IZOUTP (integer) - with a single cycle of all Fourier components
  !     of waveform xxx, having a frequency of IFWAVE (double precision) 
  !     in C.P.S., up to the Nyquist frequency corresponding to a sampling
  !     frequency of IFSMPL (double precision) in C.P.S. .
  !
  !        xxx   Waveform
  !        ---   --------
  !        Sin   Sine wave
  !        Sqr   Square wave
  !        Saw   Sawtooth wave
  !        Tri   Triangle wave
  ! ----------------------------------------------------------------------------
  PURE SUBROUTINE WvfSin(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
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
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSin,MXNITR)
    ! --- END CODE ---
  END SUBROUTINE WvfSin
! ------------------------------------------------------------------------------
  PURE SUBROUTINE WvfSqr(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
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
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSqr,MXNITR)
    ! --- END CODE ---
  END SUBROUTINE WvfSqr
! ------------------------------------------------------------------------------
  PURE SUBROUTINE WvfSaw(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
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
    CALL WVF000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNSaw,MXNITR)
    ! --- END CODE ---
  END SUBROUTINE WvfSaw
! ------------------------------------------------------------------------------
  PURE SUBROUTINE WvfTri(IFWAVE,IFSMPL,IZOUTP,OUTPUT)
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
    CALL Wvf000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,FGNTri,MXNITR)      
    ! --- END CODE ---
  END SUBROUTINE WvfTri
! ==============================================================================
  PURE FUNCTION Omega(F)
    ! Calculates angular frequency Omega for F in C.P.S.
    IMPLICIT NONE
    REAL(KIND=RKIND) :: Omega
    ! --- DUMMIES ---
    REAL(KIND=RKIND),INTENT(IN) :: F
    ! --- EXE CODE ---
    Omega=2*PI*F
    ! --- END CODE ---
  END FUNCTION Omega
! ------------------------------------------------------------------------------
  PURE FUNCTION GPCoef(GK,GMK)
    ! Calculates low-pass filter coefficient to reduce the amplitudes
    ! of the high-frequency Fourier components as these approach the
    ! Nyquist Frequency.  GK corresponds to the Kth Fourier component
    ! that this coefficient will be used with, and GMK is the maximum
    ! K that we will reach before exceeding the Nyquist Frequency.
    !
    ! REFERENCE:  "Bandlimited Wavetable Synthesis"
    ! http://hackmeopen.com/2010/11/bandlimited-wavetable-synthesis/
    IMPLICIT NONE
    REAL(KIND=RKIND) :: GPCoef
    ! --- DUMMIES ---
    INTEGER,INTENT(IN) :: GK,GMK
    ! --- EXE CODE ---
    GPCoef=COS(REAL(GK-1,RKIND)*PI/(2*REAL(GMK,RKIND)))**2
    ! --- END CODE ---      
  END FUNCTION GPCoef
! ------------------------------------------------------------------------------
  PURE SUBROUTINE Wvf000(IFWAVE,IFSMPL,IZOUTP,OUTPUT,WVFUNC,MXNITR)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: IFWAVE
    REAL(KIND=RKIND) :: IFSMPL
    INTEGER          :: IZOUTP
    REAL(KIND=RKIND) :: OUTPUT(1:IZOUTP)
    INTERFACE
       PURE FUNCTION WVFUNC(Omega,T,K)
         USE Constant
         IMPLICIT NONE
         REAL(KIND=RKIND) :: WVFUNC
         ! --- DUMMIES ---
         REAL(KIND=RKIND) :: Omega
         REAL(KIND=RKIND) :: T
         INTEGER          :: K
         INTENT(IN) :: Omega,T,K
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
    ! ..........................................................................
    ! FCFREQ is the number of cycles of the waveform that must appear
    ! in the array OUTPUT(1:IZOUTP).  Normally, this should be set to
    ! UNITY.
    ! ...........................................................................
    FCFREQ=1
    ! ..........................................................................
    ! For a sample rate IFSMPL, the Nyquist frequency is 0.5*IFSMPL,
    ! and therefore no Fourier component should appear with a frequency
    ! greater than 0.5*IFSMPL.  Since Wvf000 fills OUTPUT with exactly
    ! one cycle, 0.5*IFSMPL/IFWAVE is the frequency (in units of IFWAVE)
    ! of the highest frequency Fourier component that should appear in
    ! OUTPUT.
    ! ..........................................................................
    MAXK=MIN(INT(IFSMPL/(2*IFWAVE)),MXNITR)
    ! DO FOURIER SUMMATION FOR THIS WAVEFORM
    DO K=1,MAXK
       DO J=1,IZOUTP
          T=REAL(J-1,RKIND)/REAL(IZOUTP,RKIND)
          OUTPUT(J)=OUTPUT(J)+GPCoef(K,MAXK)*WVFUNC(Omega(FCFREQ),T,K)
       END DO
    END DO
    ! .........................................................................
    ! Find amplitude of output waveform section and normalize to UNITY.
    ABSAMP=MAXVAL(ABS(OUTPUT))
    IF (ABSAMP.NE.0) OUTPUT=OUTPUT/ABSAMP
    ! ..........................................................................
    ! --- END CODE ---
  END SUBROUTINE Wvf000
! ==============================================================================
  PURE FUNCTION FGNSin(OMG,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNSin
    REAL(KIND=RKIND) :: OMG
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMG,T,K
    ! --- EXE CODE ---
    IF (K.EQ.1) THEN
       FGNSin=SIN(REAL(K,RKIND)*OMG*T)
    ELSE
       FGNSin=0
    END IF
    ! --- END CODE ---
  END FUNCTION FGNSin
! ------------------------------------------------------------------------------
  PURE FUNCTION FGNSqr(OMG,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNSqr
    REAL(KIND=RKIND) :: OMG
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMG,T,K
    ! --- EXE CODE ---
    IF (MOD(K,2).EQ.0) THEN
       FGNSqr=0
    ELSE
       FGNSqr=SIN(REAL(K,RKIND)*OMG*T)/REAL(K,RKIND)
    END IF
    ! --- END CODE ---
  END FUNCTION FGNSqr
! ------------------------------------------------------------------------------
  PURE FUNCTION FGNSaw(OMG,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNSaw
    REAL(KIND=RKIND) :: OMG
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMG,T,K
    ! --- EXE CODE ---
    FGNSaw=-SIN(REAL(K,RKIND)*OMG*T)/REAL(K,RKIND)
    ! --- END CODE ---
  END FUNCTION FGNSaw
! ------------------------------------------------------------------------------
  PURE FUNCTION FGNTri(OMG,T,K)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: FGNTri
    REAL(KIND=RKIND) :: OMG
    REAL(KIND=RKIND) :: T
    INTEGER          :: K
    INTENT(IN) :: OMG,T,K
    ! --- EXE CODE ---
    IF (MOD(K,2).EQ.0) THEN
       FGNTri=0
    ELSE
       ! FORTRAN-77 VERSION WAS:
       !   FGNTri=(-1D0)**(DBLE(K-1)/2D0)/(DBLE(K)**2)*SIN(K*OMG*T)
       FGNTri=((-1.0_RKIND)**(REAL(K-1,RKIND)/2.0_RKIND))*(SIN(K*OMG*T)) &
              / (REAL(K,RKIND)**2)
    END IF
    ! --- END CODE ---
  END FUNCTION FGNTri
! ------------------------------------------------------------------------------
  END MODULE WveCmp
! ==============================================================================
