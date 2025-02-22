! ------------------------------------------------------------------------------
! MZ2OSC.F90
!
! SIMULATED OSCILLATOR BANK (SINE,SQUARE,SAWTOOTH,TRIANGLE) FOR MZ2 SYNTHESIZER
!
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ------------------------------------------------------------------------------

MODULE Mz2Osc
  USE Constant
  USE PFlags
  USE WveCmp
  IMPLICIT NONE

  INTEGER,PARAMETER :: N_VOC        =4 ! (SIN,SQW,SWT,TRI)
  INTEGER,PARAMETER :: V_SIN=1,V_SQW=2,V_SWT=3,V_TRI=N_VOC
  INTEGER,PARAMETER :: N_OCT        =DNOC
  INTEGER,PARAMETER :: N_WVT        =DNWT
  INTEGER,PARAMETER :: N_OSC_PER_OCT=DNOO
  INTEGER,PARAMETER :: N_SMT_PER_OCT=DNSO
  INTEGER,PARAMETER :: N_OSC        =N_OCT*N_OSC_PER_OCT
  INTEGER,PARAMETER :: N_OSC_PER_SMT=N_OSC_PER_OCT / N_SMT_PER_OCT
  INTEGER,PARAMETER :: N_TIC_PER_CYC=DWWW  
  INTEGER,PARAMETER :: FRQ_REFERENCE_OSC=DFRF
  INTEGER,PARAMETER :: REFERENCE_SMT_NUM=DSRF

  TYPE :: OscBank
     INTEGER          :: smpr=DSMP
     REAL(KIND=RKIND) :: freq(1:N_OSC)=0
     REAL(KIND=RKIND) :: incr(1:N_OSC)=0
     REAL(KIND=RKIND) :: accm(1:N_OSC)=0
     INTEGER          :: wtno(1:N_OSC)=0
     REAL(KIND=RKIND) :: vval(1:N_OSC,1:N_VOC)=0 ! VOICE in {SIN,SQW,SWT,TRI}
     REAL(KIND=RKIND),POINTER :: wts(:)     =>NULL()
     REAL(KIND=RKIND),POINTER :: wtr(:,:,:) =>NULL()
     REAL(KIND=RKIND),POINTER :: tsin(:)    =>NULL()
     REAL(KIND=RKIND),POINTER :: tsqw(:,:)  =>NULL()
     REAL(KIND=RKIND),POINTER :: tswt(:,:)  =>NULL()
     REAL(KIND=RKIND),POINTER :: ttri(:,:)  =>NULL()
  END TYPE OscBank

CONTAINS

  ELEMENTAL FUNCTION WvtNmbr(osno) RESULT(r)
    IMPLICIT NONE
    INTEGER            :: r
    INTEGER,INTENT(IN) :: osno
    ! --- VARIABLES ---
    REAL(RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_OSC,RKIND)
    ! --- EXE CODE ---
    r=INT(REAL(N_WVT,RKIND)*REAL(osno-1,RKIND)*rdenom)+1
    ! --- END CODE ---
  END FUNCTION WvtNmbr

  ELEMENTAL FUNCTION OscNmbr(wtno) RESULT(r)
    IMPLICIT NONE
    INTEGER            :: r
    INTEGER,INTENT(IN) :: wtno
    ! --- VARIABLES ---
    REAL(RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_WVT,RKIND)
    ! --- EXE CODE ---
    r=INT(REAL(N_OSC,RKIND)*REAL(wtno,RKIND)*rdenom)
    ! --- END CODE ---
  END FUNCTION OscNmbr

  SUBROUTINE OscBank_Init(ob,sr,ra)
    IMPLICIT NONE
    TYPE(OscBank),INTENT(INOUT) :: ob
    INTEGER      ,INTENT(IN)    :: sr
    LOGICAL      ,INTENT(IN)    :: ra ! TRUE => Randomize accumulators else zero
    OPTIONAL :: sr,ra
    ! --- VARIABLES ---
    LOGICAL :: lra
    INTEGER :: i,j,nseed,zseed,ms
    REAL(KIND=RKIND) :: fc1
    ! --- EXE CODE ---
    ALLOCATE(ob%wts(1:N_TIC_PER_CYC),SOURCE=0.0_RKIND,STAT=MS)
    IF (MS.NE.0) GOTO 900
    ALLOCATE(ob%wtr(1:N_TIC_PER_CYC,1:N_WVT,V_SQW:V_TRI), &
         SOURCE=0.0_RKIND,STAT=ms)
    IF (ms.NE.0) GOTO 900

    IF (PFL_VERB) WRITE(*,700) 'Initializing oscillator bank'
    lra=.FALSE.  ; IF (PRESENT(ra)) lra=ra
    ob%smpr=DSMP ; IF(PRESENT(sr)) ob%smpr=sr

    IF (lra) THEN
       IF (PFL_VERB) WRITE(*,700) 'Oscillator accumulators will be randomized'    
       CALL RANDOM_SEED(SIZE=zseed)
       CALL RANDOM_SEED(PUT=(/(DRNS,NSEED=1,zseed)/))
    END IF

    fc1=REAL(N_TIC_PER_CYC,RKIND)/REAL(ob%smpr,RKIND)
    DO j=1,N_OSC
       ob%freq(j)=OscFreq(j)
       ob%incr(j)=fc1*ob%freq(j)
       ob%accm(j)=OscAccm(lra)
       ob%wtno(j)=WvtNmbr(j)       
    END DO
    IF (PFL_VERB) WRITE(*,700) 'Oscillator accumulators initialized'
    
    IF (PFL_DBUG) THEN
       WRITE(*,700) '*DEBUG* Oscillator no, freq, incr, accm, wtno'
       DO j=1,N_OSC
          WRITE(*,710) j,ob%freq(j),ob%incr(j),ob%accm(j),ob%wtno(j)
       END DO
    END IF  

    IF (PFL_VERB) WRITE(*,700) 'Initializing wavetables'

    ob%tsin=>ob%wts
    ob%tsqw=>ob%wtr(1:N_TIC_PER_CYC,1:N_WVT,V_SQW)
    ob%tswt=>ob%wtr(1:N_TIC_PER_CYC,1:N_WVT,V_SWT)
    ob%ttri=>ob%wtr(1:N_TIC_PER_CYC,1:N_WVT,V_TRI)
    CALL WVFSIN(ob%freq(1),REAL(ob%smpr,RKIND),N_TIC_PER_CYC,ob%tsin)    
    DO j=1,N_WVT
       CALL WVFSQR(ob%freq(OscNmbr(j)), REAL(ob%smpr,RKIND), &
            N_TIC_PER_CYC,ob%tsqw(:,j))
       CALL WVFSAW(ob%freq(OscNmbr(j)), REAL(ob%smpr,RKIND), &
            N_TIC_PER_CYC,ob%tswt(:,j))
       CALL WVFTRI(ob%freq(OscNmbr(j)), REAL(ob%smpr,RKIND), &
            N_TIC_PER_CYC,ob%ttri(:,j))
    END DO
    IF (PFL_VERB) WRITE(*,700) 'Wavetables initialized'

    IF (PFL_VERB) WRITE(*,700) 'Setting up oscillator banks'
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_SIN)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_SQW)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_SWT)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_TRI)    
    IF (PFL_VERB) WRITE(*,700) 'Done!'
    RETURN
    ! --- END CODE ---
700 FORMAT('*INF (OscBank_Init):',1x,A)
710 FORMAT('*INF (OscBank_Init):',1x,I3,1X,3(ES12.4),I3)
800 FORMAT('*ERR (OscBank_Init):',1x,A)
900 WRITE(*,800) 'Out of memory' ; STOP

  CONTAINS

    ELEMENTAL FUNCTION OscFreq(n) RESULT(f)
      IMPLICIT NONE
      REAL(KIND=RKIND)   :: f
      INTEGER,INTENT(IN) :: n
      ! --- VARIABLES ---
      REAL(KIND=RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_OSC_PER_OCT,RKIND)
      REAL(KIND=RKIND)   :: x
      ! --- EXE CODE ---
      x=REAL(n-REFERENCE_SMT_NUM*N_OSC_PER_SMT,RKIND)*rdenom
      f=REAL(FRQ_REFERENCE_OSC,RKIND)*2.0_RKIND**x
      ! --- END CODE ---
    END FUNCTION OscFreq

    FUNCTION OscAccm(r) RESULT(a)
      IMPLICIT NONE
      REAL(KIND=RKIND)   :: a
      LOGICAL,INTENT(IN) :: r
      ! --- EXE CODE ---
      IF (r) THEN
         CALL RANDOM_NUMBER(a)
         a=a*REAL(N_TIC_PER_CYC,RKIND)
      ELSE
         a=0
      END IF
      ! --- END CODE ---
    END FUNCTION OscAccm
  END SUBROUTINE OscBank_Init

  SUBROUTINE OscBank_Clear(ob)
    IMPLICIT NONE
    TYPE(OscBank),INTENT(INOUT) :: ob
    ! --- EXE CODE ---
    IF (ASSOCIATED(ob%wts)) DEALLOCATE(ob%wts)
    IF (ASSOCIATED(ob%wtr)) DEALLOCATE(ob%wtr)
    ob=OscBank()
    IF (PFL_VERB) WRITE(*,700) 'Oscillator banks deallocated and structure cleared'
    RETURN
    ! --- END CODE ---
700 FORMAT('*INF (OscBank_Clear):',1x,A)
  END SUBROUTINE OscBank_Clear

  SUBROUTINE OscBank_Update(ob,msk,vce)
    IMPLICIT NONE
    TYPE(OscBank),INTENT(INOUT) :: ob
    LOGICAL      ,INTENT(IN)    :: msk(1:N_OSC)
    INTEGER      ,INTENT(IN)    :: vce
    ! --- VARIBLES ---
    INTEGER          :: j,x0,x1
    REAL(KIND=RKIND) :: y0,y1
    ! --- EXE CODE ---
    IF (vce.EQ.V_SIN) THEN
       DO j=1,N_OSC
          IF (ob%smpr.lt.2*ob%freq(j)) EXIT ! No need to update beyond F_Nyquist
          IF (msk(j)) THEN
             x0=MIN(INT(ob%accm(j))+1,N_TIC_PER_CYC)
             x1=x0+1 ; IF (x1.GT.N_TIC_PER_CYC) x1=1
             ! .................................................................
             y0=ob%tsin(x0) ; y1=ob%tsin(x1)
             ob%vval(j,V_SIN)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          ELSE
             ob%vval(j,V_SIN)=0 ! Not strictly necessary
          END IF
       END DO
    ELSE IF (vce.EQ.V_SQW) THEN       
       DO j=1,N_OSC
          IF (ob%smpr.lt.2*ob%freq(j)) EXIT ! No need to update beyond F_Nyquist
          IF (msk(j)) THEN
             x0=MIN(INT(ob%accm(j))+1,N_TIC_PER_CYC)
             x1=x0+1 ; IF (x1.GT.N_TIC_PER_CYC) x1=1
             ! .................................................................
             y0=ob%tsqw(x0,ob%wtno(j)) ; y1=ob%tsqw(x1,ob%wtno(j))
             ob%vval(j,V_SQW)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          ELSE
             ob%vval(j,V_SQW)=0 ! Not strictly necessary
          END IF
       END DO
    ELSE IF (vce.EQ.V_SWT) THEN
       DO j=1,N_OSC
          IF (ob%smpr.lt.2*ob%freq(j)) EXIT ! No need to update beyond F_Nyquist
          IF (msk(j)) THEN
             x0=MIN(INT(ob%accm(j))+1,N_TIC_PER_CYC)
             x1=x0+1 ; IF (x1.GT.N_TIC_PER_CYC) x1=1
             ! .................................................................
             y0=ob%tswt(x0,ob%wtno(j)) ; y1=ob%tswt(x1,ob%wtno(j))
             ob%vval(j,V_SWT)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          ELSE
             ob%vval(j,V_SWT)=0 ! Not strictly necessary
          END IF
       END DO
    ELSE IF (vce.EQ.V_TRI) THEN
       DO j=1,N_OSC
          IF (msk(j)) THEN
             IF (ob%smpr.lt.2*ob%freq(j)) EXIT ! No need to update beyond F_Nyquist
             x0=MIN(INT(ob%accm(j))+1,N_TIC_PER_CYC)
             x1=x0+1 ; IF (x1.GT.N_TIC_PER_CYC) x1=1
             ! .................................................................
             y0=ob%ttri(x0,ob%wtno(j)) ; y1=ob%ttri(x1,ob%wtno(j))
             ob%vval(j,V_TRI)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          ELSE
             ob%vval(j,V_TRI)=0 ! Not strictly necessary
          END IF
       END DO
    ELSE
       GOTO 900
    END IF
    RETURN
    ! --- END CODE ---    
800 FORMAT('*ERR (OscBank_Update):',1x,A)
900 WRITE(*,800) 'Invalid voice number' ; STOP
    
  CONTAINS
    
    ELEMENTAL FUNCTION Yli(x0,x,y0,y1) RESULT(y)
      IMPLICIT NONE
      REAL(KIND=RKIND)            :: y
      REAL(KIND=RKIND),INTENT(IN) :: x0,x,y0,y1
      ! --- VARIABLES ---
      REAL(KIND=RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_TIC_PER_CYC,RKIND)
      REAL(KIND=RKIND) :: m,c
      ! --- EXE CODE ---
      m=(y1-y0)*rdenom ! Avoids expensive division
      c=y0-m*x0
      y=m*x+c
      ! --- END CODE ---
    END FUNCTION Yli
  END SUBROUTINE OscBank_Update

  SUBROUTINE OscBank_Tick(ob,msk)
    IMPLICIT NONE    
    TYPE(OscBank),INTENT(INOUT) :: ob
    LOGICAL      ,INTENT(IN)    :: msk(1:N_OSC)
    OPTIONAL :: msk
    ! --- VARIABLES ---
    INTEGER :: j
    LOGICAL :: lmsk(1:N_OSC)
    ! --- END CODE ---
    lmsk=.TRUE. ; IF (PRESENT(msk)) lmsk=msk
    DO j=1,N_OSC
       IF (.NOT.lmsk(j)) CYCLE
       ob%accm(j)=ob%accm(j)+ob%incr(j)
       DO WHILE(ob%accm(j).GT.N_TIC_PER_CYC)
          ob%accm(j)=ob%accm(j)-N_TIC_PER_CYC
       END DO
    END DO
    ! --- END CODE ---
  END SUBROUTINE OscBank_Tick
    
END MODULE Mz2Osc
