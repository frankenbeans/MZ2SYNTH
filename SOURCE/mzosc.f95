! ------------------------------------------------------------------------------
! MZOSC.F95
!
! SIMULATED OSCILLATOR BANK (SINE,SQUARE,SAWTOOTH,TRIANGLE) FOR MZ2 SYNTHESIZER
!
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED
! ------------------------------------------------------------------------------

MODULE MZOsc
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
     REAL(KIND=RKIND) :: vval(1:N_OSC,1:N_VOC)=0 ! VOICE E {SIN,SQW,SWT,TRI}
     REAL(KIND=RKIND),POINTER :: tsin(:)  =>NULL()
     REAL(KIND=RKIND),POINTER :: tsqw(:,:)=>NULL()
     REAL(KIND=RKIND),POINTER :: tswt(:,:)=>NULL()
     REAL(KIND=RKIND),POINTER :: ttri(:,:)=>NULL()
  END TYPE OscBank

CONTAINS

  ELEMENTAL FUNCTION WvtNmbr(osno) RESULT(r)
    IMPLICIT NONE
    ! ----------------------
    INTEGER            :: r
    INTEGER,INTENT(IN) :: osno
    ! ----------------------
    REAL(RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_OSC,RKIND)
    ! ----------------------
    r=INT(REAL(N_WVT,RKIND)*REAL(osno-1,RKIND)*rdenom)+1
  END FUNCTION WvtNmbr

  ELEMENTAL FUNCTION OscNmbr(wtno) RESULT(r)
    IMPLICIT NONE
    ! ----------------------
    INTEGER            :: r
    INTEGER,INTENT(IN) :: wtno
    ! ----------------------
    REAL(RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_WVT,RKIND)
    ! ----------------------    
    r=INT(REAL(N_OSC,RKIND)*REAL(wtno,RKIND)*rdenom)
  END FUNCTION OscNmbr

  SUBROUTINE OscBank_Init(ob,sr,ra)
    IMPLICIT NONE
    ! --------------------------------
    TYPE(OscBank),INTENT(INOUT) :: ob
    INTEGER      ,INTENT(IN)    :: sr
    LOGICAL      ,INTENT(IN)    :: ra ! TRUE => Randomize accumulators else zero
    OPTIONAL :: sr,ra
    ! --------------------------------
    LOGICAL :: lra
    INTEGER :: i,j,nseed,zseed,ms
    REAL(KIND=RKIND) :: fc1
    REAL(KIND=RKIND),POINTER :: wts(:),wtr(:,:,:)
    ! --------------------------------
    ALLOCATE(wts(1:N_TIC_PER_CYC), &
             wtr(1:N_TIC_PER_CYC,1:N_WVT,V_SQW:V_TRI),STAT=ms)
    IF (ms.NE.0) GOTO 900

    IF (PFL_VERB) WRITE(*,700) 'Initializing oscillator bank'
    lra=.FALSE.  ; IF (PRESENT(ra)) lra=ra
    ob%smpr=DSMP ; IF(PRESENT(sr)) ob%smpr=sr

    IF (lra) THEN
       IF (PFL_VERB) WRITE(*,700) 'Oscillator accumulators randomized'    
       CALL RANDOM_SEED(SIZE=zseed)
       CALL RANDOM_SEED(PUT=(/(DRNS,NSEED=1,zseed)/))
    END IF

    fc1=REAL(N_TIC_PER_CYC,RKIND)/REAL(ob%smpr,RKIND)
    !$OMP PARALLEL DO
    DO j=1,N_OSC
       ob%freq(j)=OscFreq(j)
       ob%incr(j)=fc1*ob%freq(j)
       ob%accm(j)=OscAccm(lra)
       ob%wtno(j)=WvtNmbr(j)       
    END DO
    !$OMP END PARALLEL DO
    IF (PFL_VERB) WRITE(*,700) 'Oscillator accumulators initialized'    

    IF (PFL_VERB) WRITE(*,700) 'Initializing wavetables'    
    CALL WVFSIN(ob%freq(1),REAL(ob%smpr,RKIND),N_TIC_PER_CYC,WTS)    
    !$OMP PARALLEL DO
    DO j=1,N_WVT
       CALL WVFSQR(ob%freq(OscNmbr(j)), REAL(ob%smpr,RKIND), &
            N_TIC_PER_CYC,wtr(1:N_TIC_PER_CYC,j,V_SQW))
       CALL WVFSAW(ob%freq(OscNmbr(j)), REAL(ob%smpr,RKIND), &
            N_TIC_PER_CYC,wtr(1:N_TIC_PER_CYC,j,V_SWT))
       CALL WVFTRI(ob%freq(OscNmbr(j)), REAL(ob%smpr,RKIND), &
            N_TIC_PER_CYC,wtr(1:N_TIC_PER_CYC,j,V_TRI))
    END DO
    !$OMP END PARALLEL DO
    ob%tsin=>wts
    ob%tsqw=>wtr(1:N_TIC_PER_CYC,1:N_WVT,V_SQW)
    ob%tswt=>wtr(1:N_TIC_PER_CYC,1:N_WVT,V_SWT)
    ob%ttri=>wtr(1:N_TIC_PER_CYC,1:N_WVT,V_TRI)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_SIN)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_SQW)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_SWT)
    CALL OscBank_Update(ob,(/(.TRUE.,I=1,N_OSC)/),V_TRI)

    IF (PFL_VERB) WRITE(*,700) 'Wavetables initialized'
    IF (PFL_VERB) WRITE(*,700) 'Done!'    
    ! --- END CODE ---
    RETURN
700 FORMAT('OscBank_Init:',1x,A)
800 FORMAT('* ERROR (OscBank_Init):',1x,A)
900 WRITE(*,800) 'Out of memory' ; STOP

  CONTAINS

    ELEMENTAL FUNCTION OscFreq(n) RESULT(f)
      IMPLICIT NONE
      ! ----------------------
      REAL(KIND=RKIND)   :: f
      INTEGER,INTENT(IN) :: n
      ! ----------------------
      REAL(KIND=RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_OSC_PER_OCT,RKIND)
      REAL(KIND=RKIND)   :: x
      ! ----------------------
      x=REAL(n-REFERENCE_SMT_NUM*N_OSC_PER_SMT,RKIND)*rdenom
      f=REAL(FRQ_REFERENCE_OSC,RKIND)*2.0_RKIND**x
    END FUNCTION OscFreq

    FUNCTION OscAccm(r) RESULT(a)
      IMPLICIT NONE
      ! ------------------------------
      REAL(KIND=RKIND)   :: a
      LOGICAL,INTENT(IN) :: r
      ! ------------------------------
      IF (r) THEN
         CALL RANDOM_NUMBER(a)
         a=a*REAL(N_TIC_PER_CYC,RKIND)
      ELSE
         a=0
      END IF      
    END FUNCTION OscAccm
  END SUBROUTINE OscBank_Init

  SUBROUTINE OscBank_Clear(ob)
    IMPLICIT NONE
    ! --------------------------------
    TYPE(OscBank),INTENT(INOUT) :: ob
    ! --------------------------------
    IF (ASSOCIATED(ob%tsin)) DEALLOCATE(ob%tsin)
    IF (ASSOCIATED(ob%tsqw)) DEALLOCATE(ob%tsqw)
    IF (ASSOCIATED(ob%tswt)) DEALLOCATE(ob%tswt)
    IF (ASSOCIATED(ob%ttri)) DEALLOCATE(ob%ttri)
    ob=OscBank()
  END SUBROUTINE OscBank_Clear

  SUBROUTINE OscBank_Update(ob,msk,vce)
    IMPLICIT NONE
    ! ------------------------------------------
    TYPE(OscBank),INTENT(INOUT) :: ob
    LOGICAL      ,INTENT(IN)    :: msk(1:N_OSC)
    INTEGER      ,INTENT(IN)    :: vce
    ! ------------------------------------------
    INTEGER          :: j,x0,x1
    REAL(KIND=RKIND) :: y0,y1
    ! ------------------------------------------
    IF (vce.EQ.V_SIN) THEN
       !$OMP PARALLEL DO PRIVATE(x0,x1,y0,y1)
       DO j=1,N_OSC
          IF (msk(j)) THEN
             x0=INT(ob%accm(j))
             ! .................................................................
             y0=ob%tsin(x0) ; y1=ob%tsin(x1)
             ob%vval(j,V_SIN)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          END IF
       END DO
       !$OMP END PARALLEL DO
    ELSE IF (vce.EQ.V_SQW) THEN
       !$OMP PARALLEL DO PRIVATE(x0,x1,y0,y1)
       DO j=1,N_OSC
          IF (msk(j)) THEN
             x0=INT(ob%accm(j))
             ! .................................................................
             y0=ob%tsqw(x0,ob%wtno(j)) ; y1=ob%tsqw(x1,ob%wtno(j))
             ob%vval(j,V_SQW)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          END IF
       END DO
       !$OMP END PARALLEL DO       
    ELSE IF (vce.EQ.V_SWT) THEN
       !$OMP PARALLEL DO PRIVATE(x0,x1,y0,y1)
       DO j=1,N_OSC
          IF (msk(j)) THEN
             x0=INT(ob%accm(j))
             ! .................................................................
             y0=ob%tswt(x0,ob%wtno(j)) ; y1=ob%tswt(x1,ob%wtno(j))
             ob%vval(j,V_SWT)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          END IF
       END DO
       !$OMP END PARALLEL DO
    ELSE IF (vce.EQ.V_TRI) THEN
       !$OMP PARALLEL DO PRIVATE(x0,x1,y0,y1)
       DO j=1,N_OSC
          IF (msk(j)) THEN
             x0=INT(ob%accm(j))
             ! .................................................................
             y0=ob%ttri(x0,ob%wtno(j)) ; y1=ob%ttri(x1,ob%wtno(j))
             ob%vval(j,V_TRI)=Yli(REAL(x0,RKIND),ob%accm(j),y0,y1)
             ! .................................................................
          END IF
       END DO
       !$OMP END PARALLEL DO       
    ELSE
       GOTO 900
    END IF
    RETURN
    
800 FORMAT('* ERROR (OscBank_Update):',1x,A)
900 WRITE(*,800) 'Invalid voice number' ; STOP
    
  CONTAINS
    
    ELEMENTAL FUNCTION Yli(x0,x,y0,y1) RESULT(y)
      IMPLICIT NONE
      ! ------------------------------------------
      REAL(KIND=RKIND)            :: y
      REAL(KIND=RKIND),INTENT(IN) :: x0,x,y0,y1
      ! ------------------------------------------
      REAL(KIND=RKIND),PARAMETER :: rdenom=1.0_RKIND/REAL(N_TIC_PER_CYC,RKIND)
      REAL(KIND=RKIND) :: m,c
      ! ------------------------------------------
      m=(y1-y0)*rdenom ! Avoids expensive division
      c=y0-m*x0
      y=m*x+c
    END FUNCTION Yli
  END SUBROUTINE OscBank_Update

  SUBROUTINE OscBank_Tick(ob,msk)
    IMPLICIT NONE
    ! ------------------------------------------
    TYPE(OscBank),INTENT(INOUT) :: ob
    LOGICAL      ,INTENT(IN)    :: msk(1:N_OSC)
    OPTIONAL :: msk
    ! ------------------------------------------
    INTEGER :: j
    LOGICAL :: lmsk(1:N_OSC)
    ! ------------------------------------------
    lmsk=.TRUE. ; IF (PRESENT(msk)) lmsk=msk
    !$OMP PARALLEL DO
    DO j=1,N_OSC
       IF (.NOT.lmsk(j)) CYCLE
       ob%accm(j)=ob%accm(j)+ob%incr(j)
       IF (ob%accm(j).GT.N_TIC_PER_CYC) ob%accm(j)=ob%accm(j)-N_TIC_PER_CYC
    END DO
    !$OMP END PARALLEL DO
  END SUBROUTINE OscBank_Tick
    
END MODULE MZOsc
