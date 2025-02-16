! ------------------------------------------------------------------------------
! MZ2PNL.F95
! Graphical input panel data structures subroutines for MZ2SYNTH.
! Copyright (C) 2025 by E. Lamprecht - All rights reserved.  
! ------------------------------------------------------------------------------

MODULE MZ2Pnl
  USE Constant
  USE PFlags
  USE FImageMod, ONLY: FImage,FIM_read,FIM_stf_hflip, &
                       FIM_clear,FIM_error_ok,Luminance
  IMPLICIT NONE

  INTEGER,PARAMETER :: NVCH=ZCHS

  TYPE :: MZ2Panel
     INTEGER                  :: CS=0
     TYPE(FImage)             :: PI
     REAL(KIND=RKIND)         :: VZERO=1.0_RKIND/(2**16)
     REAL(KIND=RKIND)         :: SCANRT=DADV
     REAL(KIND=RKIND)         :: SMPLRT=DSMP
     REAL(KIND=RKIND)         :: RMCNST     =1.0_RKIND/(DFTZ*DSMP)
     REAL(KIND=RKIND),POINTER :: DTSINE(:,:)=>NULL()
     REAL(KIND=RKIND),POINTER :: DTSQWV(:,:)=>NULL()
     REAL(KIND=RKIND),POINTER :: DTSWTH(:,:)=>NULL()
     REAL(KIND=RKIND),POINTER :: DTTRNG(:,:)=>NULL()
     REAL(KIND=RKIND),POINTER :: ASINE(:)   =>NULL()
     REAL(KIND=RKIND),POINTER :: ASQWV(:)   =>NULL()
     REAL(KIND=RKIND),POINTER :: ASWTH(:)   =>NULL()
     REAL(KIND=RKIND),POINTER :: ATRNG(:)   =>NULL()     
     REAL(KIND=RKIND),POINTER :: WSINE(:)   =>NULL()
     REAL(KIND=RKIND),POINTER :: WSQWV(:)   =>NULL()
     REAL(KIND=RKIND),POINTER :: WSWTH(:)   =>NULL()
     REAL(KIND=RKIND),POINTER :: WTRNG(:)   =>NULL()
  END TYPE MZ2Panel

CONTAINS

#define FREE(W) IF (ASSOCIATED(W)) DEALLOCATE(W)

  SUBROUTINE MZ2Pnl_Clear(P)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    TYPE(MZ2Panel),INTENT(INOUT) :: P
    ! --- VARIABLES ---
    ! --- EXE CODE ---
    FREE(P%DTSINE) ; FREE(P%DTSQWV) ;
    FREE(P%DTSWTH) ; FREE(P%DTTRNG) ;
    FREE(P%ASINE)  ; FREE(P%ASQWV)  ;
    FREE(P%ASWTH)  ; FREE(P%ATRNG)  ;
    FREE(P%WSINE)  ; FREE(P%WSQWV)  ;
    FREE(P%WSWTH)  ; FREE(P%WTRNG)  ;
    P=MZ2Panel()
    ! --- END CODE ---
  END SUBROUTINE MZ2Pnl_Clear

  SUBROUTINE MZ2Pnl_Load(P,IFN,CHS,SCANRT,SMPLRT,TCFRAC)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    TYPE(Mz2Panel)  ,INTENT(INOUT) :: P
    CHARACTER(LEN=*),INTENT(IN)    :: IFN
    CHARACTER(LEN=4),INTENT(IN)    :: CHS    
    REAL(KIND=RKIND),INTENT(IN)    :: SCANRT,SMPLRT,TCFRAC
    ! --- VARIABLES ---
    REAL(KIND=RKIND),POINTER :: DLUM(:,:)
    INTEGER :: MS
    ! --- EXE CODE ---
    P%SCANRT=SCANRT
    P%SMPLRT=SMPLRT
    IF (PFL_VERB) WRITE(*,600) 'Reading input file '//TRIM(IFN)//'...'
    CALL FIM_read(P%PI,IFN,IFU)
    IF (PFL_VERB) WRITE(*,600) 'Done!'
    IF (P%PI%ecode.NE.FIM_error_ok) GOTO 900
    IF (PFL_VERB) WRITE(*,600) 'Executing flip across horizontal line...'
    CALL FIM_stf_hflip(P%PI)
    IF (PFL_VERB) WRITE(*,600) 'Done!'
    ALLOCATE(P%ASINE(1:P%PI%NROWS),P%ASQWV(1:P%PI%NROWS), &
             P%ASWTH(1:P%PI%NROWS),P%ATRNG(1:P%PI%NROWS), &
             P%WSINE(1:P%PI%NROWS),P%WSQWV(1:P%PI%NROWS), &
             P%WSWTH(1:P%PI%NROWS),P%WTRNG(1:P%PI%NROWS), &
             STAT=MS)
    IF (MS.NE.0) GOTO 910
    IF (PFL_VERB) WRITE(*,600) 'Memory allocated andbeing initialized...'
    !$OMP PARALLEL
    P%ASINE=0 ; P%ASQWV=0 ; P%ASWTH=0 ; P%ATRNG=0
    P%WSINE=0 ; P%WSQWV=0 ; P%WSWTH=0 ; P%WTRNG=0
    !$OMP END PARALLEL
    IF (PFL_VERB) WRITE(*,600) 'Done!'
    IF (MS.NE.0) GOTO 910
    DLUM=>NULL()
    SELECT CASE(CHS(1:1))       
    CASE('R')
       P%DTSINE=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,600) 'Associated sine oscillators with red channel'
    CASE('G')
       P%DTSINE=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,600) 'Associated sine oscillators with grn channel'
    CASE('B')
       P%DTSINE=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,600) 'Associated sine oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(DLUM)) THEN
          ALLOCATE(DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          !$OMP PARALLEL
          DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
          !$OMP END PARALLEL
       END IF
       P%DTSINE=>DLUM
       IF (PFL_VERB) WRITE(*,600) 'Associated sine oscillators with lum channel'
    CASE('M')
       P%DTSINE=>NULL()
       IF (PFL_VERB) WRITE(*,600) 'Sine oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    SELECT CASE(CHS(2:2))
    CASE('R')
       P%DTSQWV=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,600) 'Associated sqwv oscillators with red channel'
    CASE('G')
       P%DTSQWV=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,600) 'Associated sqwv oscillators with grn channel'
    CASE('B')
       P%DTSQWV=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,600) 'Associated sqwv oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(DLUM)) THEN
          ALLOCATE(DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          !$OMP PARALLEL
          DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
          !$OMP END PARALLEL
       END IF
       P%DTSQWV=>DLUM
       IF (PFL_VERB) WRITE(*,600) 'Associated sqwv oscillators with lum channel'
    CASE('M')
       P%DTSQWV=>NULL()
       IF (PFL_VERB) WRITE(*,600) 'Sqwv oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    SELECT CASE(CHS(3:3))
    CASE('R')
       P%DTSWTH=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,600) 'Associated swth oscillators with red channel'
    CASE('G')
       P%DTSWTH=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,600) 'Associated swth oscillators with grn channel'
    CASE('B')
       P%DTSWTH=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,600) 'Associated swth oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(DLUM)) THEN
          ALLOCATE(DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          !$OMP PARALLEL
          DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
          !$OMP END PARALLEL
       END IF
       P%DTSWTH=>DLUM
       IF (PFL_VERB) WRITE(*,600) 'Associated swth oscillators with lum channel'
    CASE('M')
       P%DTSWTH=>NULL()
       IF (PFL_VERB) WRITE(*,600) 'Swth oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    SELECT CASE(CHS(4:4))
    CASE('R')
       P%DTTRNG=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,600) 'Associated trng oscillators with red channel'
    CASE('G')
       P%DTTRNG=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,600) 'Associated trng oscillators with grn channel'
    CASE('B')
       P%DTTRNG=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,600) 'Associated trng oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(DLUM)) THEN
          ALLOCATE(DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          !$OMP PARALLEL
          DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
          !$OMP END PARALLEL
       END IF
       P%DTTRNG=>DLUM
       IF (PFL_VERB) WRITE(*,600) 'Associated trng oscillators with lum channel'
    CASE('M')
       P%DTTRNG=>NULL()
       IF (PFL_VERB) WRITE(*,600) 'Trng oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    IF (TCFRAC.LE.0.0_RKIND.OR.TCFRAC.GT.0.5_RKIND) GOTO 930
    P%RMCNST=1.0_RKIND/(TCFRAC*P%SMPLRT)
    ! --- END CODE ---
    RETURN
600 FORMAT('! INFO(Mz2Pnl_Load):' ,1X,A,999(:,1X,G12.5,1X,:,A))
700 FORMAT('! ERROR(Mz2Pnl_Load):',1X,A,999(:,1X,G12.5,1X,:,A))
900 WRITE(*,700) 'Cannot read image file '//TRIM(IFN)                     ; STOP
910 WRITE(*,700) 'Out of memory'                                          ; STOP
920 WRITE(*,700) 'Invalid colour-channel mapping (expecting RGBLM)'       ; STOP
930 WRITE(*,700) 'Invalid TCPERC'                                         ; STOP
  END SUBROUTINE MZ2Pnl_Load

  SUBROUTINE MZ2Pnl_Tick(P,DONE)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    TYPE(Mz2Panel),INTENT(INOUT) :: P
    LOGICAL       ,INTENT(INOUT) :: DONE
    ! --- VARIABLES ---
    INTEGER :: CC,NS,J
    ! --- EXE CODE ---
    DONE=.FALSE.
    NS=P%CS+1
    CC=INT((REAL(NS,RKIND)/P%SMPLRT)*P%SCANRT)+1 ! COL * S/SMPL * COL/S
    IF (CC.GT.P%PI%NCOLS) THEN
       DONE=.TRUE.
       RETURN
    END IF
    P%CS=NS
    IF (ASSOCIATED(P%DTSINE)) THEN
       !$OMP PARALLEL DO
       DO J=1,P%PI%NROWS
          CALL RM_TICK(P%ASINE(J),P%WSINE(J),P%RMCNST,P%DTSINE(CC,J),P%VZERO)
       END DO
       !$OMP END PARALLEL DO
    END IF
    IF (ASSOCIATED(P%DTSQWV)) THEN
       !$OMP PARALLEL DO
       DO J=1,P%PI%NROWS
          CALL RM_TICK(P%ASQWV(J),P%WSQWV(J),P%RMCNST,P%DTSQWV(CC,J),P%VZERO)
       END DO       
       !$OMP END PARALLEL DO
    END IF
    IF (ASSOCIATED(P%DTSWTH)) THEN
       !$OMP PARALLEL DO
       DO J=1,P%PI%NROWS
          CALL RM_TICK(P%ASWTH(J),P%WSWTH(J),P%RMCNST,P%DTSWTH(CC,J),P%VZERO)
       END DO       
       !$OMP END PARALLEL DO
    END IF
    IF (ASSOCIATED(P%DTTRNG)) THEN
       !$OMP PARALLEL DO
       DO J=1,P%PI%NROWS
          CALL RM_TICK(P%ATRNG(J),P%WTRNG(J),P%RMCNST,P%DTTRNG(CC,J),P%VZERO)
       END DO       
       !$OMP END PARALLEL DO
    END IF    
    ! --- END CODE ---
    
  CONTAINS
    
    !
    ! For a charge-pump input with a constant charge input dq per time
    ! interval dt and a first-order decay constant of -k,
    !
    !        dv(t)              dq
    ! [1]   ------ = -k v(t) + ----
    !         dt                dt
    !
    ! The value of v rises at an ever-decreasing rate until the rate
    ! of decay equals the rate of charge input, i.e., until dv/dt = 0,
    ! at which time, t1,
    !
    !               1    dq          
    ! [2]  v(t1) = ---  ---- 
    !               k    dt 
    !
    ! or equivalently
    !
    !       dq          
    ! [3]  ---- = k v(t1) 
    !       dt 
    !
    ! Clearly the dimension of k must be the reciprocal of the time unit
    ! in which t and dt are expressed.

    ELEMENTAL SUBROUTINE RM_TICK(RMA,RMW,CDC,RMI,VZR)
      IMPLICIT NONE
      ! --- DUMMY ARGS ---
      REAL(KIND=RKIND),INTENT(INOUT) :: RMA
      REAL(KIND=RKIND),INTENT(INOUT) :: RMW
      REAL(KIND=RKIND),INTENT(IN)    :: CDC
      REAL(KIND=RKIND),INTENT(IN)    :: RMI
      REAL(KIND=RKIND),INTENT(IN)    :: VZR
      ! --- EXE CODE ---
      RMA=RMA-CDC*RMA+RMI
      IF (RMA.LE.VZR) RMA=0
      RMW=RMA*CDC
      ! --- END CODE ---
    END SUBROUTINE RM_TICK
  END SUBROUTINE MZ2Pnl_Tick

END MODULE MZ2Pnl
