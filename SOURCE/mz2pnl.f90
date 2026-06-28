! ------------------------------------------------------------------------------
! MZ2PNL.F90
!
! GRAPHICAL INPUT PANEL DATA STRUCTURES SUBROUTINES FOR MZ2SYNTH.
!
! COPYRIGHT (C) 2025 BY E. LAMPRECHT - ALL RIGHTS RESERVED.  
! ------------------------------------------------------------------------------

MODULE MZ2Pnl
  USE Constant
  USE PFlags
  USE FImageMod, ONLY: FImage,FIM_read,FIM_write,FIM_stf_hflip,    &
                       FIM_clear,FIM_error_ok,Luminance,           &
                       FIM_flt_gauss, FIM_flt_edged
                            
  IMPLICIT NONE

  INTEGER,PARAMETER :: NVCH=ZCHS

  TYPE :: MZ2Panel
     INTEGER                  :: CS=0
     TYPE(FImage)             :: PI
     REAL(KIND=RKIND)         :: VZERO=1.0_RKIND/(2**16)
     REAL(KIND=RKIND)         :: SCANRT=DADV
     REAL(KIND=RKIND)         :: SMPLRT=DSMP
     REAL(KIND=RKIND)         :: RMCNST     =1.0_RKIND/(DFTZ*DSMP)
     REAL(KIND=RKIND),POINTER :: DLUM(:,:)=>NULL()
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

#define SAFEFREE(W) IF (ASSOCIATED(W)) DEALLOCATE(W) ; NULLIFY(W)

  SUBROUTINE MZ2Pnl_Clear(P)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    TYPE(MZ2Panel),INTENT(INOUT) :: P
    ! --- EXE CODE ---
    ! First free image PI if it exists - it will just ignore if not associated.
    CALL FIM_clear(P%PI)
    IF (PFL_VERB) WRITE(*,700) 'Closed graphic input file.'
    ! Then try to dellocate LUM data - this is in case a channel had been
    ! linked to luminance, which would have its own data allocated aside from
    ! that in the image. 
    SAFEFREE(P%DLUM)
    ! The rest is obvious - this is all just data allocated in the MZ2Panel.
    SAFEFREE(P%ASINE)
    SAFEFREE(P%ASQWV)
    SAFEFREE(P%ASWTH)
    SAFEFREE(P%ATRNG)
    SAFEFREE(P%WSINE)
    SAFEFREE(P%WSQWV)
    SAFEFREE(P%WSWTH)
    SAFEFREE(P%WTRNG)
    P=MZ2Panel()
    IF (PFL_VERB) WRITE(*,700) 'Panel memory deallocated and structure cleared.'
    RETURN
    ! --- END CODE ---
700 FORMAT('*INF (Mz2Pnl_Clear):',1X,A,999(:,1X,G12.5,1X,:,A))    
  END SUBROUTINE MZ2Pnl_Clear

  SUBROUTINE MZ2Pnl_Load(P,IFN,CHS,SCANRT,SMPLRT,TCFRAC,WWBLUR,WWEDGE)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    TYPE(Mz2Panel)  ,INTENT(INOUT) :: P
    CHARACTER(LEN=*),INTENT(IN)    :: IFN
    CHARACTER(LEN=4),INTENT(IN)    :: CHS    
    REAL(KIND=RKIND),INTENT(IN)    :: SCANRT,SMPLRT,TCFRAC
    INTEGER,OPTIONAL,INTENT(IN)    :: WWBLUR,WWEDGE    
    ! --- VARIABLES ---
    CHARACTER(LEN=ZSTR) :: DIFN
    INTEGER,PARAMETER   :: EDWD=4 ! NB: this MUST always be even
    INTEGER             :: MS,WB,WE
    ! --- EXE CODE ---
    P%SCANRT=SCANRT
    P%SMPLRT=SMPLRT
    IF (PFL_VERB) WRITE(*,700) 'Reading input file '//TRIM(IFN)//'...'
    CALL FIM_read(P%PI,IFN,IFU)
    IF (PFL_VERB) WRITE(*,700) 'Done!'
    IF (P%PI%ecode.NE.FIM_error_ok) GOTO 900
    ! If Gaussian Blur is specified via WWBLUR > 0 then do it
    IF (PRESENT(WWBLUR)) THEN       
       WB=MAX(0,MIN(WWBLUR+1-MODULO(WWBLUR,2), &
                MIN(P%PI%ncols,P%PI%nrows)-1+MODULO(WWBLUR,2)))
       IF (WB.LE.1) THEN
          WRITE(*,700) 'Skipping Gaussian Blur, WB=',WB
       ELSE
          WRITE(*,700) 'Applying Gaussian Blur, WB=',WB
          CALL FIM_flt_gauss(P%PI,WB)
          WRITE(*,700) 'Done!'
       END IF
    END IF
    ! If DoG Edge Detection is specified via WWEDGE > 0 then do it
    IF (PRESENT(WWEDGE)) THEN
       WE=MAX(0,MIN(WWEDGE+1-MODULO(WWEDGE,2), &
                MIN(P%PI%ncols,P%PI%nrows)-1+MODULO(WWEDGE,2)))
       IF (WE.LE.1) THEN
          WRITE(*,700) 'Skipping Edge Detect, WE=',WE
       ELSE
          WRITE(*,700) 'Applying Edge Detect, WE=',WE
          CALL FIM_flt_edged(P%PI,1,WE)
          WRITE(*,700) 'Done!'
       END IF
    END IF
    ! In DEBUG mode, write image file for viewing if there was a graphical
    ! filter applied above.    
    IF (PFL_DBUG.AND.(WB.GT.1.OR.WE.GT.1)) THEN
       DIFN='DEBUG-'//IFN
       WRITE(*,700) 'Writing processed image to file '//TRIM(DIFN)
       CALL FIM_write(P%PI,TRIM(DIFN),DFU)
       IF (P%PI%ecode.EQ.FIM_error_ok) THEN
          WRITE(*,700) 'Done!'
       ELSE
          WRITE(*,700) 'Cannot write processed image file on unit',DFU
       END IF
    END IF
    ! Do horizontal flip so that origin is bottom left of original image
    IF (PFL_VERB) WRITE(*,700) 'Executing flip across horizontal line...'
    CALL FIM_stf_hflip(P%PI)
    IF (PFL_VERB) WRITE(*,700) 'Done!'
    SELECT CASE(CHS(1:1))       
    CASE('R')
       P%DTSINE=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,700) 'Associated sine oscillators with red channel'
    CASE('G')
       P%DTSINE=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,700) 'Associated sine oscillators with grn channel'
    CASE('B')
       P%DTSINE=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,700) 'Associated sine oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(P%DLUM)) THEN
          ALLOCATE(P%DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          P%DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
       END IF
       P%DTSINE=>P%DLUM
       IF (PFL_VERB) WRITE(*,700) 'Associated sine oscillators with lum channel'
    CASE('M')
       P%DTSINE=>NULL()
       IF (PFL_VERB) WRITE(*,700) 'Sine oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    SELECT CASE(CHS(2:2))
    CASE('R')
       P%DTSQWV=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,700) 'Associated sqwv oscillators with red channel'
    CASE('G')
       P%DTSQWV=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,700) 'Associated sqwv oscillators with grn channel'
    CASE('B')
       P%DTSQWV=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,700) 'Associated sqwv oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(P%DLUM)) THEN
          ALLOCATE(P%DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          P%DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
       END IF
       P%DTSQWV=>P%DLUM
       IF (PFL_VERB) WRITE(*,700) 'Associated sqwv oscillators with lum channel'
    CASE('M')
       P%DTSQWV=>NULL()
       IF (PFL_VERB) WRITE(*,700) 'Sqwv oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    SELECT CASE(CHS(3:3))
    CASE('R')
       P%DTSWTH=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,700) 'Associated swth oscillators with red channel'
    CASE('G')
       P%DTSWTH=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,700) 'Associated swth oscillators with grn channel'
    CASE('B')
       P%DTSWTH=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,700) 'Associated swth oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(P%DLUM)) THEN
          ALLOCATE(P%DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          P%DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
       END IF
       P%DTSWTH=>P%DLUM
       IF (PFL_VERB) WRITE(*,700) 'Associated swth oscillators with lum channel'
    CASE('M')
       P%DTSWTH=>NULL()
       IF (PFL_VERB) WRITE(*,700) 'Swth oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    SELECT CASE(CHS(4:4))
    CASE('R')
       P%DTTRNG=>P%PI%DRED
       IF (PFL_VERB) WRITE(*,700) 'Associated trng oscillators with red channel'
    CASE('G')
       P%DTTRNG=>P%PI%DGRN
       IF (PFL_VERB) WRITE(*,700) 'Associated trng oscillators with grn channel'
    CASE('B')
       P%DTTRNG=>P%PI%DBLU
       IF (PFL_VERB) WRITE(*,700) 'Associated trng oscillators with blu channel'
    CASE('L')
       IF (.NOT.ASSOCIATED(P%DLUM)) THEN
          ALLOCATE(P%DLUM(1:P%PI%NCOLS,1:P%PI%NROWS),STAT=MS)
          IF (MS.NE.0) GOTO 910
          P%DLUM=MERGE(Luminance(P%PI%DRED,P%PI%DGRN,P%PI%DBLU),0.0_RKIND, &
                     P%PI%DRED.GT.0.OR.P%PI%DGRN.GT.0.OR.P%PI%DBLU.GT.0)
       END IF
       P%DTTRNG=>P%DLUM
       IF (PFL_VERB) WRITE(*,700) 'Associated trng oscillators with lum channel'
    CASE('M')
       P%DTTRNG=>NULL()
       IF (PFL_VERB) WRITE(*,700) 'Trng oscillators are muted'
    CASE DEFAULT
       GOTO 920
    END SELECT
    ! --------------------------------------------------------------------------
    ! Based on the allocation status of P%DT*, allocate P%W* and P%A* and
    ! and initialize
    ! --------------------------------------------------------------------------
    IF (PFL_VERB) WRITE(*,700) 'Memory allocation and initialization ...'
    IF (ASSOCIATED(P%DTSINE)) THEN
       ALLOCATE(P%ASINE(1:P%PI%NROWS),P%WSINE(1:P%PI%NROWS),STAT=MS)
       IF (MS.NE.0) GOTO 910
       P%ASINE=0 ; P%WSINE=0 ;
    END IF
    IF (ASSOCIATED(P%DTSQWV)) THEN
       ALLOCATE(P%ASQWV(1:P%PI%NROWS),P%WSQWV(1:P%PI%NROWS),STAT=MS)
       IF (MS.NE.0) GOTO 910
       P%ASQWV=0 ; P%WSQWV=0 ;
    END IF
    IF (ASSOCIATED(P%DTSWTH)) THEN
       ALLOCATE(P%ASWTH(1:P%PI%NROWS),P%WSWTH(1:P%PI%NROWS),STAT=MS)
       IF (MS.NE.0) GOTO 910
       P%ASWTH=0 ; P%WSWTH=0 ;
    END IF
    IF (ASSOCIATED(P%DTTRNG)) THEN
       ALLOCATE(P%ATRNG(1:P%PI%NROWS),P%WTRNG(1:P%PI%NROWS),STAT=MS)
       IF (MS.NE.0) GOTO 910
       P%ATRNG=0 ; P%WTRNG=0 ;
    END IF
    IF (PFL_VERB) WRITE(*,700) 'Done!'
    IF (TCFRAC.LE.0.0_RKIND.OR.TCFRAC.GT.0.5_RKIND) GOTO 930
    P%RMCNST=1.0_RKIND/(TCFRAC*P%SMPLRT)
    RETURN
    ! --- END CODE ---
700 FORMAT('*INF (Mz2Pnl_Load):',1X,A,999(:,1X,G12.5,1X,:,A))
800 FORMAT('*ERR (Mz2Pnl_Load):',1X,A,999(:,1X,G12.5,1X,:,A))
900 WRITE(*,800) 'Cannot read image file '//TRIM(IFN)                     ; STOP
910 WRITE(*,800) 'Out of memory'                                          ; STOP
920 WRITE(*,800) 'Invalid colour-channel mapping (expecting RGBLM)'       ; STOP
930 WRITE(*,800) 'Invalid TCPERC'                                         ; STOP
  END SUBROUTINE MZ2Pnl_Load

  SUBROUTINE MZ2Pnl_Tick(P,DONE)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    TYPE(Mz2Panel),INTENT(INOUT) :: P
    LOGICAL       ,INTENT(INOUT) :: DONE
    ! --- VARIABLES ---
    INTEGER :: CC,NS,TI,I
    ! --- EXE CODE ---
    DONE=.FALSE.
    TI=NINT(SIGN(1.0_RKIND,P%SCANRT)) ! Increment can be positive or negative
    NS=P%CS+TI
    ! -- Stop if panel goes off image on the left or right side --
    CC=INT((REAL(NS,RKIND)/P%SMPLRT)*P%SCANRT)+1 ! SMPL * S/SMPL * COL/S = COL
    IF (CC.LT.0.OR.CC.GT.P%PI%NCOLS) THEN
       DONE=.TRUE.
       RETURN
    END IF
    ! -- Update current sample value in structure
    P%CS=NS
    ! -- Tick all ratemeters that need it!
    IF (ASSOCIATED(P%DTSINE)) THEN
       DO CONCURRENT(I=LBOUND(P%ASINE,1):UBOUND(P%ASINE,1))
          CALL RM_TICK(P%ASINE(I),P%WSINE(I),P%RMCNST,P%DTSINE(CC,I),P%VZERO)
       END DO
    END IF
    IF (ASSOCIATED(P%DTSQWV)) THEN
       DO CONCURRENT(I=LBOUND(P%ASQWV,1):UBOUND(P%ASQWV,1))
          CALL RM_TICK(P%ASQWV(I),P%WSQWV(I),P%RMCNST,P%DTSQWV(CC,I),P%VZERO)
       END DO
    END IF
    IF (ASSOCIATED(P%DTSWTH)) THEN
       DO CONCURRENT(I=LBOUND(P%ASWTH,1):UBOUND(P%ASWTH,1))
          CALL RM_TICK(P%ASWTH(I),P%WSWTH(I),P%RMCNST,P%DTSWTH(CC,I),P%VZERO)
       END DO
    END IF
    IF (ASSOCIATED(P%DTTRNG)) THEN
       DO CONCURRENT(I=LBOUND(P%ATRNG,1):UBOUND(P%ATRNG,1))
          CALL RM_TICK(P%ATRNG(I),P%WTRNG(I),P%RMCNST,P%DTTRNG(CC,I),P%VZERO)
       END DO
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
