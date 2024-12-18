! ------------------------------------------------------------------------------
! MZPNL.F95
!
! INPUT 'PANEL' SUBROUTINES FOR MZ2 SYNTHESIZER.
!
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ------------------------------------------------------------------------------

MODULE MZPNL
  USE PFlags
  USE Constant
  USE Logist,   ONLY: LTDATA,LTSCAL
  USE MZOsc,    ONLY: N_OSC, N_TIC_PER_CYC
  USE FImageMod,ONLY: FImage, FIM_read, FIM_clear, FIM_error_ok, Luminance
  IMPLICIT NONE

  TYPE(FImage),SAVE            :: PNLIMG
  REAL(KIND=RKIND),ALLOCATABLE :: DTSINE(:,:)
  REAL(KIND=RKIND),ALLOCATABLE :: DTSQWV(:,:)
  REAL(KIND=RKIND),ALLOCATABLE :: DTSWTH(:,:)
  REAL(KIND=RKIND),ALLOCATABLE :: DTTRNG(:,:)
  INTEGER                      :: NC
  INTEGER                      :: NR
  REAL(KIND=RKIND)             :: N_COL_PER_SEC
  INTEGER                      :: N_SMP_PER_COL
  INTEGER                      :: N_SMP_PER_SEC
  INTEGER                      :: PCOL,CCOL,NCOL,SMP_IN_COL
  REAL(KIND=RKIND)             :: FTRZ=DFTZ
  INTEGER                      :: TRZIDXL,TRZIDXR
  REAL(KIND=RKIND),ALLOCATABLE :: ITRZ(:),TRZ_SINE(:,:),TRZ_SQWV(:,:),  &
                                  TRZ_SWTH(:,:),TRZ_TRNG(:,:)
  REAL(KIND=RKIND),ALLOCATABLE :: WSINE(:),WSQWV(:),WSWTH(:),WTRNG(:)
  INTEGER,PARAMETER            :: NVCH=ZCHS,CHSINE=1,CHSQWV=2,CHSWTH=3, &
                                  CHTRNG=NVCH

CONTAINS

  SUBROUTINE MZPNL_LOAD(IFN,CHS,AFTRZ,ANCLPS,ANSMPR)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    CHARACTER(LEN=*) :: IFN
    CHARACTER(LEN=4) :: CHS
    REAL(KIND=RKIND) :: AFTRZ,ANCLPS
    INTEGER          :: ANSMPR
    INTENT(IN) :: IFN,CHS,AFTRZ
    OPTIONAL   :: AFTRZ
    ! --- PARAMETERS ---
    REAL(KIND=RKIND) :: SIGML=-2,SIGMR=+2
    ! --- VARIABLES ---
    INTEGER :: MS, J, K
    ! --- EXE CODE  ---
    IF (ANCLPS.LT.1) GOTO 950
    N_SMP_PER_SEC=ANSMPR
    N_SMP_PER_COL=NINT(REAL(N_SMP_PER_SEC,RKIND) / ANCLPS)
    N_COL_PER_SEC=REAL(N_SMP_PER_SEC,RKIND) / REAL(N_SMP_PER_COL,RKIND)
    IF (PFL_DBUG) THEN
       WRITE(*,600) 'NO. COLS PER SEC ADJUSTED TO ',N_COL_PER_SEC, &
                    'FROM ',ANCLPS
       WRITE(*,600) 'NO. SAMPLES PER COLUMN SET TO ',N_SMP_PER_COL
    END IF
    CALL FIM_read(PNLIMG,IFN,IFU)
    IF (PNLIMG%ecode.NE.FIM_error_ok) GOTO 900
    IF (PNLIMG%nrows.NE.N_OSC)        GOTO 910
    NC=PNLIMG%NCOLS+1
    NR=PNLIMG%NROWS
    ALLOCATE(DTSINE(0:NC,1:NR),DTSQWV(0:NC,1:NR),        &
             DTSWTH(0:NC,1:NR),DTTRNG(0:NC,1:NR),STAT=MS)
    IF (MS.NE.0) GOTO 920
    DTSINE(0,:)=0 ; DTSINE(NC,:)=0
    DTSQWV(0,:)=0 ; DTSQWV(NC,:)=0
    DTSWTH(0,:)=0 ; DTSWTH(NC,:)=0
    DTTRNG(0,:)=0 ; DTTRNG(NC,:)=0

    ALLOCATE(WSINE(1:N_OSC),WSQWV(1:N_OSC),WSWTH(1:N_OSC),WTRNG(1:N_OSC), &
             STAT=MS)
    IF (MS.NE.0) GOTO 920    

    SELECT CASE(CHS(CHSINE:CHSINE))
    CASE('R')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSINE(J,K)=PNLIMG%DRED(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('G')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSINE(J,K)=PNLIMG%DGRN(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('B')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSINE(J,K)=PNLIMG%DBLU(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('L')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSINE(J,K)=Luminance(PNLIMG%DRED(J,NR-K+1), &
               PNLIMG%DGRN(J,NR-K+1),PNLIMG%DBLU(J,NR-K+1))
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('M')
       DTSINE=0
    CASE DEFAULT
       GOTO 930       
    END SELECT

    SELECT CASE(CHS(CHSQWV:CHSQWV))
    CASE('R')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSQWV(J,K)=PNLIMG%DRED(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('G')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSQWV(J,K)=PNLIMG%DGRN(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('B')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSQWV(J,K)=PNLIMG%DBLU(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('L')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSQWV(J,K)=Luminance(PNLIMG%DRED(J,NR-K+1), &
               PNLIMG%DGRN(J,NR-K+1),PNLIMG%DBLU(J,NR-K+1))
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('M')
       DTSQWV=0
    CASE DEFAULT
       GOTO 930 
    END SELECT

    SELECT CASE(CHS(CHSWTH:CHSWTH))
    CASE('R')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSWTH(J,K)=PNLIMG%DRED(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('G')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSWTH(J,K)=PNLIMG%DGRN(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('B')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSWTH(J,K)=PNLIMG%DBLU(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('L')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTSWTH(J,K)=Luminance(PNLIMG%DRED(J,NR-K+1), &
               PNLIMG%DGRN(J,NR-K+1),PNLIMG%DBLU(J,NR-K+1))
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('M')
       DTSWTH=0
    CASE DEFAULT
       GOTO 930 
    END SELECT

    SELECT CASE(CHS(CHTRNG:CHTRNG))
    CASE('R')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTTRNG(J,K)=PNLIMG%DRED(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('G')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTTRNG(J,K)=PNLIMG%DGRN(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('B')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTTRNG(J,K)=PNLIMG%DBLU(J,NR-K+1)
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('L')
       !$OMP PARALLEL WORKSHARE
       FORALL(J=1:NC-1,K=1:NR)
          DTTRNG(J,K)=Luminance(PNLIMG%DRED(J,NR-K+1), &
               PNLIMG%DGRN(J,NR-K+1),PNLIMG%DBLU(J,NR-K+1))
       END FORALL
       !$OMP END PARALLEL WORKSHARE
    CASE('M')
       DTTRNG=0
    CASE DEFAULT
       GOTO 930 
    END SELECT

    CALL FIM_clear(PNLIMG)

    IF (PRESENT(AFTRZ)) FTRZ=AFTRZ
    TRZIDXR=NINT(FTRZ*REAL(N_SMP_PER_COL,RKIND)/2)
    IF (2*TRZIDXR+1.GT.INT(0.4*REAL(N_SMP_PER_COL,RKIND))) GOTO 940       
    TRZIDXL=-TRZIDXR
    IF (PFL_DBUG) WRITE(*,600) 'TRZIDXL=',TRZIDXL
    IF (PFL_DBUG) WRITE(*,600) 'TRZIDXR=',TRZIDXR
    ALLOCATE(ITRZ(TRZIDXL:TRZIDXR),&
         TRZ_SINE(TRZIDXL:TRZIDXR,1:N_OSC),&
         TRZ_SQWV(TRZIDXL:TRZIDXR,1:N_OSC),&
         TRZ_SWTH(TRZIDXL:TRZIDXR,1:N_OSC),&                  
         TRZ_TRNG(TRZIDXL:TRZIDXR,1:N_OSC),STAT=MS) ; IF (MS.NE.0) GOTO 920
    CALL LTDATA(ITRZ,TRZIDXL,TRZIDXR,SIGML,SIGMR)
    IF (PFL_DBUG) THEN
       WRITE(*,600) 'LOGISTIC FUNCTION LOOKUP TABLE:'
       WRITE(*,600) '------------------------------ '
       DO J=TRZIDXL,TRZIDXR
          WRITE(*,600) 'POINT:',J,'YVALUE:',REAL(ITRZ(J))
       END DO
    END IF

    CCOL=0
    SMP_IN_COL=0
    CALL MZPNL_UPDATE_TRZ(CCOL)
    ! --- END CODE  ---
    RETURN
600 FORMAT('! INFO(MZP_LOAD):' ,1X,A,999(:,1X,G12.5,1X,:,A))
700 FORMAT('! ERROR(MZP_LOAD):',1X,A,999(:,1X,G12.5,1X,:,A))
900 WRITE(*,700) 'PROBLEM READING IMAGE FILE: '//TRIM(IFN)                ; STOP
910 WRITE(*,700) 'NUM ROWS.NE.720:  '//TRIM(IFN)                          ; STOP
920 WRITE(*,700) 'MEMORY ALLOCATION FAILURE '//TRIM(IFN)                  ; STOP
930 WRITE(*,700) 'INVALID CHARACTER IN CHS: '//TRIM(CHS)                  ; STOP
940 WRITE(*,700) 'INVALID FTRZ:  PLEASE SELECT SMALLER VALUE'             ; STOP
950 WRITE(*,700) 'INVALID ANCPS: EXPECTING REAL ANCLPS.GE.1'              ; STOP
  END SUBROUTINE MZPNL_LOAD

  SUBROUTINE MZPNL_CLEAR()
    IMPLICIT NONE
    ! --- EXE CODE ---
    CALL FIM_clear(PNLIMG)
    IF (ALLOCATED(DTSINE))   DEALLOCATE(DTSINE)
    IF (ALLOCATED(DTSQWV))   DEALLOCATE(DTSQWV)
    IF (ALLOCATED(DTSWTH))   DEALLOCATE(DTSWTH)
    IF (ALLOCATED(DTTRNG))   DEALLOCATE(DTTRNG)
    FTRZ=DFTZ
    IF (ALLOCATED(ITRZ))     DEALLOCATE(ITRZ)
    IF (ALLOCATED(TRZ_SINE)) DEALLOCATE(TRZ_SINE)
    IF (ALLOCATED(TRZ_SQWV)) DEALLOCATE(TRZ_SQWV)
    IF (ALLOCATED(TRZ_SWTH)) DEALLOCATE(TRZ_SWTH)
    IF (ALLOCATED(TRZ_TRNG)) DEALLOCATE(TRZ_TRNG)
    IF (ALLOCATED(WSINE))    DEALLOCATE(WSINE)
    IF (ALLOCATED(WSQWV))    DEALLOCATE(WSQWV)
    IF (ALLOCATED(WSWTH))    DEALLOCATE(WSWTH)
    IF (ALLOCATED(WTRNG))    DEALLOCATE(WTRNG)
    ! --- END CODE ---
  END SUBROUTINE MZPNL_CLEAR

  SUBROUTINE MZPNL_TICK()
    ! --------------------------------------------------------------------------
    ! +...+                                                               +....*
    ! 1   |                                                               |
    ! ----+---------------------------------------------------------------+-----
    ! a...bcd............................................................ef....g
    !                                                                          1
    !                                                                    9999990
    ! 1234567............................................................4567890
    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    ! --- EXE CODE ---
    SMP_IN_COL=SMP_IN_COL+1
    IF (SMP_IN_COL.EQ.1) THEN
       ! 1
       PCOL=CCOL
       CCOL=CCOL+1
       NCOL=CCOL+1
       CALL MZPNL_TICK_DATA_TRZ(SMP_IN_COL)
    ELSE IF (SMP_IN_COL.GE.2.AND.SMP_IN_COL.LE.TRZIDXR) THEN
       ! 2 .. 5
       CALL MZPNL_TICK_DATA_TRZ(SMP_IN_COL)
    ELSE IF (SMP_IN_COL.EQ.TRZIDXR+1) THEN
       ! 6
       CALL MZPNL_TICK_DATA_SDS(CCOL)
    ELSE IF (SMP_IN_COL.GE.TRZIDXR+2.AND. &
             SMP_IN_COL.LE.N_SMP_PER_COL+TRZIDXL-1) THEN
       ! 7.. 94
       ! NOP
    ELSE IF (SMP_IN_COL.EQ.N_SMP_PER_COL+TRZIDXL) THEN
       ! 95
       CALL MZPNL_UPDATE_TRZ(CCOL)
       CALL MZPNL_TICK_DATA_TRZ(-(N_SMP_PER_COL-SMP_IN_COL))
    ELSE IF (SMP_IN_COL.GE.N_SMP_PER_COL+TRZIDXL+1.AND. &
             SMP_IN_COL.LE.N_SMP_PER_COL-1) THEN
       ! 96..99
       CALL MZPNL_TICK_DATA_TRZ(-(N_SMP_PER_COL-SMP_IN_COL))
    ELSE IF(SMP_IN_COL.EQ.N_SMP_PER_COL) THEN
       ! 100=*
       CALL MZPNL_TICK_DATA_TRZ(-(N_SMP_PER_COL-SMP_IN_COL))
       SMP_IN_COL=0
    END IF
    ! --- END CODE  ---
  END SUBROUTINE MZPNL_TICK
    
  SUBROUTINE MZPNL_TICK_DATA_TRZ(TP)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    INTEGER,INTENT(IN) :: TP
    ! --- VARIABLES ---
    INTEGER :: J
    ! --- EXE CODE  ---
    !$OMP PARALLEL DO
    DO J=1,N_OSC
       WSINE(J)=TRZ_SINE(TP,J)
       WSQWV(J)=TRZ_SQWV(TP,J)
       WSWTH(J)=TRZ_SWTH(TP,J)
       WTRNG(J)=TRZ_TRNG(TP,J)
    END DO
    !$OMP END PARALLEL DO
    ! --- END CODE  ---
  END SUBROUTINE MZPNL_TICK_DATA_TRZ

  SUBROUTINE MZPNL_TICK_DATA_SDS(CI)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    INTEGER,INTENT(IN) :: CI
    ! --- VARIABLES ---
    INTEGER :: J
    ! --- EXE CODE ---
    !$OMP PARALLEL DO
    DO J=1,N_OSC
       WSINE(J)=DTSINE(CI,J)
       WSQWV(J)=DTSQWV(CI,J)
       WSWTH(J)=DTSWTH(CI,J)
       WTRNG(J)=DTTRNG(CI,J)
    END DO
    !$OMP END PARALLEL DO
    ! --- END CODE ---
  END SUBROUTINE MZPNL_TICK_DATA_SDS

  SUBROUTINE MZPNL_UPDATE_TRZ(CI)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    INTEGER,INTENT(IN) :: CI
    ! --- VARIABLES ---
    INTEGER            :: J
    REAL(KIND=RKIND)   :: NDTSINE,NDTSQWV,NDTSWTH,NDTTRNG
    ! --- EXE CODE ---    
    !$OMP PARALLEL DO PRIVATE(NDTSINE,NDTSQWV,NDTSWTH,NDTTRNG)
    DO J=1,N_OSC
       IF (CI.LT.NCOL) THEN
          NDTSINE=DTSINE(CI+1,J) ; NDTSQWV=DTSQWV(CI+1,J)
          NDTSWTH=DTSWTH(CI+1,J) ; NDTTRNG=DTTRNG(CI+1,J)
       ELSE
          NDTSINE=0 ; NDTSQWV=0 ; NDTSWTH=0 ; NDTTRNG=0
       END IF
       CALL LTSCAL(TRZ_SINE(:,J),ITRZ,TRZIDXL,TRZIDXR,DTSINE(CI,J),NDTSINE)
       CALL LTSCAL(TRZ_SQWV(:,J),ITRZ,TRZIDXL,TRZIDXR,DTSQWV(CI,J),NDTSQWV)
       CALL LTSCAL(TRZ_SWTH(:,J),ITRZ,TRZIDXL,TRZIDXR,DTSWTH(CI,J),NDTSWTH)
       CALL LTSCAL(TRZ_TRNG(:,J),ITRZ,TRZIDXL,TRZIDXR,DTTRNG(CI,J),NDTTRNG)
    END DO
    !$OMP END PARALLEL DO
    ! --- END CODE ---
  END SUBROUTINE MZPNL_UPDATE_TRZ
  
END MODULE MZPNL
