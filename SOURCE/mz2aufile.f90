! ------------------------------------------------------------------------------
! MZ2AUFILE.F90
!
! SUN AUDIO FILE FORMAT HANDLING SUBROUTINES FOR MZ2 (OUTPUT ONLY)
!
! #NEW:2026-05-06a:
!
!      (a)  Added support for 64-bit real samples.
!
!      (b)  Made ZSMP a member of the SunAu struct so that it can be
!           computed once when the structure is first populated and
!           used when needed in the sample-writing routines.
!
! #NEW:2026-05-01a:
!
!      (a)  Added offensively missing support for I32 PCM.
!
!      (b)  To Au_WrtSmp_XXX, added robust checking to make sure these are
!           never used with a SunAu 'object' that has previously written
!           a header that does not match the respective function's sample
!           format.  Previously a programmer could erroneously set up a
!           16 bit PCM audio file and write 32 bit real samples to it and
!           never know it till chaos ensued.
!
!      (c)  Check that in each call to Au_WrtSmp_XXX, the input dummy
!           variable array AUDS(1:) has a size equal to SNAU%NCHN, i.e,
!           that each call contains just one sample for each channel in
!           the header.
!
! COPYRIGHT (C) 2024,2026 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ------------------------------------------------------------------------------

MODULE Mz2AuFile
  USE ISO_C_BINDING
  USE PFlags
  IMPLICIT NONE

  INTEGER(C_INT32_T),PARAMETER :: DFMAGIC=INT(Z'2E736E64',C_INT32_T)  ! AU TYPE
  INTEGER(C_INT32_T),PARAMETER :: HDRSIZE=28                          ! HDR SIZE
  INTEGER(C_INT32_T),PARAMETER :: DATSIZE=INT(Z'FFFFFFFF',C_INT32_T)  ! UNKNOWN

  INTEGER,PARAMETER :: AUF_PCM_LINEAR_16B=3
  INTEGER,PARAMETER :: AUF_PCM_LINEAR_32B=5      ! #NEW:2026-05-01a
  INTEGER,PARAMETER :: AUF_FLT_LINEAR_32B=6
  INTEGER,PARAMETER :: AUF_FLT_LINEAR_64B=7      ! #NEW:2026-05-06a
  INTEGER,PARAMETER :: AUF_DEF_SAMPLERATE=44100
  INTEGER,PARAMETER :: AUF_DEF_NMCHANNELS=2
  LOGICAL,PARAMETER :: AUF_DEF_ENDIANNESS=.TRUE. ! Big-endian by default

  TYPE :: SunAu
     INTEGER            :: AUFU=0                   ! Audio file unit
     INTEGER(C_INT32_T) :: ENCD=AUF_FLT_LINEAR_32B
     INTEGER(C_INT32_T) :: SMPR=AUF_DEF_SAMPLERATE
     INTEGER(C_INT32_T) :: NCHN=AUF_DEF_NMCHANNELS
     LOGICAL            :: MCBE=AUF_DEF_ENDIANNESS
     LOGICAL            :: OVWT=.FALSE.
     INTEGER            :: BCUR=1                   ! Buffer cursor
     INTEGER            :: ZSMP=0 !#NEW:2026-05-06a ! Sample-size in bytes
     INTEGER(KIND=C_INT8_T),POINTER :: BUFF(:)=>NULL()
  END TYPE SunAu

  INTERFACE Au_WrtSmp
     ! -------------------------------------------------------------------------
     ! Buffered routines for writing audio samples to file.  Using a
     ! subroutine that does not match the format that was stored in
     ! SunAu%ENCD during the header write operation will halt the program
     ! with an error.
     ! -------------------------------------------------------------------------
     MODULE PROCEDURE Au_WrtSmp_I16
     MODULE PROCEDURE Au_WrtSmp_I32 ! #NEW:2026-05-01a
     MODULE PROCEDURE Au_WrtSmp_R32
     MODULE PROCEDURE Au_WrtSmp_R64 ! #NEW:2026-05-06a
  END INTERFACE Au_WrtSmp

  INTERFACE Nord
     ! -------------------------------------------------------------------------
     ! Byte-swapping routines - if the host is little-endian the byte order will
     ! be swapped to big-endian as necessary.
     ! -------------------------------------------------------------------------
     MODULE PROCEDURE Nord_I16
     MODULE PROCEDURE Nord_I32
     MODULE PROCEDURE Nord_R32
     MODULE PROCEDURE Nord_R64 ! #NEW:2026-05-06a
  END INTERFACE Nord
  
CONTAINS

  SUBROUTINE Au_WrtHdr(SNAU,AUFN,AUFU,ENCD,SMPR,NCHN,OVWT,STAT) 
    IMPLICIT NONE
    TYPE(SunAu)        :: SNAU
    CHARACTER(LEN=*)   :: AUFN
    INTEGER            :: AUFU
    INTEGER(C_INT32_T) :: ENCD,SMPR,NCHN
    LOGICAL            :: OVWT
    INTEGER            :: STAT
    INTENT(IN)    :: AUFN,AUFU,ENCD,SMPR,NCHN,OVWT
    INTENT(INOUT) :: SNAU,STAT
    ! --- VARIABLES ---
    INTEGER :: ZBUF,MS
    CHARACTER(LEN=:),ALLOCATABLE :: FSTA
    ! --- EXE CODE  ---
    SELECT CASE (ENCD)
    CASE (AUF_PCM_LINEAR_16B)
       SNAU%ZSMP=2
    CASE (AUF_PCM_LINEAR_32B,AUF_FLT_LINEAR_32B)
       SNAU%ZSMP=4       
    CASE (AUF_FLT_LINEAR_64B) ! #NEW:2026-05-06a
       SNAU%ZSMP=8
    CASE DEFAULT
       GOTO 900
    END SELECT
    IF (SMPR.LT.8000.OR.SMPR.GT.48000) GOTO 910
    IF (NCHN.LT.1.OR.NCHN.GT.2)        GOTO 920
    SNAU%AUFU=AUFU
    SNAU%ENCD=ENCD
    SNAU%SMPR=SMPR
    SNAU%NCHN=NCHN
    SNAU%MCBE=MachBE()
    SNAU%OVWT=OVWT
    ZBUF=SNAU%ZSMP*SNAU%SMPR*SNAU%NCHN
    ALLOCATE(SNAU%BUFF(1:ZBUF),STAT=MS)
    IF (MS.NE.0) GOTO 930
    IF (OVWT) THEN
       FSTA='REPLACE'
    ELSE
       FSTA='NEW'
    END IF
    OPEN(FILE=AUFN,UNIT=SNAU%AUFU,FORM='UNFORMATTED',ACCESS='STREAM', &
         POSITION='REWIND',ACTION='WRITE',STATUS=FSTA,IOSTAT=STAT)
    IF (STAT.NE.0) RETURN
    IF (PFL_VERB) WRITE(*,700) 'File '//TRIM(AUFN)//' opened in mode '&
                                      //TRIM(FSTA)//' on unit',SNAU%AUFU
    WRITE(SNAU%AUFU,IOSTAT=STAT) &
         Nord_I32((/DFMAGIC,HDRSIZE,DATSIZE,ENCD,SMPR,NCHN,0/),SNAU%MCBE)
    IF (PFL_VERB) THEN
       IF (.NOT.SNAU%MCBE) THEN
          WRITE(*,700) 'Hardware is little-endian so samples will be converted'
       END IF
       WRITE(*,700) 'Audio file header write status is',STAT
    END IF
    RETURN
    ! --- END CODE  ---
700 FORMAT('*INF (Au_WrtHdr):',1X,A,:,999(1x,I0))
800 FORMAT('*ERR (Au_WrtHdr): INVALID',1X,A,1X,'=',I0)
810 FORMAT('*ERR (Au_WrtHdr):',1X,A)
900 WRITE(*,800) 'ENCD',ENCD ; STOP    
910 WRITE(*,800) 'SMPR',SMPR ; STOP
920 WRITE(*,800) 'NCHN',NCHN ; STOP
930 WRITE(*,810) 'BUFFER ALLOCATION FAILED' ; STOP 
  END SUBROUTINE Au_WrtHdr

  SUBROUTINE Au_WrtSmp_I16(SNAU,AUDS,STAT)
    IMPLICIT NONE
    TYPE(SunAu)             :: SNAU
    INTEGER(KIND=C_INT16_T) :: AUDS(1:) ! Audio sample array (SIZE=SNAU%NCHN)
    INTEGER                 :: STAT     ! Write IOSTAT
    INTENT(IN)    :: AUDS
    INTENT(INOUT) :: SNAU,STAT
    ! --- VARIABLES ---
    INTEGER :: BS,BE,ZTFR
    ! --- EXE CODE  ---
    IF (SNAU%ENCD.NE.AUF_PCM_LINEAR_16B) GOTO 900 ! #NEW:2026-05-01a
    IF (SNAU%NCHN.NE.SIZE(AUDS))         GOTO 910 ! #NEW:2026-05-01a
    STAT=0 ! Fake out as OK unless there REALLY was a write error
    IF (SNAU%BCUR.GT.UBOUND(SNAU%BUFF,1)) THEN
       WRITE(SNAU%AUFU,IOSTAT=STAT) SNAU%BUFF
       IF (STAT.NE.0) RETURN
       SNAU%BCUR=1
    END IF
    ZTFR=SNAU%ZSMP*SIZE(AUDS)
    BS=SNAU%BCUR
    BE=SNAU%BCUR+ZTFR-1
    SNAU%BUFF(BS:BE)=TRANSFER(Nord_I16(AUDS,SNAU%MCBE),SNAU%BUFF(BS:BE))
    SNAU%BCUR=BE+1
    RETURN
    ! --- END CODE  --- #NEW:2026-05-01a
800 FORMAT('*ERR (Au_WrtSmp_I16):',1X,A,1X,I0)
900 WRITE(*,800) 'Invalid call with encoding format=',SNAU%ENCD ; STOP
910 WRITE(*,800) 'Invalid call with SIZE(AUDS).NE.SNAU%NCHN=',SNAU%NCHN ; STOP
  END SUBROUTINE Au_WrtSmp_I16

  SUBROUTINE Au_WrtSmp_I32(SNAU,AUDS,STAT) ! #NEW:2026-05-01a
    IMPLICIT NONE
    TYPE(SunAu)             :: SNAU
    INTEGER(KIND=C_INT32_T) :: AUDS(1:) ! Audio sample array (SIZE=SNAU%NCHN)
    INTEGER                 :: STAT     ! Write IOSTAT
    INTENT(IN)    :: AUDS
    INTENT(INOUT) :: SNAU,STAT
    ! --- VARIABLES ---
    INTEGER :: BS,BE,ZTFR
    ! --- EXE CODE  ---
    IF (SNAU%ENCD.NE.AUF_PCM_LINEAR_32B) GOTO 900 ! #NEW:2026-05-01a
    IF (SNAU%NCHN.NE.SIZE(AUDS))         GOTO 910 ! #NEW:2026-05-01a
    STAT=0 ! Fake out as OK unless there REALLY was a write error
    IF (SNAU%BCUR.GT.UBOUND(SNAU%BUFF,1)) THEN
       WRITE(SNAU%AUFU,IOSTAT=STAT) SNAU%BUFF
       IF (STAT.NE.0) RETURN
       SNAU%BCUR=1
    END IF
    ZTFR=SNAU%ZSMP*SIZE(AUDS)
    BS=SNAU%BCUR
    BE=SNAU%BCUR+ZTFR-1
    SNAU%BUFF(BS:BE)=TRANSFER(Nord_I32(AUDS,SNAU%MCBE),SNAU%BUFF(BS:BE))
    SNAU%BCUR=BE+1
    RETURN
    ! --- END CODE  --- #NEW:2026-05-01a
800 FORMAT('*ERR (Au_WrtSmp_I32):',1X,A,1X,I0)
900 WRITE(*,800) 'Invalid call with encoding format=',SNAU%ENCD ; STOP
910 WRITE(*,800) 'Invalid call with SIZE(AUDS).NE.SNAU%NCHN=',SNAU%NCHN ; STOP
  END SUBROUTINE Au_WrtSmp_I32
  
  SUBROUTINE Au_WrtSmp_R32(SNAU,AUDS,STAT)
    IMPLICIT NONE
    TYPE(SunAu)             :: SNAU
    REAL(KIND=C_FLOAT)      :: AUDS(1:) ! Audio sample array (SIZE=SNAU%NCHN)
    INTEGER                 :: STAT     ! Write IOSTAT
    INTENT(IN)    :: AUDS
    INTENT(INOUT) :: SNAU,STAT
    ! --- VARIABLES ---
    INTEGER :: BS,BE,ZTFR
    ! --- EXE CODE  ---
    IF (SNAU%ENCD.NE.AUF_FLT_LINEAR_32B) GOTO 900 ! #NEW:2026-05-01a
    IF (SNAU%NCHN.NE.SIZE(AUDS))         GOTO 910 ! #NEW:2026-05-01a
    STAT=0 ! Fake out as OK unless there REALLY was a write error
    IF (SNAU%BCUR.GT.UBOUND(SNAU%BUFF,1)) THEN
       WRITE(SNAU%AUFU,IOSTAT=STAT) SNAU%BUFF
       IF (STAT.NE.0) RETURN
       SNAU%BCUR=1
    END IF
    ZTFR=SNAU%ZSMP*SIZE(AUDS)
    BS=SNAU%BCUR
    BE=SNAU%BCUR+ZTFR-1
    SNAU%BUFF(BS:BE)=TRANSFER(Nord_R32(AUDS,SNAU%MCBE),SNAU%BUFF(BS:BE))
    SNAU%BCUR=BE+1
    RETURN
    ! --- END CODE  --- #NEW:2026-05-01a
800 FORMAT('*ERR (Au_WrtSmp_R32):',1X,A,1X,I0)
900 WRITE(*,800) 'Invalid call with encoding format=',SNAU%ENCD ; STOP
910 WRITE(*,800) 'Invalid call with SIZE(AUDS).NE.SNAU%NCHN=',SNAU%NCHN ; STOP
  END SUBROUTINE Au_WrtSmp_R32

  SUBROUTINE Au_WrtSmp_R64(SNAU,AUDS,STAT) ! #NEW:2026-05-06a
    IMPLICIT NONE
    TYPE(SunAu)             :: SNAU
    REAL(KIND=C_DOUBLE)     :: AUDS(1:) ! Audio sample array (SIZE=SNAU%NCHN)
    INTEGER                 :: STAT     ! Write IOSTAT
    INTENT(IN)    :: AUDS
    INTENT(INOUT) :: SNAU,STAT
    ! --- VARIABLES ---
    INTEGER :: BS,BE,ZTFR
    ! --- EXE CODE  ---
    IF (SNAU%ENCD.NE.AUF_FLT_LINEAR_64B) GOTO 900
    IF (SNAU%NCHN.NE.SIZE(AUDS))         GOTO 910
    STAT=0 ! Fake out as OK unless there REALLY was a write error
    IF (SNAU%BCUR.GT.UBOUND(SNAU%BUFF,1)) THEN
       WRITE(SNAU%AUFU,IOSTAT=STAT) SNAU%BUFF
       IF (STAT.NE.0) RETURN
       SNAU%BCUR=1
    END IF
    ZTFR=SNAU%ZSMP*SIZE(AUDS)
    BS=SNAU%BCUR
    BE=SNAU%BCUR+ZTFR-1
    SNAU%BUFF(BS:BE)=TRANSFER(Nord_R64(AUDS,SNAU%MCBE),SNAU%BUFF(BS:BE))
    SNAU%BCUR=BE+1
    RETURN
    ! --- END CODE  ---
800 FORMAT('*ERR (Au_WrtSmp_R64):',1X,A,1X,I0)
900 WRITE(*,800) 'Invalid call with encoding format=',SNAU%ENCD ; STOP
910 WRITE(*,800) 'Invalid call with SIZE(AUDS).NE.SNAU%NCHN=',SNAU%NCHN ; STOP
  END SUBROUTINE Au_WrtSmp_R64
  
  SUBROUTINE Au_Close(SNAU,STAT)
    IMPLICIT NONE
    TYPE(SunAu)             :: SNAU
    INTEGER                 :: STAT ! Write IOSTAT
    INTENT(INOUT) :: SNAU,STAT
    ! --- EXE CODE  ---
    IF (SNAU%BCUR.GT.1) THEN
       WRITE(SNAU%AUFU,IOSTAT=STAT) SNAU%BUFF(1:SNAU%BCUR-1)
       IF (STAT.NE.0) RETURN
       IF (PFL_VERB) WRITE(*,700) 'Buffer flushed into unit',SNAU%AUFU
    END IF
    CLOSE(SNAU%AUFU,IOSTAT=STAT)
    IF (PFL_VERB) THEN
       WRITE(*,700) 'Closed file unit',SNAU%AUFU
       WRITE(*,700) 'Status of operation is',STAT
    END IF    
    IF (ASSOCIATED(SNAU%BUFF)) DEALLOCATE(SNAU%BUFF)
    SNAU=SunAu()
    IF (PFL_VERB) WRITE(*,700) 'Buffer deallocated'
    RETURN
    ! --- END CODE  ---
700 FORMAT('*INF (Au_Close):',1X,A,:,999(1x,I0))
  END SUBROUTINE Au_Close

  PURE FUNCTION MachBE()
    ! Return .TRUE. if host machine is big-endian, .FALSE. otherwise
    IMPLICIT NONE
    LOGICAL :: MachBE
    ! --- VARIABLES ---
    INTEGER(KIND=C_INT8_T) :: TC(1:2)
    ! --- EXE CODE  ---
    TC=TRANSFER(INT(1,C_INT16_T),TC)
    MachBE=TC(1).EQ.0 ! .TRUE.=>BIG ENDIAN HARDWARE
    ! --- END CODE  ---
  END FUNCTION MachBE

  ELEMENTAL FUNCTION Nord_I16(I,MBE)
    IMPLICIT NONE
    INTEGER(KIND=C_INT16_T)            :: Nord_I16
    INTEGER(KIND=C_INT16_T),INTENT(IN) :: I
    LOGICAL,INTENT(IN)                 :: MBE
    ! --- EXE CODE ---
    Nord_I16=I
    IF (.NOT.MBE) THEN
       ! Little-endian machine
       CALL MVBITS(I,0,8,Nord_I16,8)
       CALL MVBITS(I,8,8,Nord_I16,0)
    END IF
    ! --- END CODE ---
  END FUNCTION Nord_I16

  ELEMENTAL FUNCTION Nord_I32(I,MBE)
    IMPLICIT NONE
    INTEGER(KIND=C_INT32_T)            :: Nord_I32
    INTEGER(KIND=C_INT32_T),INTENT(IN) :: I
    LOGICAL,INTENT(IN)                 :: MBE
    ! --- EXE CODE ---
    Nord_I32=I
    IF (.NOT.MBE) THEN
       ! Little-endian machine       
       CALL MVBITS(I, 0,8,Nord_I32,24)
       CALL MVBITS(I, 8,8,Nord_I32,16)
       CALL MVBITS(I,16,8,Nord_I32, 8)
       CALL MVBITS(I,24,8,Nord_I32, 0)
    END IF
    ! --- END CODE ---
  END FUNCTION Nord_I32

  ELEMENTAL FUNCTION Nord_R32(R,MBE)
    IMPLICIT NONE
    REAL(KIND=C_FLOAT)            :: Nord_R32
    REAL(KIND=C_FLOAT),INTENT(IN) :: R
    LOGICAL,INTENT(IN)            :: MBE
    ! --- VARIABLES ---
    INTEGER(KIND=C_INT32_T) :: I0,I
    ! --- EXE CODE ---
    Nord_R32=R
    IF (.NOT.MBE) THEN
       ! Little-endian machine
       I0=TRANSFER(R,I)
       I=0 ! Shut -Wall up talking
       CALL MVBITS(I0, 0,8,I,24)
       CALL MVBITS(I0, 8,8,I,16)
       CALL MVBITS(I0,16,8,I, 8)
       CALL MVBITS(I0,24,8,I, 0)
       Nord_R32=TRANSFER(I,Nord_R32)
    END IF
    ! --- END CODE ---
  END FUNCTION Nord_R32

  ELEMENTAL FUNCTION Nord_R64(R,MBE) ! #NEW:2026-05-06a
    IMPLICIT NONE
    REAL(KIND=C_DOUBLE)            :: Nord_R64
    REAL(KIND=C_DOUBLE),INTENT(IN) :: R
    LOGICAL,INTENT(IN)             :: MBE
    ! --- VARIABLES ---
    INTEGER(KIND=C_INT64_T) :: I0,I
    ! --- EXE CODE ---
    Nord_R64=R
    IF (.NOT.MBE) THEN
       ! Little-endian machine
       I0=TRANSFER(R,I)
       I=0 ! Shut -Wall up talking
       CALL MVBITS(I0, 0,8,I,56)
       CALL MVBITS(I0, 8,8,I,48)
       CALL MVBITS(I0,16,8,I,40)
       CALL MVBITS(I0,24,8,I,32)
       CALL MVBITS(I0,32,8,I,24)
       CALL MVBITS(I0,40,8,I,16)
       CALL MVBITS(I0,48,8,I, 8)
       CALL MVBITS(I0,56,8,I, 0)       
       Nord_R64=TRANSFER(I,Nord_R64)
    END IF
    ! --- END CODE ---
  END FUNCTION Nord_R64

END MODULE Mz2AuFile
