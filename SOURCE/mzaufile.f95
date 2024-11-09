! Copyright (C) 2024 by E. Lamprecht
! See LICENSE.md in this repository.

#include "defaults.inc"

MODULE MZAUFILE
  USE ISO_C_BINDING
  USE BREVERSE
  IMPLICIT NONE

  INTEGER(C_INT32_T),PARAMETER :: DFMAGIC=INT(Z'2E736E64')  ! IDENTIFIES AU TYPE
  INTEGER(C_INT32_T),PARAMETER :: DFENCOD=0                 ! NULL PLACEHOLDER VALUE
  INTEGER(C_INT32_T),PARAMETER :: DFSMPRT=DSMPR             ! CD QUALITY BY DEFAULT
  INTEGER(C_INT32_T),PARAMETER :: DFNCHAN=DNCHAN            ! STEREO BY DEFAULT
  INTEGER(C_INT32_T),PARAMETER :: FHDRSZE=24                ! SIZE OF FIXED HEADER

  INTEGER,PARAMETER :: AUF_PCM_LINEAR_16B=3
  INTEGER,PARAMETER :: AUF_FLT_LINEAR_32B=6
  
  TYPE AU_T
     INTEGER(C_INT32_T) :: MAGIC=DFMAGIC
     INTEGER(C_INT32_T) :: HDSZE=0
     INTEGER(C_INT32_T) :: DTSZE=0
     INTEGER(C_INT32_T) :: ENCOD=DFENCOD
     INTEGER(C_INT32_T) :: SMPRT=DFSMPRT
     INTEGER(C_INT32_T) :: NCHAN=DFNCHAN
     CHARACTER(C_CHAR) ,POINTER :: HINFO(:)=>NULL()
     INTEGER(C_INT16_T),POINTER :: SDATA(:)=>NULL()
     REAL   (C_FLOAT)  ,POINTER :: RDATA(:)=>NULL()
  END TYPE AU_T

CONTAINS

  SUBROUTINE AU_CREATE(AU,HINFO,SMPRT,NCHAN,SDATA,RDATA)
    IMPLICIT NONE
    ! --- DUMMIES ---
    TYPE(AU_T)       :: AU
    CHARACTER(LEN=*) :: HINFO
    INTEGER          :: SMPRT
    INTEGER          :: NCHAN
    INTEGER          :: SDATA(:)
    REAL(C_FLOAT)    :: RDATA(:)
    INTENT(INOUT)    :: AU
    INTENT(IN)       :: HINFO,SMPRT,NCHAN,SDATA
    OPTIONAL         :: HINFO,SMPRT,NCHAN,SDATA,RDATA
    ! --- VARIABLES ---
    INTEGER :: VHDRSZE
    INTEGER :: NSAMPLES
    INTEGER :: J,MS
    ! --- EXE CODE ---
    VHDRSZE=0
    IF (PRESENT(SMPRT)) AU%SMPRT=SMPRT
    IF (PRESENT(NCHAN)) AU%NCHAN=NCHAN
    IF (PRESENT(HINFO)) THEN
       VHDRSZE=LEN(HINFO)
       ALLOCATE(AU%HINFO(1:VHDRSZE),STAT=MS) ; IF (MS.NE.0) GOTO 900
       AU%HINFO(1:VHDRSZE)=HINFO
    END IF
    AU%HDSZE=FHDRSZE+VHDRSZE
    IF (PRESENT(SDATA)) THEN
       AU%ENCOD=AUF_PCM_LINEAR_16B
       NSAMPLES=SIZE(SDATA)
       ALLOCATE(AU%SDATA(1:NSAMPLES),STAT=MS)
       AU%SDATA=(/(INT(SDATA(J),C_INT16_T),J=1,NSAMPLES)/)
       AU%DTSZE=INT(NSAMPLES*C_SIZEOF(AU%SDATA(1)),C_INT32_T)
    ELSE IF (PRESENT(RDATA)) THEN
       AU%ENCOD=AUF_FLT_LINEAR_32B
       NSAMPLES=SIZE(RDATA)
       ALLOCATE(AU%RDATA(1:NSAMPLES),STAT=MS)
       AU%RDATA=(/(RDATA(J),J=1,NSAMPLES)/)
       AU%DTSZE=INT(NSAMPLES*C_SIZEOF(AU%RDATA(1)),C_INT32_T)
    ELSE
       GOTO 910
    END IF
    ! --- END CODE ---
    RETURN
900 STOP '* ERROR(AU_CREATE): MEMORY ALLOCATION FAILURE'
910 STOP '* ERROR(AU_CREATE): EXPECTING ONE OF SDATA OR RDATA'
  END SUBROUTINE AU_CREATE

  SUBROUTINE AU_DESTROY(AU)
    IMPLICIT NONE
    ! --- DUMMIES ---
    TYPE(AU_T)       :: AU
    INTENT(INOUT)    :: AU
    ! --- VARIABLES ---
    TYPE(AU_T) :: DAU
    ! --- EXE CODE ---
    IF (ASSOCIATED(AU%HINFO)) DEALLOCATE(AU%HINFO)
    IF (ASSOCIATED(AU%SDATA)) DEALLOCATE(AU%SDATA)
    IF (ASSOCIATED(AU%RDATA)) DEALLOCATE(AU%RDATA)
    AU=DAU
    ! --- END CODE ---    
  END SUBROUTINE AU_DESTROY

  SUBROUTINE AU_WRITE(AU,OFN,OFU,RPL)
    IMPLICIT NONE
    ! --- DUMMIES ---
    TYPE(AU_T)        :: AU
    CHARACTER(LEN=*)  :: OFN
    INTEGER           :: OFU
    LOGICAL           :: RPL
    INTENT(INOUT)     :: AU
    INTENT(IN)        :: OFN,RPL
    INTENT(IN)        :: OFU
    OPTIONAL          :: RPL
    ! --- VARIABLES ---
    CHARACTER(LEN=1)  :: EGGS
    LOGICAL           :: LRPL
    CHARACTER(LEN=12) :: FMODE
    ! --- EXE CODE ---
    LRPL=.FALSE. ; IF (PRESENT(RPL)) LRPL=RPL
    FMODE='NEW'  ; IF (LRPL) FMODE='REPLACE'
    OPEN(UNIT=OFU,FILE=OFN,FORM='UNFORMATTED',ACCESS='STREAM',ACTION='WRITE', &
         STATUS=FMODE,ERR=900)
    CALL ENDIAN(EGGS)
    IF (EGGS.EQ.'B') THEN
       WRITE(OFU)   AU%MAGIC,AU%HDSZE,AU%DTSZE,AU%ENCOD,AU%SMPRT,AU%NCHAN,    &
                    AU%HINFO
       IF (AU%ENCOD.EQ.AUF_PCM_LINEAR_16B) THEN
          WRITE(OFU) AU%SDATA
       ELSE IF (AU%ENCOD.EQ.AUF_FLT_LINEAR_32B) THEN
          WRITE(OFU) AU%RDATA
       ELSE
          GOTO 910
       END IF
    ELSE IF (EGGS.EQ.'L') THEN
       WRITE(OFU)   BREV(AU%MAGIC), &
                    BREV(AU%HDSZE), &
                    BREV(AU%DTSZE), &
                    BREV(AU%ENCOD), &
                    BREV(AU%SMPRT), &
                    BREV(AU%NCHAN), &
                    AU%HINFO
       IF (AU%ENCOD.EQ.AUF_PCM_LINEAR_16B) THEN
          WRITE(OFU) BREV(AU%SDATA)
       ELSE IF (AU%ENCOD.EQ.AUF_FLT_LINEAR_32B) THEN
          WRITE(OFU) BREV(AU%RDATA)
       ELSE
          GOTO 910
       END IF
    ELSE
       GOTO 999
    END IF
    CLOSE(OFU)
    ! --- END CODE ---
    RETURN
800 FORMAT('* ERROR(AU_WRITE):',999(:,1x,A,I0))
900 WRITE(*,800) 'CANNOT CREATE FILE "'//TRIM(OFN)//'" IN MODE "'&
                 //TRIM(FMODE)//'" ON UNIT ',OFU
    STOP
910 WRITE(*,800) 'UNSUPPORTED ENCODING FORMAT',AU%ENCOD
    STOP
999 CONTINUE
  END SUBROUTINE AU_WRITE
 
END MODULE MZAUFILE
