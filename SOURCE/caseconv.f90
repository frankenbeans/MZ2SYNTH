! ----------------------------------------------------------------------
! CASECONV.F90
!
! FORTRAN SUBROUTINES FOR IN-PLACE CASE CONVERSION OF CHARACTER STRINGS.
!
! COPYRIGHT (C) 2026 BY E. LAMPRECHT -  ALL RIGHTS RESERVED.
! ----------------------------------------------------------------------

MODULE CaseConv
  IMPLICIT NONE
  INTEGER,PARAMETER :: SHFT=ICHAR('a')-ICHAR('A')
  PRIVATE :: SHFT,INRANG,ISLOWR,ISUPPR
  PUBLIC  :: TOLOWR,TOUPPR

CONTAINS
  ! ----------------------------------------------------------------------
  ! PURE FUNCTION INRANG(C,CA,CZ)
  ! RETURN .TRUE. IF C IS IN RANGE CA TO CZ
  !
  PURE FUNCTION INRANG(C,CA,CZ)
    IMPLICIT NONE
    LOGICAL :: INRANG
    CHARACTER(LEN=1),INTENT(IN) :: C,CA,CZ
    INRANG=(ICHAR(C).GE.ICHAR(CA).AND.ICHAR(C).LE.ICHAR(CZ))
  END FUNCTION INRANG
  
  ! ----------------------------------------------------------------------
  ! PURE FUNCTION ISLOWR(C)
  ! RETURN .TRUE. IF C IS A LOWER CASE LETTER, .FALSE. OTHERWISE
  !
  PURE FUNCTION ISLOWR(C)
    IMPLICIT NONE
    LOGICAL :: ISLOWR
    CHARACTER(LEN=1),INTENT(IN) :: C
    ISLOWR=INRANG(C,'a','z')
  END FUNCTION ISLOWR

  ! ----------------------------------------------------------------------
  ! PURE FUNCTION ISUPPR(C)
  ! RETURN .TRUE. IF C IS AN UPPER CASE LETTER, .FALSE. OTHERWISE
  !
  PURE FUNCTION ISUPPR(C)
    IMPLICIT NONE
    LOGICAL :: ISUPPR
    CHARACTER(LEN=1),INTENT(IN) :: C
    ISUPPR=INRANG(C,'A','Z')
  END FUNCTION ISUPPR
  
  ! ----------------------------------------------------------------------
  ! PURE SUBROUTINE TOUPPR(C)
  ! CONVERT VARIABLE-LENGTH CHARACTER STRING C TO UPPER CASE, IN PLACE.
  ! 
  PURE SUBROUTINE TOUPPR(S)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: S
    INTEGER :: J
    DO J=1,LEN(S)
       IF (ISLOWR(S(J:J))) S(J:J)=ACHAR(ICHAR(S(J:J))-SHFT)
    END DO
  END SUBROUTINE TOUPPR
  
  ! ----------------------------------------------------------------------
  ! PURE SUBROUTINE TOLOWR(C)
  ! CONVERT VARIABLE-LENGTH CHARACTER STRING C TO LOWER CASE, IN PLACE.
  ! 
  PURE SUBROUTINE TOLOWR(S)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: S
    INTEGER :: J
    DO J=1,LEN(S)
       IF (ISUPPR(S(J:J))) S(J:J)=ACHAR(ICHAR(S(J:J))+SHFT)
    END DO    
  END SUBROUTINE TOLOWR
  
END MODULE CaseConv
