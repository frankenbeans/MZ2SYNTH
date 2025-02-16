! ----------------------------------------------------------------------
! CASECONV.F95
!
! FORTRAN ROUTINES FOR CONVERTING STRINGS TO UPPERCASE OR LOWERCASE.
!
! COPYRIGHT (C) 2025 BY E. LAMPRECHT -  ALL RIGHTS RESERVED.
! ----------------------------------------------------------------------

MODULE CaseConv
  IMPLICIT NONE

CONTAINS
  
  ! ----------------------------------------------------------------------
  ! SUBROUTINE TOUPPR(C)
  ! --------------------
  ! CONVERT VARIABLE-LENGTH CHARACTER STRING C TO UPPER CASE.
  ! 
  SUBROUTINE TOUPPR(C)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: C
    CALL LTRSHF(C,.TRUE.)
  END SUBROUTINE TOUPPR
  
  ! ----------------------------------------------------------------------
  ! SUBROUTINE TOLOWR(C)
  ! --------------------
  ! CONVERT VARIABLE-LENGTH CHARACTER STRING C TO LOWER CASE.
  ! 
  SUBROUTINE TOLOWR(C)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: C
    CALL LTRSHF(C,.FALSE.)
  END SUBROUTINE TOLOWR
  
  ! ----------------------------------------------------------------------
  ! SUBROUTINE LTRSHF(C,TOUP)
  ! -------------------------
  ! SHIFT SINGLE-LETTER CHARACTER STRING C TO UPPER CASE IF LOGICAL TOUP
  ! CONTAINS THE VALUE .TRUE., OR TO LOWER CASE OTHERWISE.
  !
  ! NOTE:  ONLY LETTERS 'A-z' ARE AFFECTED;  ALL OTHER CHARACTERS ARE
  ! ----   IGNORED.
  !
  SUBROUTINE LTRSHF(C,TOUP)
    IMPLICIT NONE
    ! --- DUMMY ARGS ---
    CHARACTER(LEN=*),INTENT(INOUT) :: C
    LOGICAL         ,INTENT(IN)    :: TOUP
    ! --- VARIABLES ---
    INTEGER :: SHFT,J
    LOGICAL :: TOSHFT
    SHFT=ICHAR('a')-ICHAR('A')
    IF (TOUP) SHFT=-1*SHFT
    DO J=1,LEN(C)
       TOSHFT=(TOUP.AND.ICHAR(C(J:J)).GE.ICHAR('a')                            &
                   .AND.ICHAR(C(J:J)).LE.ICHAR('z'))                           &
              .OR.                                                             &
              (.NOT.TOUP  .AND.ICHAR(C(J:J)).GE.ICHAR('A')                     &
                          .AND.ICHAR(C(J:J)).LE.ICHAR('Z'))
       IF (TOSHFT) C(J:J)=CHAR(ICHAR(C(J:J))+SHFT)
    END DO
  END SUBROUTINE LTRSHF
  
END MODULE CaseConv
