! ------------------------------------------------------------------------------
! SFREALCOMP.F95
!
! Practical real-number comparisons for SFDEFLIC project
!
! Copyright (C) 2024 by E. Lamprecht.   See LICENSE.md in this repository.
! ------------------------------------------------------------------------------

MODULE SFRealComp
  USE Constant, ONLY: RKIND
  IMPLICIT NONE

CONTAINS

  PURE FUNCTION REQ(R1,R2)
    ! -- TRUE if R1 ~= R2, FALSE otherwise --
    IMPLICIT NONE
    LOGICAL :: REQ
    REAL(KIND=RKIND),INTENT(IN) :: R1,R2
    ! --- EXE CODE ---
    REQ=(ABS(R1-R2).LE.TINY(R1))
    ! --- END CODE ---
  END FUNCTION REQ

  PURE FUNCTION RNE(R1,R2)
    ! -- TRUE if R1 not ~= R2, FALSE otherwise --
    IMPLICIT NONE
    LOGICAL :: RNE
    REAL(KIND=RKIND),INTENT(IN) :: R1,R2
    ! --- EXE CODE ---
    RNE=(ABS(R1-R2).GT.TINY(R1))
    ! --- END CODE ---
  END FUNCTION RNE

  ELEMENTAL FUNCTION RIZ(R1)
    ! -- TRUE if R1 ~= 0, FALSE otherwise --
    IMPLICIT NONE
    LOGICAL :: RIZ
    REAL(KIND=RKIND),INTENT(IN) :: R1
    ! --- EXE CODE ---
    RIZ=ABS(R1).LE.TINY(R1)
    ! --- END CODE ---
  END FUNCTION RIZ

  ELEMENTAL FUNCTION RNZ(R1)
    ! -- TRUE if R1 not ~= 0, FALSE otherwise --
    IMPLICIT NONE
    LOGICAL :: RNZ
    REAL(KIND=RKIND),INTENT(IN) :: R1
    ! --- EXE CODE ---
    RNZ=ABS(R1).GT.TINY(R1)
    ! --- END CODE ---
  END FUNCTION RNZ
  
END MODULE SFRealComp
  
  
  
  
