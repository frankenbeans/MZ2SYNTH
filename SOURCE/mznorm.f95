! Copyright (C) 2024 by E. Lamprecht
! See LICENSE.md in this repository.

MODULE MZNORM
  USE CONSTANT, ONLY: RKIND
  IMPLICIT NONE

CONTAINS

  SUBROUTINE MZN_norm(DI, DO, NDB,CMP)
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND) :: DI(:), DO(:), NDB
    LOGICAL          :: CMP
    INTENT(INOUT)    :: DI, DO
    TARGET           :: DI, DO
    INTENT(IN)       :: NDB
    OPTIONAL         :: DO,CMP
    ! --- VARIABLES ---
    INTEGER                  :: J
    REAL(KIND=RKIND)         :: MAXDI
    REAL(KIND=RKIND),POINTER :: POUT(:)
    LOGICAL                  :: LCMP
    ! --- EXE CODE ---    
    POUT=>DI     ; IF (PRESENT(DO))  POUT=>DO
    LCMP=.FALSE. ; IF (PRESENT(CMP)) LCMP=CMP
    
    !$OMP PARALLEL WORKSHARE
    MAXDI=MAXVAL(ABS(DI))
    !$OMP END PARALLEL WORKSHARE
    
    !$OMP PARALLEL DO
    DO J=LBOUND(DI,1),UBOUND(DI,1)
       POUT(J)=DI(J)*(10.0_RKIND**(NDB/20.0_RKIND)) / MAXDI
    END DO
    !$OMP END PARALLEL DO
    IF (LCMP) THEN
       !$OMP PARALLEL WORKSHARE
       POUT=MZN_clip(POUT)
       !$OMP END PARALLEL WORKSHARE
    END IF
    ! --- END CODE ---
  END SUBROUTINE MZN_norm

  ELEMENTAL FUNCTION MZN_clip(X)
    ! +------------------------------------------------------------------------+
    ! | REFERENCE:  Fabián Esqueda, Stefan Bilbao & Vesa Välimäki;             |
    ! |            "Antialiased soft clipping using a polynomial approximation |
    ! |             of the integrated bandlimited ramp function";              |
    ! |            General Musical Acoustics: Paper ICA2016-750                |
    ! |            PROCEEDINGS of the 22nd International Congress on Acoustics |
    ! |            5 - 9 September 2016                                        |
    ! |            (http://www.ica2016.org.ar/ica2016proceedings/ica2016/      |
    ! |             ICA2016-0750.pdf)                                          |
    ! |                                                                        |
    ! |            *** citing ***                                              |
    ! |                                                                        |
    ! |            T.  Araya  and  A.  Suyama.                                 |
    ! |            "Sound  effector  capable  of  imparting  plural  sound     |
    ! |             effects like distortion and other effects."                |
    ! |            US Patent 5,570,424, 29 Oct. 1996                           |
    ! +------------------------------------------------------------------------+
    IMPLICIT NONE
    REAL(KIND=RKIND) :: MZN_clip
    ! --- DUMMIES ---
    REAL(KIND=RKIND), INTENT(IN) :: X
    ! --- EXE CODE ---
    IF (ABS(X).LT.1) THEN
       MZN_clip=0.5_RKIND*(3.0_RKIND*X-X**3)
    ELSE
       MZN_clip=SIGN(1.0_RKIND,X) ! VALUE OF 1.0_R WITH SIGN OF X
    END IF
    ! --- END CODE ---
  END FUNCTION MZN_clip

END MODULE MZNORM
