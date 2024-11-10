* ----------------------------------------------------------------------
*     LOGIST.F
*
*     LOGISTIC FUNCTION (SIGMOID CURVE)
*
*     COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
* ----------------------------------------------------------------------
*
      FUNCTION LOGIST(X)
      IMPLICIT NONE
      DOUBLE PRECISION LOGIST
*     --- DUMMY ARG ---
      DOUBLE PRECISION X
*     --- EXE CODE  ---
      LOGIST=1.0D0 / (1.0D0 + EXP(-X))
*     --- END CODE  ---
      END
* ======================================================================
      SUBROUTINE LTDATA(ADATA,ADIML,ADIMR,LXVAL,RXVAL)
      IMPLICIT NONE
*     --- DUMMY ARG ---
      DOUBLE PRECISION ADATA,LXVAL,RXVAL
      INTEGER          ADIML,ADIMR
      DIMENSION ADATA(ADIML:ADIMR)
*     --- VARIABLES ---
      INTEGER          J
      DOUBLE PRECISION D2XM,D2XC
*     --- EXTERNAL ---
      DOUBLE PRECISION LOGIST
*     --- EXE CODE ---
      IF (.NOT.ADIML.LT.ADIMR) GOTO 900
      IF (.NOT.LXVAL.LT.RXVAL) GOTO 910
      D2XM=(RXVAL-LXVAL)/(DBLE(ADIMR)-DBLE(ADIML))
      D2XC=LXVAL-D2XM*DBLE(ADIML)
      DO 10 J=ADIML,ADIMR
         ADATA(J)=LOGIST(D2XM*DBLE(J)+D2XC)
 10   CONTINUE
*     --- END CODE ---
      GOTO 999
 800  FORMAT('! ERROR(LTDATA):',1X,A)
 900  WRITE(*,800) 'EXPECTING ADIML.LT.ADIMR'
      STOP
 910  WRITE(*,800) 'EXPECTING LXVAL.LT.RXVAL'
      STOP
 999  END
* ======================================================================
      SUBROUTINE LTSCAL(ADATAO,ADATAI,ADIML,ADIMR,YVAL0,YVAL1)
      IMPLICIT NONE
*     --- DUMMY ARG ---
      DOUBLE PRECISION ADATAO,ADATAI,YVAL0,YVAL1
      INTEGER          ADIML, ADIMR
      DIMENSION ADATAO(ADIML:ADIMR),ADATAI(ADIML:ADIMR)
*     --- VARIABLES ---
      INTEGER J
      DOUBLE PRECISION SCLM,SCLC
*     --- EXE CODE  ---
      IF (.NOT.ADATAI(ADIML).LT.ADATAI(ADIMR)) GOTO 900
      SCLM=(YVAL1-YVAL0) / (ADATAI(ADIMR)-ADATAI(ADIML))
      SCLC=YVAL0-SCLM*ADATAI(ADIML)
*     $OMP PARALLEL DO
      DO 10 J=ADIML,ADIMR
         ADATAO(J)=SCLM*ADATAI(J)+SCLC
 10   CONTINUE
*     $OMP END PARALLEL DO
*     --- END CODE  ---
      GOTO 999
 800  FORMAT('! ERROR(LTSCAL):',1X,A)
 900  WRITE(*,800) 'EXPECTING ADATAI(ADIML).LT.ADATAI(ADIMR)'
      STOP      
 999  END
* ======================================================================
      
     
      
