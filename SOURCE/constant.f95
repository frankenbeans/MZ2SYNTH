! ------------------------------------------------------------------------------
! CONSTANT.F95
!
! PROGRAM-WIDE CONSTANTS FOR MZ2
!
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ------------------------------------------------------------------------------

#ifndef CONST_RKIND
#define CONST_RKIND 8
#endif

#ifndef CONST_IFU
#define CONST_IFU 21
#endif

#ifndef CONST_OFU
#define CONST_OFU 22
#endif

#ifndef CONST_ZSTR
#define CONST_ZSTR 1024
#endif

#ifndef CONST_DIFN
#define CONST_DIFN 'input.ppm'
#endif

#ifndef CONST_DWFN
#define CONST_DWFN '/usr/lib/mz2wave.tbl'
#endif

#ifndef CONST_DOFN
#define CONST_DOFN 'output.au'
#endif

#ifndef CONST_DNOC
#define CONST_DNOC 10
#endif

#ifndef CONST_DNOO
#define CONST_DNOO 72
#endif

#ifndef CONST_DNSO
#define CONST_DNSO 12
#endif

#ifndef CONST_DFRF
#define CONST_DFRF 440 
#endif

#ifndef CONST_DSRF
#define CONST_DSRF 49
#endif

#ifndef CONST_DCHS
#define CONST_DCHS 'RGBM'
#endif

#ifndef CONST_DVML
#define CONST_DVML 0.1_RKIND
#endif

#ifndef CONST_DADV
#define CONST_DADV 10.0_RKIND
#endif

#ifndef CONST_DFTZ
#define CONST_DFTZ (1.0_RKIND/3.0_RKIND)
#endif

#ifndef CONST_DSMP
#define CONST_DSMP 44100
#endif

#ifndef CONST_DNCH
#define CONST_DNCH 2
#endif

#ifndef CONST_DWWW
#define CONST_DWWW 2**16
#endif

#ifndef CONST_DRNS
#define CONST_DRNS 11235813
#endif

#ifndef CONST_DFXP
#define CONST_DFXP .FALSE.
#endif

#ifndef CONST_DZRP
#define CONST_DZRP .FALSE.
#endif


MODULE CONSTANT
  IMPLICIT NONE

  ! --- REAL NUMBER KIND ---
  INTEGER,PARAMETER :: RKIND=CONST_RKIND ! DOUBLE PRECISION
  ! --- FORTRAN INPUT AND OUTPUT FILE UNIT NUMBERS ---
  INTEGER,PARAMETER :: IFU  =CONST_IFU   ! UNIT FOR TEMPORARY FILES (INPUT)
  INTEGER,PARAMETER :: OFU  =CONST_OFU   ! UNIT FOR TEMPORARY FILES (OUTPUT)
  ! --- DEFAULT STRING LENGTH
  INTEGER,PARAMETER :: ZSTR =CONST_ZSTR  ! String length
  ! --- PROGRAM DEFAULT FILENAMES ---  
  CHARACTER(LEN=ZSTR),PARAMETER :: DIFN = CONST_DIFN ! Input image file name
  CHARACTER(LEN=ZSTR),PARAMETER :: DWFN = CONST_DWFN ! Wavetable file name
  CHARACTER(LEN=ZSTR),PARAMETER :: DOFN = CONST_DOFN ! Output audio file name
  ! --- MUSICAL PARAMETERS ---
  INTEGER            ,PARAMETER :: DNOC = CONST_DNOC ! Number of octaves in span
  INTEGER            ,PARAMETER :: DNOO = CONST_DNOO ! Number of oscillator freq
  INTEGER            ,PARAMETER :: DNSO = CONST_DNSO ! Number of semitones/oct
  INTEGER            ,PARAMETER :: DFRF = CONST_DFRF ! Calibration frequency ref
  INTEGER            ,PARAMETER :: DSRF = CONST_DSRF ! Calibration semitone num.
  ! --- PROGRAM DEFAULT OPTION VALUES ---
  INTEGER            ,PARAMETER :: ZCHS = 4
  CHARACTER(LEN=ZCHS),PARAMETER :: DCHS = CONST_DCHS ! Colour channel spec
  REAL(KIND=RKIND)   ,PARAMETER :: DVML = CONST_DVML ! Volume multiplier
  REAL(KIND=RKIND)   ,PARAMETER :: DADV = CONST_DADV ! Advance rate (col/s)
  REAL(KIND=RKIND)   ,PARAMETER :: DFTZ = CONST_DFTZ ! Transition zone fraction
  REAL(KIND=RKIND)   ,PARAMETER :: DSMP = CONST_DSMP ! Sampling rate (c.p.s.)
  INTEGER            ,PARAMETER :: DNCH = CONST_DNCH ! Number of output channels
  INTEGER            ,PARAMETER :: DWWW = CONST_DWWW ! Width of wavetable window
  INTEGER            ,PARAMETER :: DRNS = CONST_DRNS ! Random seed for phase ini
  LOGICAL            ,PARAMETER :: DFXP = CONST_DFXP ! Fixed phase mode
  LOGICAL            ,PARAMETER :: DZRP = CONST_DZRP ! Initialize phases with zero
END MODULE CONSTANT
