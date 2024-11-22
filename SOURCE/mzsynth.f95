! ------------------------------------------------------------------------------
! MZSYNTH.F95
!
! MZ2 MAIN SUBROUTINES
!
! COPYRIGHT (C) 2024 BY E. LAMPRECHT - ALL RIGHTS RESERVED.
! ------------------------------------------------------------------------------

#define PROGNAME 'MZ2SYNTH'
#define PROGVERS '0.1/2024-11-20'
#define PROGCOPY 'Copyright (C) by E. Lamprecht.   All rights reserved.'

PROGRAM MZSYNTH  
  USE Constant
  USE PFlags
  USE MZOSC
  USE MZPNL
  USE MZAUFILE
  IMPLICIT NONE

  ! INPUT PARAMETERS
  INTEGER,PARAMETER   :: ZARG=ZSTR
  CHARACTER(LEN=ZARG) :: PIFN=DIFN,WFN=DWFN,OFN=DOFN
  CHARACTER(LEN=NVCH) :: VCHS=DCHS
  REAL(KIND=RKIND)    :: VMUL=DVML
  REAL(KIND=RKIND)    :: ACPS=DADV
  REAL(KIND=RKIND)    :: TRZF=DFTZ
  LOGICAL             :: ABIN=.FALSE.
  LOGICAL             :: CMPR=.FALSE.
  LOGICAL             :: FXPH=DFXP
  LOGICAL             :: ZRPH=DZRP
  INTEGER             :: SMPR=DSMP

  ! DERIVED PARAMETERS
  INTEGER             :: ZDATA
  LOGICAL             :: MCBE

  ! --- EXE CODE ---
  CALL MZSYN_MAIN()
  ! --- END CODE ---

CONTAINS

  SUBROUTINE MZSYN_MAIN()
    IMPLICIT NONE
    ! --- EXE CODE ---
    CALL MZSYN_CMDLINE()
    CALL MZSYN_INITSYN()
    CALL MZSYN_GENERATE()    
    ! --- END CODE ---
  END SUBROUTINE MZSYN_MAIN
  
  SUBROUTINE MZSYN_CMDLINE()
    IMPLICIT NONE
    ! --- VARIABLES ---
    CHARACTER(LEN=ZARG) :: CARG='',RARG=''
    CHARACTER(LEN=10)   :: FMRD='',FMWT=''
    INTEGER             :: K,NARG=0
    LOGICAL             :: FEX,HLPX
    ! --- INTERFACE ---
    INTERFACE
       SUBROUTINE TOUPPR(C)
         IMPLICIT NONE
         CHARACTER(LEN=*) :: C
         INTENT(IN)       :: C           
       END SUBROUTINE TOUPPR
    END INTERFACE
    ! --- EXE CODE  ---
    HLPX=.TRUE.
    NARG=0
    DO 
       NARG=NARG+1
       CALL GET_COMMAND_ARGUMENT(NARG,CARG)
       CARG=ADJUSTL(CARG)
       IF (CARG.EQ.'') EXIT
       HLPX=.FALSE. ! There are options so don't just do helpscreen and quit...
       IF (CARG(1:1).EQ.'-') CALL TOUPPR(CARG)
       SELECT CASE (CARG)
       ! --- UNARY ARGUMENTS ---
       CASE('-H','-HELP')
          HLPX=.TRUE. ! ... unless the help-option asks for that behaviour.
          EXIT
       CASE('-V','-VERBOSE')
          PFL_VERB=.NOT.PFL_VERB
          RARG='VERBOSE MODE'
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'ON'
       CASE('-D','-DEBUG')
          PFL_DBUG=.NOT.PFL_DBUG
          RARG='DEBUG MODE'
          IF (PFL_DBUG) THEN
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'ON'
          ELSE
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'OFF'
          END IF
       CASE('-P','-DYNAMIC-COMPRESSION')
          CMPR=.NOT.CMPR
          RARG='DYNAMIC COMPRESSION'
          IF (CMPR) THEN
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'ON'
          ELSE
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'OFF'
          END IF          
       CASE('-W','-OVERWRITE')
          PFL_OVWT=.NOT.PFL_OVWT
          RARG='OVERWRITE MODE'
          IF (PFL_OVWT) THEN
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'ON'
          ELSE
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'OFF'
          END IF
       CASE('-X','-FIXED-PHASE')
          FXPH=.NOT.FXPH
          RARG='FIXED PHASE MODE'
          IF (FXPH) THEN
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'ON'
          ELSE
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'OFF'
          END IF          
       CASE('-Z','-ZERO-PHASE')
          ZRPH=.NOT.ZRPH
          RARG='ZERO PHASES INITIALLY'
          IF (FXPH) THEN
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'ON'
          ELSE
             IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':',TRIM(RARG),'OFF'
          END IF          
       ! --- BINARY ARGUMENTS ---
       CASE('-A','-ADVANCE')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET ADVANCE RATE'
          NARG=NARG+1
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 900
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          READ(CARG,*,ERR=900) ACPS
          IF (ACPS.LT.1) GOTO 900
          IF (PFL_VERB) WRITE(*,710) 'ADVANCE RATE =',ACPS,'COL/S'
       CASE('-C','-CHANNEL-SELECT')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET CHANNEL DEFINITIONS'
          NARG=NARG+1          
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 910
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          IF (LEN(TRIM(CARG)).NE.NVCH) GOTO 910          
          DO K=1,NVCH
             IF (SCAN(CARG(K:K),'RGBLM').EQ.0) GOTO 910             
          END DO
          VCHS=CARG(1:NVCH)
          IF (PFL_VERB) WRITE(*,700) 'CHANNEL SELECTOR =',VCHS
       CASE('-M','-VOLUME-MULTIPLIER')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET VOLUME MULTIPLIER'
          NARG=NARG+1
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 920
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          READ(CARG,*,ERR=920) VMUL
          IF (VMUL.LT.0) GOTO 920
          IF (PFL_VERB) WRITE(*,710) 'VOLUME MULTIPLIER =',VMUL
       CASE('-O','-OUTPUT-FILE')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET OUTPUT FILE'
          NARG=NARG+1
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 940
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          OFN=CARG
          IF (PFL_VERB) WRITE(*,700) 'OUTPUT FILE IS',TRIM(OFN)
          INQUIRE(FILE=OFN,EXIST=FEX,WRITE=FMWT)
          IF (FEX.AND..NOT.PFL_OVWT) GOTO 945
          IF (FEX.AND.PFL_OVWT.AND.FMWT.EQ.'NO') GOTO 947
       CASE('-R','-TRANSITION')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET TRANSITION FRACTION'
          NARG=NARG+1
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 950
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          READ(CARG,*,ERR=948) TRZF
          IF (TRZF.LT.0.OR.TRZF.GT.1) GOTO 948
          IF (PFL_VERB) WRITE(*,710) 'TRANSITION FRACTION =',TRZF
       CASE('-S','-SAMPLING-RATE')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET SAMPLING RATE'
          NARG=NARG+1
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 950
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          READ(CARG,*,ERR=948) SMPR
          IF (SMPR.LT.DSMP) GOTO 948
          IF (PFL_VERB) WRITE(*,710) 'SAMPLING RATE =',SMPR
       CASE('-T','-WAVE-TABLE')
          IF (PFL_VERB) WRITE(*,700) TRIM(CARG),':','SET WAVE TABLE'
          NARG=NARG+1
          IF (NARG.GT.COMMAND_ARGUMENT_COUNT()) GOTO 950
          CALL GET_COMMAND_ARGUMENT(NARG,CARG)
          WFN=CARG
          INQUIRE(FILE=WFN,EXIST=FEX)
          ABIN=.NOT.FEX ! REVERSE SENSE OF LOGICAL VARIABLE
          IF (PFL_VERB) WRITE(*,700) 'WAVE TABLE IS ',TRIM(WFN)
          IF (ABIN) THEN
             IF (PFL_VERB) WRITE(*,700) 'WAVE TABLE TO BE GENERATED AB INITIO'
          ELSE
             IF (PFL_VERB) WRITE(*,700) 'WAVE TABLE TO BE READ FROM FILE'
          END IF  
       CASE DEFAULT
          IF (CARG(1:1).EQ.'-') GOTO 990
          PIFN=CARG
          IF (PFL_VERB) WRITE (*,700) 'INPUT FILE IS',TRIM(PIFN)
          INQUIRE(FILE=PIFN,EXIST=FEX,READ=FMRD)
          IF (.NOT.FEX)      GOTO 960
          IF (TRIM(FMRD).EQ.'NO') GOTO 960
       END SELECT
    END DO
    ! If in help-mode, just print a help-screen and halt execution.
    IF (HLPX) THEN
       CALL MZSYN_HELP()
       STOP
    END IF
    ! --- END CODE  ---
    RETURN
700 FORMAT('MZSYNTH:',999(:,1X,A))
710 FORMAT('MZSYNTH:',999(:,1X,A,:,1X,G13.6))
800 FORMAT('!ERROR:',999(:,1X,A))
900 WRITE(*,800) 'ADVANCE RATE MUST BE PRESENT AND >= 1'         ; STOP
910 WRITE(*,800) 'CHANNEL MULTIPLIER MUST BE FOUR OF [RGBLM]'    ; STOP
920 WRITE(*,800) 'VOLUME MULTIPLIER MUST BE PRESENT AND >= 0'    ; STOP
940 WRITE(*,800) 'EXPECTING OUTPUT FILE NAME'                    ; STOP
945 WRITE(*,800) 'OUTPUT FILE EXISTS AND OVERWRITE MODE IS OFF.' ; STOP
947 WRITE(*,800) 'OUTPUT FILE CANNOT BE OPENED IN WRITE MODE.'   ; STOP
948 WRITE(*,800) 'SAMPLING RATE MUST BE INT >= 44100'            ; STOP
950 WRITE(*,800) 'EXPECTING WAVEFORM FILE NAME'                  ; STOP
960 WRITE(*,800) 'CANNOT READ INPUT FILE',TRIM(PIFN)             ; STOP
990 WRITE(*,800) 'INVALID COMMAND LINE OPTION',TRIM(CARG)        ; STOP
  END SUBROUTINE MZSYN_CMDLINE

  SUBROUTINE MZSYN_HELP()
    IMPLICIT NONE
    WRITE(*,700) PROGNAME//' '//PROGVERS
    WRITE(*,700) PROGCOPY
    WRITE(*,700) ''
    WRITE(*,700) 'SYNOPSIS:  mz2 [options...] [input_filename]'
    WRITE(*,700) ''
    WRITE(*,700) 'OPTIONS:'
    ! --- Unary options ---
    WRITE(*,710) '-h','-help','',                         &
         'Print help-screen and halt processing','off'
    WRITE(*,710) '-v','-verbose','',                      &
         'Toggle verbose text output mode','off'
    WRITE(*,710) '-d','-debug','',                        &
         'Toggle debugging output mode','off'
    WRITE(*,710) '-p','-dynamic-compression','',          &
         'Toggle dynamic compression','off'
    WRITE(*,710) '-w','-overwrite','',                    &
         'Toggle overwrite mode','off'
    WRITE(*,710) '-x','-fixed-phase','',                  &
         'Toggle fixed phase mode','off'
    WRITE(*,710) '-x','-zero-phase','',                   &
         'Toggle zero phase mode','off'
    WRITE(*,710) '-a','-advance','<r>',                   &
         'Set advance rate in cols/sec ','10'
    WRITE(*,710) '-c','-channel-select','<sqwt>',         &
         'Set (s)in,s(q)r,sa(w),(t)riangle colours','RGBL'
    WRITE(*,710) '-m','-volume-multiplier','<m>',         &
         'Set volume multiplier (m > 0)','1.0'
    WRITE(*,710) '-o','-output-file','<ofn>',             &
         'Write output to file <ofn>','note *'
    WRITE(*,710) '-s','-sampling-rate','<s>',             &
         'Set sampling rate to <s> c.p.s.','44100'    
    WRITE(*,710) '-r','-transition','<t>',                &
         'Set transition factor to <t>','0.333'
    WRITE(*,710) '-t','-wave-table','<wvt>',              &
         'Write wavetable file <wvt>','note *'
    WRITE(*,700) ''
    WRITE(*,700) '* Default wavetable file is '//TRIM(DWFN)
    WRITE(*,700) '* Default input  file is    '//TRIM(DIFN)
    WRITE(*,700) '* Default output file is    '//TRIM(DOFN)
    WRITE(*,700) '* NB NB NB: wavetable must match current sampling-rate'
    WRITE(*,700) ''
        
700 FORMAT(A)
710 FORMAT(2X,A,T5,'|',A,T25,A,T32,A,T73,'[',A,']')
  END SUBROUTINE MZSYN_HELP

  SUBROUTINE MZSYN_INITSYN()
    IMPLICIT NONE
    ! --- VARIABLES ---
    INTEGER :: FS
    ! --- EXE CODE ---
    CALL MZOSC_INIT(ABIN,WFN,SMPR,PHRND=.NOT.ZRPH)
    CALL MZPNL_LOAD(PIFN,VCHS,AFTRZ=TRZF,ANCLPS=ACPS)
    CALL AU_WRTHDR(OFN,OFU,AUF_FLT_LINEAR_32B,SMPR,DNCH,MCBE,PFL_OVWT,FS)
    IF (FS.NE.0) GOTO 900
    ZDATA=NC*N_SMP_PER_COL
    IF (PFL_VERB) WRITE(*,'(A,1X,I0)') 'NUMBER OF SAMPLES TO GENERATE:',ZDATA
    ! --- END CODE ---
    RETURN
800 FORMAT('!ERROR:',999(:,1X,A))
900 WRITE(*,800) 'FILE OUTPUT ERROR WHILE WRITING '//TRIM(OFN) ; STOP    
  END SUBROUTINE MZSYN_INITSYN

  SUBROUTINE MZSYN_GENERATE()
    IMPLICIT NONE
    ! --- VARIABLES ---
    INTEGER :: J,K,FS
    REAL(KIND=RKIND) :: TDATA
    ! --- EXE CODE  ---
    J=0
    ! ..........................................................................
    ! BEGIN MAIN LOOP |
    ! ----------------+
10  J=J+1
    IF (PFL_VERB) THEN
       IF (MOD(J,SMPR).EQ.0) WRITE(*,700) 'SMPL=',J, &
            '; %COMPLETE=',100.0*REAL(J)/REAL(ZDATA),  &
            '; TIME/S=',REAL(J) / N_SMP_PER_SEC
    END IF
    CALL MZPNL_TICK()
    CALL MZOSC_TICK((/(FXPH.OR.WSINE(K).NE.0.OR.WSQWV(K).NE.0.OR. &
                               WSWTH(K).NE.0.OR.WTRNG(K).NE.0,K=1,N_OSC)/))
    TDATA=0
    !$OMP PARALLEL DO REDUCTION(+:TDATA)
    DO K=1,N_OSC
       IF (WSINE(K).NE.0) TDATA=TDATA+WSINE(K)*LKT_SINE(NINT(OSC_ACCM(K)),K)
       IF (WSQWV(K).NE.0) TDATA=TDATA+WSQWV(K)*LKT_SQWV(NINT(OSC_ACCM(K)),K)
       IF (WSWTH(K).NE.0) TDATA=TDATA+WSWTH(K)*LKT_SWTH(NINT(OSC_ACCM(K)),K)
       IF (WTRNG(K).NE.0) TDATA=TDATA+WTRNG(K)*LKT_TRNG(NINT(OSC_ACCM(K)),K)
    END DO
    !$OMP END PARALLEL DO
    TDATA=VMUL*TDATA
    IF (CMPR) TDATA=MZSYNTH_CLIP(TDATA)
    CALL AU_WRTSMP(OFU,REAL((/TDATA,TDATA/),C_FLOAT),MCBE,FS)
    ! WRITE(OFU,IOSTAT=FS) NORD32F(REAL((/TDATA,TDATA/),C_FLOAT),MCBE)
    IF (FS.NE.0) GOTO 900
    IF (J.LT.ZDATA) GOTO 10
    ! --------------+
    ! END MAIN LOOP |
    ! ..........................................................................
    CALL AU_CLOSE(OFU,STAT=FS)
    IF (FS.NE.0) GOTO 900
    RETURN
    ! --- END CODE  ---
700 FORMAT('MZSYN_GENERATE:',1X,999(:,A,G12.5))
800 FORMAT('MZSYN_GENERATE: ERROR WRITING OUTPUT FILE',1X,A,1X,'TO UNIT',1X,I0)
900 WRITE(*,800) TRIM(OFN),OFU ; STOP    
  END SUBROUTINE MZSYN_GENERATE

  ELEMENTAL FUNCTION MZSYNTH_CLIP(X)
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
    REAL(KIND=RKIND) :: MZSYNTH_CLIP
    ! --- DUMMIES ---
    REAL(KIND=RKIND), INTENT(IN) :: X
    ! --- EXE CODE ---
    IF (ABS(X).LT.1) THEN
       MZSYNTH_CLIP=0.5_RKIND*(3.0_RKIND*X-X**3)
    ELSE
       MZSYNTH_CLIP=SIGN(1.0_RKIND,X) ! VALUE OF 1.0_R WITH SIGN OF X
    END IF
    ! --- END CODE ---
  END FUNCTION MZSYNTH_CLIP
END PROGRAM
