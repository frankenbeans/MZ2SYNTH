!+============================================================================+
!| FIMAGEMOD.F95                                                              |
!|                                                                            |
!| FORTRAN 2003 routines for reading and writing binary or plain PGM and PPM  |
!| image files.  Both 8-bit and 16-bit variants of the PGM and PPM formats    |
!| are supported.                                                             |
!|                                                                            |
!| In the FImage record, RGB data is stored in two-dimensional arrays that    |
!| associated with the pointers dred, dgrn and dblu.  Data is indexed by      |
!| (col,row);  for example, the red value of the top left-hand pixel of the   |
!| image is dred(1,1) and the green value of the bottom right-hand is         |
!| dgrn(ncols,nrows).  It may be useful to recall that FORTRAN stores 2D      |
!| arrays in column major order.                                              |
!|                                                                            |
!| PGM data is converted and stored internally as RGB data, where RGB tuples  |
!| are selected so as to have the same luminance as the original PGM data.    |
!| Upon writing in the PGM format, RGB data is converted to luminance values  |
!| which are then written to the output file.                                 |
!|                                                                            |
!| It is safe to change any of the following values between reading and       |
!| writing an FImage record:                                                  |
!|                                                                            |
!|   * maxval                                                                 |
!|   * dred, dgrn and dblu                                                    |
!|   * subtype                                                                |
!|                                                                            |
!| The library will simply write the output file in the formated indicated by |
!| subtype and maxval;  for example, changing the subtype from PPM to PGM     |
!| will result in a PGM file being written using the stored RGB data.         |
!|                                                                            |
!| Changing ncols or nrows is not safe.                                       |
!|                                                                            |
!| SUPPORTED PLATFORMS:  Fortran 2003 (for stream I/O);  preprocessing by the |
!| -------------------   C preprocessor is required in order to resolve the   |
!|                       portability macros as indicated below.               |
!|                                                                            |
!| BUGS AND LIMITATIONS:                                                      |
!| --------------------                                                       |
!|                   (i) Only single-layer formats are supported.             |
!|                                                                            |
!|                  (ii) Am pretty sure that the portability macros are not   |
!|                       required with current versions of GFortran.  Should  |
!|                       seriously look at removing this ugly kludge.         |
!|                       (PRI=MED)                                            |
!|                                                                            |
!| Copyright (C) 2023 by E. Lamprecht.  All rights reserved.                  |
!+============================================================================+

! $CVSHeader: MZ2/SOURCE/fimagemod.f95,v 1.3 2024/11/09 22:00:08 emman Exp $

MODULE FImageMod
  USE Constant,    ONLY: RKIND
  USE SFRealComp , ONLY: RNZ
  IMPLICIT NONE

  INTEGER,PARAMETER :: FIM_bufferlen=1024
  INTEGER,PARAMETER :: FIM_comlen   =128
  INTEGER,PARAMETER :: FIM_ncomments=64

  ENUM,BIND(C) 
  ENUMERATOR :: FIM_subtype_nul=0,  &  ! Un-initialized structure
                FIM_subtype_pgm=1,  &  ! PGM grayscale PnM image
                FIM_subtype_ppm=2      ! PPM RGB PnM image
  ENUMERATOR :: FIM_nchan_pgm  =1,  &  ! Number of channels in PGM image
                FIM_nchan_ppm  =3      ! Number of channels in PPM image
  ENUMERATOR :: FIM_error_nul  =0,  &  ! Un-initialized structure
                FIM_error_ok   =-1, &  ! No error
                FIM_error_frd  =1,  &  ! File read  error
                FIM_error_fwt  =2,  &  ! File write error
                FIM_error_pre  =3,  &  ! Premature EOF
                FIM_error_hdr  =4,  &  ! Invalid header
                FIM_error_dat  =5      ! Invalid data
  ENUMERATOR :: FIM_grscl_nul  =0,  &  ! Un-initialized structure
                FIM_grscl_amn  =1,  &  ! Grey = arithmetic mean (r,g,b)
                FIM_grscl_gmn  =2,  &  ! Grey = geometric  mean (r,g,b)
                FIM_grscl_mnv  =3,  &  ! Grey = minimum value (r,g,b)
                FIM_grscl_mxv  =4,  &  ! Grey = maximum value (r,g,b)
                FIM_grscl_rvl  =5,  &  ! Grey = r
                FIM_grscl_gvl  =6,  &  ! Grey = g
                FIM_grscl_bvl  =7,  &  ! Grey = b
                FIM_grscl_lum  =8      ! Grey = Luminance (r,g,b)
  END ENUM

  TYPE FImage
     INTEGER                                 :: subtype=FIM_subtype_nul
     INTEGER                                 :: ncols=0,nrows=0
     INTEGER                                 :: maxval=0
     REAL(KIND=RKIND),DIMENSION(:,:),POINTER :: dred=>NULL() ! Red   channel
     REAL(KIND=RKIND),DIMENSION(:,:),POINTER :: dgrn=>NULL() ! Green channel
     REAL(KIND=RKIND),DIMENSION(:,:),POINTER :: dblu=>NULL() ! Blue  channel
     CHARACTER(LEN=FIM_comlen)               :: comments(1:FIM_ncomments)=''
     INTEGER                                 :: ncomments=0
     INTEGER                                 :: ecode=FIM_error_nul
  END TYPE FImage

CONTAINS ! /// ------------------------------------------------------------ ///
!==============================================================================
  SUBROUTINE FIM_new(fim,ncols,nrows,maxval,subtype)
    ! +-----------------------------------------------------------------------+
    ! | ALLOCATE, and link to pointers in fim, memory for an RGB image of     |
    ! | dimensions ncols*nrows, specifying the PGM/PPM maxval parameter as    |
    ! | maxval and specifying the type of the PNM image as subtype.           |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT) :: fim
    INTEGER,     INTENT(IN)    :: nrows,ncols,maxval,subtype
    ! --- Variables       ---
    INTEGER :: ms
    ! --- Executable code ---
    ! +-----------------------------------------------------------------------+
    ! | Deallocate all allocatable members of FImage fim, and zero the rest.  |
    ! +-----------------------------------------------------------------------+
    CALL FIM_clear(fim)
    ! +-------------------+
    ! | Validation checks |
    ! +-------------------+
    IF (nrows.LT.1) GOTO 900
    IF (ncols.LT.1) GOTO 910
    IF (maxval.LT.1.OR.maxval.GT.2**16-1) GOTO 920
    IF (subtype.LT.FIM_subtype_pgm.OR.subtype.GT.FIM_subtype_ppm) GOTO 930
    ! +-----------------+
    ! | Initializations |
    ! +-----------------+
    fim%ncols=ncols
    fim%nrows=nrows
    fim%maxval=maxval
    fim%subtype=subtype
    ! +-------------------------------------------------------------+
    ! | Allocate memory for RGB data according to image dimensions. |
    ! +-------------------------------------------------------------+
    ALLOCATE(fim%dred(1:ncols,1:nrows),STAT=ms) ; IF (ms.NE.0) GOTO 940
    ALLOCATE(fim%dgrn(1:ncols,1:nrows),STAT=ms) ; IF (ms.NE.0) GOTO 940
    ALLOCATE(fim%dblu(1:ncols,1:nrows),STAT=ms) ; IF (ms.NE.0) GOTO 940
    fim%ecode=FIM_error_ok
    ! +------------------------------------------------------------------+
    ! | Note:  Lookup tables not initialized here, because they are only |
    ! | ----   required when reading or writing files in PPM or PGM      |
    ! |        formats.  They are initialized just in time if needed.    |
    ! +------------------------------------------------------------------+
    ! --- END CODE ---
    GOTO 999
900 WRITE(*,991) 'nrows', nrows
    STOP
910 WRITE(*,991) 'ncols', ncols
    STOP
920 WRITE(*,993) maxval
    STOP
930 WRITE(*,994) subtype
    STOP
940 WRITE(*,992) 'Out of memory'
    STOP
991 FORMAT('*** ERROR (FIM_new): invalid ', A, I6, '; should be >=1')
992 FORMAT('*** ERROR (FIM_new): ', A)
993 FORMAT('*** ERROR (FIM_new): invalid maxval  ', I6, '; should be <=65536')
994 FORMAT('*** ERROR (FIM_new): invalid subtype ', I6)
999 CONTINUE
  END SUBROUTINE FIM_new
!==============================================================================
  SUBROUTINE FIM_copy(fim,sim)
    ! +-----------------------------------------------------------------------+
    ! | Make an identical copyof sim in fim.                                  |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT) :: fim
    TYPE(Fimage),INTENT(IN)    :: sim
    ! --- Variables ---
    INTEGER :: c,r
    ! --- Executable code ---
    ! +-------------------+
    ! | Validation checks |
    ! +-------------------+
    IF (sim%nrows.LT.1) GOTO 900
    IF (sim%ncols.LT.1) GOTO 910
    IF (sim%maxval.LT.1.OR.sim%maxval.GT.2**16-1) GOTO 920
    IF (sim%subtype.LT.FIM_subtype_pgm.OR.sim%subtype.GT.FIM_subtype_ppm) GOTO 930
    ! +--------------------------------+
    ! | Create new image based on sim. |
    ! +--------------------------------+
    CALL FIM_new(fim,sim%ncols,sim%nrows,sim%maxval,sim%subtype)
    !$OMP PARALLEL DO
    DO r=1,fim%nrows
       DO c=1,fim%ncols
          fim%dred(c,r)=sim%dred(c,r)
          fim%dgrn(c,r)=sim%dgrn(c,r)
          fim%dblu(c,r)=sim%dblu(c,r)
       END DO
    END DO
    !$OMP END PARALLEL DO
    fim%comments=sim%comments
    fim%ncomments=sim%ncomments
    fim%ecode=sim%ecode
    ! --- End code        ---
    GOTO 999
900 WRITE(*,991) 'nrows', sim%nrows
    STOP
910 WRITE(*,991) 'ncols', sim%ncols
    STOP
920 WRITE(*,992) sim%maxval
    STOP
930 WRITE(*,993) sim%subtype
    STOP
991 FORMAT('*** ERROR (FIM_copy): invalid ', A, I6, '; should be >=1')
992 FORMAT('*** ERROR (FIM_copy): invalid maxval  ', I6, '; should be <=65536')
993 FORMAT('*** ERROR (FIM_copy): invalid subtype ', I6)
999 CONTINUE
  END SUBROUTINE FIM_copy
!==============================================================================
  SUBROUTINE FIM_read(fim,fname,funit)
    ! +-----------------------------------------------------------------------+
    ! | Read the file named fname, using unit funit, and store the PNM data   |
    ! | in the FImage structure fim.  Note that this function calls FIM_new   |
    ! | to create a new image structure.                                      |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    CHARACTER(LEN=*),INTENT(IN)    :: fname
    INTEGER         ,INTENT(IN)    :: funit
    ! --- Parameters      ---
    INTEGER,PARAMETER              :: PFORM_asc =0,PFORM_bin=1
    ! --- Variables       ---
    CHARACTER(LEN=2)               :: pcode
    INTEGER                        :: psubtype
    INTEGER                        :: pform
    INTEGER                        :: pnbps
    INTEGER                        :: nchan
    INTEGER                        :: pncols, pnrows, pmaxval
    CHARACTER(LEN=FIM_bufferlen)   :: linebuf
    INTEGER,ALLOCATABLE            :: tmp(:)
    INTEGER                        :: ms
    CHARACTER(LEN=FIM_comlen)      :: tcomments(1:FIM_ncomments)
    INTEGER                        :: j,tncomments
    LOGICAL                        :: pcb_rderr
    ! --- Executable code ---
    ! +-----------------------------+
    ! | Preliminary initializations |
    ! +-----------------------------+
    CALL FIM_clear(fim) ! Clear up everything before starting.
    pform=0  ! Shut up -Wall
    tncomments=0
    tcomments=''
    ! +---------------------------------------------------------------+
    ! | Read header and figure out size, type and maxval of PNM file. |
    ! | Also work out if this is a binary or 'plain' PNM.             |
    ! +---------------------------------------------------------------+
    OPEN(UNIT=funit,FILE=fname,ACTION='READ',STATUS='OLD', &
         ACCESS='STREAM',FORM='FORMATTED',ERR=900)
    ! +-------------------------------------------------+
    ! | Read pcode line, storing any leading comments.  |
    ! +-------------------------------------------------+
    CALL process_comments_and_blanks(pcb_rderr) ; IF (pcb_rderr) GOTO 910
    READ(UNIT=linebuf,FMT=*,ERR=920) pcode
    ! +--------------------------------------------------+
    ! | Is this a PGM (grayscale) or a PPM (RGB) image ? |
    ! +--------------------------------------------------+
    SELECT CASE(pcode)
    CASE('P2','P5')             ! +-------------------+
       psubtype=FIM_subtype_pgm ! | It's a PGM image. |
       nchan=FIM_nchan_pgm      ! +-------------------+
    CASE('P3','P6')             ! +-------------------+ 
       psubtype=FIM_subtype_ppm ! | It's a PPM image. |
       nchan=FIM_nchan_ppm      ! +-------------------+
    CASE DEFAULT                ! +-------------------+
       GOTO 910                 ! | Invalid PNM type. |
    END SELECT                  ! +-------------------+
    ! +-----------------------------------------+
    ! | Is this a 'plain' or binary PNM image ? |
    ! +-----------------------------------------+
    SELECT CASE(pcode)
    CASE('P2','P3')    ! +----------------+
       pform=PFORM_asc ! | It's plain.    |
    CASE('P5','P6')    ! +----------------+
       pform=PFORM_bin ! | It's binary.   |
    CASE DEFAULT       ! +----------------+
       GOTO 910        ! | Oops, invalid! |
    END SELECT         ! +----------------+
    ! +-------------------------------------------------------------+
    ! | Read <ncols> x <nrows> line, storing any leading comments.  |
    ! +-------------------------------------------------------------+
    CALL process_comments_and_blanks(pcb_rderr)  ; IF (pcb_rderr) GOTO 920
    READ(UNIT=linebuf,FMT=*,ERR=920) pncols, pnrows
    IF (pncols.LT.1.OR.pnrows.LT.1) GOTO 920
    ! +----------------------------------------------------+
    ! | Read <maxval> line, storing  any leading comments, |
    ! | and check for validity.                            |
    ! +----------------------------------------------------+
    CALL process_comments_and_blanks(pcb_rderr)  ; IF (pcb_rderr) GOTO 930
    READ(UNIT=linebuf,FMT=*,ERR=930) pmaxval
    IF (pmaxval.LT.1.OR.pmaxval.GT.65535) GOTO 930 ! Oops, invalid!
    IF (pmaxval.LE.255) THEN
       pnbps=1
    ELSE
       pnbps=2
    END IF
    ! +----------------------------------------------------+
    ! | Create new FIM structure now that we know all the  |
    ! | necessary parameters, and add stored comments.     |
    ! +----------------------------------------------------+
    CALL FIM_new(fim,pncols,pnrows,pmaxval,psubtype)
    fim%ecode=FIM_error_ok ;
    DO j=1,tncomments
       CALL FIM_add_comment(fim,tcomments(j))
    END DO
    ! +-------------------------------------------------------------+
    ! | Allocate temporary space for reading in grayscale/RGB data. |
    ! +-------------------------------------------------------------+
    ALLOCATE(tmp(1:nchan*fim%ncols*fim%nrows),STAT=ms)
    IF (ms.NE.0) GOTO 940
    ! +----------------------------------------------------------------+
    ! | Select the correct internal routine to read in data, depending |
    ! | on whether this is a plain or binary PNM image.  After this    |
    ! | operation, the tmp array contains the numbers that represented |
    ! | the grayscale or RGB data in the input file, with no scaling   |
    ! | or gamma de-correction.                                        |
    ! +----------------------------------------------------------------+
    IF (pform.EQ.PFORM_asc) THEN      ! +--------------------------+
       CALL read_asc_data             ! | Read in plain PNM data.  |
    ELSE IF (pform.EQ.PFORM_bin) THEN ! +--------------------------+
       CALL read_bin_data             ! | Read in binary PNM data. |
    ELSE                              ! +--------------------------+
       GOTO 910                       ! | Oops, invalid.           |
    END IF                            ! +--------------------------+
    ! +----------------------------------------------------------------+
    ! | Kick out to the end of the routine if error was encountered in |
    ! | whichever "read" routine was called.                           |
    ! +----------------------------------------------------------------+
    IF (fim%ecode.NE.FIM_error_OK) GOTO 999
    ! +----------------------------------------------------------------+
    ! | Rearrange, and do gamma de-correction, RGB data.  After this   |
    ! | operation, the fim%dred, fim%dgrn and fim%dblu grids will      |
    ! | contain gamma-decorrected linear brightness data scaled from   |
    ! | 0.0 to 1.0.  Note that PGM data is converted to RGB tuples     |
    ! | with luminance equal to that of the linearized PGM data.       |
    ! +----------------------------------------------------------------+
    CALL arrange_data
    ! +----------------------------------------------------+
    ! | Close input file and deallocate temporary storage. |
    ! +----------------------------------------------------+
    ! --- END CODE ---
    GOTO 999
900 WRITE(*,991) TRIM(fname)
    fim%ecode=FIM_error_frd ; GOTO 999
910 WRITE(*,992) TRIM(fname), ' ID code ("P2", "P3", "P5" or "P6")'
    fim%ecode=FIM_error_hdr ; GOTO 999
920 WRITE(*,992) TRIM(fname), '<width> x <height>, with both >=1'
    fim%ecode=FIM_error_hdr ; GOTO 999
930 WRITE(*,992) TRIM(fname), '<maxval> >=1 and <=65535'
    fim%ecode=FIM_error_hdr ; GOTO 999
940 WRITE(*,993) TRIM(fname), 'Out of memory for tmp storage.'
    STOP
991 FORMAT('*** WARNING (FIM_read_pnm): Cannot read input file '  ,A)
992 FORMAT('*** WARNING (FIM_read_pnm): Error or EOF in "',A, '", expecting ',A)
993 FORMAT('*** ERROR (FIM_read_pnm): While reading input file ',A,', ',A)
! +-------------------------------------------------------+
! | Common exit-point for this routine.  Clean up & quit. |
! +-------------------------------------------------------+
999 CALL tidy ;

  CONTAINS ! /// ----------------------------------------------------- ///

    SUBROUTINE process_comments_and_blanks(rderr)
      IMPLICIT NONE
      LOGICAL,INTENT(INOUT) :: rderr
      ! +-----------------------------------------------------------------+
      ! | Skip blank lines and load comments.                             |
      ! +-----------------------------------------------------------------+
10    READ(UNIT=funit,FMT='(A)',END=900,ERR=900) linebuf
      linebuf=ADJUSTL(linebuf)
      IF (linebuf(1:1).EQ.'#') THEN
         IF (tncomments.LT.FIM_ncomments) THEN
            tncomments=tncomments+1
            tcomments(tncomments)=linebuf(1:FIM_comlen)
         END IF
         GOTO 10
      ELSE IF (linebuf.EQ.'') THEN
         ! --- Ignore blank lines ---
         GOTO 10
      END IF
      rderr=.FALSE.
      ! --- END CODE ---
      GOTO 999
900   rderr=.TRUE.
999   CONTINUE
    END SUBROUTINE process_comments_and_blanks

    SUBROUTINE read_asc_data
      IMPLICIT NONE
      ! +-----------------------------------------------------------------+
      ! | Read huge chunk of ASCII data into temporary storage array and  |
      ! | crash with an error on premature EOF.                           |
      ! +-----------------------------------------------------------------+
      READ(UNIT=funit,FMT=*,ERR=900,END=900) tmp          
      ! --- END CODE ---
      GOTO 999
900   WRITE(*,991) TRIM(fname)
      fim%ecode=FIM_error_pre ; GOTO 999
991   FORMAT('*** ERROR (FIM_read_pnm): Prem. EOF before asc. data in ',A)
999   CONTINUE
    END SUBROUTINE read_asc_data

    SUBROUTINE read_bin_data
      IMPLICIT NONE
      CHARACTER(LEN=1),ALLOCATABLE :: chtmp(:)
      INTEGER                      :: ms,fp,j
      ! +-----------------------------------------------------------------+
      ! | Allocate chunk of memory to contain stream of binary PNM bytes. |
      ! +-----------------------------------------------------------------+
      ALLOCATE(chtmp(1:pnbps*nchan*FIM%ncols*FIM%nrows),STAT=ms)
      IF (ms.NE.0) GOTO 910
      ! +-----------------------------------------------------------------+
      ! | Determine current position in input file (i.e., end of header), |
      ! | close and re-open file in unformatted mode for binary transfer, |
      ! | re-position to end of header in the input file and read binary  |
      ! | data in one go.                                                 |
      ! +-----------------------------------------------------------------+
      INQUIRE(UNIT=funit,POS=fp)
      CLOSE(funit)
      OPEN(UNIT=funit,FILE=fname,ACTION='READ',STATUS='OLD', &
           ACCESS='STREAM',FORM='UNFORMATTED',ERR=900)
      READ(UNIT=funit,POS=fp)
      READ(UNIT=funit,ERR=900,END=920) chtmp
      ! +-----------------------------------------------------------------+
      ! | Interpret chunk of memory as one- or two-byte integers, depend- |
      ! | ing on whether pnbps is 1 or 2, and populate tmp store accord-  |
      ! | ingly.                                                          |
      ! +-----------------------------------------------------------------+
      IF (pnbps.EQ.1) THEN
         !$OMP PARALLEL WORKSHARE
         tmp=IACHAR(chtmp)
         !$OMP END PARALLEL WORKSHARE
      ELSE IF (pnbps.EQ.2) THEN
         !$OMP PARALLEL WORKSHARE
         tmp=(/(ISHFT(IACHAR(chtmp(j)),8)+IACHAR(chtmp(j+1)), &
              j=1,SIZE(chtmp),2) /)
         !$OMP END PARALLEL WORKSHARE
      END IF
      ! --- END CODE ---
      GOTO 999
900   WRITE(*,991) TRIM(fname)
      fim%ecode=FIM_error_pre ; GOTO 999
910   WRITE(*,992) 'Out of memory'
      STOP
920   WRITE(*,993) TRIM(fname)
      fim%ecode=FIM_error_frd ; GOTO 999
991   FORMAT('*** ERROR (FIM_read_pnm): Prem. EOF before bin. data in ',A)
992   FORMAT('*** ERROR (FIM_read_pnm): ',A)
993   FORMAT('*** ERROR (FIM_read_pnm): Cannot re-open file ',A)
! +----------------------------------------------------------+
! | Common exit point --- deallocate chtmp and quit routine. |
! +----------------------------------------------------------+
999   DEALLOCATE(chtmp)          
    END SUBROUTINE read_bin_data

    SUBROUTINE arrange_data
      IMPLICIT NONE
      INTEGER  :: n,m,r,c
      ! +-----------------------------------------------------------------+
      ! | Put temporary (linear) array into 2D r,g,b arrays in column-    |
      ! | major order, and do anti-gamma correction at the same time.     |
      ! +-----------------------------------------------------------------+
      n=0
      m=SIZE(tmp)
      IF (fim%subtype.EQ.FIM_subtype_ppm) THEN
         !$OMP PARALLEL DO PRIVATE(n)
         DO r=1,fim%nrows
            DO c=1,fim%ncols
               n=nchan*((r-1)*fim%ncols+c)
               fim%dred(c,r)=GCL(real(tmp(n-2),kind=rkind)/fim%maxval)
               fim%dgrn(c,r)=GCL(real(tmp(n-1),kind=rkind)/fim%maxval)
               fim%dblu(c,r)=GCL(real(tmp(n-0),kind=rkind)/fim%maxval)
            END DO
         END DO
         !$OMP END PARALLEL DO
      ELSE
         !$OMP PARALLEL DO PRIVATE(n)
         DO r=1,fim%nrows
            DO c=1,fim%ncols
               n=nchan*(r-1)*fim%ncols+c
               fim%dred(c,r)=GCL(real(tmp(n),kind=rkind)/fim%maxval)
               fim%dgrn(c,r)=GCL(real(tmp(n),kind=rkind)/fim%maxval)
               fim%dblu(c,r)=GCL(real(tmp(n),kind=rkind)/fim%maxval)
            END DO
         END DO
         !$OMP END PARALLEL DO
      END IF
    END SUBROUTINE arrange_data

    SUBROUTINE tidy
      IMPLICIT NONE
      LOGICAL :: fopen
      INQUIRE(UNIT=funit,OPENED=fopen)
      IF (fopen) CLOSE(UNIT=funit)
      IF (ALLOCATED(tmp))   DEALLOCATE(tmp)
    END SUBROUTINE tidy
  END SUBROUTINE FIM_read
!==============================================================================
  SUBROUTINE FIM_write(fim,fname,funit,maxval,subtype,plain)
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    CHARACTER(LEN=*),INTENT(IN)    :: fname
    INTEGER         ,INTENT(IN)    :: funit
    INTEGER,OPTIONAL,INTENT(IN)    :: maxval
    INTEGER,OPTIONAL,INTENT(IN)    :: subtype
    LOGICAL,OPTIONAL,INTENT(IN)    :: plain
    ! --- Parameters      ---
    INTEGER,PARAMETER              :: LBT  =2**8-1
    ! --- Variables       ---
    INTEGER                        :: ms,nbps,nsmp,nchan
    INTEGER                        :: r,c,n,j
    CHARACTER(LEN=1),ALLOCATABLE   :: chtmp(:)
    CHARACTER(LEN=2)               :: pcode
    LOGICAL                        :: tplain
    ! --- Executable code ---
    ! *** /////////////////////////////////////////////////////////////////////
    ! *** /// SHOULD THESE MODIFY THE STRUCTURE, OR SHOULD WE USE TEMPORARY ///
    ! *** /// VERSIONS OF THIS INSTEAD?                                     ///
    ! *** /////////////////////////////////////////////////////////////////////
    ! +-----------------------------------------------------------------------+
    ! | Put subtype and maxval parameters in FImage structure fim before      |
    ! | writing to output file.  Next write will use the same values if the   |
    ! | optional parameters are not present.                                  |
    ! |                                                                       |
    ! | By default, the values already in the structure are used if the       |
    ! | optional parameters are not specified.                                |
    ! +-----------------------------------------------------------------------+
    IF (PRESENT(subtype)) fim%subtype=subtype
    IF (PRESENT(maxval))  fim%maxval =maxval
    tplain=.FALSE. ; IF (PRESENT(plain)) tplain=plain
    fim%ecode=FIM_error_OK
    ! +-----------------------------------------------------------------------+
    ! | Figure out nbps (# bytes/sample) and nchan (#channels) from subtype   |
    ! | and maxval.                                                           |
    ! +-----------------------------------------------------------------------+
    IF (fim%subtype.EQ.FIM_subtype_pgm.AND.fim%maxval.LE.2**8-1) THEN
       nbps=1
       nchan=FIM_nchan_pgm
       IF (tplain.EQV..TRUE.) THEN
          pcode='P2'
       ELSE
          pcode='P5'
       END IF
    ELSE IF (fim%subtype.EQ.FIM_subtype_pgm.AND.fim%maxval.LE.2**16-1) THEN
       nbps=2
       nchan=FIM_nchan_pgm       
       IF (tplain.EQV..TRUE.) THEN
          pcode='P2'
       ELSE
          pcode='P5'
       END IF
    ELSE IF (fim%subtype.EQ.FIM_subtype_ppm.AND.fim%maxval.LE.2**8-1) THEN
       nbps=1
       nchan=FIM_nchan_ppm
       IF (tplain.EQV..TRUE.) THEN
          pcode='P3'
       ELSE
          pcode='P6'
       END IF
    ELSE IF (fim%subtype.EQ.FIM_subtype_ppm.AND.fim%maxval.LE.2**16-1) THEN
       nbps=2
       nchan=FIM_nchan_ppm
       IF (tplain.EQV..TRUE.) THEN
          pcode='P3'
       ELSE
          pcode='P6'
       END IF
    ELSE
       GOTO 900
    END IF

    ! +-----------------------------------------------------------------------+
    ! | Check that image contains some data and calculate the number of RGB   |
    ! | size based in nchan and the image dimensions.                         |
    ! +-----------------------------------------------------------------------+
    IF (fim%nrows.LT.1.OR.fim%nrows.LT.1) GOTO 910
    nsmp=nchan*fim%ncols*fim%nrows
    ! +-----------------------------------------------------------------------+
    ! | Allocate chunk of memory to hold binary output data for writing.      |
    ! +-----------------------------------------------------------------------+
    ALLOCATE(chtmp(1:nsmp*nbps),STAT=ms)
    IF (ms.NE.0) GOTO 920
    ! +-----------------------------------------------------------------------+
    ! | Populate temporary memory chunk with binary data as appropriate, in   |
    ! | accordance with the desired subtype and maxval parameters.            |
    ! | This stuff uses as few subroutine calls as possible to speed up the   |
    ! | execution, so it looks a bit repetitive.                              |
    ! +-----------------------------------------------------------------------+
    IF (fim%subtype.EQ.FIM_subtype_pgm.AND.fim%maxval.LE.2**8-1) THEN
       ! +-------------------------+
       ! | Handle  8-bit PGM data. |
       ! +-------------------------+
       !$OMP PARALLEL DO PRIVATE(n)
       DO r=1,fim%nrows
          DO c=1,fim%ncols
             n=nchan*nbps*((r-1)*fim%ncols+c) 
             chtmp(n)=ACHAR(NINT(fim%maxval*LGC(Luminance(fim%dred(c,r), &
                                                          fim%dgrn(c,r), &
                                                          fim%dblu(c,r)))))
          END DO
       END DO
       !$OMP END PARALLEL DO
    ELSE IF (fim%subtype.EQ.FIM_subtype_pgm.AND.fim%maxval.LE.2**16-1) THEN
       ! +-------------------------+
       ! | Handle 16-bit PGM data. |
       ! +-------------------------+
       !$OMP PARALLEL DO PRIVATE(n)
       DO r=1,fim%nrows
          DO c=1,fim%ncols
             n=nchan*nbps*((r-1)*fim%ncols+c)
             chtmp(n-1)=ACHAR(ISHFT(NINT(fim%maxval*LGC(Luminance(fim%dred(c,r), &
                                                                  fim%dgrn(c,r), &
                                                                  fim%dblu(c,r)))),-8))
             chtmp(n-0)=ACHAR( IAND(NINT(fim%maxval*LGC(Luminance(fim%dred(c,r), &
                                                                  fim%dgrn(c,r), &
                                                                  fim%dblu(c,r)))),LBT))
          END DO
       END DO
       !$OMP END PARALLEL DO
    ELSE IF (fim%subtype.EQ.FIM_subtype_ppm.AND.fim%maxval.LE.2**8-1) THEN
       ! +-------------------------+
       ! | Handle  8-bit PPM data. |
       ! +-------------------------+
       !$OMP PARALLEL DO PRIVATE(n)
       DO r=1,fim%nrows
          DO c=nbps,fim%ncols
             n=nchan*nbps*((r-1)*fim%ncols+c)
             chtmp(n-2)=ACHAR(NINT(fim%maxval*LGC(fim%dred(c,r))))
             chtmp(n-1)=ACHAR(NINT(fim%maxval*LGC(fim%dgrn(c,r))))
             chtmp(n-0)=ACHAR(NINT(fim%maxval*LGC(fim%dblu(c,r))))
          END DO
       END DO
       !$OMP END PARALLEL DO       
    ELSE IF (fim%subtype.EQ.FIM_subtype_ppm.AND.fim%maxval.LE.2**16-1) THEN
       ! +-------------------------+
       ! | Handle 16-bit PPM data. |
       ! +-------------------------+
       !$OMP PARALLEL DO PRIVATE(n)
       DO r=1,fim%nrows
          DO c=1,fim%ncols
             n=nchan*nbps*((r-1)*fim%ncols+c)
             chtmp(n-5)= &
                  ACHAR(ISHFT(NINT(fim%maxval*LGC(fim%dred(c,r))),-8))
             chtmp(n-4)= &
                  ACHAR( IAND(NINT(fim%maxval*LGC(fim%dred(c,r))),LBT))
             chtmp(n-3)= &
                  ACHAR(ISHFT(NINT(fim%maxval*LGC(fim%dgrn(c,r))),-8))
             chtmp(n-2)= &
                  ACHAR( IAND(NINT(fim%maxval*LGC(fim%dgrn(c,r))),LBT))
             chtmp(n-1)= &
                  ACHAR(ISHFT(NINT(fim%maxval*LGC(fim%dblu(c,r))),-8))
             chtmp(n-0)= &
                  ACHAR( IAND(NINT(fim%maxval*LGC(fim%dblu(c,r))),LBT))
          END DO
       END DO
       !$OMP END PARALLEL DO
    ELSE
       ! +----------------+
       ! | OOPS, invalid! |
       ! +----------------+
       GOTO 900
    END IF
    ! +-----------------------------------------------------------------------+
    ! | Open output file in FORMATTED mode and write file header, with stored |
    ! | comments all written immediately below the format code.               |
    ! +-----------------------------------------------------------------------+
    OPEN(UNIT=funit,FILE=fname, FORM='FORMATTED',ACTION='READWRITE', &
         ACCESS='STREAM', STATUS='REPLACE',ERR=930)
    WRITE(UNIT=funit,FMT=995,ERR=930) pcode
    DO j=1,fim%ncomments
       WRITE(UNIT=funit,FMT=996,ERR=930) TRIM(fim%comments(j))
    END DO
    WRITE(UNIT=funit,FMT=997,ERR=930) fim%ncols, fim%nrows, fim%maxval
    IF (tplain.EQV..FALSE.) THEN
       ! +----------------------------------------------------------+
       ! | Close and reopen output file in unformatted stream mode, |
       ! | setting up to append.                                    |
       ! +----------------------------------------------------------+
       CLOSE(UNIT=funit)
       OPEN(UNIT=funit,FILE=fname, FORM='UNFORMATTED',ACTION='READWRITE', &
            ACCESS='STREAM', STATUS='OLD',POSITION='APPEND',ERR=930)
       WRITE(UNIT=funit,ERR=930) chtmp
    ELSE
       ! +--------------------------------------------------------------------+
       ! | Write out data as ASCII in slightly different formats depending on |
       ! | whether we are writing a PPM or PGM image and depending on maxval. |
       ! +--------------------------------------------------------------------+
       IF (fim%subtype.EQ.FIM_subtype_pgm) THEN
          IF (fim%maxval.LE.2**8-1) THEN
             WRITE(UNIT=funit,FMT='(12(I6))',ERR=930) IACHAR(chtmp)
          ELSE
             WRITE(UNIT=funit,FMT='(12(I6))',ERR=930) &
                  (/(ISHFT(IACHAR(chtmp(j)),8)+IACHAR(chtmp(j+1)), &
                  j=1,SIZE(chtmp),2)/)
          END IF
       ELSE
          IF (fim%maxval.LE.2**8-1) THEN
             WRITE(UNIT=funit,ERR=930,FMT= &
               '(I6,I6,I6," ",I6,I6,I6," ",I6,I6,I6)') IACHAR(chtmp)
          ELSE
             WRITE(UNIT=funit,ERR=930,FMT= &
               '(I6,I6,I6," ",I6,I6,I6," ",I6,I6,I6)') &
               (/(ISHFT(IACHAR(chtmp(j)),8)+IACHAR(chtmp(j+1)), &
               j=1,SIZE(chtmp),2)/)
          END IF
       END IF
    END IF
    ! --- END CODE ---
    GOTO 999
900 WRITE(*,991) fim%subtype, fim%maxval
    fim%ecode=FIM_error_hdr ; GOTO 999
910 WRITE(*,992) fim%ncols, fim%nrows
    fim%ecode=FIM_error_hdr ; GOTO 999
920 WRITE(*,993) 'Out of memory.'
    STOP
930 WRITE(*,994) fname, funit
    fim%ecode=FIM_error_fwt ; GOTO 999
    ! --- FORMATS  ---
991 FORMAT('*** WARNING (FIM_write): invalid fim%subtype / fim%maxval ',I6,I6)
992 FORMAT('*** WARNING (FIM_write): invalid fim%ncols or fim%nrows: ',2(I12))
993 FORMAT('*** ERROR (FIM_write): ',A)
994 FORMAT('*** WARNING (FIM_write): cannot write output file ',A,' on unit',I6)
995 FORMAT(A2)
996 FORMAT(A)
997 FORMAT(I0,' ',I0,/,I0)
! +-----------------------------------------------------------+
! | Common exit point --- tidy up and exit from this routine. |
! +-----------------------------------------------------------+
999 CALL tidy

  CONTAINS ! ---------------- !

    SUBROUTINE tidy
      IMPLICIT NONE
      LOGICAL :: fopen
      INQUIRE(UNIT=funit,OPENED=fopen)
      IF (fopen) CLOSE(UNIT=funit)
      IF (ALLOCATED(chtmp)) DEALLOCATE(chtmp)
    END SUBROUTINE tidy
  END SUBROUTINE FIM_write
!==============================================================================
  SUBROUTINE FIM_clear(fim)
    ! +-----------------------------------------------------------------------+
    ! | Deallocate all pointers/allocatable members and zero the rest.        |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments --- 
    TYPE(FImage),INTENT(INOUT) :: fim
    ! --- Executable code ---
    fim%ncols =0
    fim%nrows =0
    fim%maxval=0
    ! +--------------------------------------------+
    ! | Deallocate image data arrays.              |
    ! | Note reverse order of memory deallocation. |
    ! +--------------------------------------------+
    IF (ASSOCIATED(fim%dblu)) DEALLOCATE(fim%dblu)
    IF (ASSOCIATED(fim%dgrn)) DEALLOCATE(fim%dgrn)
    IF (ASSOCIATED(fim%dred)) DEALLOCATE(fim%dred)
    
    ! +-------------------------------------------------+
    ! | Nullify pointers and initialize static members. |
    ! +-------------------------------------------------+
    NULLIFY(fim%dred,fim%dgrn,fim%dblu)
    fim%subtype=FIM_subtype_nul
    fim%ncomments=0
    fim%comments=''
    fim%ecode=FIM_error_nul
  END SUBROUTINE FIM_clear
!==============================================================================
  SUBROUTINE FIM_add_comment(fim,com)
    ! +-----------------------------------------------------------------------+
    ! | Store comment com in FImage structure fim.  Only the first FIM_comlen |
    ! | characters will be stored for each comment, and only FIM_ncomments    |
    ! | will be stored, any comments subsequently added being simply ignored. |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT) :: fim
    CHARACTER(LEN=*)           :: com
    ! --- Executable code ---    
    IF (fim%ncomments.LT.FIM_ncomments) THEN
       fim%ncomments=fim%ncomments+1
       fim%comments(fim%ncomments)=com
    END IF
  END SUBROUTINE FIM_add_comment
!==============================================================================
  SUBROUTINE FIM_read_unf(fim,fname,funit)
    ! +-----------------------------------------------------------------------+
    ! | Read FImage fim, unformatted and in stream mode, from the file 'fname'|
    ! | on unit funit.  The file is opened and closed again in this routine.  |
    ! | Note that this function calls FIM_new to create a new image structure.|
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT)  :: fim
    CHARACTER(LEN=*),INTENT(IN) :: fname
    INTEGER,INTENT(IN)          :: funit
    ! --- Variables       ---
    TYPE(FImage)                :: tfim
    ! --- Executable code ---    
    ! +-----------------------------------------------------------------+
    ! | Open input file (in READ mode).                                 |
    ! +-----------------------------------------------------------------+
    OPEN(UNIT=funit,FILE=fname,FORM='UNFORMATTED',ACTION='READ', &
         ACCESS='STREAM',STATUS='OLD',ERR=900)

    ! +-----------------------------------------------------------------+
    ! | Read header information in the correct order, and verify. A few |
    ! | moments' reflection will show that we do not need to read the   |
    ! | lookup tables --- they will be handled automatically if the     |
    ! | subroutines FIM_read() or FIM_write() need to be called.        |
    ! +-----------------------------------------------------------------+
    READ(UNIT=funit,ERR=900,END=900) tfim%subtype,  &
                                     tfim%ncols,    &
                                     tfim%nrows,    &
                                     tfim%maxval
    tfim%ecode=FIM_error_OK        ! Innocent until proven guilty
    CALL FIM_verify_header(tfim)
    IF (tfim%ecode.NE.FIM_error_OK) GOTO 910

    ! +------------------------------------------------------------------+
    ! | Initialize FIM structure using the information obtained from the |
    ! | header section of the unformatted input file.                    |
    ! +------------------------------------------------------------------+
    CALL FIM_new(fim,tfim%ncols,tfim%nrows,tfim%maxval,tfim%subtype)
    fim%ecode=FIM_error_OK

    ! +------------------------------------------------------------------+
    ! | Read the remaining information from the unformatted input file.  |
    ! +------------------------------------------------------------------+
    READ(UNIT=funit,ERR=900,END=900) fim%dred,     &
                                     fim%dgrn,     &
                                     fim%dblu,     &
                                     fim%comments, &
                                     fim%ncomments

    ! +------------------------------------------------------------------+
    ! | Close the input file.                                            |
    ! +------------------------------------------------------------------+
    CALL tidy
    ! --- END CODE ---
    GOTO 999
900 WRITE(*,990) 'Error reading file ' // fname
    fim%ecode=FIM_error_frd ; CALL tidy ; GOTO 999
910 WRITE(*,991) 'Bad header information in file ' // fname ; GOTO 999
990 FORMAT('*** ERROR (FIM_read_unf): ',A)
991 FORMAT('*** WARNING (FIM_read_unf): ',A)
999 CONTINUE

  CONTAINS ! -------------- !
    
    SUBROUTINE tidy
      IMPLICIT NONE
      LOGICAL :: fopen
      INQUIRE(UNIT=funit,OPENED=fopen)
      IF (fopen) CLOSE(UNIT=funit)      
    END SUBROUTINE tidy
    
  END SUBROUTINE FIM_read_unf
!==============================================================================
  SUBROUTINE FIM_write_unf(fim,fname,funit)
    ! +-----------------------------------------------------------------------+
    ! | Write FImage fim, unformatted and in stream mode, to the file 'fname' |
    ! | on unit funit.  The file is opened and closed again in this routine.  |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT)  :: fim
    CHARACTER(LEN=*),INTENT(IN) :: fname
    INTEGER,INTENT(IN)          :: funit
    ! --- Executable code ---
    fim%ecode=FIM_error_OK
    ! +-----------------------------------------------------------------+
    ! | First make sure you're not writing out an uninitialized FImage. |
    ! +-----------------------------------------------------------------+
    IF (.NOT.(ASSOCIATED(fim%dred).AND.ASSOCIATED(fim%dgrn).AND. & 
              ASSOCIATED(fim%dgrn))) GOTO 900
    ! +-----------------------------------------------------------------+
    ! | Now open output file (in REPLACE mode).                         |
    ! +-----------------------------------------------------------------+
    OPEN(UNIT=funit,FILE=fname,FORM='UNFORMATTED',ACTION='READWRITE', &
         ACCESS='STREAM',STATUS='REPLACE',ERR=910)

    ! +-----------------------------------------------------------------+
    ! | Write structure out in the correct order. We do not need to     |
    ! | worry about the lookup tables --- see note under FIM_read for   |
    ! | more information.                                               |
    ! +-----------------------------------------------------------------+
    WRITE(UNIT=funit,ERR=910) fim%subtype,  &
                              fim%ncols,    &
                              fim%nrows,    &
                              fim%maxval,   &
                              fim%dred,     &
                              fim%dgrn,     &
                              fim%dblu,     &
                              fim%comments, &
                              fim%ncomments

    ! +-------------------------------+
    ! | Close the file after writing. |
    ! +-------------------------------+
    CALL tidy
    ! --- END CODE ---
    GOTO 999
900 WRITE(*,990) 'FIM data pointer(s) unallocated.'
    STOP
910 WRITE(*,990) 'Error writing file ' // fname
    fim%ecode=FIM_error_fwt ; CALL tidy ; GOTO 999
990 FORMAT('*** ERROR (FIM_write_unf): ',A)
999 CONTINUE

  CONTAINS ! -------------- !
    
    SUBROUTINE tidy
      IMPLICIT NONE
      LOGICAL :: fopen
      INQUIRE(UNIT=funit,OPENED=fopen)
      IF (fopen) CLOSE(UNIT=funit)      
    END SUBROUTINE tidy

  END SUBROUTINE FIM_write_unf
!==============================================================================
  FUNCTION FIM_check_header_ok(fim)
    ! +-----------------------------------------------------------------------+
    ! | Return .TRUE. if the information in the header of FImage fim makes    |
    ! | sense, or .FALSE. otherwise.                                          |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Return type     ---
    LOGICAL                 :: FIM_check_header_ok
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(IN) :: fim
    ! --- Executable code --- 
    FIM_check_header_ok=(fim%subtype.EQ.FIM_subtype_pgm.OR.        &
                         fim%subtype.EQ.FIM_subtype_ppm).AND.      &
                        (fim%ncols .GE.1.AND.fim%nrows .GE.1.AND.  &
                         fim%maxval.GE.1.AND.fim%maxval.LE.2**16-1)
  END FUNCTION FIM_check_header_ok
!==============================================================================
  FUNCTION FIM_check_data_ok(fim)
    ! +-----------------------------------------------------------------------+
    ! | Return .FALSE. if any RGB data is less than 0 or greater than 1, or   |
    ! | otherwise .TRUE. indicating that all is OK.                           |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Return type     ---
    LOGICAL                 :: FIM_check_data_ok
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(IN) :: fim
    ! --- Executable code ---    
    IF (.NOT.(ASSOCIATED(fim%dred).AND.ASSOCIATED(fim%dgrn) &
        .AND.ASSOCIATED(fim%dblu))) GOTO 900

    IF (fim%subtype.EQ.FIM_subtype_ppm) THEN
       !$OMP PARALLEL WORKSHARE
       FIM_check_data_ok= &
            .NOT.(ANY(fim%dred.LT.0.OR.fim%dred.GT.1.OR. &
                      fim%dgrn.LT.0.OR.fim%dgrn.GT.1.OR. &
                      fim%dblu.LT.0.OR.fim%dblu.GT.1))
       !$OMP END PARALLEL WORKSHARE
    ELSE IF (fim%subtype.EQ.FIM_subtype_pgm) THEN
       !$OMP PARALLEL WORKSHARE
       FIM_check_data_ok= &
            .NOT.(ANY(Luminance(fim%dred,fim%dgrn,fim%dblu).LT.0.OR. &
                      Luminance(fim%dred,fim%dgrn,fim%dblu).GT.1))
       !$OMP END PARALLEL WORKSHARE
    ELSE
       GOTO 910
    END IF
    ! --- END CODE ---
    GOTO 999
900 WRITE(*,991) 'Data pointer(s) dred, dgrn and/or dblu not associated.'
    STOP
910 WRITE(*,991) 'Invalid subtype.'
    STOP
991 FORMAT('*** ERROR (FIM_check_data): ')
999 CONTINUE
  END FUNCTION FIM_check_data_ok
!==============================================================================
  SUBROUTINE FIM_verify_header(fim)
    ! +-----------------------------------------------------------------------+
    ! | Terminate program with an error message if the header of fim does not |
    ! | make sense, or do nothing otherwise.                                  |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT) :: fim
    ! --- Executable code ---
    IF (.NOT.FIM_check_header_ok(fim)) THEN
       WRITE(*,991)
       fim%ecode=FIM_error_hdr
    END IF
991 FORMAT('*** WARNING (FIM_verify_header): Invalid header info in fim.')
  END SUBROUTINE FIM_verify_header
!==============================================================================
  SUBROUTINE FIM_verify_data(fim)
    ! +-----------------------------------------------------------------------+
    ! | Terminate program with an error message if the data of fim does not   |
    ! | make sense, or do nothing otherwise.                                  |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT) :: fim
    ! --- Executable code ---
    IF (.NOT.FIM_check_data_ok(fim)) THEN
       WRITE(*,991)
       fim%ecode=FIM_error_dat
    END IF
991 FORMAT('*** WARNING (FIM_verify_data): Invalid RGB information in fim.')
  END SUBROUTINE FIM_verify_data
!==============================================================================
  SUBROUTINE FIM_fit_domain(fim)
    ! +-----------------------------------------------------------------------+
    ! | Adjust domain of linear RGB values to [0..1].                         |
    ! +-----------------------------------------------------------------------+
    ! --- Dummy arguments ---      
    IMPLICIT NONE
    TYPE(FImage),INTENT(INOUT)     :: fim
    ! --- Executable code ---
    !$OMP PARALLEL
    fim%dred=min(max(fim%dred,0.0_RKIND),1.0_RKIND)
    fim%dgrn=min(max(fim%dgrn,0.0_RKIND),1.0_RKIND)
    fim%dblu=min(max(fim%dblu,0.0_RKIND),1.0_RKIND)
    !$OMP END PARALLEL
    ! --- End code        ---
  END SUBROUTINE FIM_fit_domain
!==============================================================================
!==============================================================================
! * * *  C O L O U R   T R A N S F O R M A T I O N   F U N C T I O N S    * * *
!==============================================================================
  SUBROUTINE FIM_ctf_gray(fim,mthd)
    ! +-----------------------------------------------------------------------+
    ! | Wrapper for FIM_ctf_grey(fim, mthd)                                   |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    INTEGER,OPTIONAL,INTENT(IN)    :: mthd
    ! --- Executable code ---
    CALL FIM_ctf_grey(fim,mthd)
    ! --- End code        ---
  END SUBROUTINE FIM_ctf_gray
!==============================================================================
  SUBROUTINE FIM_ctf_grey(fim, mthd)
    ! +-----------------------------------------------------------------------+
    ! | Greyscale image fim using method specified in mthd, which must be in  |
    ! | the domain of the enumerator FIM_grscl_nul to FIM_grscl_lum .  See    |
    ! | comments under the enumerator for a summary of available algorithms.  |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    INTEGER,OPTIONAL,INTENT(IN)    :: mthd
    ! --- Parameters      ---
    INTEGER,PARAMETER :: dmthd=FIM_grscl_lum
    ! --- Variables       ---
    INTEGER :: lmthd
    ! --- Executable code ---
    lmthd=dmthd
    IF (PRESENT(mthd)) lmthd=mthd
    SELECT CASE(lmthd)
    CASE(FIM_grscl_nul)
       RETURN
    CASE (FIM_grscl_amn)
       !$OMP PARALLEL WORKSHARE
       fim%dred=AritMean(fim%dred,fim%dgrn,fim%dblu)
       fim%dgrn=fim%dred
       fim%dblu=fim%dred
       !$OMP END PARALLEL WORKSHARE
    CASE (FIM_grscl_gmn)
       !$OMP PARALLEL WORKSHARE
       fim%dred=GeomMean(fim%dred,fim%dgrn,fim%dblu)
       fim%dgrn=fim%dred
       fim%dblu=fim%dred
       !$OMP END PARALLEL WORKSHARE
    CASE (FIM_grscl_mnv)
       !$OMP PARALLEL WORKSHARE
       fim%dred=MIN(fim%dred,fim%dgrn,fim%dblu)
       fim%dgrn=fim%dred
       fim%dblu=fim%dred
       !$OMP END PARALLEL WORKSHARE
    CASE (FIM_grscl_mxv)
       !$OMP PARALLEL WORKSHARE
       fim%dred=MAX(fim%dred,fim%dgrn,fim%dblu)
       fim%dgrn=fim%dred
       fim%dblu=fim%dred
       !$OMP END PARALLEL WORKSHARE         
    CASE (FIM_grscl_rvl)
       !$OMP PARALLEL WORKSHARE
       fim%dgrn=fim%dred
       fim%dblu=fim%dred 
       !$OMP END PARALLEL WORKSHARE
    CASE (FIM_grscl_gvl)
       !$OMP PARALLEL WORKSHARE
       fim%dred=fim%dgrn
       fim%dblu=fim%dgrn
       !$OMP END PARALLEL WORKSHARE
    CASE (FIM_grscl_bvl)
       !$OMP PARALLEL WORKSHARE
       fim%dred=fim%dblu
       fim%dgrn=fim%dblu
       !$OMP END PARALLEL WORKSHARE
    CASE (FIM_grscl_lum)
       !$OMP PARALLEL WORKSHARE
       fim%dred=Luminance(fim%dred,fim%dgrn,fim%dblu)
       fim%dgrn=fim%dred
       fim%dblu=fim%dred
       !$OMP END PARALLEL WORKSHARE
    CASE DEFAULT
       GOTO 900
    END SELECT
    ! --- End code        ---
    GOTO 999
900 WRITE(*,991) LMTHD, FIM_grscl_nul, FIM_grscl_lum
    STOP
991 FORMAT('*** ERROR (FIM_ctf_gray): Invalid mthd =', I0, ', expecting ',    &
           I0, ' to ', I0)
999 RETURN
  END SUBROUTINE FIM_ctf_grey
!==============================================================================
  SUBROUTINE FIM_ctf_iter(fim,afun,adat,cmsk)
    ! +-----------------------------------------------------------------------+
    ! | Iterate the function 'afun' over the red (cmsk(1).eq.T),              |
    ! | green (cmsk(2).eq.T) and blue (cmsk(3).eq.T), of 'fim' passing the    |
    ! | real array 'adat' (indexed from zero) to afun.  The values of the red,|
    ! | green and blue arrays are replaced, so the programmer has to make a   |
    ! | copy of fim first if the original values must be preserved.           |
    ! |                                                                       |
    ! | The functions APOLY (polynomial), AEXPO (exponential), AAINV          |
    ! | (additive inverse), and AMINV (multiplicative inverse) functions are  |
    ! | provided as these are likely to be required.                          |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---
    TYPE(FImage),INTENT(INOUT)  :: fim
    INTERFACE
       PURE FUNCTION AFUN(X,CFS)
         USE Constant, ONLY:  RKIND
         IMPLICIT NONE
         ! ---INTERFACE RETURN TYPE----------
         REAL(RKIND)                 :: AFUN
         ! ---INTERFACE DUMMY ARGUMENTS -----
         REAL(KIND=RKIND),INTENT(IN) :: X
         REAL(KIND=RKIND),INTENT(IN) :: CFS(0:)
       END FUNCTION AFUN
    END INTERFACE
    REAL(KIND=RKIND),INTENT(IN) :: adat(0:)
    LOGICAL,OPTIONAL,INTENT(IN) :: cmsk(1:3)
    ! --- Variables       ---
    INTEGER :: c,r
    LOGICAL :: lcmsk(1:3)
    ! --- Executable code ---
    lcmsk=.TRUE.
    IF(PRESENT(cmsk)) lcmsk=cmsk    
    IF (lcmsk(1).EQV..FALSE.) GOTO 10
    !$OMP PARALLEL DO
    DO R=1,fim%nrows
       DO C=1,fim%ncols
          fim%dred(c,r)=afun(fim%dred(c,r),adat)
       END DO
    END DO
    !$OMP END PARALLEL DO
10  CONTINUE
    IF (lcmsk(2).EQV..FALSE.) GOTO 20
    !$OMP PARALLEL DO
    DO R=1,fim%nrows
       DO C=1,fim%ncols
          fim%dgrn(c,r)=afun(fim%dgrn(c,r),adat)
       END DO
    END DO
    !$OMP END PARALLEL DO
20  CONTINUE
    IF (lcmsk(3).EQV..FALSE.) GOTO 30
    !$OMP PARALLEL DO
    DO R=1,fim%nrows
       DO C=1,fim%ncols
          fim%dblu(c,r)=afun(fim%dblu(c,r),adat)
       END DO
    END DO
    !$OMP END PARALLEL DO
30  CONTINUE
    ! --- End code        ---
  END SUBROUTINE FIM_ctf_iter
!==============================================================================
  PURE FUNCTION APOLY(V,D)
    IMPLICIT NONE
    REAL(KIND=RKIND) :: APOLY
    ! --- Dummy arguments ---
    REAL(KIND=RKIND),INTENT(IN) :: V
    REAL(KIND=RKIND),INTENT(IN) :: D(0:)
    ! --- Variables       ---
    INTEGER :: J
    ! --- Executable code ---
    APOLY=0
    DO J=0,UBOUND(D,1)
       APOLY=APOLY+(D(J)*(V**J))
    END DO
    ! --- End code        ---
  END FUNCTION APOLY
!============================================================================== 
    PURE FUNCTION AEXPO(V,D)
    IMPLICIT NONE
    REAL(KIND=RKIND) :: AEXPO
    ! --- Dummy arguments ---
    REAL(KIND=RKIND),INTENT(IN) :: V
    REAL(KIND=RKIND),INTENT(IN) :: D(0:)
    ! --- Executable code ---
    AEXPO=D(0)*(V**D(1))
    ! --- End code        ---
  END FUNCTION AEXPO
!==============================================================================
  PURE FUNCTION AAINV(V,D)
    IMPLICIT NONE
    REAL(KIND=RKIND) :: AAINV
   ! --- Dummy arguments ---
    REAL(KIND=RKIND),INTENT(IN) :: V
    REAL(KIND=RKIND),INTENT(IN) :: D(0:)
    ! --- Executable code ---
    AAINV=D(0)+D(1)*(1-V)
    ! --- End code        ---    
  END FUNCTION AAINV
!==============================================================================
  PURE FUNCTION AMINV(V,D)
    IMPLICIT NONE
    REAL(KIND=RKIND) :: AMINV
   ! --- Dummy arguments ---
    REAL(KIND=RKIND),INTENT(IN) :: V
    REAL(KIND=RKIND),INTENT(IN) :: D(0:)
    ! --- Variables       ---
    REAL(KIND=RKIND) :: A
    ! --- Executable code ---
    A=0
    IF (RNZ(V)) THEN
       A=1/V
    ELSE
       A=1
    END IF
    AMINV=D(0)+D(1)*A
    ! --- End code        ---        
  END FUNCTION AMINV
!============================================================================== 
! * * *   G A M M A   C O R R E C T I O N    A N D    L U M I N A N C E   * * *
!==============================================================================
! +---------------------------------------------------------------------------+
! | REFERENCES:                                                               |
! |                                                                           |
! | (1) Gamma FAQ -- Frequently Asked Questions about Gamma, by C. Poynton,   |
! |     2012-12-16a, http://www.poynton.com                                   |
! |                                                                           |
! | (2) ppm manual page, by J. Poskanzer, 2012-04-08 (NetPBM distribution)    |
! |                                                                           |
! | (3) pgm manual page, by J. Poskanzer, 1991-11-12 (NetPBM distribution)    |
! |                                                                           |
! | (4) https://stackoverflow.com/a/19045659                                  |
! +---------------------------------------------------------------------------+

  PURE FUNCTION LGC(L) RESULT(GC)
    ! +-----------------------------------------------------------------------+
    ! | Perform gamma-correction on linear RGB or grayscale value L, and      |
    ! | return gamma-corrected value.                                         |
    ! |                                                                       |
    ! | ARGUMENTS:                                                            |
    ! |   L :  Linear RGB or grayscale component value                        |
    ! |        Real number in the range [0,1]                                 |
    ! |                                                                       |
    ! | RETURN VALUE:                                                         |
    ! |   GC : Gamma-corrected value                                          |
    ! |        Real number in the range [0,1]                                 |
    ! |                                                                       |
    ! | REFERENCE: (1) Section 8                                              |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND)             :: GC
    REAL(KIND=RKIND), INTENT(IN) :: L
    ! --- CONSTANTS ---
    REAL(KIND=RKIND), PARAMETER :: ExpGamma        = 2.2_RKIND
    REAL(KIND=RKIND), PARAMETER :: ExpOneOverGamma = 1.0_RKIND/ExpGamma
    REAL(KIND=RKIND), PARAMETER :: ExpCoefficient  = 1.099_RKIND
    REAL(KIND=RKIND), PARAMETER :: ExpSubtrahend   = 0.099_RKIND
    REAL(KIND=RKIND), PARAMETER :: LinearThreshold = 0.018_RKIND
    ! +-----------------------------------------------------------------------+
    ! | LinearCoefficient differs slightly from the value 4.5 (exactly) given |
    ! | in (Ref. 1), but the change is necessary to avoid a discontinuity.    |
    ! | What's done here is similar to the approach used in NetPBM.           |
    ! +-----------------------------------------------------------------------+
    REAL(KIND=RKIND), PARAMETER :: LinearCoefficient =                        &
         (ExpCoefficient*(LinearThreshold**ExpOneOverGamma)-ExpSubtrahend) /  &
         LinearThreshold
    ! --- EXE CODE ---
    IF (L.LE.LinearThreshold) THEN
       GC=LinearCoefficient*L
    ELSE
       GC=ExpCoefficient*(L**(ExpOneOverGamma))-ExpSubtrahend
    END IF
    ! --- END CODE ---
  END FUNCTION LGC
!==============================================================================
  PURE FUNCTION GCL(GC) RESULT(L)
    ! +-----------------------------------------------------------------------+
    ! | Perform inverse gamma-correction on linear RGB or grayscale value     |
    ! | GC, and return gamma-corrected value.                                 |
    ! |                                                                       |
    ! | ARGUMENTS:                                                            |
    ! |   GC : Gamma-corrected value                                          |
    ! |        Real number in the range [0,1]                                 |
    ! |                                                                       |
    ! | RETURN VALUE:                                                         |
    ! |   L :  Linear RGB or grayscale component value                        |
    ! |        Real number in the range [0,1]                                 |
    ! |                                                                       |
    ! |                                                                       |
    ! | REFERENCE: (1) Section 8                                              |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND)             :: L
    REAL(KIND=RKIND), INTENT(IN) :: GC
    ! --- CONSTANTS ---
    REAL(KIND=RKIND), PARAMETER :: ExpGamma        = 2.2_RKIND
    REAL(KIND=RKIND), PARAMETER :: ExpOneOverGamma = 1.0_RKIND/ExpGamma
    REAL(KIND=RKIND), PARAMETER :: ExpCoefficient  = 1.099_RKIND
    REAL(KIND=RKIND), PARAMETER :: ExpSubtrahend   = 0.099_RKIND
    REAL(KIND=RKIND), PARAMETER :: LinearThreshold = 0.018_RKIND
    REAL(KIND=RKIND), PARAMETER :: Threshold =                                &
         ExpCoefficient*LinearThreshold**(ExpOneOverGamma)-ExpSubtrahend
    ! +-----------------------------------------------------------------------+
    ! | LinearCoefficient differs slightly from the value 4.5 (exactly) given |
    ! | in (Ref. 1), but the change is necessary to avoid a discontinuity.    |
    ! | What's done here is similar to the approach used in NetPBM.           |
    ! +-----------------------------------------------------------------------+
    REAL(KIND=RKIND), PARAMETER :: LinearCoefficient =                        &
         (ExpCoefficient*(LinearThreshold**ExpOneOverGamma)-ExpSubtrahend) /  &
         LinearThreshold
    ! --- EXE CODE ---
    IF (GC.LE.Threshold) THEN
       L=GC/LinearCoefficient
    ELSE
       L=((GC+ExpSubtrahend)/ExpCoefficient)**ExpGamma
    END IF
    ! --- END CODE ---
  END FUNCTION GCL
!==============================================================================
  ELEMENTAL FUNCTION Luminance(LR,LG,LB) RESULT(LUM)
    ! +-----------------------------------------------------------------------+
    ! | Compute the luminance of an RGB tuple (LR,LG,LB).                     |
    ! |                                                                       |
    ! | ARGUMENTS:                                                            |
    ! |   LR : \  Linear red, green and blue tuple, expressed as real         |
    ! |   LG : )  numbers in the range [0,1].                                 |
    ! |   LB : /                                                              |
    ! |                                                                       |
    ! | RETURN VALUE:                                                         |
    ! |   LUM :  The luminance of the linear RGB tuple (LR,LG,LB),            |
    ! |          expressed as a real number in the range [0,1].               |
    ! |                                                                       |
    ! | REFERENCE: (1) Section 4                                              |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND)             :: LUM
    REAL(KIND=RKIND), INTENT(IN) :: LR,LG,LB
    ! --- CONSTANTS --
    REAL(KIND=RKIND), PARAMETER  :: CR=0.2126_RKIND
    REAL(KIND=RKIND), PARAMETER  :: CG=0.7152_RKIND
    REAL(KIND=RKIND), PARAMETER  :: CB=0.0722_RKIND
    ! --- EXE CODE ---
    LUM=CR*LR+CG*LG+CB*LB
    ! --- END CODE ---
  END FUNCTION Luminance
!==============================================================================
  ELEMENTAL FUNCTION AritMean(LR,LG,LB) RESULT(AVG)
    ! +-----------------------------------------------------------------------+
    ! | Compute the arithmetic mean of an RGB tuple (LR,LG,LB).               |
    ! |                                                                       |
    ! | ARGUMENTS:                                                            |
    ! |   LR : \  Linear red, green and blue tuple, expressed as real         |
    ! |   LG : )  numbers in the range [0,1].                                 |
    ! |   LB : /                                                              |
    ! |                                                                       |
    ! | RETURN VALUE:                                                         |
    ! |   AVG :  The arithmetic mean of the linear RGB tuple (LR,LG,LB),      |
    ! |          expressed as a real number in the range [0,1].               |
    ! |                                                                       |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND)             :: AVG
    REAL(KIND=RKIND), INTENT(IN) :: LR,LG,LB
    ! --- CONSTANTS ---
    REAL(KIND=RKIND),PARAMETER :: FCTR=1.0_RKIND/3
    ! --- EXE CODE ---
    AVG=FCTR*(LR+LG+LB)
    ! --- END CODE ---
  END FUNCTION AritMean
!==============================================================================
  ELEMENTAL FUNCTION GeomMean(LR,LG,LB) RESULT(GMN)
    ! +-----------------------------------------------------------------------+
    ! | Compute the geometric mean of an RGB tuple (LR,LG,LB).                |
    ! |                                                                       |
    ! | ARGUMENTS:                                                            |
    ! |   LR : \  Linear red, green and blue tuple, expressed as real         |
    ! |   LG : )  numbers in the range [0,1].                                 |
    ! |   LB : /                                                              |
    ! |                                                                       |
    ! | RETURN VALUE:                                                         |
    ! |   GMN :  The geometric mean of the linear RGB tuple (LR,LG,LB),       |
    ! |          expressed as a real number in the range [0,1].               |
    ! |                                                                       |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND)             :: GMN
    REAL(KIND=RKIND), INTENT(IN) :: LR,LG,LB
    ! --- CONSTANTS ---
    REAL(KIND=RKIND),PARAMETER :: EXPO=1.0_RKIND/3
    ! --- EXE CODE ---
    GMN=(LR*LG*LB)**EXPO
    ! --- END CODE ---
  END FUNCTION GeomMean
!==============================================================================
  ELEMENTAL FUNCTION Median(LR,LG,LB) RESULT(MDN)
    ! +-----------------------------------------------------------------------+
    ! | Compute the median of an RGB tuple (LR,LG,LB).                        |
    ! |                                                                       |
    ! | ARGUMENTS:                                                            |
    ! |   LR : \  Linear red, green and blue tuple, expressed as real         |
    ! |   LG : )  numbers in the range [0,1].                                 |
    ! |   LB : /                                                              |
    ! |                                                                       |
    ! | RETURN VALUE:                                                         |
    ! |   MDN :  The median of the linear RGB tuple (LR,LG,LB),               |
    ! |          expressed as a real number in the range [0,1].               |
    ! |                                                                       |
    ! | REFERENCE: (4)                                                        |
    ! |                                                                       |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- DUMMIES ---
    REAL(KIND=RKIND)             :: MDN
    REAL(KIND=RKIND), INTENT(IN) :: LR,LG,LB
    ! --- EXE CODE ---
    MDN=MAX(MIN(LR,LG),MIN(MAX(LR,LG),LB))
    ! --- END CODE ---
  END FUNCTION Median
!==============================================================================
! * * *  S P A T I A L   T R A N S F O R M A T I O N   F U N C T I O N S  * * *
!==============================================================================
  SUBROUTINE FIM_stf_hflip(fim)
    ! +-----------------------------------------------------------------------+
    ! | Flip image across a horizontal dividing line.                         |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    ! --- Variables       ---
    INTEGER     :: C,R1,R2
    REAL(RKIND) :: TR,TG,TB
    ! --- Executable code ---
    !$OMP PARALLEL DO PRIVATE(C,R1,R2,TR,TG,TB)
    DO R1=1,fim%nrows/2
       R2=FIM%nrows+1-R1 
       DO C=1,fim%ncols
          TR=fim%dred(C,R1)
          TG=fim%dgrn(C,R1)
          TB=fim%dblu(C,R1)
          fim%dred(C,R1)=fim%dred(C,R2)
          fim%dgrn(C,R1)=fim%dgrn(C,R2)
          fim%dblu(C,R1)=fim%dblu(C,R2)
          fim%dred(C,R2)=TR
          fim%dgrn(C,R2)=TG
          fim%dblu(C,R2)=TB
       END DO
    END DO
    !$OMP END PARALLEL DO
    ! --- End code        ---
  END SUBROUTINE FIM_stf_hflip
!==============================================================================
  SUBROUTINE FIM_stf_vflip(fim)
    ! +-----------------------------------------------------------------------+
    ! | Flip image across a   vertical dividing line.                         |
    ! +-----------------------------------------------------------------------+    
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    ! --- Executable code ---
    ! --- Variables       ---
    INTEGER     :: C1,C2,R
    REAL(RKIND) :: TR,TG,TB
    ! --- Executable code ---
    !$OMP PARALLEL DO PRIVATE(C1,C2,R,TR,TG,TB)
    DO C1=1,fim%ncols/2
       C2=fim%ncols+1-C1
       DO R=1,fim%nrows
          TR=fim%dred(C1,R)
          TG=fim%dgrn(C1,R)
          TB=fim%dblu(C1,R)
          fim%dred(C1,R)=fim%dred(C2,R)
          fim%dgrn(C1,R)=fim%dgrn(C2,R)
          fim%dblu(C1,R)=fim%dblu(C2,R)
          fim%dred(C2,R)=TR
          fim%dgrn(C2,R)=TG
          fim%dblu(C2,R)=TB
       END DO
    END DO
    !$OMP END PARALLEL DO
    ! --- End code        ---
  END SUBROUTINE FIM_stf_vflip
!==============================================================================
  SUBROUTINE FIM_stf_rotate(fim, rop)
    ! +-----------------------------------------------------------------------+
    ! | Rotate image around an angle of 'rop', which may be 90 degrees, 180   |
    ! | degrees or 270 degrees (or negatives of these).  If any other angles  |
    ! | are chosen, the subroutine crashes on error.                          |
    ! +-----------------------------------------------------------------------+
    IMPLICIT NONE
    ! --- Dummy arguments ---      
    TYPE(FImage),INTENT(INOUT)     :: fim
    INTEGER     ,INTENT(IN)        :: rop
    ! --- Variables       ---
    REAL(RKIND),POINTER :: red(:,:), grn(:,:), blu(:,:)
    REAL(RKIND)         :: tr , tg , tb
    INTEGER             :: c1,c2,r1,r2,t
    INTEGER             :: ms
    ! --- Executable code ---
    IF (abs(rop).EQ.90.OR.abs(rop).EQ.270) THEN
       ALLOCATE(red(1:fim%nrows,1:fim%ncols),grn(1:fim%nrows,1:fim%ncols), &
                blu(1:fim%nrows,1:fim%ncols),stat=ms)
       IF (MS.ne.0) goto 900
       IF (rop.EQ.90.or.rop.eq.-270) THEN
          !$OMP PARALLEL DO PRIVATE (r1,r2,c1,c2,tr,tg,tb)
          DO r1=1,fim%nrows
             c2=fim%nrows+1-r1
             DO c1=1,fim%ncols
                r2=c1
                red(c2,r2)=fim%dred(c1,r1)
                grn(c2,r2)=fim%dgrn(c1,r1)
                blu(c2,r2)=fim%dblu(c1,r1)
             END DO
          END DO
          !$OMP END PARALLEL DO
       ELSE
          !$OMP PARALLEL DO PRIVATE (r1,r2,c1,c2,tr,tg,tb)
          DO r1=1,fim%nrows
             c2=r1
             DO c1=1,fim%ncols
                r2=fim%ncols+1-c1
                red(c2,r2)=fim%dred(c1,r1)
                grn(c2,r2)=fim%dgrn(c1,r1)
                blu(c2,r2)=fim%dblu(c1,r1)
             END DO
          END DO
          !$OMP END PARALLEL DO
       END IF
       DEALLOCATE(fim%dred, fim%dgrn, fim%dblu, stat=ms) 
       IF (ms.ne.0) GOTO 900
       fim%dred=>red ; fim%dgrn=>grn ; fim%dblu=>blu
       t=fim%nrows
       fim%nrows=fim%ncols
       fim%ncols=t
    ELSE IF (abs(rop).EQ.180) THEN
       red=>fim%dred
       grn=>fim%dgrn
       blu=>fim%dblu
       !$OMP PARALLEL DO PRIVATE (r1,r2,c1,c2,tr,tg,tb)
       DO r1=1,fim%nrows/2
          r2=fim%nrows+1-r1
          DO c1=1,fim%ncols
             c2=fim%ncols+1-c1
             tr=red(c1,r1)        ;tg        =grn(c1,r1);tb        =blu(c1,r1)
             red(c1,r1)=red(c2,r2);grn(c1,r1)=grn(c2,r2);blu(c1,r1)=blu(c2,r2)
             red(c2,r2)=tr        ;grn(c2,r2)=tg        ;blu(c2,r2)=tb
          END DO
       END DO
       !$OMP END PARALLEL DO
       IF (MOD(fim%nrows,2).NE.0) THEN
          r1=fim%nrows/2+1
          !$OMP PARALLEL DO PRIVATE(c1,c2,tr,tg,tb)
          DO c1=1,fim%ncols/2
             c2=fim%ncols-c1+1
             tr=red(c1,r1)        ;tg        =grn(c1,r1);tb        =blu(c1,r1)
             red(c1,r1)=red(c2,r1);grn(c1,r1)=grn(c2,r1);blu(c1,r1)=blu(c2,r1)
             red(c2,r1)=tr        ;grn(c2,r1)=tg        ;blu(c2,r1)=tb             
          END DO
          !$OMP END PARALLEL DO
       END IF
    ELSE
       GOTO 910
    END IF    
    ! --- End code        ---
    GOTO 999
900 WRITE(*,991)     ; STOP
910 WRITE(*,992) rop ; STOP
991 FORMAT('*** WARNING (FIM_stf_rotate): memory allocation/deallocation')
992 FORMAT('*** WARNING (FIM_stf_rotate): invalid angle ',I6)
999 CONTINUE
  END SUBROUTINE FIM_stf_rotate
!==============================================================================
END MODULE FImageMod

!*******
!* EOF *
!*******
