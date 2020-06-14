C$PROG GNITTER   - Opens & processes graphic scaler init file
C
C     ******************************************************************
C     BY W.T. MILNER AT ORPH - LAST MODIFIED 05/22/2005
C     ******************************************************************
C
      SUBROUTINE GNITTER(IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      common/dd13a/ gmodty(16),gvmod(16),gvsn(16),gvidx(16),ngvme,
     $              gty(16),gpv(16),gecn(16),gesn(16),ngecl
      character*8   gmodty,    gvmod
      integer*4                          gvsn,    gvidx,    ngvme
      integer*4             gpv,    gecn,    gesn,    ngecl
      character*4   gty
C     ------------------------------------------------------------------
      common/dd13b/ gc(280),gn(280),ga(280),gf(280),gbuf(280),ngrap
      integer*4     gc,     gn,     ga,     gf,     gbuf,     ngrap
C     ------------------------------------------------------------------
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    STRLEN,NXBL,NXNB
C
      INTEGER*4    IERR,JERR,KIND,ISTAT,NLN,IA,I,J,N
      integer*4    ib,ifind
C
      INTEGER*4    IV(4)
C
      REAL*4       XV
C
      CHARACTER*80 CWDRAW,CNAM
      character*80 clwdl
C
      CHARACTER*4  CWD(20)
      character*4  clwd(2,40)
C
      EQUIVALENCE (CWDRAW,IWDRAW),(CWD,IWD)
      equivalence (clwdl,lwd),(clwd,lwd)
C
      CHARACTER*1  COMBYT
C
      EQUIVALENCE (COMBYT,IWD)
C
      INTEGER*4    MXSCA,LU
      DATA         MXSCA,LU/8,17/
C
      INTEGER*4    BLANK
      character*4  cBLANK
      equivalence  (cBLANK, BLANK)
      DATA         cBLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Pick up file name and open the mighyt snit-file
C     ------------------------------------------------------------------
C
      IERR=0
C
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 500
      CNAM=CWDRAW(IA:STRLEN(CWDRAW))
C
      CLOSE(UNIT=LU)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAM,
     &     STATUS   = 'OLD',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) GO TO 510
C
      GNITNAM=CNAM
C
C     ------------------------------------------------------------------
C     Initialize scaler labels  & limits for whatever reason
C     ------------------------------------------------------------------
C
      DO 20 J=1,MXSCA
      DO 10 I=1,3
      LAG(I,J)=BLANK
   10 CONTINUE
      GLIMON(J)='NULL'
      GLIMLO(J)=0.0
      GLIMHI(J)=0.0
   20 CONTINUE
C
C     ------------------------------------------------------------------
C     Read and process the snit-file
C     ------------------------------------------------------------------
C
      N=0                                    !Init # of scalers
      NLN=0                                  !Init line# counter
C
   50 READ(LU,55,END=200)IWD
   55 FORMAT(20A4)
C
      NLN=NLN+1                              !Inc line# counter
C
      IF(CWD(1).EQ.' ')    GO TO 50          !Ignore blanks
C
      IF(COMBYT.EQ.'#')    GO TO 50          !Ignore comments
C
      IF(CWD(1).EQ.'$END') GO TO 200
C
      IA=NXBL(IWD,1,80)                      !Locate label field
      IF(IA.GT.12)   GO TO 520               !Tst for error
      IF(IA.LE.0)    GO TO 520               !Tst for error
C
      N=N+1                                  !Inc # of scalers
      IF(N.GT.MXSCA) GO TO 540               !Tst for too many
C
      CALL LODUP(IWD,1,IA,LAG(1,N),1)        !load up the label
C
      ib = ifind(iwd,'3b'x,1,80)
      if (ib .le. 0) ib = 81
      ib = ib -1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,ib,NTER) !Reformat CNAF fields
      IF(NTER.NE.0)  GO TO 520               !Tst for Error
C
      read(clwdl,1001) gmodty(n)
1001  format(a8)
      if (gmodty(n) .eq. 'SIS3820') go to 1010
      if (gmodty(n) .eq. 'CAEN820') go to 1010
      gmodty(n) = ' '
      go to 1030
*
1010  read(clwdl,1020) gmodty(n),nn(n),aa(n)
1020  format(a8,2i8)
      if (nn(n) .le. 0) go to 550
      cc(n) = -1
      gty(n) = 'VME '
      go to 50  
*
1030  continue
**      DO 60 I=1,4
**      CALL MILV(LWD(1,I),IV(I),XV,KIND,JERR)
**      IF(JERR.NE.0) GO TO 520
**   60 CONTINUE
C
**      CC(N)=IV(1)
**      NN(N)=IV(2)
**      AA(N)=IV(3)
**      FF(N)=0
C
**      gty(n) = '    '
**      if (clwd(1,5) .eq. 'ECL ') gty(n) = 'ECL '
      if (nf .eq. 5) then
        read(clwdl,60) cc(n),nn(n),aa(n),ff(n),gty(n)
60      format(4i8,a4)
      elseif (nf .eq. 4 .and. ityp(4) .eq. 2) then
        read(clwdl,60) cc(n),nn(n),aa(n),ff(n)
        gty(n) = ' '
      elseif (nf .eq. 4 .and. ityp(4) .eq. 1) then
        read(clwdl,61) cc(n),nn(n),aa(n),gty(n)
61      format(3i8,a4)
        ff(n) = 0
      elseif (nf .eq. 3) then
        read(clwdl,60) cc(n),nn(n),aa(n)
        ff(n) = 0
        gty(n) = ' '
      elseif (nf. lt. 3) then
        go to 520
      endif
*
**      write(*,9000) n,cc(n),nn(n),aa(n),ff(n),gty(n)
**9000  format (5i8,a4)
*
      GO TO 50                               !Go back for more
C
  200 CLOSE(LU)                              !Close snit-file
      NSCA=N                                 !Save # of scalers
      IF(NSCA.GT.8) NSCA=8                   !Limit to 8
      NRATE=NSCA                             !Save # of scalers
      WRITE(CMSSG,205)NSCA
  205 FORMAT(I3,' scalers set up')
      CALL MESSLOG(LOGUT,LOGUP)
**      do i=1,nsca
**       write(*,900) i,cc(i),nn(i),aa(i),ff(i),gty(i)
**900    format(5i6,a4)
**      enddo
      call gsetecl
      call gsetpar
*
      RETURN                                 !Return
C
C     ------------------------------------------------------------------
C     Error Returns
C     ------------------------------------------------------------------
C
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error in snit-file name')
      GO TO 1000
C
  510 WRITE(CMSSG,515)CNAM
  515 FORMAT('Unable to open snit-file - ',A)
      GO TO 1000
C
  520 WRITE(CMSSG,525)NLN
  525 FORMAT('Syntax error on snit-file line# = ',I3)
      GO TO 1000
C
  530 WRITE(CMSSG,535)NSCA
  535 FORMAT(I3,' scalers set up')
      GO TO 1000
C
  540 WRITE(CMSSG,545)MXSCA,NLN
  545 FORMAT('More than max of ',I2,' scalers defined on line#',I3)
      GO TO 1000
C
  550 write(cmssg,555) nln
  555 format(1h ,'VME scaler number must be greater than 0 on ',i3)
      go to 1000
C
 1000 IERR=1
      NSCA=0
      NRATE=0
      GNITNAM='Undefined!'
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
