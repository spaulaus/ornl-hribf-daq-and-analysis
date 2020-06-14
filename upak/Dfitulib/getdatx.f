C$PROG GETDATX   - Reads data sets from file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE GETDATX(IDN,IERR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MXDAT=500)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),LWD(4,20),ITYP(40),IBLN(101)
C
      CHARACTER*4  IWD1
C
      EQUIVALENCE (IWD1,IWD(1))
C
      DATA LU/1/
C
      SAVE
C
C     ------------------------------------------------------------------
C     READS DATA-SETS FORM DATA FILE - LU=1
C     ------------------------------------------------------------------
C
      IERR=0
      IF(IDN.NE.-1) GO TO 100
C
C     ------------------------------------------------------------------
C     CONSTRUCT DIRECTORY IF IDN=-1 
C     ------------------------------------------------------------------
C
      NB=0
      NSET=0
C
   10 NB=NB+1
      READ(LU,15,END=50,ERR=210)IWD
   15 FORMAT(20A4)
      CALL CASEUP(IWD)
      IF(IWD1.EQ.'DATA') GO TO 20
      GO TO 10
   20 NSET=NSET+1
      IF(NSET.GT.100) NSET=100
      IBLN(NSET)=NB
      GO TO 10
   50 RETURN
C
C     ------------------------------------------------------------------
C     READ REQUESTED DATA-SET
C     ------------------------------------------------------------------
C
  100 IF(IDN.LT.0.OR.IDN.GT.NSET) GO TO 200
      NB=IBLN(IDN)
      NDAT=0
C
      REWIND LU
      DO 105 I=1,NB
      READ(LU,15,END=210,ERR=210)IWD
  105 CONTINUE
C
  110 READ(LU,15,END=210,ERR=210)IWD
      CALL CASEUP(IWD)
      IF(IWD1.EQ.'ENDA') RETURN
C
      NDAT=NDAT+1
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 220
      IF(NF.LT.2)   GO TO 220
C
      CALL MILV4(LWD(1,1),IV,XIN(NDAT),KIND,JERR)
      IF(JERR.NE.0) GO TO 220
      CALL MILV4(LWD(1,2),IV,YIN(NDAT),KIND,JERR)
      IF(JERR.NE.0) GO TO 220
      CALL MILV4(LWD(1,3),IV,UIN(NDAT),KIND,JERR)
      IF(JERR.NE.0) GO TO 220
      IF(NDAT.GE.MXDAT) RETURN
      GO TO 110
C
C     ------------------------------------------------------------------
C     Error RETURNS
C     ------------------------------------------------------------------
  200 WRITE(CMSSG,205)IDN
  205 FORMAT('Requested ID =',I5,' is out of range')
      IERR=1
      GO TO 500
C
  210 WRITE(CMSSG,215)IDN
  215 FORMAT('Error or end-of-file trying to read ID =',I4)
      IERR=2
      GO TO 500
C
  220 WRITE(CMSSG,225)NDAT,IDN
  225 FORMAT('Syntax error processing record#',I4,' of ID =',I4)
      IERR=3
      GO TO 500
C
  500 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
