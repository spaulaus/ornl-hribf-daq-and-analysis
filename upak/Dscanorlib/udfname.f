C$PROG UDFNAME   - Extracts UDF file name & UDF RECL from IWD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE UDFNAME(IWD,NAMFIL,RECL,IERR)
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
      INTEGER*4    NAMFIL(20),IWD(20),RECL,IERR
C
      INTEGER*4    LWD(2,40),ITYP(40),NF,NTER,IV,KIND
C
      INTEGER*4    IFIND,NXBL,NXNB,IT,JT,IA,IB,JA,JB,I,J,N
C
      REAL*4       XV
C
      INTEGER*4    X2C,X0D,X21
C
      DATA         X2C,X0D,X21/Z'2C',Z'0D',Z'21'/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     Process the form:  UDF  filename  <RECL>
C
C     ------------------------------------------------------------------
C     First locate & extract the filename
C     ------------------------------------------------------------------
C
      IA=5
      IB=80
      RECL=0
      IERR=0
C
      DO 10 I=1,20
      NAMFIL(I)=Z'20202020'
   10 CONTINUE
C
      JA=NXNB(IWD,IA,IB)
      IF(JA.LE.0) GO TO 100
      JT=IFIND(IWD,X2C,JA,IB)
      JB=NXBL(IWD,JA,IB)
      IF(JT.GT.0.AND.JT.LT.JB) THEN
                               JB=JT-1
                               GO TO 20
                               ENDIF
      JB=JB-1
      IF(JB.LE.0)     GO TO 100
C
   20 IF(JB-JA.GT.79) GO TO 100
      IF(JA.GT.JB)    GO TO 100
C
      N=0
      DO 30 J=JA,JB
      CALL ILBYTE(IT,IWD,J-1)
      IF(IT.EQ.X0D) RETURN            !TERMINATE ON CR
      IF(IT.EQ.X21) RETURN            !TERMINATE ON !
      CALL ISBYTE(IT,NAMFIL,N)
      N=N+1
   30 CONTINUE
C
C     ------------------------------------------------------------------
C     Next locate & extract the RECL specification - if any
C     ------------------------------------------------------------------
C
      IA=JB+1
C
      CALL GREAD(IWD,LWD,ITYP,NF,IA,IB,NTER)
C
      IF(NTER.NE.0) GO TO 200
C
      CALL MILV(LWD(1,1),IV,XV,KIND,IERR)
C
      IF(IERR.NE.0) GO TO 200
C
      RECL=IV
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
  100 WRITE(CMSSG,105)
  105 FORMAT('Error extraction filename field from UDF command')
      GO TO 500
C
  200 WRITE(CMSSG,205)
  205 FORMAT('Error extracting RECL field from UDF command')
      GO TO 500
C
  500 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
