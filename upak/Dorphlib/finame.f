C$PROG FINAME    - Loads first contiguous string (IA,IB) to NAMFIL
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE FINAME(IWD,IA,IB,NAMFIL,IERR)
C
      INTEGER*4 MESS(13),NAMFIL(20),IWD(1)
C
      CHARACTER*52 MSC
C
      EQUIVALENCE (MSC,MESS(1))
C
      INTEGER*4   X2C,X0D,X21
C
      DATA        X2C,X0D,X21/Z'2C',Z'0D',Z'21'/
C
      DATA MSC
     &/'SYNTAX ERROR RETRIEVING FILE-NAME                   '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     LOADS FIRST CONTIGUOUS BYTE-STRING (IA,IB) FROM IWD TO NAMFIL
C     TERMINATES ON BLANK OR COMMA
C     ------------------------------------------------------------------
C
      IERR=0
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
      RETURN
C
  100 IERR=1
      RETURN
      END
