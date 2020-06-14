C$PROG GREAD     - General string re-formatter
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 03/07/2002
C     ******************************************************************
C
      SUBROUTINE GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
C
C     ------------------------------------------------------------------
      DIMENSION KWD(320),JWD(80),IWD(1),LWD(1),ITYP(1)
C
      CHARACTER*4 JMINC
C
      EQUIVALENCE (JMINC,JMIN)
C
      INTEGER*4    X21,X0D,X3B
C
      DATA         X21,X0D,X3B/'21'X,'0D'X,'3B'X/
C
      DATA JNUM,JALP,JDEL,MAXB,LF/1,2,3,320,8/
C
      SAVE JNUM,JALP,JDEL,MAXB,LF
C     ------------------------------------------------------------------
C
      NTER=0
C
      JMIN=IMIN
C
      IF(JMINC.NE.'SETW') GO TO 5
C
      LF=IMAX
      RETURN
    5 NF=0
C
C     ------------------------------------------------------------------
C     NTER = # BYTES TRUNCATED (THIS IS BAD STUFF)
C     ------------------------------------------------------------------
C
      DO 10 I=1,MAXB
      KWD(I)='20202020'X
   10 CONTINUE
      DO 15 I=1,80
      JWD(I)='00000020'X
   15 CONTINUE
C
C     ------------------------------------------------------------------
C     UNPACK BYTES INTO FULL WORDS
C     ------------------------------------------------------------------
C
      DO 20 I=IMIN,IMAX
      CALL ILBYTE(JTEMP,IWD,I-1)
      IF(JTEMP.EQ.X0D) GO TO 25    !TERMINATE ON CR
      IF(JTEMP.EQ.X3B) GO TO 25    !TERMINATE ON ;
      IF(JTEMP.EQ.X21) GO TO 25    !TERMINATE ON !
      JWD(I)=JTEMP
   20 CONTINUE
   25 NS=0
      JLO=IMIN
      JHI=IMAX
      KHI=0
   30 NS=NS+1
      KLO=KHI+1
      KHI=KLO+LF-1
C
C     ------------------------------------------------------------------
C     STARTING AT JLO, FIND THE FIRST NON-DELIMITER
C     ------------------------------------------------------------------
C
      IF(JLO.GT.JHI) GO TO 150
      DO 40 I=JLO,JHI
      IF(KINDA(JWD(I)).NE.JDEL) GO TO 45
   40 CONTINUE
      GO TO 150
C
C     ------------------------------------------------------------------
C     LEFT  JUSTIFY FIELDS STARTING WITH A NON-NUMERIC
C     RIGHT JUSTIFY FIELDS STARTING WITH A NUMERIC CHARACTER
C     ------------------------------------------------------------------
C
   45 JLO=I
      IF(KINDA(JWD(I)).EQ.JNUM) GO TO 100
      K=KLO
      NF=NS
      ITYP(NF)=1
      DO 50 I=JLO,JHI
      IF(KINDA(JWD(I)).EQ.JDEL) GO TO 55
      IF(K.LE.KHI) GO TO 48
      NTER=NTER+1
      GO TO 50
   48 KWD(K)=JWD(I)
      K=K+1
   50 CONTINUE
      GO TO 150
   55 JLO=I+1
      GO TO 30
C
C     ------------------------------------------------------------------
C     RIGHT JUSTIFY NUMERIC FIELDS
C
C     FIND NEXT DELIMITER
C     ------------------------------------------------------------------
C
  100 DO 110 I=JLO,JHI
      IF(KINDA(JWD(I)).EQ.JDEL) GO TO 120
  110 CONTINUE
      I=JHI+1
  120 JUP=I-1
      NDO=JUP-JLO+1
      IF(NDO.LT.1) GO TO 30
      IF(NDO.LE.LF) GO TO 125
      NTER=NTER+(NDO-LF)
      NDO=LF
  125 K=KHI
      J=JUP
      NF=NS
      ITYP(NF)=2
      DO 130 N=1,NDO
      KWD(K)=JWD(J)
      J=J-1
      K=K-1
  130 CONTINUE
      JLO=JUP+2
      GO TO 30
C
C     ------------------------------------------------------------------
C     NOW LOAD IT ALL INTO LWD
C     ------------------------------------------------------------------
C
  150 DO 160 I=1,MAXB
      JTEMP=KWD(I)
      CALL ISBYTE(JTEMP,LWD,I-1)
  160 CONTINUE
      RETURN
      END
