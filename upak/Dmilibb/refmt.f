C$PROG REFMT     - Reformats specified region of input line (like GREAD)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE REFMT(IWD,LWD,ITYP,KNOP,IA,IB,NF,NTER)
C
      DIMENSION KWD(320),JWD(80),IWD(1),LWD(1),ITYP(1),KNOP(1)
C
      DATA JNUM,JALP,JDEL,MAXB,LU,LF/1,2,3,320,1,8/
C
      SAVE
C
C     ------------------------------------------------------------------
C     IWD IS PACKED INPUT LINE IMAGE
C     JWD IS AN UNPACKED LINE IMAGE
C     KWD IS A REFORMATTED IMAGE WITH ALPHAMERIC FIELDS LEFT  JUSTIFIED
C                                AND  NUMERIC    FIELDS RIGHT JUSTIFIED
C     LWD IS A PACKED VERSION OF KWD
C     ITYP(I) = 1 SAYS ITH FIELD IS ALPHAMERIC
C     ITYP(I) = 2 SAYS ITH FIELD IS NUMERIC
C     KNOP(I) = 1,2,3,4 SAYS PRECEDING DELIMITER WAS  + - * /
C     ------------------------------------------------------------------
C   
      NS=0
      JLO=IA
      JHI=IB
      KHI=0
      KNOP(1)=1
      NTER=0
      DO 10 I=1,320
      KWD(I)=Z'20202020'
   10 CONTINUE
C   
C     UNPACK IWD INTO JWD
C   
      DO 20 I=IA,IB
      CALL ILBYTE(JTEMP,IWD,I-1)
      JWD(I)=JTEMP
   20 CONTINUE
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
      KINDL=KINDEL(JWD(I))
      IF(KINDL.EQ.0) GO TO 45
      KNOP(NS)=KINDL
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
      KINDL=KINDEL(JWD(I))
      IF(KINDL.NE.0) GO TO 55
      IF(K.LE.KHI) GO TO 48
      NTER=NTER+1
      GO TO 50
   48 KWD(K)=JWD(I)
      K=K+1
   50 CONTINUE
      GO TO 150
   55 KNOP(NS+1)=KINDL
      JLO=I+1
      GO TO 30
C   
C     ------------------------------------------------------------------
C     RIGHT JUSTIFY NUMERIC FIELDS
C     FIND NEXT DELIMITER
C     ------------------------------------------------------------------
C   
  100 DO 110 I=JLO,JHI
      KINDL=KINDEL(JWD(I))
      IF(KINDL.NE.0) GO TO 120
  110 CONTINUE
      I=JHI+1
  120 KNOP(NS+1)=KINDL
      JUP=I-1
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
