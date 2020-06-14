C$PROG PLIST
      SUBROUTINE PLIST(LWD,ITYP,NF,LIST,NL,IERR,MSER)
C
      INTEGER*4 MSG(10,2),MSER(10)
      CHARACTER*40 MSC(2)
C
      INTEGER*4 LWD(2,40),ITYP(40)
C
      INTEGER*4 LIST(2002)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'ERROR DECODING LIST                     ',
     2'                                        '/
      DATA MAXL/2000/
C
      CHARACTER*4  LTYP
C
      INTEGER*4    TO
      character*4  cto
      equivalence (cto,to)
      DATA         cTO/'TO  '/
C
      SAVE
C
C     **************************************************************
C     PROCESS PARM LISTS OF THE FORM - P TO P       (LTYP='LOOP')
C                                      P TO P,I     (LTYP='LOOP')
C                                      P,P,P ---    (LTYP='LIST')
C     **************************************************************
C
      IERR=0
      IF(NF.LT.1) GO TO 210
      LTYP='LIST'
      IF(LWD(1,2).EQ.TO) LTYP='LOOP'
      IF(LTYP.EQ.'LIST') GO TO 110
      IF(LTYP.EQ.'LOOP') GO TO 150
      GO TO 210
C
C     **************************************************************
C     DECODE EXPLICIT LIST TYPE
C     **************************************************************
C
  110 DO 130 I=1,NF
      IF(ITYP(I).NE.2) GO TO 210
      IV=NVALU(LWD(1,I),IERR,MSER)
      IF(IERR.NE.0) RETURN
      LIST(I)=IV
  130 CONTINUE
      NL=NF
      RETURN
C
C     **************************************************************
C     DECODE THE FORM - IA TO IB,IC
C                  OR - IA TO IB
C     **************************************************************
C
  150 IF(NF.LT.3) GO TO 210
      IF(ITYP(1).NE.2) GO TO 210
      IA=NVALU(LWD(1,1),IERR,MSER)
      IF(IERR.NE.0) RETURN
      IF(ITYP(3).NE.2) GO TO 210
      IB=NVALU(LWD(1,3),IERR,MSER)
      IF(IERR.NE.0) RETURN
      IC=1
      IF(NF.LT.4) GO TO 160
      IF(ITYP(4).NE.2) GO TO 210
      ICX=NVALU(LWD(1,4),IERR,MSER)
      IF(IERR.NE.0) RETURN
      IF(ICX.GT.0) IC=ICX
C
  160 N=0
      DO 170 I=IA,IB,IC
      N=N+1
      IF(N.GT.MAXL) GO TO 210
      LIST(N)=I
  170 CONTINUE
      NL=N
      RETURN
C
  210 JJ=1
      GO TO 300
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
      NL=0
      RETURN
      END
