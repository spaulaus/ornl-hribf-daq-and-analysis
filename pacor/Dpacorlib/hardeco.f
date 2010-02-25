C$PROG HARDECO
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE HARDECO(JT,ICOD,NAME,I1,I2)
C
      CHARACTER*4   ICOD,NAME(3)
C
      INTEGER*4     IT(5),JT(5)
C
      INTEGER*4     LWD(2,40),ITYP(40)
C
      SAVE
C
C     ************************************************************
C     DECODES ONE CONTIGUOUS FIELD FROM "JT" OF THE FORM:
C
C     cII  or  cII-JJ  or  cII,JJ  or  name:II,JJ
C
C     c     IS RETURNED IN - ICODE
C     name  IS RETURNED IN - NAME
C     II    IS RETURNED IN - I1
C     JJ    IS RETURNED IN - I2 
C     ************************************************************
C
      ICOD   ='    '
      NAME(1)='    '
      NAME(2)='    '
      NAME(3)='    '
      I1=0
      I2=0
      DO 10 I=1,5
      IT(I)=JT(I)
   10 CONTINUE
C
      LCOL=IFIND(IT,'3A'X,1,20)
      IF(LCOL.GT.0) GO TO 200
C
      LEQU=IFIND(IT,'3D'X,1,20)
      IF(LEQU.GT.0) GO TO 300
C
      LDIG=NXDG(IT,1,20)
      IF(LDIG.LE.0) RETURN
C
      IB=LDIG-1
      CALL LODUP(IT,1,IB,ICOD,1)
C
      CALL ISUBB(IT,1,20,'2D'X,'2C'X)
C
      CALL GREAD(IT,LWD,ITYP,NF,LDIG,20,NTER)
      IF(NTER.NE.0) RETURN
C
      CALL MILV(LWD(1,1),I1,XV,KIND,IERR)
      CALL MILV(LWD(1,2),I2,XV,KIND,IERR)
      RETURN
C
  200 IB=LCOL-1
      CALL LODUP(IT,1,IB,NAME,1)
C
      IB=LCOL+1
      CALL GREAD(IT,LWD,ITYP,NF,IB,20,NTER)
C
      CALL MILV(LWD(1,1),I1,XV,KIND,IERR)
      CALL MILV(LWD(1,2),I2,XV,KIND,IERR)
C
      RETURN
C
  300 IA=LEQU+1
      IB=LSNB(IT,IA,20)
      IF(IB.LT.IA) IB=IA
      IF(IB.GT.IA+11) IB=IA+11
      ICOD='MT  '
      CALL LODUP(IT,IA,IB,NAME,1)
      RETURN
      END
