C$PROG ELEMENTX  - Finds element from ASCII name
C
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE ELEMENTX(ICH,II,IN,IGO,IC)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SYMBOL/  BLNK,PNT,GASS,MNS,NUCN(105),ICHR(5)
      INTEGER*4       BLNK,PNT,GASS,MNS,NUCN,     ICHR
C     ------------------------------------------------------------------
      COMMON/ABSS/    AZ(5,10),AA(5,10),INUM(5,10),IKEY(5,10),
     &                FRACT(5,10),NCOM(10),PRES(10),THCK(10),IONZ(10)
C
      REAL*4          AZ,AA,FRACT,PRES,THCK,IONZ
C
      INTEGER*4       INUM,IKEY,NCOM
C     ------------------------------------------------------------------
      COMMON/ABS2/    KEY,FRCT
      INTEGER*4       KEY
      REAL*4              FRCT
C     ------------------------------------------------------------------
      COMMON/PROJ/    PZ(100),PA(100),EP(500)
      REAL*4          PZ,     PA,     EP
C     ------------------------------------------------------------------
      INTEGER*4       ICH(80),II,IN,IGO,IC
C
      INTEGER*4       JAWD(4),JNWD(4),JMWD(4)
C
      INTEGER*4       I,II0,JA,JN,IJ,IK,IL,JAD,JND,JZ,JMD,NC,IFLG
C
      INTEGER*4       X30,X39,X41,X5A
      DATA            X30,X39,X41,X5A/'30'X,'39'X,'41'X,'5A'X/
C
      EXTERNAL        PACK
C
      CHARACTER*4     JADC,JMDC
C
      EQUIVALENCE     (JADC,JAD),(JMDC,JMD)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IGO=1
      II0=II
      JA=0
      JN=1
      DO 10 I=1,4
      JAWD(I)=BLNK
      JNWD(I)=BLNK
      JMWD(I)=BLNK
   10 CONTINUE
C
      IJ=1
      IK=1
      IL=1
C
      IF(ICH(II).LT.X30.OR.ICH(II).GT.X39) GO TO 50
C
      JAWD(IJ)=ICH(II)
      II=II+1
      IJ=IJ+1
C
   20 IF(ICH(II).LT.X30.OR.ICH(II).GT.X39) GO TO 30
C
      JAWD(IJ)=ICH(II)
      II=II+1
      IJ=IJ+1
      GO TO 20
C
   30 CALL PACK(JAWD,JAD,4)
      CALL SQUEZR(JAD,1,4)
      READ(JADC,40)JA
   40 FORMAT(I4)
C
   50 IF(ICH(II).LT.X41.OR.ICH(II).GT.X5A) GO TO 900
C
      JNWD(IK)=ICH(II)
      II=II+1
      IK=IK+1
C
      IF(ICH(II).LT.X41.OR.ICH(II).GT.X5A) GO TO 60
C
      JNWD(IK)=ICH(II)
      II=II+1
C
   60 CALL PACK(JNWD,JND,4)
      DO 70 I=1,100
      IF(JND.EQ.NUCN(I)) GO TO 80
   70 CONTINUE
      GO TO 900
C
   80 JZ=I
C
      IF(ICH(II).LT.X30.OR.ICH(II).GT.X39) GO TO 500
C
      JMWD(IL)=ICH(II)
      II=II+1
      IL=IL+1
C
   90 IF(ICH(II).LT.X30.OR.ICH(II).GT.X39) GO TO 100
C
      JMWD(IL)=ICH(II)
      II=II+1
      IL=IL+1
      GO TO 90
C
  100 CALL PACK(JMWD,JMD,4)
      CALL SQUEZR(JMD,1,4)
      READ(JMDC,40)JN
C
  500 IF(IC.EQ.0) GO TO 600
C
      PZ(IN)=JZ
      PA(IN)=JA
      IGO=2
      RETURN
C
  600 NC=NCOM(IN)+1
      AZ(NC,IN)=JZ
      AA(NC,IN)=JA
      INUM(NC,IN)=JN
      FRACT(NC,IN)=FRCT
      IKEY(NC,IN)=KEY
      NCOM(IN)=NC
      IGO=2
      FRCT=1.
      CALL BLANK(ICH,II,IFLG)
      RETURN
C
  900 II=II0
      RETURN
      END
