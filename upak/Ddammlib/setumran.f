C$PROG SETUMRAN  - Set all channels of IDO to randomized ICNT
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SETUMRAN(IDN,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C   
      COMMON/TDX2/ LUDI,LUHI,LUDO,LUHO
C     ------------------------------------------------------------------
      INTEGER*4    IDAT(8192),JDAT(8192)
C
      INTEGER*4    HEDF(32),MSER(10),NDX(4)
C   
      INTEGER*2    HEDH(64),MINC(4),MAXC(4),ND,NHW
C   
      EQUIVALENCE (IDAT,IDATF(49153)),(JDAT,IDATF(57345))
C
      EQUIVALENCE (HEDH(1),HEDF(1)),
     &            (ND     ,HEDH(1)),
     &            (NHW    ,HEDH(2)),
     &            (MINC(1),HEDH(15)),
     &            (MAXC(1),HEDH(19))
C   
      DATA NDX/1,1,0,0/
C
      REAL*4   COUNT,FAC
C
C     ------------------------------------------------------------------
      SAVE
C   
C     ------------------------------------------------------------------
C     SETS ALL CHANNELS OF IDO (OUTPUT) TO "ICNT"
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      CALL IVALU(LWD(1,2),IDO,IERR)
      IF(IERR.NE.0) GO TO 300
C   
      ICNT=0
      IF(NF.LT.3) GO TO 20
C   
      CALL IVALU(LWD(1,3),ICNT,IERR)
      IF(IERR.NE.0) GO TO 300
C   
   20 CALL HISIO('INIT',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('READ',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
C   
      NHWO=NHW
      NCXO=MAXC(1)-MINC(1)+1
      NCYO=MAXC(2)-MINC(2)+1
C   
      MINYO=MINC(2)+1
      MAXYO=MAXC(2)+1
C   
C     ------------------------------------------------------------------
C     SET ALL CHANNELS TO ICNT
C     ------------------------------------------------------------------
C   
      CALL  SLICO(LUDO,LUHO,IDO,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      COUNT=ICNT
C
      FAC=0.5*SQRT(COUNT)
C   
      DO 100 JY=MINYO,MAXYO
C
      DO 50 I=1,NCXO
      IDAT(I)=COUNT+FAC*GRAN12(ISEED)+0.5
      IF(IDAT(I).LT.0) IDAT(I)=0
   50 CONTINUE
C   
      CALL  SLICO(LUDO,LUHO,IDO,JY,IDAT,IERR)
      IF(IERR.NE.0) RETURN
  100 CONTINUE
      CALL  SLICO(LUDO,LUHO,IDO,-1,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      RETURN
C   
  300 WRITE(CMSSG,305)
  305 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      CALL MESSLOG(6,7)
      IERR=1
      RETURN
      END
C$PROG GRAN12
      FUNCTION GRAN12(ISEED)
C
      SUM=0.0
C
      DO 10 I=1,12
      SUM=SUM+RAN(ISEED)
   10 CONTINUE
      GRAN12=2.0*SUM-12.0
      RETURN
      END
