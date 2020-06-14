C$PROG RELISET   - Sets up relative intensity specifications
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE RELISET
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
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C     ------------------------------------------------------------------   
      COMMON/SM28/ RELI(44),NREL,NPKK,KRELF
      CHARACTER*4                     KRELF
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMX
C
      EQUIVALENCE (KMX,LWD(1,2))
C
      REAL*4 XV(2)
C
      SAVE
C
C     ------------------------------------------------------------------
C     PROCESS RELATIVE INTENSITY COMMANDS - RELI CH,REL
C                                           RELI OFF
C                                           RELI ON
C                                           RELI ZOT
C     ------------------------------------------------------------------
C
      IF(KMX.EQ.'ZOT ') GO TO 200
      IF(KMX.EQ.'OFF ') GO TO 220
      IF(KMX.EQ.'ON  ') GO TO 240
C
      IF(NF.NE.3)            GO TO 300
C
      DO 20 I=1,2
      CALL MILV(LWD(1,I+1),IV,XV(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 300
   20 CONTINUE
C
      IX=201
      DIF=32000.0
C
      DO 50 I=1,NPAT
      DIFX=ABS(PAT(I,1)-XV(1))
      IF(DIFX.GT.5.0) GO TO 50
      IF(DIFX.LT.DIF) THEN
                      DIF=DIFX
                      IX=I
                      ENDIF
   50 CONTINUE
C
      IF(IX.EQ.201) GO TO 300
      PAT(IX,13)=XV(2)
      RETURN
C
  200 DO 210 I=1,200
      PAT(I,13)=0.0
  210 CONTINUE
      RETURN
C
  220 KRELF='OFF '
      RETURN
C
  240 KRELF='ON  '
      RETURN
C
  300 WRITE(CMSSG,305)
  305 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
