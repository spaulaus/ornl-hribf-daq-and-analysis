C$PROG FONTMO
      SUBROUTINE FONTMO(MODE,KMD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/XXX/ BOLDFLG,UNDLFLG,BOLDON,UNDLON,BOLDOF(2),UNDLOF(2)
      CHARACTER*4 BOLDFLG,UNDLFLG,BOLDON,UNDLON,BOLDOF,   UNDLOF
C
      COMMON/YYY/ KLIS,KAUTOSP,KPAGLAB,KTOFLAG,NPAGSP
      CHARACTER*4 KLIS,KAUTOSP,KPAGLAB,KTOFLAG
C
      BYTE BBOLDON(4),BUNDLON(4),BBOLDOF(8),BUNDLOF(8)
C
      EQUIVALENCE (BBOLDON,BOLDON),(BUNDLON,UNDLON),
     &            (BBOLDOF,BOLDOF),(BUNDLOF,UNDLOF)
C
c                          '[', '1', 'm'
      DATA BBOLDON/Z'1B',Z'5B',Z'31',Z'6D'/
c                          '[', '4', 'm'
      DATA BUNDLON/Z'1B',Z'5B',Z'34',Z'6D'/
C
c                         '[',   '2',  '2',  'm'
      DATA BBOLDOF/Z'1B',Z'5B',Z'32',Z'32',Z'6D',Z'00',Z'00',Z'00'/
c                         '[',   '2',  '4',  'm'
      DATA BUNDLOF/Z'1B',Z'5B',Z'32',Z'34',Z'6D',Z'00',Z'00',Z'00'/
C
      DATA BOLDFLG,UNDLFLG/'BOF$','UOF$'/
C
      CHARACTER*4  MODE,KMD
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C
C     *************************************************************
C     FONTMO TURNS "BOLD" AND "UNDERLINE" ON/OFF AS FOLLOWS:
C
C     MODE = 'DO  ' PROCESSES KMD BUT DOES NOT CHANGE GLOBAL STATE
C     MODE = 'SET ' PROCESSES KMD AND CHANGES GLOBAL STATE 
C     MODE = 'RES ' RESETS TO GLOBAL STATE
C     *************************************************************
C
      IF(KLIS.NE.'LN03') RETURN
C   
      IF(MODE.EQ.'DO  ') GO TO 100
      IF(MODE.EQ.'SET ') GO TO 200
      IF(MODE.EQ.'RES ') GO TO 300
                         RETURN
C
  100 IF(KMD.EQ.'BON$')  GO TO 110
      IF(KMD.EQ.'UON$')  GO TO 120
      IF(KMD.EQ.'BOF$')  GO TO 130
      IF(KMD.EQ.'UOF$')  GO TO 140
                         RETURN
C
  110 WRITE(7,115)BOLDON
  115 FORMAT(A4,$)
      RETURN
  120 WRITE(7,115)UNDLON
      RETURN
  130 WRITE(7,135)BOLDOF
  135 FORMAT(A4,A2,$)
      RETURN
  140 WRITE(7,135)UNDLOF
      RETURN
C
  200 IF(KMD.EQ.'BON$')  GO TO 210
      IF(KMD.EQ.'UON$')  GO TO 220
      IF(KMD.EQ.'BOF$')  GO TO 230
      IF(KMD.EQ.'UOF$')  GO TO 240
                         RETURN
C
  210 WRITE(7,115)BOLDON
      BOLDFLG='BON$'
      RETURN
  220 WRITE(7,115)UNDLON
      UNDLFLG='UON$'
      RETURN
  230 WRITE(7,135)BOLDOF
      BOLDFLG='BOF$'
      RETURN
  240 WRITE(7,135)UNDLOF
      UNDLFLG='UOF$'
      RETURN
C
  300 IF(KMD.EQ.'BON$')  GO TO 310
      IF(KMD.EQ.'UON$')  GO TO 320
      IF(KMD.EQ.'BOF$')  GO TO 330
      IF(KMD.EQ.'UOF$')  GO TO 340
                         RETURN
C
  310 IF(KMD.NE.BOLDFLG) WRITE(7,135)BOLDOF 
      RETURN
  320 IF(KMD.NE.UNDLFLG) WRITE(7,135)UNDLOF
      RETURN
  330 IF(KMD.NE.BOLDFLG) WRITE(7,115)BOLDON
      RETURN
  340 IF(KMD.NE.UNDLFLG) WRITE(7,115)UNDLON
      RETURN
      END
