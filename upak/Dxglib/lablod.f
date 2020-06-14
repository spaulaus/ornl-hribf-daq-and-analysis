C$PROG LABLOD    - Sets up labels for graphic display
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE LABLOD
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/LAXX/ LA7(20,10),LA8(20,10),LA9(20,10),NL7,NL8,NL9,KOL
      INTEGER*4                                     NL7,NL8,NL9
      CHARACTER*4  LA7,       LA8,       LA9,                   KOL
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,KOLT,CLWD(2,40)
C
      EQUIVALENCE (KMD,LWD(1,1)),(CLWD,LWD)
C
      DATA KOL/'OGRE'/
C
      INTEGER*4    NXNB,LSNB,IA,IB,IERR,I,J
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'LAZ7') GO TO 100
      IF(KMD.EQ.'LAZ8') GO TO 110
      IF(KMD.EQ.'LAZ9') GO TO 120
      IF(KMD.EQ.'LAL ') GO TO 130
      IF(KMD.EQ.'LAC ') GO TO 170
C
      IF(KMD.EQ.'LA7 ') GO TO 200
      IF(KMD.EQ.'LA8 ') GO TO 300
      IF(KMD.EQ.'LA9 ') GO TO 400
C
      RETURN
C
  100 NL7=0
      RETURN
  110 NL8=0
      RETURN
  120 NL9=0
      RETURN
C
  130 DO 140 J=1,NL7
      WRITE(6,135)J,(LA7(I,J),I=1,20)
  135 FORMAT(1H ,'LA7',I3,2X,20A4)
  140 CONTINUE
C
      DO 150 J=1,NL8
      WRITE(6,145)J,(LA8(I,J),I=1,20)
  145 FORMAT(1H ,'LA8',I3,2X,20A4)
  150 CONTINUE
C
      DO 160 J=1,NL9
      WRITE(6,155)J,(LA9(I,J),I=1,20)
  155 FORMAT(1H ,'LA9',I3,2X,20A4)
  160 CONTINUE
      RETURN 
C
  170 KOLT=CLWD(1,2)
      IF(KOLT.EQ.'WHIT') GO TO 180
      IF(KOLT.EQ.'RED ') GO TO 181
      IF(KOLT.EQ.'GREE') GO TO 182
      IF(KOLT.EQ.'BLUE') GO TO 183
      IF(KOLT.EQ.'RG  ') GO TO 184
      IF(KOLT.EQ.'RB  ') GO TO 185
      IF(KOLT.EQ.'GB  ') GO TO 186
      GO TO 910
C
  180 KOL='OWHI'
      RETURN
  181 KOL='ORED'
      RETURN
  182 KOL='OGRE'
      RETURN
  183 KOL='OBLU'
      RETURN
  184 KOL='ORG '
      RETURN
  185 KOL='ORB '
      RETURN
  186 KOL='OGB '
      RETURN
C
  200 NL7=NL7+1
      IF(NL7.GT.10) NL7=10
      DO 210 I=1,20
      LA7(I,NL7)='    '
  210 CONTINUE
      IA=NXNB(IWDRAW,4,80)
      IB=LSNB(IWDRAW,IA,80)
      CALL LODUP(IWDRAW,IA,IB,LA7(1,NL7),1)
      RETURN
C
  300 NL8=NL8+1
      IF(NL8.GT.10) NL8=10
      DO 310 I=1,20
      LA8(I,NL8)='    '
  310 CONTINUE
      IA=NXNB(IWDRAW,4,80)
      IB=LSNB(IWDRAW,IA,80)
      CALL LODUP(IWDRAW,IA,IB,LA8(1,NL8),1)
      RETURN
C
  400 NL9=NL9+1
      IF(NL9.GT.10) NL9=10
      DO 410 I=1,20
      LA9(I,NL9)='    '
  410 CONTINUE
      IA=NXNB(IWDRAW,4,80)
      IB=LSNB(IWDRAW,IA,80)
      CALL LODUP(IWDRAW,IA,IB,LA9(1,NL9),1)
      RETURN
  910 WRITE(LOGUT,915)
  915 FORMAT(1H ,'ILLEGAL LABEL COLOR SPECIFICATION - IGNORED')
      IERR=1
      RETURN
      END
