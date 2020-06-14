C$PROG LISTUM    - Lists/logs contents of buffers 1/2
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE LISTUM(KMD,ILOC,IHIC)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      CHARACTER*4  KMD
C   
      INTEGER*4 IBUF(33024),KHF(20),KDAT(16)
C   
      EQUIVALENCE (XBUF(1),IBUF(1))
      EQUIVALENCE (ID,KHF(1)),(LH,KHF(9)),(LD,KHF(10)),(NC,KHF(12))
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      JB=1
      IF(KMD.EQ.'PR2 ') JB=2
      IF(KMD.EQ.'D2  ') JB=2
C   
      IHOF=IOFF(1,JB)
      IDOF=IOFF(2,JB)
      DO 10 I=1,20
      NDX=IHOF+I
      KHF(I)=IBUF(NDX)
   10 CONTINUE
C   
      IF(KMD.EQ.'D1  ') GO TO 200
      IF(KMD.EQ.'D2  ') GO TO 200
C   
C     ------------------------------------------------------------------
C     LIST ENTIRE CONTENTS OF BUFFER-JB
C     ------------------------------------------------------------------
C   
      WRITE(7,40)ID
      WRITE(7,50)LH
      WRITE(7,60)LD
      WRITE(7,70)NC
   40 FORMAT(1H1,'ID =',I8/)
   50 FORMAT(1H ,'LH =',I8/)
   60 FORMAT(1H ,'LD =',I8/)
   70 FORMAT(1H ,'NC =',I8/)
      JFC=0
      IHI=IDOF
      MAX=IDOF+NC
  100 ILO=IHI+1
      IF(ILO.GT.MAX) RETURN
      IHI=ILO+15
      IF(IHI.GT.MAX) IHI=MAX
      N=0
      DO 105 I=ILO,IHI
      N=N+1
      ADD=0.5
      IF(XBUF(I).LT.0.0) ADD=-0.5
      KDAT(N)=XBUF(I)+ADD
  105 CONTINUE
      WRITE(7,110)JFC,(KDAT(I),I=1,N)
  110 FORMAT(1H ,I6,2X,16I7)
      JFC=JFC+16
      GO TO 100
C   
C     ------------------------------------------------------------------
C     DISPLAY SPECIFIED PORTION OF BUFFER-JB
C     ------------------------------------------------------------------
C   
  200 JFC=ILOC
      IF(JFC.LT.0)   JFC=0
      IF(JFC.GT.NC-1)JFC=NC-1
      JLC=IHIC
      IF(JLC.LT.JFC) JLC=JFC
      IF(JLC.GT.NC-1)JLC=NC-1
      NUMC=JLC-JFC+1
      IHI=IDOF+JFC
      MAX=IHI+NUMC
  210 ILO=IHI+1
      IF(ILO.GT.MAX) RETURN
      IHI=ILO+7
      IF(IHI.GT.MAX) IHI=MAX
      N=0
      DO 215 I=ILO,IHI
      N=N+1
      ADD=0.5
      IF(XBUF(I).LT.0.0) ADD=-0.5
      KDAT(N)=XBUF(I)+ADD
  215 CONTINUE
      WRITE(6,220)JFC,(KDAT(I),I=1,N)
  220 FORMAT(1H ,I5,'-',8I9)
      JFC=JFC+8
      GO TO 210
      END
