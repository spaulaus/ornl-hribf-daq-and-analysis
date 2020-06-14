C$PROG LABLMAN   - Key processor for labeling
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE LABLMAN(MODE,IDW,KEY,KX,KY)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLLL/ NUWFLG1(20),NUWFLG2(20),NUWFLG3(20)
      CHARACTER*4  NUWFLG1,    NUWFLG2,    NUWFLG3
C     ------------------------------------------------------------------
      COMMON/LAXX/ LA7(20,10),LA8(20,10),LA9(20,10),NL7,NL8,NL9,KOL
      INTEGER*4                                     NL7,NL8,NL9
      CHARACTER*4  LA7,       LA8,       LA9,                   KOL
C     ------------------------------------------------------------------
C
      INTEGER*4    IDW,KX,KY
C
      CHARACTER*4  MODE,KEY
C
      INTEGER*4    LS7(20,10,20),LS8(20,10,20),LS9(20,10,20)
      INTEGER*4    NS7(20),NS8(20),NS9(20)
C
      CHARACTER*4  ISET7(20),ISET8(20),ISET9(20)
C
      INTEGER*4    LX7(20),LY7(20)
      INTEGER*4    LX8(20),LY8(20)
      INTEGER*4    LX9(20),LY9(20)
      INTEGER*4    LX(5),LY(5)
      INTEGER*4    JVX(20,20),JVY(20,20),NUMV(20)
      INTEGER*4    MX7(2,20),MX8(2,20),MX9(2,20)
      INTEGER*4    MY7(2,20),MY8(2,20),MY9(2,20)
C
      CHARACTER*80 CLA7(10),CLA8(10),CLA9(10)
      CHARACTER*80 CLS7(10,20),CLS8(10,20),CLS9(10,20)
C
      EQUIVALENCE (CLA7,LA7),(CLA8,LA8),(CLA9,LA9)
      EQUIVALENCE (CLS7,LS7),(CLS8,LS8),(CLS9,LS9)
C
      DATA LX7,LY7,ISET7/20*0,20*0,20*'NO  '/
      DATA LX8,LY8,ISET8/20*0,20*0,20*'NO  '/
      DATA LX9,LY9,ISET9/20*0,20*0,20*'NO  '/
C
      INTEGER*4    MAXCH,NCH,IA,NN,JX,JY,JYI,NDO,N,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      MAXCH=0
C
      IF(NUWFLG1(IDW).EQ.'YES ') THEN
                                 ISET7(IDW)='NO  '
                                 ISET8(IDW)='NO  '
                                 ISET9(IDW)='NO  '
                                 NUMV(IDW) =0
                                 NUWFLG1(IDW)='NO  '
                                 ENDIF
C
      IF(MODE.NE.'INIT') GO TO 50
C
      DO 10 I=1,20
      ISET7(I)='NO  '
      ISET8(I)='NO  '
      ISET9(I)='NO  '
      NUMV(I)=0
   10 CONTINUE
      RETURN
C
   50 IF(KEY.EQ.'7   ') GO TO 200
      IF(KEY.EQ.'8   ') GO TO 300
      IF(KEY.EQ.'9   ') GO TO 400
      IF(KEY.EQ.'0   ') GO TO 60
      IF(KEY.EQ.';   ') GO TO 100
      IF(KEY.EQ.'=   ') GO TO 110
      IF(KEY.EQ.'-   ') GO TO 120
C
      RETURN
C
   60 IF(ISET7(IDW).EQ.'YES ') THEN
      CALL PLOTPIX(IDW,KOL,MX7(1,IDW),MY7(1,IDW),2)
      ISET7(IDW)='NO  '
                               ENDIF
      IF(ISET8(IDW).EQ.'YES ') THEN
      CALL PLOTPIX(IDW,KOL,MX8(1,IDW),MY8(1,IDW),2)
      ISET8(IDW)='NO  '
                               ENDIF
      IF(ISET9(IDW).EQ.'YES ') THEN
      CALL PLOTPIX(IDW,KOL,MX9(1,IDW),MY9(1,IDW),2)
      ISET9(IDW)='NO  '
                               ENDIF
      RETURN
C
  100 NUMV(IDW)=0
      RETURN
C
  110 NN=NUMV(IDW)+1
      IF(NN.EQ.1) CALL DRACIR(IDW,KOL,KX,KY)
      IF(NN.EQ.2) CALL DRACIR(IDW,KOL,JVX(1,IDW),JVY(1,IDW))
      IF(NN.GT.20) RETURN
      NUMV(IDW)=NN
      JVX(NN,IDW)=KX
      JVY(NN,IDW)=KY
      IF(NN.LT.2) RETURN
      CALL PLOTPIX(IDW,KOL,JVX(NN-1,IDW),JVY(NN-1,IDW),2)
      RETURN
C
  120 IF(NUMV(IDW).LT.1) RETURN
C
      IF(NUMV(IDW).EQ.1) THEN
                         CALL DRACIR(IDW,KOL,JVX(1,IDW),JVY(1,IDW))
                         NUMV(IDW)=0
                         RETURN
                         ENDIF
C
      IA=NUMV(IDW)-1
      CALL PLOTPIX(IDW,KOL,JVX(IA,IDW),JVY(IA,IDW),2)
      NUMV(IDW)=NUMV(IDW)-1
      IF(NUMV(IDW).EQ.1) CALL DRACIR(IDW,KOL,JVX(1,IDW),JVY(1,IDW))
      RETURN
C
  200 IF(ISET7(IDW).NE.'YES ') THEN
      JX=KX+1
      JY=KY+12
      JYI=KY
      ISET7(IDW)='YES '
      CALL LACOPY(LA7(1,1),LS7(1,1,IDW),NL7,NS7(IDW))
      LX7(IDW)=JX
      LY7(IDW)=JY
      GO TO 220
      ENDIF
C
      JX=LX7(IDW)
      JY=LY7(IDW)
      JYI=JY-12
      ISET7(IDW)='NO  '
      GO TO 240
C
  220 NDO=NL7
      DO 230 N=1,NDO
      CALL LABLOUT(IDW,CLA7(N),KOL,JX,JY,NCH)
      IF(NCH.GT.MAXCH) MAXCH=NCH
      JY=JY+12
  230 CONTINUE
      MX7(1,IDW)=JX-4
      MX7(2,IDW)=JX-4
      MY7(1,IDW)=JYI
      MY7(2,IDW)=JYI+12*NDO+5
      GO TO 500
C
  240 NDO=NS7(IDW)
      DO 250 N=1,NDO
      CALL LABLOUT(IDW,CLS7(N,IDW),KOL,JX,JY,NCH)
      IF(NCH.GT.MAXCH) MAXCH=NCH
      JY=JY+12
  250 CONTINUE
      GO TO 500
C
  300 IF(ISET8(IDW).NE.'YES ') THEN
      JX=KX+1
      JY=KY+12
      JYI=KY
      ISET8(IDW)='YES '
      CALL LACOPY(LA8(1,1),LS8(1,1,IDW),NL8,NS8(IDW))
      LX8(IDW)=JX
      LY8(IDW)=JY
      GO TO 320
      ENDIF
C
      JX=LX8(IDW)
      JY=LY8(IDW)
      JYI=JY-12
      ISET8(IDW)='NO  '
      GO TO 340
C
  320 NDO=NL8
      DO 330 N=1,NDO
      CALL LABLOUT(IDW,CLA8(N),KOL,JX,JY,NCH)
      IF(NCH.GT.MAXCH) MAXCH=NCH
      JY=JY+12
  330 CONTINUE
      MX8(1,IDW)=JX-4
      MX8(2,IDW)=JX-4
      MY8(1,IDW)=JYI
      MY8(2,IDW)=JYI+12*NDO+5
      GO TO 500
C
  340 NDO=NS8(IDW)
      DO 350 N=1,NDO
      CALL LABLOUT(IDW,CLS8(N,IDW),KOL,JX,JY,NCH)
      IF(NCH.GT.MAXCH) MAXCH=NCH
      JY=JY+12
  350 CONTINUE
      GO TO 500
C
  400 IF(ISET9(IDW).NE.'YES ') THEN
      JX=KX+1
      JY=KY+12
      JYI=KY
      ISET9(IDW)='YES '
      CALL LACOPY(LA9(1,1),LS9(1,1,IDW),NL9,NS9(IDW))
      LX9(IDW)=JX
      LY9(IDW)=JY
      GO TO 420
      ENDIF
C
      JX=LX9(IDW)
      JY=LY9(IDW)
      JYI=JY-12
      ISET9(IDW)='NO  '
      GO TO 440
C
  420 NDO=NL9
      DO 430 N=1,NDO
      CALL LABLOUT(IDW,CLA9(N),KOL,JX,JY,NCH)
      IF(NCH.GT.MAXCH) MAXCH=NCH
      JY=JY+12
  430 CONTINUE
      MX9(1,IDW)=JX-4
      MX9(2,IDW)=JX-4
      MY9(1,IDW)=JYI
      MY9(2,IDW)=JYI+12*NDO+5
      GO TO 500
C
  440 NDO=NS9(IDW)
      DO 450 N=1,NDO
      CALL LABLOUT(IDW,CLS9(N,IDW),KOL,JX,JY,NCH)
      IF(NCH.GT.MAXCH) MAXCH=NCH
      JY=JY+12
  450 CONTINUE
C
  500 LX(1)=JX-4
      LY(1)=JYI-1
      LX(2)=LX(1)+6*MAXCH+8
      LY(2)=LY(1)
      LX(3)=LX(2)
      LY(3)=LY(1)+12*NDO+6
      LX(4)=LX(1)
      LY(4)=LY(3)
      CALL PLOTPIX(IDW,KOL,LX,LY,4)
      RETURN
      END
