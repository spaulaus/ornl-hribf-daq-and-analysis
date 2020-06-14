C$PROG BANDRA    - Draws the mighty banana
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BANDRA(IDW,ID,IFLG,P,Q,NUMXY)
C   
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
C   
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C   
      DIMENSION P(64),Q(64)
C   
      INTEGER*4 FLG1,FLG2
C
      CHARACTER*4 IFLG,KOL
C   
      CHARACTER*8 CIDS
C   
      DATA FLG1,FLG2/0,0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NUMXY.LE.0) RETURN
C   
      NXY=NUMXY
      IF(IFLG.EQ.'RED ') THEN
                         NXY=NXY+1
                         P(NXY)=P(1)
                         Q(NXY)=Q(1)
                         ENDIF
C   
      KOL='OGRE'
      CIDS=' '
C
      CALL PLOTXY(IDW,KOL,P,Q,NXY)
C
      IF(NXY.EQ.1) RETURN
C   
      WRITE(CIDS,20)ID
   20 FORMAT(I4)
      CALL SQUEZL(CIDS,1,4)
C
      XM=(P(1)+P(2))/2.0
      YM=(Q(1)+Q(2))/2.0
      IX=AA(1,IDW)+BB(1,IDW)*XM
      JY=AA(2,IDW)+BB(2,IDW)*YM-15              
      CALL TEXOUT(IDW,CIDS,KOL,IX,JY)
      RETURN
      END
