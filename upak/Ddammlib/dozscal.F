C$PROG DOZSCAL   - Displays counts vs color/grey/dots for 2-D displays
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DOZSCAL(IDW,KPLO,AZ,BZ,NZADD,NZVAL)
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
      COMMON/XLDD/ FIGDAT(4,20)
      INTEGER*4    FIGDAT
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
#if (PTRSIZE == 64)
      INTEGER*8    GCOR,   GCON                             !Alpha STAR8
#else
      INTEGER*4    GCOR,   GCON                             !Alpha STAR8
#endif
C     ------------------------------------------------------------------
      COMMON/PL10/ IXOF(25),JYOF(25),SYMPO(25),KOLOFF,KINZMAP
      CHARACTER*4                                     KINZMAP
C     ------------------------------------------------------------------
      COMMON/PL12/ KZSCAL,NZLEV
      CHARACTER*4  KZSCAL
C     ------------------------------------------------------------------
C
      CHARACTER*4  KPLO
C
      INTEGER*4    ZLIS(30),SYMPO
C
      REAL*8       DAZ,DBZ,XDAT,ARG
C
      CHARACTER*8  CVAL
C
      INTEGER*2    DOTS(2,1000)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KZSCAL.NE.'ZSON') RETURN
C
      NDO=NZVAL+1
C
      IF(KPLO.EQ.'LOG ') GO TO 200
C
      DO 110 JC=1,NDO
      XDAT=(FLOAT(JC)-AZ)/BZ
      RND=0.5
      IF(XDAT.LT.0.0D0) RND=-0.5
      ZLIS(JC)=XDAT+RND
  110 CONTINUE
      GO TO 300
C
  200 DAZ=AZ
      DBZ=BZ
      DO 210 JC=1,NDO
      ARG=(DFLOAT(JC)-DAZ)/DBZ
      IF(ARG.LT.1.0D-10) ARG=1.0D-10
      XDAT=DEXP(ARG)
      ZTST=XDAT-NZADD
      RND=0.5
      IF(ZTST.LT.0.0) RND=-1.5
      ZLIS(JC)=ZTST+RND
  210 CONTINUE
C
  300 IX=FIGDAT(3,1)-64
      JX=IX+54
      IY=12
      JY=4
      I=NDO
      IGC=KOLOFF+NDO
C
      DO 400 II=1,NDO
      WRITE(CVAL,310)ZLIS(I)
  310 FORMAT(I8)
C
      CALL TEXOUT(IDW,CVAL,'WHIT',IX,IY)
C
      IF(II.EQ.1) GO TO 390
C
      IF(KINZMAP.EQ.'DOTS') GO TO 320
C
      CALL XX_FILLRECTANGLE(DPY,WDID(IDW),GCON(IGC),JX,JY,8,8)
      GO TO 390
C
  320 NP=0
      NDO=SYMPO(I)
      DO 330 NN=1,NDO
      NP=NP+1
      DOTS(1,NP)=IXOF(NN)+JX
      DOTS(2,NP)=JYOF(NN)+JY+6
  330 CONTINUE
      CALL XX_DRAWPOINTS(DPY,WDID(IDW),GCON(2),DOTS,NP,MODE_OR)
C
  390 IY=IY+12
      JY=JY+12
      IGC=IGC-1
      I=I-1
  400 CONTINUE
      RETURN
      END
