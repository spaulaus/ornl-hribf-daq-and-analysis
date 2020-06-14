C$PROG SPKSUM    - Computes sum-of-counts for all IDs in spk-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPKSUM(LU)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      COMMON/GN01/ NDLINES,IBELL
C
      COMMON/XAM1/ NAMFI(20)
C     ------------------------------------------------------------------
      INTEGER*4 IDAT(16384),IHED(32),JDLST(256),IDLST(255),NDX(4)
C
      INTEGER*4 LSUMA(3,8),LSUMB(3,8),LIDL(8),NCHL(8)
C
      REAL*8 SUM,SUMI
C
      EQUIVALENCE (IDAT,IDATF)
C
      EQUIVALENCE (NUMSPK,JDLST(1)),(IDLST,JDLST(2))
C
      DATA NDX/1,0,0,0/
C
      SAVE
C
C     ==================================================================
C
      WRITE(LOGUP,20)(NAMFI(I),I=1,20)
   20 FORMAT(1H1,'COUNTS VS SPK-ID FOR FILE - ',20A4/)
C
      CALL SPKIO(6,LU,0,IHED,64,JDLST,NDX,0,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
C
      SUM=0.0
      NL=0
      NLN=0
      DO 300 NH=1,NUMSPK
C
      NC=16384
      ID=IDLST(NH)
      CALL SPKIO(1,LU,ID,IHED,64,IDAT,NDX,NC,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
      NCH=IHED(12)
      IF(NCH.GT.16384) NCH=16384
      SUMI=0.0D0
C
      DO 150 I=1,NCH
      SUMI=SUMI+IDAT(I)
  150 CONTINUE
C
      NL=NL+1
      SUM=SUM+SUMI
      CALL DFASCII(SUMI,LSUMB(1,NL),11)
      LIDL(NL)=IDLST(NH)
      NCHL(NL)=IHED(12)
      CALL DFASCII(SUMI,LSUMA(1,NL),8)
      IF(NL.LT.8) GO TO 300
      WRITE(LOGUT,208)LIDL
      WRITE(LOGUT,210)NCHL
      WRITE(LOGUT,212)((LSUMA(I,J),I=1,2),J=1,8)
      WRITE(LOGUP,214)LIDL
      WRITE(LOGUP,216)NCHL
      WRITE(LOGUP,218)LSUMB
  208 FORMAT(1H ,'ID#=',8I9)
  210 FORMAT(1H ,'NCH=',8I9)
  212 FORMAT(1H ,'SUM=',8(1X,2A4))
  214 FORMAT(1H ,'ID#=',8I12)
  216 FORMAT(1H ,'NCH=',8I12)
  218 FORMAT(1H ,'SUM=',8(1X,2A4,A3)/)
C
      NLN=NLN+3
      IF(NLN.LT.NDLINES) GO TO 230
      WRITE(LOGUT,220)IBELL
  220 FORMAT(1H ,A1,' THERE IS MORE - TYPE: [RETURN] TO CONTINUE',$)
      READ(5,222)IDUM
  222 FORMAT(A4)
      NLN=0
C
  230 NL=0
C
  300 CONTINUE
      IF(NL.LE.0) GO TO 310
      WRITE(LOGUT,208)(LIDL(J),J=1,NL)
      WRITE(LOGUT,210)(NCHL(J),J=1,NL)
      WRITE(LOGUT,212)((LSUMA(I,J),I=1,2),J=1,NL)
      WRITE(LOGUP,214)(LIDL(J),J=1,NL)
      WRITE(LOGUP,216)(NCHL(J),J=1,NL)
      WRITE(LOGUP,218)((LSUMB(I,J),I=1,3),J=1,NL)
  310 WRITE(LOGUP,315)
  315 FORMAT(1H )
      WRITE(LOGUT,320)SUM
      WRITE(LOGUP,320)SUM
  320 FORMAT(1H ,'TOTAL SUM =',1PD16.9)
      RETURN
      END
