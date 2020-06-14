C$PROG HISSUM    - Computes/displays # of counts in his-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE HISSUM(LUH,LUD,KIDA)
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
      COMMON/XAM2/ ILOH,IHIH,ILOF,IHIF,JBOFH,JBOFF,MXBN
C
      PARAMETER (MXNH=6144)
C
      COMMON/XAM3/ IDLST(MXNH),IHED(32),NUMHIS
C
      COMMON/XAM1/ NAMFI(20)
C
      INTEGER*2   IBUH(65536),IDIRH(64)
      INTEGER*4   IBUF(32768)
      INTEGER*4   IDIRF(32)
      INTEGER*4   LSUMA(3,8),LSUMB(3,8),LIDL(8),NCHL(8)
C
      REAL*8 SUM,SUMI
C
      EQUIVALENCE (IBUH ,IDATF),
     &            (IBUF ,IDATF),
     &            (IDIRH,IDIRF)
C
      CHARACTER*4  KIDA
C
      SAVE
C
C     ------------------------------------------------------------------
C
C
      IF(KIDA.EQ.'USDA') WRITE(LOGUP,15)NAMFI
      IF(KIDA.NE.'USDA') WRITE(LOGUP,20)NAMFI
   15 FORMAT(1H1,'UNSIGNED - COUNTS VS HIS-ID FOR FILE - ',20A4/)
   20 FORMAT(1H1,'SIGNED - COUNTS VS HIS-ID FOR FILE - ',20A4/)
C
      DO 22 I=1,MXNH
      IDLST(I)=I
   22 CONTINUE
C
      CALL DIRKIN(LUD,NHIS,NBYTS,IERR)
      IF(IERR.NE.0) RETURN
C
      NBLK=NBYTS/512
      IF(512*NBLK.LT.NBYTS) NBLK=NBLK+1
      MXBN=NBLK
      CALL DINPUT(LUH,LUD,0,IERR)
      IF(IERR.NE.0) RETURN
C
      NL=0
      SUM=0.0
      NREC=1
      NLN=0
C
      DO 300 NH=1,NHIS
      NREC=NREC+1
      READ(LUD,REC=NREC,IOSTAT=IOS)IDIRF
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      ND=IDIRH(1)
      NHW=IDIRH(2)
      IOF=IDIRF(12)
      LH=1
      DO 50 I=1,ND
      LH=LH*(IDIRH(I+18)-IDIRH(I+14)+1)
   50 CONTINUE
      LH=NHW*LH
      NDO=LH/NHW
      NCHS=LH
      SUMI=0.0D0
C
      IF(NHW.EQ.2)       GO TO 100
      IF(KIDA.EQ.'SIDA') GO TO 80
C
      JDOF=IOF                          !HALF-WORD DISK OFFSET
      DO 70 I=1,NDO
      JDOF=JDOF+1                       !NEXT DISK HALF-WORD TO USE
      IF(JDOF.GT.IHIH) CALL DINPUT(LUH,LUD,1,IERR)
      IF(IERR.NE.0) RETURN
C
      NDX=JDOF+JBOFH
      IADD=IBUH(NDX)
      SUMI=SUMI+IAND(IADD,65535)
   70 CONTINUE
      GO TO 200
C
   80 JDOF=IOF                          !HALF-WORD DISK OFFSET
      DO 90 I=1,NDO
      JDOF=JDOF+1                       !NEXT DISK HALF-WORD TO USE
      IF(JDOF.GT.IHIH) CALL DINPUT(LUH,LUD,1,IERR)
      IF(IERR.NE.0) RETURN
C
      NDX=JDOF+JBOFH
      SUMI=SUMI+IBUH(NDX)
   90 CONTINUE
      GO TO 200
C
  100 JDOF=IOF/2                        !FULL-WORD DISK OFFSET
      DO 150 I=1,NDO
      JDOF=JDOF+1                       !NEXT DISK FULL-WORD TO USE
      IF(JDOF.GT.IHIF) CALL DINPUT(LUH,LUD,1,IERR)
      IF(IERR.NE.0) RETURN
C
      SUMI=SUMI+IBUF(JDOF+JBOFF)
  150 CONTINUE
C
  200 NL=NL+1
      SUM=SUM+SUMI
      CALL DFASCII(SUMI,LSUMB(1,NL),11)
      LIDL(NL)=IDLST(NH)
      NCHL(NL)=NCHS
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
