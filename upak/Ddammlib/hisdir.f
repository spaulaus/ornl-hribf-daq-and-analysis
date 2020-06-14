C$PROG HISDIR    - Reads drr-file and displays directory
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE HISDIR(LUD)
C
      COMMON/GN01/ NDLINES,IBELL
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      PARAMETER   (MXNH=6144)
C
      INTEGER*4    IDLST(MXNH),IDIRF(32),MMX(2),MMY(2)
C
      INTEGER*2    IDIRH(64)
C
      EQUIVALENCE (IDIRH(1),IDIRF(1))
C
      CHARACTER*4  IDUM
C
      SAVE
C
C     ------------------------------------------------------------------
C
      READ(LUD,REC=1,IOSTAT=IOS)IDIRF
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      NH=IDIRF(4)
      NBLK=(IDIRF(5)+255)/256
      WRITE(CMSSG,10)NH,NBLK,(IDIRF(I),I=7,12)
   10 FORMAT('NH =',I4,'   #NBLKS =',I6,I10,'/',I2,'/',I2,I4,
     &2(1H:,I2))
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,15)(IDIRF(I),I=13,32)
   15 FORMAT(20A4)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,20)
   20 FORMAT(19('----'))
      CALL MESSLOG(0,LOGUP)
C
      WRITE(CMSSG,30)
   30 FORMAT('   HID BPC D  PX  PY   LX MINX MAXX   LY MINY MAXY',
     &' X-LABEL      Y-LABEL')
      CALL MESSLOG(LOGUT,LOGUP)
C
      NBN=NH+1
      NDO=(NH+31)/32
      IA=-31
      DO 40 I=1,NDO
      IA=IA+32
      IB=IA+31
      NBN=NBN+1
      READ(LUD,REC=NBN,IOSTAT=IOS)(IDLST(J),J=IA,IB)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
   40 CONTINUE
C
      NBN=1
      NLN=0
      DO 100 JH=1,NH
      ID=IDLST(JH)
      NBN=NBN+1
      READ(LUD,REC=NBN,IOSTAT=IOS)IDIRF
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      NDM=IDIRH(1)
      NBIT=16*IDIRH(2)
      IPX=IDIRH(3)
      IPY=IDIRH(4)
      LX =IDIRH(11)
      LY =IDIRH(12)
      MMX(1)=IDIRH(15)
      MMY(1)=IDIRH(16)
      MMX(2)=IDIRH(19)
      MMY(2)=IDIRH(20)
C
      IF(NDM.EQ.2) GO TO 70
C
      WRITE(CMSSG,60)ID,NBIT,NDM,IPX,LX,MMX,(IDIRF(I),I=13,15)
   60 FORMAT(I6,I4,I2,I4,4X,3I5,16X,3A4)
      GO TO 90
C
   70 WRITE(CMSSG,80)ID,NBIT,NDM,IPX,IPY,LX,MMX,LY,MMY,
     &                   (IDIRF(I),I=13,18)
   80 FORMAT(I6,I4,I2,2I4,6I5,1X,3A4,1X,3A4)
C
   90 CALL MESSLOG(LOGUT,LOGUP)
      NLN=NLN+1
      IF(NLN.LT.NDLINES-2) GO TO 100
      NLN=0
      WRITE(LOGUT,92)IBELL
   92 FORMAT(1H ,A1,' THERE IS MORE - DO CR TO CONT - X TO QUIT',$)
      READ(5,94)IDUM
   94 FORMAT(A4)
      IF(IDUM.NE.'    ') RETURN
      WRITE(CMSSG,30)
      CALL MESSLOG(LOGUT,0)
C
  100 CONTINUE
      RETURN
      END
