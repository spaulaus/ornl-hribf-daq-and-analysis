C$PROG LDIR      - Lists the complete his-file directory
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE LDIR(LIN)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 DIRF(32)
      INTEGER*2 DIRH(64)
C
      EQUIVALENCE (DIRF,DIRH)
C
      SAVE
C
C     ==================================================================
C
      IREC=1
C           
      READ(LIN,REC=IREC,IOSTAT=IOS)DIRF
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      WRITE(LOGUP,10)(DIRF(I),I=1,3)
   10 FORMAT(1H ,3A4)
C
      WRITE(LOGUP,20)DIRF(4)
   20 FORMAT(1H ,'N-HIS =',I8)
C
      WRITE(LOGUP,30)DIRF(5)
   30 FORMAT(1H ,'N-HWD =',I8)
C
      WRITE(LOGUP,40)(DIRF(I),I=7,12)
   40 FORMAT(1H ,'DATIM =',6I8)
C
      WRITE(LOGUP,50)(DIRF(I),I=13,32)
   50 FORMAT(1H ,'TITLE =',20A4)
C
      NHIS=DIRF(4)
C
      DO 200 N=1,NHIS
      IREC=IREC+1
      READ(LIN,REC=IREC,IOSTAT=IOS)DIRF
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      WRITE(LOGUP,60)DIRH(1)
   60 FORMAT(1H ,'H-DIM =',I8)
C
      WRITE(LOGUP,70)DIRH(2)
   70 FORMAT(1H ,'N-HPC =',I8)
C
      WRITE(LOGUP,80)(DIRH(I),I=3,6)
   80 FORMAT(1H ,'H-PAR =',4I8)
C
      WRITE(LOGUP,90)(DIRH(I),I=7,10)
   90 FORMAT(1H ,'L-PAR =',4I8)
C
      WRITE(LOGUP,100)(DIRH(I),I=11,14)
  100 FORMAT(1H ,'L-HIS =',4I8)
C
      WRITE(LOGUP,110)(DIRH(I),I=15,18)
  110 FORMAT(1H ,'MINC  =',4I8)
C
      WRITE(LOGUP,120)(DIRH(I),I=19,22)
  120 FORMAT(1H ,'MAXC  =',4I8)
C
      WRITE(LOGUP,130)DIRF(12)
  130 FORMAT(1H ,'DOFF  =',I16)
C
      WRITE(LOGUP,140)(DIRF(I),I=13,15)
  140 FORMAT(1H ,'XLABL =',3A4)
C
      WRITE(LOGUP,150)(DIRF(I),I=16,18)
  150 FORMAT(1H ,'YLABL =',3A4)
C
      WRITE(LOGUP,160)(DIRF(I),I=19,22)
  160 FORMAT(1H ,'CALCO =',4Z12)
C
      WRITE(LOGUP,170)(DIRF(I),I=23,32)
  170 FORMAT(1H ,'TITLE =',10A4)
C
  200 CONTINUE
C
      NDO=(NHIS+31)/32
C
      DO 250 N=1,NDO
C
      IREC=IREC+1
      READ(LIN,REC=IREC,IOSTAT=IOS)DIRF
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   RETURN
                   ENDIF
C
      WRITE(LOGUP,210)DIRF
  210 FORMAT(1H ,'IDLST =',8I8)
  250 CONTINUE
C
      RETURN
      END
