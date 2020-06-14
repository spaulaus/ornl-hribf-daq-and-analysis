C$PROG LOOPEX
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE LOOPEX(LIN,LCI)
C   
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C   
      INTEGER*4 LINE(20,500),LINTY(500),ISORN(500),NOL
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*4  KMD,IDONE
C   
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4    BLANK
      character*4   cBLANK
      equivalence  (cBLANK, BLANK)
      DATA         cBLANK/'    '/
C
      SAVE
C   
C     ************************************************************
C   
C     ROUTINE TO READ AND PROCESS LOOPS (NESTING NOT SUPPORTED)
C   
C     ************************************************************
C     LINTY - GIVES LINE-TYPE (LOOP, ENDLOOP OR COMMAND)
C     LINE  - CONTAINS ALL LINES READ FROM LOOP TO MATCHING ENDL
C   
C     LINTY(I) = '    '   'LOOP'                 'ENDL'
C     ************************************************************
      DO 10 I=1,100
      LINTY(I)=BLANK
   10 CONTINUE
      IERR=0
      NOL=1
      CALL LABLOOP('INIT',IWD)
C   
C     ************************************************************
C     PICK UP FIRST LINE (READ BY MAIN PROG) AND LOOP-COUNT
C     ************************************************************
C   
      DO 15 I=1,20
      LINE(I,1)=IWD(I)
   15 CONTINUE
      ISORN(1)=ISORL
C   
      CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER)
C   
      NLOO=ISVAL(LWD(1,2),ITYP(2),JERR)
      IF(JERR.NE.0) IERR=1
C   
C     ************************************************************
C     READ INPUT LINES AND STORE UNTIL MATCHING ENDL IS ENCOUNTERED
C     ************************************************************
C   
   20 NOL=NOL+1
C   
      IF(NOL.GT.500)       THEN
                           IERR=1
                           WRITE(CMSSG,22)
                           CALL ERRLOG(LOGUT,LOGUP)
                           STOP 1
                           ENDIF
C   
   22 FORMAT('MAX NO. OF LOOP LINES = 100 - EXCEEDED')
C
      READ(LIN,25,END=520)IWD
   25 FORMAT(20A4)
C
      CALL LISSOR('SOR ',IWD)
C
      CALL CASEUP(IWD)
C
      CALL LABLOOP('SAVE',IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER)
C  
      DO 26 I=1,20
      IWDRAW(I)=IWD(I)
   26 CONTINUE
C
C*    CALL CASEUP1(IWDRAW)
C
C*    CALL CASEUP(IWD)
C   
      DO 30 I=1,20
      LINE(I,NOL)=IWDRAW(I)
   30 CONTINUE
      ISORN(NOL)=ISORL
C   
      IF(KMD.NE.'ENDL') GO TO 20
C
      IF(IERR.NE.0)    GO TO 500
C   
      WRITE(LOGUP,90)
   90 FORMAT(1H ,'************ - LISTING OF EXPANDED LOOP FOLLOWS')
C   
C     ************************************************************
C     PROCESS LOOP-LIST 
C     ************************************************************
C
      ISOSAV=ISORL
C
      DO 300 NL=1,NLOO
C
      CALL LABLOOP('SET ',IWD)
C
      DO 280 N=1,NOL
C
      ISORL=ISORN(N)
C
      IF(N.EQ.1.AND.NL.GT.1) GO TO 280
C   
      DO 220 I=1,20
      IWD(I)=LINE(I,N)
      IWDRAW(I)=IWD(I)
  220 CONTINUE
C
      CALL LISSOR('GEN ',IWD)
C
      CALL LABLOOP('SUB ',IWD)
C
      CALL CASEUP(IWD)
C   
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      IDONE='    '
      IERR=0
C   
      CALL CALLER(IDONE,IERR)
C
      IF(IERR.NE.0) GO TO 540
C
      IF(IDONE.NE.'YES ') GO TO 530
C
  280 CONTINUE
  300 CONTINUE
C
      WRITE(LOGUP,305)
  305 FORMAT(1H ,'************ - END OF EXPANDED LOOP LISTING')
      GO TO 610
C   
  500 WRITE(CMSSG,505)
  505 FORMAT(1H ,'LOOP NOT EXECUTED')
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('END-OF-FILE ENCOUNTERED LOOKING FOR ENDL')
      GO TO 600
C
  530 WRITE(CMSSG,535)
  535 FORMAT('COMMAND NOT RECOGNIZED - LOOP EXECUTION ABORTED')
      GO TO 600
C
  540 WRITE(CMSSG,545)
  545 FORMAT(1H ,'LOOP EXECUTION ABORTED')
C
  600 CALL ERRLOG(LOGUT,LOGUP)   
  610 ISORL=ISOSAV
      RETURN
      END
