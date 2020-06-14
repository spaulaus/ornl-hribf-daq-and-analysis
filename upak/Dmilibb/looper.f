C$PROG LOOPER    - Loop manager
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE LOOPER(LIN,LCI)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      CHARACTER*4  CCMSSG(28)
      EQUIVALENCE (CCMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C   
      INTEGER*4 LINE(20,100),LINTY(100),LINGO(100),KOUNT(100),NOL
C
      CHARACTER*4  CLINTY(100)
C
      EQUIVALENCE (CLINTY,LINTY)
C
      CHARACTER*4  KMD,KMI,ICMT,IDONE
C   
      EQUIVALENCE (KMD,LWD(1,1)),(KMI,IWD(1))
C
      INTEGER*4    X2A
C
      DATA         X2A/Z'2A'/
C
      SAVE
C   
C     ------------------------------------------------------------------
C   
C     ROUTINE TO READ AND PROCESS LOOPS (NESTING SUPPORTED)
C   
C     ------------------------------------------------------------------
C     LINTY - GIVES LINE-TYPE (LOOP, ENDLOOP OR COMMAND)
C     LINGO - GIVES LINE-NUMB OF ASSOCIATED ENDL FOR LINTY=LOOP
C     LINGO - GIVES LINE-NUMB OF ASSOCIATED LOOP FOR LINTY=ENDL
C     KOUNT - GIVES LOOP-COUNT FOR                   LINTY=LOOP
C     KOUNT - GIVES NUMBER ALREADY DONE FOR          LINTY=ENDL
C     LINE  - CONTAINS ALL LINES READ FROM LOOP TO MATCHING ENDL
C   
C     LINTY(I) = '    '   'LOOP'                 'ENDL'
C     LINGO(I) = 0        L# OF ASSO ENDL        L# OF ASSO LOOP
C     KOUNT(I) = 0        #-TO-DO                #-DONE
C     ------------------------------------------------------------------
C     RESET ASSOCIATED ARRAYS AND POINTERS
C     ------------------------------------------------------------------
C   
      DO 10 I=1,100
      CLINTY(I)='    '
      LINGO(I)=0
      KOUNT(I)=0
   10 CONTINUE
C   
      LTERM=0
      IF(LIN.NE.LCI) LTERM=LOGUT
C   
      IERR=0
      NEND=0
      NLOO=1
      NOL=1
C   
C     ------------------------------------------------------------------
C     PICK UP FIRST LINE (READ BY MAIN PROG) AND LOOP-COUNT
C     ------------------------------------------------------------------
C   
      DO 15 I=1,20
      LINE(I,1)=IWD(I)
   15 CONTINUE
C   
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      KOUNT(1)=ISVAL(LWD(1,2),ITYP(2),JERR)
      IF(JERR.NE.0) IERR=1
C   
      CLINTY(1)='LOOP'
C   
C     ------------------------------------------------------------------
C     READ INPUT LINES AND STORE UNTIL MATCHING ENDL IS ENCOUNTERED
C     ------------------------------------------------------------------
C   
   20 NOL=NOL+1
C   
      IF(NOL.GT.100)       THEN
                           IERR=1
                           WRITE(CMSSG,22)
                           CALL MESSLOG(LOGUT,LOGUP)
                           NOL=100
                           ENDIF
C   
   22 FORMAT('MAX NO. OF LOOP LINES = 100- EXCEEDED')
C
      IF(LIN.EQ.LCI) WRITE(6,24)
   24 FORMAT(' -',$)
C   
      READ(LIN,25,END=520)IWD
   25 FORMAT(20A4)
C   
      WRITE(CMSSG,25)IWD
      CCMSSG(21)='----'
      CCMSSG(22)='LOOP'
      CCMSSG(23)='-SOU'
      CCMSSG(24)='RCE '
      CALL MESSLOG(LTERM,LOGUP)
C
      DO 26 I=1,20
      IWDRAW(I)=IWD(I)
   26 CONTINUE
C
      CALL CASEUP1(IWDRAW)
C
      CALL CASEUP(IWD)
C   
      IF(KMI.EQ.'CMDF') KMI='CMD '
      IF(KMI.EQ.'CMD ') THEN
                           IERR=1
                           WRITE(CMSSG,28)
                           CALL MESSLOG(LOGUT,LOGUP)
                           GO TO 20
                           ENDIF
C   
   28 FORMAT('OPENING CMD-FILE NOT ALLOWED WITHIN LOOP')
C   
      IF(KMI.EQ.'KILL') RETURN
C   
      DO 30 I=1,20
      LINE(I,NOL)=IWDRAW(I)
   30 CONTINUE
C   
      IF(KMI.EQ.'LOOP') GO TO 40
      IF(KMI.EQ.'ENDL') GO TO 50
      GO TO 20
C   
   40 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      KOUNT(NOL)=ISVAL(LWD(1,2),ITYP(2),JERR)
      IF(JERR.NE.0) IERR=1
C   
      CLINTY(NOL)='LOOP'
      NLOO=NLOO+1
      GO TO 20
C   
   50 CLINTY(NOL)=KMI
      KOUNT(NOL)=0
      NEND=NEND+1
      IF(NEND.NE.NLOO) GO TO 20
      IF(IERR.NE.0)    GO TO 500
C   
C     ------------------------------------------------------------------
C     FINISH CONTRUCTION OF ASSOCIATED POINTER LIST
C     ------------------------------------------------------------------
C   
   60 NNU=0
C   
      DO 80 N=1,NOL
      IF(CLINTY(N).NE.'LOOP') GO TO 80
      IF(LINGO(N).NE.0     ) GO TO 80
C   
      IS=N+1
      DO 70 I=IS,NOL
      IF(CLINTY(I).EQ.'    ') GO TO 70
      IF(LINGO(I).NE.0     )  GO TO 70
      IF(CLINTY(I).EQ.'LOOP') GO TO 80
      LINGO(I)=N
      LINGO(N)=I
      NNU=NNU+1
      GO TO 80
   70 CONTINUE
   80 CONTINUE
      IF(NNU.NE.0) GO TO 60
C   
      WRITE(CMSSG,90)
   90 FORMAT('************ - LISTING OF EXPANDED LOOP FOLLOWS')
      CALL MESSLOG(LOGUT,LOGUP)
C   
C     ------------------------------------------------------------------
C     PROCESS LOOP-LIST AS DIRECTED BY ASSOCIATED LISTS
C     ------------------------------------------------------------------
C   
      N=0
C   
  100 N=N+1
      IF(N.GT.NOL) GO TO 530
      IF(CLINTY(N).EQ.'ENDL') GO TO 110
      IF(CLINTY(N).EQ.'LOOP') GO TO 120
      GO TO 200
C   
  110 KOUNT(N)=KOUNT(N)+1
      LDX=LINGO(N)
      IF(KOUNT(N).GE.KOUNT(LDX)) GO TO 100
      N=LDX
      GO TO 100
C   
  120 LDX=LINGO(N)
      NUM=KOUNT(N)
      KOUNT(LDX)=0
      IF(NUM.GT.0) GO TO 100
      N=LDX
      GO TO 100
C   
  200 IF(MSGF.NE.'    ') GO TO 510
C   
      DO 220 I=1,20
      IWD(I)=LINE(I,N)
      IWDRAW(I)=IWD(I)
  220 CONTINUE
C
      CALL CASEUP(IWD)
C   
      IF(KMI.EQ.'WO  ')    THEN
                           WRITE(LOGUT,222)
  222                      FORMAT(1H ,'Type [RETURN] to continue',$)
                           READ(LCI,224,END=100,ERR=100)IXX
  224                      FORMAT(A4)
                           GO TO 100
                           ENDIF
C
      WRITE(CMSSG,230)IWDRAW
  230 FORMAT(20A4)
C   
      ICMT='NO  '
      LPR=LOGUP
      CALL ILBYTE(ITW,IWD,0)
      IF(KMI.EQ.'    ')    THEN
                           LPR=0
                           ICMT='YES '
                           ENDIF
      IF(KMI.EQ.'COM ')    ICMT='YES '
      IF(ITW.EQ.X2A)       ICMT='YES '
C   
      CALL MESSLOG(LOGUT,LPR)
      IF(ICMT.EQ.'YES ')   GO TO 100
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
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,240)
  240 FORMAT('COMMAND NOT RECOGNIZED - LOOP EXECUTION ABORTED')
      GO TO 600
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('LOOP NOT EXECUTED')
      GO TO 600
C
  510 WRITE(CMSSG,515)
  515 FORMAT('LOOP EXECUTION ABORTED VIA INTERRUPT')
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('END-OF-FILE ENCOUNTERED LOOKING FOR ENDL')
      GO TO 600
C
  530 WRITE(CMSSG,535)
  535 FORMAT('************ - END OF EXPANDED LOOP LISTING')
      GO TO 600
C
  540 WRITE(CMSSG,545)
  545 FORMAT('LOOP EXECUTION ABORTED')
C   
  600 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
