C$PROG FILOPEN   - Opens SPK, HIS & DRR files
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 01/31/2005
C     ******************************************************************
C
      SUBROUTINE FILOPEN(IDEV,LU,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
      character*80 cnamfil(20)
      equivalence (cnamfil, namfil)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,EXT,IX,NU,DUMY
C   
      INTEGER*4 NAM(20),IDEV(2)
      character*80 cnam
      equivalence (cnam, nam)
C   
      EQUIVALENCE (KMD,LWD(1,1))
C   
      DATA EXT/'    '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN HIS- & SPK-FILES
C     ------------------------------------------------------------------
C   
      IERR=0                                 !RESET ERROR FLAG
      LUS=LU
      LUH=LU
      LUX=LU
      LUD=LU+1
      CLOSE(UNIT=LUH)                        !CLOSE DATA-FILE
      CLOSE(UNIT=LUD)                        !CLOSE DIR-FILE
C
      IF(KFIL(LU).EQ.'HIS ') THEN            !TST FOR HIS-FILE OPEN
      CALL HISMAND('CLOS',NAM,LUD,LUC(LU),DUMY,IERR) !IF YES, DEASSIGN
                             ENDIF
C
      KFIL(LU)='    '                        !SET DATA-FILE CLOSED
      KFIL(LU+1)='    '                      !SET DIR- FILE CLOSED
C   
      CALL BILNAM(IWDRAW,IDEV,EXT,NAM,IX,LD,NU,IERR) !GET FNAME, EXT
C   
      IF(IERR.NE.0)    GO TO 500             !TST FOR ERROR
C   
c     Check here to see if the filename is already set.  If the file is
c     already open, some systems get slowly unhappy if it is reopened.
      do i=1,20
         if (i .ne. LU) then
            if(cnamfil(i) .eq. cnam) then
               go to 520
            endif
         endif
      enddo

      DO 10 I=1,20
      NAMFIL(I,LU)=NAM(I)
   10 CONTINUE
C   
      IF(IX.EQ.'.HIS') GO TO 100             !TST FOR HIS-FILE
      IF(IX.EQ.'.his') GO TO 100             !TST FOR his-file
      IF(IX.EQ.'.SPK') GO TO 200             !TST FOR SPK-FILE
      if(IX.EQ.'.spk') GO TO 200             !TST FOR spk-file
                       GO TO 500             !OTHERWISE ERROR
C   
C     ------------------------------------------------------------------
C     OPEN HIS-FILE FOR INPUT
C     ------------------------------------------------------------------
C   
  100 IF(KMD.EQ.'IN  ') GO TO 105
      IF(KMD.EQ.'QF  ') GO TO 110
      IF(KMD.EQ.'RF  ') GO TO 110
      IF(KMD.EQ.'SF  ') GO TO 110
      IF(KMD.EQ.'HOU ') GO TO 120
      IF(KMD.EQ.'OU  ') GO TO 510
                        GO TO 500
C
  105 CALL HISMAND('OPEN',NAM,LUD,LUX,'IN  ',IERR)
      IF(IERR.NE.0) GO TO 600
CX    CALL EXPANHIS('IN  ',LUD,LUX,IERR)
      GO TO 150
C
  110 CALL HISMAND('OPEN',NAM,LUD,LUX,'RO  ',IERR)
      IF(IERR.NE.0) GO TO 600
CX    CALL EXPANHIS('IN  ',LUD,LUX,IERR)
      GO TO 150
C
  120 CALL HISMAND('OPEN',NAM,LUD,LUX,'RW  ',IERR)
      IF(IERR.NE.0) GO TO 600
      CALL EXPANHIS('OUT ',LUD,LUX,IERR)
      GO TO 150
C
  150 IF(IERR.NE.0) GO TO 600
      LUC(LU) =LUX                           !SAVE CHANNEL #
      LUC(LUD)=LUD
      KFIL(LUH)='HIS '
      KFIL(LUD)='DIR '
      RETURN
C   
C     ------------------------------------------------------------------
C     OPEN/CREATE SPK-FILES FOR INPUT/OUTPUT
C     ------------------------------------------------------------------
C   
  200 IF(NU.EQ.'NEW ')  GO TO 230
      IF(NU.EQ.'new ')  GO TO 230
      IF(KMD.EQ.'OU  ') GO TO 220
      IF(KMD.EQ.'IN  ') GO TO 210
      IF(KMD.EQ.'QF  ') GO TO 210
      IF(KMD.EQ.'RF  ') GO TO 210
      IF(KMD.EQ.'SF  ') GO TO 210
                        GO TO 500
C   
  210 CALL SPKMAN('OPEN',NAM,LUS,'RO  ',IERR)
      GO TO 250
C
  220 CALL SPKMAN('OPEN',NAM,LUS,'RW  ',IERR)
      GO TO 250
C
  230 CALL SPKMAN('CREA',NAM,LUS,'RW  ',IERR)
      GO TO 250
C
  250 IF(IERR.NE.0) GO TO 600
      KFIL(LUS)='SPK '                       !SET SPK-FILE OPEN
      LUC(LUD) =LUD
      RETURN
C   
C     ------------------------------------------------------------------
C     ERROR RETURNS
C     ------------------------------------------------------------------
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR OR ILLEGAL REQUEST')
      GO TO 550
C
  510 WRITE(CMSSG,515)
  515 FORMAT('OPENING OF HIS-FILE FOR OUTPUT NOT ALLOWED')
C   
 520  continue
      write(CMSSG,525)
 525  format('This file is already open. Use DFIL to see.')
C   
  550 CALL MESSLOG(LOGUT,LOGUP)
  600 IERR=1
      RETURN
      END
