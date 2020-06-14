C$PROG ASAPRED
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ASAPIN1/ MDYHMS(5),NAMFIL(20)
C
      COMMON/ASAPIN2/ ITIT(20)
C
      COMMON/ASAPIN3/ ID,ILO,IHI,KWD,BIAS,ECAL,EO,FW,FWB,WDNC,DX,
     &                VW,BSTD,MAXPPS
C
      COMMON/ASAPIN4/ SAREA(500),SERR(500),GAREA(500),GERR(500),
     &                AFWHM(500),XG(500),CENT(500),EGAM(500),
     &                RAT(500),IFLAG(500),NPK
C
      INTEGER*4 IWD(20)
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,IWD(1))
C
      DATA  NAMPROG/'ASAP','RED '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CMSSG=' '
      MSGF='    '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'asapred.log',
     &     STATUS     = 'UNKNOWN')
C
      CLOSE(UNIT=LOGUP,DISP='DELETE')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'asapred.log',
     &     STATUS     = 'NEW')
C
      LU=1
   10 WRITE(6,15)
   15 FORMAT(1H ,'ENTER FILENAME->',$)
      READ(5,20)IWD
   20 FORMAT(20A4)
C
      IF(KMD.EQ.'end ') STOP
      IF(KMD.EQ.'END ') STOP
C
      CALL LOGOPEN(LU,IWD,IERR)
      IF(IERR.NE.0) GO TO 10
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'ENTER COMMAND-->',$)
      READ(5,20)IWD
      CALL CASEUP(IWD)
C
      IF(KMD.EQ.'FILE') GO TO 10
      IF(KMD.EQ.'REW ') GO TO 200
      IF(KMD.EQ.'END ') STOP
      GO TO 300
C
  200 REWIND LU
      GO TO 100
C
  300 CALL ASAPIN(LU,IERR)
      IF(IERR.NE.0) GO TO 100
C
      WRITE(6,302)ID
  302 FORMAT(1H ,'ID  =',I8)
C
      WRITE(6,305)MDYHMS,NAMFIL
  305 FORMAT(1H ,5A4,4X,20A4)
C
      WRITE(6,310)ITIT
  310 FORMAT(1H ,20A4)
C
      WRITE(6,315)ILO,IHI,KWD
  315 FORMAT(1H ,'ILO, IHI, KWD   =',3I12)
C
      WRITE(6,320)BIAS,ECAL,EO
  320 FORMAT(1H ,'BIAS,ECAL,EO    =',3F12.3)
C
      WRITE(6,325)FW,FWB,WDNC
  325 FORMAT(1H ,'FW,  FWB, WDNC  =',3F12.3)
C
      WRITE(6,330)DX,VW,BSTD
  330 FORMAT(1H ,'DX    VW, BSTD  =',F12.3,8X,A4,F12.3)
C
      WRITE(6,335)MAXPPS,NPK
  335 FORMAT(1H ,'MAXPPS,    NPK  =',2I12)
C
      DO 350 I=1,NPK
      WRITE(6,340)SAREA(I),SERR(I),GAREA(I),GERR(I),AFWHM(I),XG(I),
     &            CENT(I),EGAM(I),RAT(I),IFLAG(I)
  340 FORMAT(1H ,4F10.1,5F10.2,2X,A4)
  350 CONTINUE
      GO TO 100
      END
C$PROG LOGOPEN
      SUBROUTINE LOGOPEN(LU,NAMF,IERR)    
C
      INTEGER*4 NAMF(20),NAMFIL(20)
C
      CHARACTER*80 CNAMFIL
C
      EQUIVALENCE (CNAMFIL,NAMFIL)
C
      SAVE
C
      IERR=0
C
      DO 10 I=1,20
      NAMFIL(I)=NAMF(I)
   10 CONTINUE
C
      CLOSE(UNIT=LU)
      IA=LSNB(NAMFIL,1,80)
      CALL ISBYTE(0,NAMFIL,IA)
C
      OPEN(UNIT     = LU,
     &     FILE     = CNAMFIL,
     &     STATUS   = 'OLD',
     &     ACCESS   = 'SEQUENTIAL',
     &     FORM     = 'FORMATTED',
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL OPENERR(ISTAT)
                     IERR=ISTAT
                     ENDIF
      RETURN
      END
C$PROG ASAPIN
C
      SUBROUTINE ASAPIN(LU,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ASAPIN1/ MDYHMS(5),NAMFIL(20)
C
      COMMON/ASAPIN2/ ITIT(20)
C
      COMMON/ASAPIN3/ ID,ILO,IHI,KWD,BIAS,ECAL,EO,FW,FWB,WDNC,DX,
     &                VW,BSTD,MAXPPS
C
      COMMON/ASAPIN4/ SAREA(500),SERR(500),GAREA(500),GERR(500),
     &                AFWHM(500),XG(500),CENT(500),EGAM(500),
     &                RAT(500),IFLAG(500),NPK
C
      INTEGER*4     LINE(30)
      CHARACTER*120 CLINE
      EQUIVALENCE  (CLINE,LINE)
C
      DIMENSION FITRES(9)
C
      CHARACTER*4  FLAG
C
      EQUIVALENCE (FLAG,LINE(30))
C
      SAVE
C
      IERR=0
C
      NPK=0
C
   50 READ(LU,110,END=2000,ERR=2100)LINE
      IF(FLAG.EQ.'FIL$') GO TO 200
      GO TO 50
C
  100 READ(LU,110,END=2000,ERR=2100) LINE
  110 FORMAT(30A4)
C
      IF(FLAG.EQ.'TIT$') GO TO 300
      IF(FLAG.EQ.'DAT$') GO TO 400
      IF(FLAG.EQ.'SAP$') GO TO 500
      IF(FLAG.EQ.'QIT$') RETURN
                         GO TO 100
C
  200 READ(CLINE,205)MDYHMS,NAMFIL
  205 FORMAT(5X,5A4,4X,20A4)
      GO TO 100
C
  300 READ(CLINE,305)ITIT
  305 FORMAT(5X,20A4)
      GO TO 100
C
  400 READ(CLINE,405)ID,ILO,IHI,KWD,BIAS,ECAL,EO,FW,FWB,WDNC,DX,
     &                    VW,BSTD,MAXPPS
  405 FORMAT(1X,4I6,7F8.0,4X,A4,F8.0,I8)
      GO TO 100
C
  500 NPK=NPK+1
      READ(CLINE,505)SAREA(NPK),SERR(NPK),GAREA(NPK),GERR(NPK),
     &               AFWHM(NPK),XG(NPK),CENT(NPK),EGAM(NPK),
     &               RAT(NPK),IFLAG(NPK)
  505 FORMAT(1X,9F10.0,1X,A4)
      GO TO 100
C
 2000 WRITE(CMSSG,2005)
 2005 FORMAT('END-OF-FILE ENCOUNTERED READING INPUT')
      GO TO 3000
C
 2100 WRITE(CMSSG,2105)
 2105 FORMAT('ERROR READING INPUT FILE')
      GO TO 3000
C
 3000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG OPENERR
      SUBROUTINE OPENERR(IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
      WRITE(CMSSG,10)IOS
   10 FORMAT('ERROR OPENING FILE - ZSTAT =',Z10)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
