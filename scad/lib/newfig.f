C$PROG NEWFIG    - Special NEWFIG for program SCUDD, SCOP, etc
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
C     ******************************************************************
C
C     DELETES ALL EXISTING WINDOWS AND CREATES A NEW WINDOW
C     CONFIGURATION
C     
C     IFIG    -  SPECIFIES WINDOW CONFIGURATION NUMBER
C
C     FIGDAT(1,J) = X-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
C     FIGDAT(2,J) = Y-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
C     FIGDAT(3,J) = WIDTH(PIX)               OF JTH-WINDOW
C     FIGDAT(4,J) = HEIGHT(PIX)              OF JTH-WINDOW
C
C     FIGLIM(1,K) - POINTS TO FIRST FIGDAT ENTRY FOR KTH  CONFIG
C     FIGLIM(2,K) - POINTS TO LAST  FIGDAT ENTRY FOR KTH  CONFIG
C     NFIG        = NUMBER OF WINDOW CONFIGURATIONS IN FIGDAT
C
C     FIGDATS     - CONTAINS STANDARD (DEFAULT) FIGDAT-DATA
C     FIGLIMS     - CONTAINS STANDARD (DEFAULT) FIGLIM-DATA
C
C     TMPDAT      - CONTAINS TEMP FIGDAT-DATA (WHILE READING FILE)
C     TMPIDN      - CONTAINS TEMP WINDOW-IDS  (WHILE READING FILE)
C     ------------------------------------------------------------------
C
      SUBROUTINE NEWFIG(LU,IWD,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8 
C
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
C
      COMMON/XLDD/ FIGDAT(4,20)
C
      INTEGER*4 NAMF(20),IWD(20),JWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 IV(5)
C
      CHARACTER*4 IUSE(100)
C
      INTEGER*4 FIGLIMS(2,8),FIGLIM(2,50),TMPIDN(100)
C
      INTEGER*4 FIGDATL(144)
C
      INTEGER*4 FIGDATS(4,36),FIGDATA(4,100),TMPDAT(4,100)
C
      REAL*4 XV,WINDAT
C
      CHARACTER*80 CNAMF
C
      CHARACTER*4 KMD
C
      EQUIVALENCE (FIGDATS,FIGDATL)
      EQUIVALENCE (KMD,LWD)
      EQUIVALENCE (CNAMF,NAMF)
C
      DATA FIGDATL/
     &         0,         0,       245,       245,   !- 1 -  1
     &         0,         0,       245,       245,   !- 2 -  2
     &       250,         0,       245,       245,   !- 2 -  3
     &         0,         0,       245,       245,   !- 3 -  4
     &       250,         0,       245,       245,   !- 3 -  5
     &       500,         0,       245,       245,   !- 3 -  6
     &         0,         0,       245,       245,   !- 4 -  7
     &       250,         0,       245,       245,   !- 4 -  8
     &       500,         0,       245,       245,   !- 4 -  9
     &       750,         0,       246,       245,   !- 4 - 10
     &         0,         0,       245,       245,   !- 5 - 11
     &       250,         0,       245,       245,   !- 5 - 12
     &       500,         0,       245,       245,   !- 5 - 13
     &       750,         0,       246,       245,   !- 5 - 14
     &         0,       250,       245,       245,   !- 5 - 15 
     &         0,         0,       245,       245,   !- 6 - 16
     &       250,         0,       245,       245,   !- 6 - 17
     &       500,         0,       245,       245,   !- 6 - 18
     &       750,         0,       246,       245,   !- 6 - 19
     &         0,       250,       245,       245,   !- 6 - 20
     &       250,       250,       245,       245,   !- 6 - 21
     &         0,         0,       245,       245,   !- 7 - 22
     &       250,         0,       245,       245,   !- 7 - 23
     &       500,         0,       245,       245,   !- 7 - 24
     &       750,         0,       246,       245,   !- 7 - 25
     &         0,       250,       245,       245,   !- 7 - 26
     &       250,       250,       245,       245,   !- 7 - 27
     &       500,       250,       245,       245,   !- 7 - 28
     &         0,         0,       245,       245,   !- 8 - 29
     &       250,         0,       245,       245,   !- 8 - 30
     &       500,         0,       245,       245,   !- 8 - 31
     &       750,         0,       246,       245,   !- 8 - 32 
     &         0,       250,       245,       245,   !- 8 - 33
     &       250,       250,       245,       245,   !- 8 - 34
     &       500,       250,       245,       245,   !- 8 - 35
     &       750,       250,       245,       245/   !- 8 - 36
C
C     ------------------------------------------------------------------
C
      DATA FIGLIMS/01,01, 02,03, 04,06, 07,10, 11,15, 16,21,
     &             22,28, 29,36/
C
      DATA NCALL/0/
      DATA NDATS,NDAT,NTAB/36,0,0/
      DATA NFIGS,NFIG,NLIM/8,0,0/
      DATA MAXFIG,MAXFID,MAXLIN,MAXWIN/8,50,100,20/
C
      INTEGER*4  IFIGI,STARS
C
      character*4 cIFIGI, cSTARS
      equivalence (cIFIGI, IFIGI), (cSTARS, STARS)
      DATA       cIFIGI,cSTARS/'FIGI','****'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0                                   !RESET ERROR FLAG
C
      IF(IWD(1).EQ.IFIGI) GO TO 10
      IF(NCALL.GT.0)      GO TO 50
C
C     ************************************************************
C     COPY STANDARD FIG-DATA TO CURRENT FIG-DATA IF NCALL=0 
C     ************************************************************
C
   10 DO 20 J=1,NDATS
      DO 15 I=1,4
      FIGDATA(I,J)=FIGDATS(I,J)
   15 CONTINUE
   20 CONTINUE
      DO 30 J=1,NFIGS
      FIGLIM(1,J)=FIGLIMS(1,J)
      FIGLIM(2,J)=FIGLIMS(2,J)
   30 CONTINUE
      MAXFIG=NFIGS
      NCALL=1
      IF(IWD(1).EQ.IFIGI) RETURN
C
   50 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'FIGF') GO TO 100
      IF(KMD.EQ.'FIG ') GO TO 400
                        GO TO 510
C
C     ************************************************************
C     OPEN SCREEN-FIG FILE AND RE-DEFINE SCREEN-FIG ID'S 
C     ************************************************************
C
  100 CLOSE(UNIT=LU)
C
      CALL FINAME(IWD,5,80,NAMF,IERR)
      IF(IERR.NE.0) GO TO 520
C
      OPEN(UNIT    = LU,
     &     FILE    = CNAMF,
     &     STATUS  = 'OLD',
     &     ACCESS  = 'SEQUENTIAL',
     &     IOSTAT  = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL IOFERR(ISTAT)
                     IERR=1
                     CLOSE(UNIT=LU)
                     RETURN
                     ENDIF
C
      REWIND LU
C
      NL=0
  110 READ(LU,115,END=150,ERR=530)JWD
  115 FORMAT(20A4)
      IF(JWD(1).EQ.STARS) GO TO 110
      CALL GREAD(JWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 540
      IF(NF.NE.5)   GO TO 540
C
      DO 120 I=1,5
      CALL MILV(LWD(1,I),IV(I),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 540
  120 CONTINUE
C
      NL=NL+1
      IF(NL.GT.MAXLIN)    GO TO 550
      IF(IV(1).LE.0)      GO TO 560
      IF(IV(1).GT.MAXFID) GO TO 560
      DO 130 I=2,5
      IF(IV(I).LT.0)      GO TO 570
      IF(IV(I).GT.1024)   GO TO 570
  130 CONTINUE
      TMPIDN(NL)  =IV(1)
      TMPDAT(1,NL)=IV(2)
      TMPDAT(2,NL)=IV(3)
      TMPDAT(3,NL)=IV(4)
      TMPDAT(4,NL)=IV(5)
      GO TO 110
C
  150 NDO=NL
      DO 160 I=1,NDO
      IUSE(I)='NO  '
  160 CONTINUE
      DO 165 I=1,50
      FIGLIM(1,I)=0
      FIGLIM(2,I)=0
  165 CONTINUE
      NN=0
      DO 200 KID=1,MAXFID
      DO 180 J=1,NDO
      IF(IUSE(J).EQ.'YES ') GO TO 180
      IF(TMPIDN(J).NE.KID) GO TO 180
      IUSE(J)='YES '
      MAXFIG=KID
      NN=NN+1
      FIGLIM(2,KID)=NN
      IF(FIGLIM(1,KID).EQ.0) FIGLIM(1,KID)=NN
      DO 170 I=1,4
      FIGDATA(I,NN)=TMPDAT(I,NN)
  170 CONTINUE
  180 CONTINUE
  200 CONTINUE
      GO TO 710
C
C     ************************************************************
C     PROCESS A - FIG ID# - REQUEST 
C     ************************************************************
C
  400 IF(NTER.NE.0) GO TO 500
      CALL MILV(LWD(1,2),IFIG,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
      IF(IFIG.LE.0.OR.IFIG.GT.MAXFIG)GO TO 500 !TST FOR LEGAL FIG
      IF(FIGLIM(1,IFIG).LE.0)        GO TO 500
C
      ILO=FIGLIM(1,IFIG)                       !LO FIGDATA POINTER
      IHI=FIGLIM(2,IFIG)                       !HI FIGDATA POINTER
      IF((IHI-ILO+1).GT.MAXWIN) GO TO 580      !TST TOO MANY WINS
C
      DO 410 I=1,20                            !LOOP TO DELETE ALL
      IF(WINFLG(1,I).EQ.0) GO TO 410           !EXISTING WINDOWS
      WINFLG(1,I)=0
  410 CONTINUE
C
      ID=0                                     !RESET WINDOW ID#
C
      DO 440 I=ILO,IHI                         !LOOP ON FIGDATA INDEXES
      ID=ID+1                                  !INC WINDOW ID#
C
      DO 420 J=1,4                             !LOOP ON FIGDATA DATA
      FIGDAT(J,ID)=FIGDATA(J,I)                !SAVE IN FIGDAT
  420 CONTINUE
C
  440 CONTINUE
      NUMWIN=ID
      RETURN
C
C     ************************************************************
C     SEND ERROR MESSAGES FROM - FIG  REQUEST 
C     ************************************************************
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR OR ILLEGAL FIG ID - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C
C     ************************************************************
C     SEND ERROR MESSAGES FROM - FIGF REQUEST
C     ************************************************************
C
  510 WRITE(CMSSG,515)
  515 FORMAT('ILLEGAL NEWFIG CALL - CMD IGNORED')
      GO TO 700
C
  520 WRITE(CMSSG,525)
  525 FORMAT('SYNTAX ERROR IN FILENAME SPECIFICATION - CMD IGNORED')
      GO TO 700
C
  530 WRITE(CMSSG,535)NL+1
  535 FORMAT('ERR READING SCREEN-FIG-FILE - LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  540 WRITE(CMSSG,545)NL+1
  545 FORMAT('SYNTAX ERR IN SCREEN-FIG-DATA - LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  550 WRITE(CMSSG,555)MAXLIN
  555 FORMAT('NO. SCREEN-FIG DATA LINES .GT.',I2,' - CMD IGNORED')
      GO TO 700
C
  560 WRITE(CMSSG,565)NL,MAXFID
  565 FORMAT('ILLEGAL FIG-ID (.GT.',I2,') ON LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  570 WRITE(CMSSG,575)NL
  575 FORMAT('ILLEGAL SCREEN COOR ON LINE#',I3,' CMD IGNORED')
      GO TO 700
C
  580 WRITE(CMSSG,585)MAXWIN
  585 FORMAT('NO. WINDOWS REQUESTED .GT.',I2,' - CMD IGNORED')
C
  700 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
  710 CLOSE(UNIT=LU)
      RETURN
      END
