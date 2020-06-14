C$PROG NEWFIG    - Processes FIG-files & deletes/creates window configs
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
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
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !32bit STAR4
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLDD/ FIGDAT(4,20)
      INTEGER*4    FIGDAT
C     ------------------------------------------------------------------
C
      INTEGER*4 NAMF(20),IWD(20),JWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 IV(5)
C
      INTEGER*4 FIGLIMS(2,20),FIGLIM(2,100),TMPIDN(500)
C
      INTEGER*4 FIGDATL(392)
C
      INTEGER*4 FIGDATS(4,98),FIGDATA(4,500),TMPDAT(4,500)
C
      INTEGER*4 FIGI
      CHARACTER*4 cFIGI
      EQUIVALENCE (FIGI,CFIGI)
      DATA      cFIGI/'FIGI'/
C
      REAL*4    XV
C
      CHARACTER*80 CNAMF
C
      CHARACTER*4  KMD,CJWD(20),IUSE(100)
C
      EQUIVALENCE (FIGDATS,FIGDATL)
      EQUIVALENCE (KMD,LWD),(CJWD,JWD)
      EQUIVALENCE (CNAMF,NAMF)
C
      INTEGER*4    I
C
      DATA (FIGDATL(I),I=  1, 40)/
     &     505,       0,     500,     500,    !- 1
     &       0,       0,     500,     500,    !- 2
     &     505,       0,     500,     500,    !- 2
     &       0,     320,     570,     295,    !- 3
     &       0,       0,     570,     295,    !- 3
     &     576,       0,     440,     440,    !- 3
     &       0,     320,     500,     295,    !- 4
     &       0,       0,     500,     295,    !- 4
     &     505,     320,     500,     295,    !- 4
     &     505,       0,     500,     295/    !- 4
      DATA (FIGDATL(I),I= 41, 80)/
     &       0,     440,     500,     195,    !- 5
     &       0,     220,     500,     195,    !- 5
     &       0,       0,     500,     195,    !- 5
     &     505,       0,     500,     435,    !- 5
     &       0,     440,     500,     195,    !- 6
     &       0,     220,     500,     195,    !- 6
     &       0,       0,     500,     195,    !- 6
     &     505,     440,     500,     195,    !- 6
     &     505,     220,     500,     195,    !- 6
     &     505,       0,     500,     195/    !- 6
      DATA (FIGDATL(I),I= 81,120)/
     &       0,     480,     500,     135,    !- 7
     &       0,     320,     500,     135,    !- 7
     &       0,     160,     500,     135,    !- 7
     &       0,       0,     500,     135,    !- 7
     &     505,       0,     500,     500,    !- 7
     &       0,     480,     500,     135,    !- 8
     &       0,     320,     500,     135,    !- 8
     &       0,     160,     500,     135,    !- 8
     &       0,       0,     500,     135,    !- 8
     &     505,     480,     500,     135/    !- 8
      DATA (FIGDATL(I),I=121,160)/
     &     505,     320,     500,     135,    !- 8
     &     505,     160,     500,     135,    !- 8
     &     505,       0,     500,     135,    !- 8
     &       0,     512,     500,     104,    !- 9
     &       0,     384,     500,     104,    !- 9
     &       0,     256,     500,     104,    !- 9
     &       0,     128,     500,     104,    !- 9
     &       0,       0,     500,     104,    !- 9
     &     505,       0,     500,     500,    !- 9
     &       0,     512,     500,     104/    !-10
      DATA (FIGDATL(I),I=161,200)/
     &       0,     384,     500,     104,    !-10
     &       0,     256,     500,     104,    !-10
     &       0,     128,     500,     104,    !-10
     &       0,       0,     500,     104,    !-10
     &     505,     512,     500,     104,    !-10
     &     505,     384,     500,     104,    !-10
     &     505,     256,     500,     104,    !-10
     &     505,     128,     500,     104,    !-10
     &     505,       0,     500,     104,    !-10
     &       0,       0,    1010,     500/    !-11
      DATA (FIGDATL(I),I=201,240)/
     &       0,     320,    1010,     295,    !-12
     &       0,       0,    1010,     295,    !-12
     &       0,     424,    1010,     188,    !-13
     &       0,     212,    1010,     188,    !-13
     &       0,       0,    1010,     188,    !-13
     &       0,     480,    1010,     135,    !-14
     &       0,     320,    1010,     135,    !-14
     &       0,     160,    1010,     135,    !-14
     &       0,       0,    1010,     135,    !-14
     &       0,     632,     400,     134/    !-15
      DATA (FIGDATL(I),I=241,280)/
     &       0,     474,     400,     134,    !-15
     &       0,     316,     400,     134,    !-15
     &       0,     158,     400,     134,    !-15
     &       0,       0,     400,     134,    !-15
     &     405,     632,     400,     134,    !-15
     &     405,     474,     400,     134,    !-15
     &     405,     316,     400,     134,    !-15
     &     405,     158,     400,     134,    !-15
     &     405,       0,     400,     134,    !-15
     &     810,     632,     400,     134/    !-15
      DATA (FIGDATL(I),I=281,320)/
     &     810,     474,     400,     134,    !-15
     &     810,     316,     400,     134,    !-15
     &     810,     158,     400,     134,    !-15
     &     810,       0,     400,     134,    !-15
     &     210,       0,     800,     800,    !-16
     &     400,       0,     610,     610,    !-17
     &     170,       0,     840,     840,    !-18
     &     400,       0,     840,     840,    !-19
     &       0,     632,     300,     134,    !-20
     &       0,     474,     300,     134/    !-20
      DATA (FIGDATL(I),I=321,360)/
     &       0,     316,     300,     134,    !-20
     &       0,     158,     300,     134,    !-20
     &       0,       0,     300,     134,    !-20
     &     305,     632,     300,     134,    !-20
     &     305,     474,     300,     134,    !-20
     &     305,     316,     300,     134,    !-20
     &     305,     158,     300,     134,    !-20
     &     305,       0,     300,     134,    !-20
     &     610,     632,     300,     134,    !-20
     &     610,     474,     300,     134/    !-20
      DATA (FIGDATL(I),I=361,392)/
     &     610,     316,     300,     134,    !-20
     &     610,     158,     300,     134,    !-20
     &     610,       0,     300,     134,    !-20
     &     915,     632,     300,     134,    !-20
     &     915,     474,     300,     134,    !-20
     &     915,     316,     300,     134,    !-20
     &     915,     158,     300,     134,    !-20
     &     915,       0,     300,     134/    !-20
C
      DATA FIGLIMS/ 1, 1,   2, 3,   4, 6,   7,10,  11,14,  15,20,  
     &             21,25,  26,33,  34,39,  40,49,  50,50,  51,52,  
     &             53,55,  56,59,  60,74,  75,75,  76,76,  77,77,  
     &             78,78,  79,98/  
C
      INTEGER*4 NCALL,IERR,NF,NTER,LU,ISTAT,NDO,NN,NL,IHI,ILO,J
C
      INTEGER*4 KIND,KID,ID,IFIG
C
      INTEGER*4 NDATS,NDAT,NTAB
      INTEGER*4 NFIGS,NFIG,NLIM
      INTEGER*4 MAXFIG,MAXFID,MAXLIN,MAXWIN
C
      DATA      NCALL/0/
      DATA      NDATS,NDAT,NTAB/98,0,0/
      DATA      NFIGS,NFIG,NLIM/20,0,0/
      DATA      MAXFIG,MAXFID,MAXLIN,MAXWIN/20,50,100,20/
C
      SAVE
C
C     ==================================================================
C
      IERR=0                                   !RESET ERROR FLAG
C
      IF(IWD(1).EQ.FIGI) GO TO 10
      IF(NCALL.GT.0)     GO TO 50
C
C     ------------------------------------------------------------------
C     COPY STANDARD FIG-DATA TO CURRENT FIG-DATA IF NCALL=0 
C     ------------------------------------------------------------------
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
      IF(IWD(1).EQ.FIGI) RETURN
C
   50 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'FIGF') GO TO 100
      IF(KMD.EQ.'FIG ') GO TO 400
                        GO TO 510
C
C     ------------------------------------------------------------------
C     OPEN SCREEN-FIG FILE AND RE-DEFINE SCREEN-FIG ID'S 
C     ------------------------------------------------------------------
C
  100 CLOSE(UNIT=LU)
C
      CALL FINAME(IWD,5,80,NAMF,IERR)
      IF(IERR.NE.0) GO TO 520
C
      OPEN(UNIT    = LU,
     &     FILE    = CNAMF,
     &     STATUS  = 'OLD',
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
      IF(CJWD(1).EQ.'****') GO TO 110
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
C     ------------------------------------------------------------------
C     PROCESS A - FIG ID# - REQUEST 
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES FROM - FIG  REQUEST 
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR OR ILLEGAL FIG ID - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES FROM - FIGF REQUEST
C     ------------------------------------------------------------------
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
