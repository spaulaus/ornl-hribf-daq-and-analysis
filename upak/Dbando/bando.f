C$PROG BANDO
C
C     BAN-FILE MANIPULATION PROGRAM
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
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      COMMON/III/ LIN,LCM,LCI
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,KMI,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMI,IWD(1))
C
      CHARACTER*4 CNAMPROG(2)
      EQUIVALENCE (CNAMPROG,NAMPROG)
      DATA  CNAMPROG/'BAND','O   '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     INITIALIZE SOME STUFF
C     ------------------------------------------------------------------
C
      IOPN='NO  '                           !SET INPUT NOT OPEN
      OOPN='NO  '                           !SET OUTPUT NOT OPEN
      LUI=1                                 !LU FOR INPUT BAN-FILE
      LUO=2                                 !LU FOR OUTPUT BAN FILE
      LIN=5                                 !LU FOR CON: CMD INPUT
      LCI=5                                 !LU FOR CON: CMD INPUT
      LCM=4                                 !LU FOR CMD-FILE INPUT
C
C     ------------------------------------------------------------------
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
      MSGF='    '
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'bando.log',
     &     STATUS     = 'REPLACE')
C
      CALL HELPMAN
C
      GO TO 100
C
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 IF(LIN.EQ.5) WRITE(6,105)
  105 FORMAT(' BANDO->',$)
C
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
C
      CALL CASEUP1(IWDRAW)
      CALL CASEUP1(IWD)
C
      IF(KMI.EQ.'    ') GO TO 130
      IF(KMI.EQ.'COM ') GO TO 130
      GO TO 140
C
  130 IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWD
                     CALL MESSLOG(LOGUT,LOGUP)
                     ENDIF
      GO TO 100
C
  140 WRITE(CMSSG,110)IWD
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'END ') STOP
C
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
C
      IDONE='    '
      IERR=0
      CALL CALLER(IDONE,IERR)
C
      IF(IDONE.EQ.'YES ') GO TO 100
C
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG BANDOM
      SUBROUTINE BANDOM(IDONE,IERR)
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
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      COMMON/III/ LIN,LCM,LCI
C     ------------------------------------------------------------------
      INTEGER*4 NAMCMD(20),NAMFD(20),NAMI(5),NAMO(5)
C
      INTEGER*4 JDIR(880)
C
      INTEGER*4 TIT(20),FIL(6),KPAR(9),IX(64),IY(64)
      INTEGER*4 MSG(7),IDL(10),NUPAR(3)
      INTEGER*4 DG
      INTEGER*4 IDSK(2),EXT
C
      INTEGER*4 RECLVALU      
C
      CHARACTER*80 CNAMF
C
      CHARACTER*4  KMD,IDONE,KIND,IONOF
C
      EQUIVALENCE (KMD,LWD(1,1))
      EQUIVALENCE (IDI,IDL(1)),(IDO,IDL(2)),(CNAMF,NAMFD)
C
      DATA NAMI,NAMO/10*Z'20202020'/
      DATA IDSK,EXT/3*Z'20202020'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'HELP') GO TO 400
      IF(KMD.EQ.'LON ') GO TO 40
      IF(KMD.EQ.'LOF ') GO TO 40
      IF(KMD.EQ.'CMD ') GO TO 50
C
      IF(KMD.EQ.'IN  ') GO TO 60
      IF(KMD.EQ.'OU  ') GO TO 60
      IF(KMD.EQ.'NUOU') GO TO 60
C
      IF(NTER.NE.0)     GO TO 450
C
      IF(KMD.EQ.'REN ') GO TO 100
      IF(KMD.EQ.'DE  ') GO TO 200
      IF(KMD.EQ.'ON  ') GO TO 200
      IF(KMD.EQ.'OFF ') GO TO 200
      IF(KMD.EQ.'IO  ') GO TO 220
      IF(KMD.EQ.'COPY') GO TO 240
      IF(KMD.EQ.'NUP ') GO TO 260
      IF(KMD.EQ.'PXY ') GO TO 260
      IF(KMD.EQ.'DDRI') GO TO 300
      IF(KMD.EQ.'DDRO') GO TO 310
      IF(KMD.EQ.'DI  ') GO TO 350
      IF(KMD.EQ.'DO  ') GO TO 360
C
      RETURN
C
   40 LISFLG=KMD
      GO TO 1000
C
   50 CALL CMDOPEN(IWD,NAMCMD,LCI,LIN,LCM,IERR)
      GO TO 1000
C
C     **************************************************************
C     GET FILENAME AND OPEN INPUT AND OUTPUT FILES
C     **************************************************************
C
   60 IA=NXBL(IWD,3,80)
      IF(IA.LE.0) GO TO 450
      IB=LSNB(IWD,IA,80)
C*    CALL FILGIT(IWD,IA,IB,NAMFD,IERR)
C
      CALL BILNAM(IWD,IDSK,EXT,NAMFD,IX,LD,NU,IERR)
C
      IF(IERR.NE.0) GO TO 450
C
      IF(KMD.EQ.'IN  ') GO TO 70
      IF(KMD.EQ.'OU  ') GO TO 80
      IF(KMD.EQ.'NUOU') GO TO 90
      GO TO 450
C
   70 CLOSE(UNIT=LUI)
      DO 75 I=1,5
      NAMI(I)=NAMFD(I)
   75 CONTINUE
      IOPN='NO  '
C   
      OPEN(UNIT   = LUI,               !OPEN OLD BAN-FILE FOR INPUT
     &     FILE   = CNAMF,
     &     STATUS = 'OLD',
     &     ACCESS = 'DIRECT',
     &     RECL   = RECLVALU(80),
     &     IOSTAT = IOS)
C
      CALL OPENERR(IOS)
C
      IF(IOS.EQ.0) IOPN='YES '
      IF(IOS.NE.0) GO TO 1000
      KMD='DDRI'
      GO TO 300
C
   80 CLOSE(UNIT=LUO)
      DO 85 I=1,5
      NAMO(I)=NAMFD(I)
   85 CONTINUE
      OOPN='NO  '
C   
      OPEN(UNIT   = LUO,               !OPEN OLD BAN-FILE FOR OUTPUT
     &     FILE   = CNAMF,
     &     STATUS = 'OLD',
     &     ACCESS = 'DIRECT',
     &     RECL   = RECLVALU(80),
     &     IOSTAT = IOS)
C
      CALL OPENERR(IOS)
C
      IF(IOS.EQ.0) OOPN='YES '
      IF(IOS.NE.0) GO TO 1000
      KMD='DDRO'
      GO TO 310
C
   90 CLOSE(UNIT=LUO)
      DO 95 I=1,5
      NAMO(I)=NAMFD(I)
   95 CONTINUE
      OOPN='NO  '
C   
      OPEN(UNIT   = LUO,               !CREATE NEW BAN-FILE FOR OUTPUT
     &     FILE   = CNAMF,
     &     STATUS = 'NEW',
     &     ACCESS = 'DIRECT',
     &     RECL   = RECLVALU(80),
     &     IOSTAT = IOS)
C
      CALL OPENERR(IOS)
C
      IF(IOS.NE.0) GO TO 1000
      IF(IOS.EQ.0) OOPN='YES '
      CALL BANIO(0,LUO,FIL,TIT,IH,IDI,DG,IX,IY,NP,NID,KPAR,MSG,IERR)
      IF(IERR.NE.0) GO TO 420
      GO TO 1000
C
C     **************************************************************
C     RENAME ID NUMBERS ON THE OUTPUT FILE
C     **************************************************************
C
  100 IF(NF.NE.3) GO TO 450
      IF(OOPN.NE.'YES ') GO TO 530
      DO 110 J=1,2
      CALL LIMIV(LWD(1,J+1),1,9999,IDL(J),IERR)
      IF(IERR.NE.0) GO TO 500
  110 CONTINUE
C
      CALL CHID(IDI,IDO)
      GO TO 1000
C
C     **************************************************************
C     "ON", "OFF" AND DELETE ID NUMBERS ON OUTPUT FILE
C     **************************************************************
  200 IF(NF.LT.2) GO TO 450
      IF(OOPN.NE.'YES ') GO TO 530
      KIND=KMD
      DO 210 J=2,NF
      CALL LIMIV(LWD(1,J),1,9999,ID,IERR)
      IF(IERR.NE.0) GO TO 500
      CALL ONOF(KIND,ID)
  210 CONTINUE
      GO TO 1000
C
C     **************************************************************
C     READ "IDI" FROM INPUT AND STORE AS "IDO" ON OUTPUT
C     **************************************************************
C
  220 IF(NF.NE.3) GO TO 450
      IF(IOPN.NE.'YES ') GO TO 520
      IF(OOPN.NE.'YES ') GO TO 530
C
      DO 225 J=1,2
      CALL LIMIV(LWD(1,J+1),1,99999,IDL(J),IERR)
      IF(IERR.NE.0) GO TO 500
  225 CONTINUE
C
      CALL EXISTR(LUI,IDI,LOC,IONOF)
C
      CALL BANIO(1,LUI,FIL,TIT,IH,IDI,DG,IX,IY,NP,NID,KPAR,MSG,IERR)
C
      IF(IERR.NE.0) GO TO 420
C
      CALL BANIO(2,LUO,FIL,TIT,IH,IDO,DG,IX,IY,NP,NID,KPAR,MSG,IERR)
C
      IF(IERR.NE.0) GO TO 420
      WRITE(CMSSG,230)IDI,NAMI,NAMO,IDO
  230 FORMAT(I5,' from ',5A4,' stored on ',5A4,' as ',I5)
      CALL MESSLOG(LOGUT,LOGUP)
      IF(IONOF.EQ.'ON  ') GO TO 1000
      KIND='OFF '
      CALL ONOF(KIND,IDO)
C
      GO TO 1000
C
C     **************************************************************
C     COPY ID-LST FROM INPUT TO OUTPUT FILES (NO ID CHANGE)
C     **************************************************************
C
  240 IF(IOPN.NE.'YES ') GO TO 520
      IF(OOPN.NE.'YES ') GO TO 530
      IF(NF.LT.2) GO TO 450
      DO 250 J=2,NF
      CALL LIMIV(LWD(1,J),1,99999,ID,IERR)
      IF(IERR.NE.0) GO TO 500
C
      CALL EXISTR(LUI,ID,LOC,IONOF)
      CALL BANIO(1,LUI,FIL,TIT,IH,ID,DG,IX,IY,NP,NID,KPAR,MSG,IERR)
C
      IF(IERR.NE.0) GO TO 420
C
      CALL BANIO(2,LUO,FIL,TIT,IH,ID,DG,IX,IY,NP,NID,KPAR,MSG,IERR)
C
      IF(IERR.NE.0) GO TO 420
      WRITE(CMSSG,230)ID,NAMI,NAMO,ID
      CALL MESSLOG(LOGUT,LOGUP)
      IF(IONOF.EQ.'ON  ') GO TO 250
      KIND='OFF '
      CALL ONOF(KIND,ID)
  250 CONTINUE
      GO TO 1000
C
C     **************************************************************
C     SET NUPM, IAUX AND JAUX FOR ID ON OUTPUT FILE
C     **************************************************************
C
  260 IF(OOPN.NE.'YES ') GO TO 530
      DO 262 I=1,3
      NUPAR(I)=0
  262 CONTINUE
      IF(NF.LT.3) GO TO 450
      IF(NF.GT.5) GO TO 450
      IF(KMD.EQ.'PXY '.AND.NF.NE.4) GO TO 450
      CALL LIMIV(LWD(1,2),1,99999,ID,IERR)
      IF(IERR.NE.0) GO TO 500
      N=0
      ILO=-9999999
      IHI= 9999999
      IF(KMD.EQ.'NUP ') GO TO 266
      ILO=1
      IHI=1000
  266 DO 270 I=3,NF
      N=N+1
      CALL LIMIV(LWD(1,I),ILO,IHI,NUPAR(N),IERR)
      IF(IERR.NE.0) GO TO 500
  270 CONTINUE
      CALL NUPM(KMD,ID,NUPAR)
      GO TO 1000
C
C     **************************************************************
C     DISPLAY INPUT OR OUTPUT FILE DIRECTORIES
C     **************************************************************
C
  300 IF(IOPN.EQ.'YES ') GO TO 315
      GO TO 520
  310 IF(OOPN.EQ.'YES ') GO TO 315
      GO TO 530
C
  315 IF(KMD.EQ.'DDRI') LU=LUI
      IF(KMD.EQ.'DDRO') LU=LUO
C
      CALL BANIO(5,LU,FIL,TIT,IH,ID,DG,JDIR,IY,NP,NID,KPAR,MSG,IERR)
      IF(IERR.NE.0) GO TO 420
C
      IF(KMD.EQ.'DDRI') THEN
                        WRITE(CMSSG,320)NAMI,NID
                        CALL MESSLOG(LOGUT,LOGUP)
                        ENDIF
C
      IF(KMD.EQ.'DDRO') THEN
                        WRITE(CMSSG,325)NAMO,NID
                        CALL MESSLOG(LOGUT,LOGUP)
                        ENDIF
C
  320 FORMAT('Input file ',5A4,' contains ',I3,' ID,s')
  325 FORMAT('Output file ',5A4,' contains ',I3,' ID,s')
C
      IHH=0
  330 ILL=IHH+1
      IF(ILL.GT.NID) GO TO 1000
      IHH=ILL+9
      IF(IHH.GT.NID) IHH=NID
      WRITE(CMSSG,340)(JDIR(I),I=ILL,IHH)
  340 FORMAT(10I6)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 330
C
C     **************************************************************
C     DISPLAY CONTENTS OF BANANA # ID FROM INPUT OR OUTPUT FILE
C     **************************************************************
C
  350 IF(IOPN.NE.'YES ') GO TO 520
      LU=LUI
      GO TO 365
  360 IF(OOPN.NE.'YES ') GO TO 530
      LU=LUO
  365 IF(NF.NE.2) GO TO 450
      CALL LIMIV(LWD(1,2),1,99999,ID,IERR)
      IF(IERR.NE.0) GO TO 500
      CALL BANSHO(LU,ID)
      GO TO 1000
C
C     **************************************************************
C     DISPLAY HELP AND OTHER MESSAGES
C     **************************************************************
C
  400 CALL HELPMAN
      GO TO 1000
C
  420 WRITE(CMSSG,425)MSG
  425 FORMAT(7A4)
      GO TO 600
C
  450 WRITE(CMSSG,455)
  455 FORMAT('SYNTAX ERROR - COMMAND IGNORED')
      GO TO 600
C
  500 WRITE(CMSSG,510)
  510 FORMAT('ILLEGAL VALUE/S - COMMAND IGNORED')
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('INPUT FILE NOT OPEN')
      GO TO 600
C
  530 WRITE(CMSSG,535)
  535 FORMAT('OUTPUT FILE NOT OPEN')
C
  600 CALL MESSLOG(LOGUT,LOGUP)
C
 1000 IDONE='YES '
      RETURN
      END
C$PROG BANSHO
      SUBROUTINE BANSHO(LU,ID)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20),BLANK
C
      CHARACTER*4 CBLANK
      EQUIVALENCE (CBLANK, BLANK)
      DATA      CBLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL DIRIN(LU,IERR)
      IF(IERR.NE.0) RETURN
C
      MI=-ID
      DO 30 I=1,880
      IF(IDIR(I).EQ.ID) GO TO 50
      IF(IDIR(I).EQ.MI) GO TO 50
   30 CONTINUE
      WRITE(CMSSG,40)ID
   40 FORMAT('ID #',I5,'  DOES NOT EXIST')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
   50 CALL LOCBAN(I,NBN,III,III,III)
      DO 80 I=1,12
      CALL DREAD(LU,IWD,NBN+1,80,IOS)
      IF(IOS.NE.0) RETURN
      DO 62 J=2,20
      IF(IWD(J).NE.BLANK) GO TO 64
   62 CONTINUE
      GO TO 75
   64 WRITE(CMSSG,70)(IWD(J),J=1,19)
   70 FORMAT(19A4)
      CALL MESSLOG(LOGUT,LOGUP)
   75 NBN=NBN+1
   80 CONTINUE
      RETURN
      END
C$PROG CALLER
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C
      IF(IDONE.NE.'YES ') CALL LWDMOD
C
      IF(IDONE.NE.'YES ') CALL BANDOM(IDONE,IERR)
C
      RETURN
      END
C$PROG CHID
      SUBROUTINE CHID(IOLD,INEW)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40)
C
      CHARACTER*80 CIWD
      EQUIVALENCE (CIWD,IWD)
C
      INTEGER*4 FIL(6),IHTONP(6),DG
C     ------------------------------------------------------------------
C
      INTEGER*4 BLANK
      CHARACTER*4 CBLANK
      EQUIVALENCE (CBLANK, BLANK)
      DATA      CBLANK/'    '/
C
      CHARACTER*4  LAB
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO RENAME AN ID # FROM "IOLD" TO "INEW" ON OUTPUT FILE
C     ------------------------------------------------------------------
C
      CALL DIRIN(LUO,IERR)
      IF(IERR.NE.0) RETURN
C
      DO 25 I=1,880
      IF(IDIR(I).EQ.INEW)  GO TO 42
      IF(IDIR(I).EQ.-INEW) GO TO 42
   25 CONTINUE
C
      DO 30 I=1,880
      IF(IDIR(I).EQ.IOLD)  GO TO 50
      IF(IDIR(I).EQ.-IOLD) GO TO 50
   30 CONTINUE
C
      WRITE(CMSSG,40)IOLD
   40 FORMAT('ID # ',I5,'  DOES NOT EXIST')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
   42 WRITE(CMSSG,44)INEW
   44 FORMAT('ID #',I5,'  ALREADY EXISTS')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
C     **************************************************************
C     REPLACE ID IN DIRECTORY
C     **************************************************************
C
   50 JOLD=IDIR(I)
      IDIR(I)=INEW
      IF(JOLD.LT.0) IDIR(I)=-IDIR(I)
      IEXIS=I
      CALL DLOUT(LUO,IDIR,IEXIS)
C
C     **************************************************************
C     REPLACE ID IN INP-LINE
C     **************************************************************
C
      CALL LOCBAN(IEXIS,NBN,III,III,III)    !GET 1ST LINE NUMBER
C
C     ************************************** READ IN FIL,IH,IBN,.NP
C
      CALL DREAD(LUO,IWD,NBN+1,80,IOS)
      IF(IOS.NE.0) GO TO 500
C
      IA=NXNB(IWD,4,80)
      IF(IA.LE.0) GO TO 500
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0) GO TO 500
      IF(IB-IA.GT.22) GO TO 500
C
      DO 208 I=1,6
      FIL(I)=BLANK
      IHTONP(I)=0
  208 CONTINUE
C
      CALL LODUP(IWD,IA,IB,FIL,1)
      IA=IB+1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 500
      IF(NF.GT.4) NF=4
C
      DO 210 J=1,NF
      CALL MILV(LWD(1,J),IHTONP(J),XV,KIND,MERR)
      IF(MERR.NE.0) GO TO 500
  210 CONTINUE
C
      IH=IHTONP(1)
      DG=IHTONP(3)
      NP=IHTONP(4)
C
      LAB='INP '
      IF(JOLD.LT.0) LAB='OFF '
      DO 320 I=1,20
      IWD(I)=BLANK
  320 CONTINUE
      WRITE(CIWD,330)LAB,FIL,IH,INEW,DG,NP        !WRITE OUT FILNAM
  330 FORMAT(7A4,2X,4I5)
C
      CALL DRITE(LUO,IWD,NBN+1,80,IOS)
      IF(IOS.NE.0) RETURN
C
      WRITE(CMSSG,335)IOLD,INEW
  335 FORMAT('ID #',I5,'  RE-NAMED TO',I5)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  500 WRITE(CMSSG,510)
  510 FORMAT('ERROR DECODING INP-LINE - ONLY DIRECTORY MODIFIED')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG CMDOPEN
      SUBROUTINE CMDOPEN(IWD,NAMCMD,LCI,LIN,LCM,IERR)
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
      INTEGER*4 IWD(20),NAMCMD(20),NAMFIL(20),DISK(2)
C
      CHARACTER*80 CNAMFIL
C
      EQUIVALENCE (CNAMFIL,NAMFIL)
C
      CHARACTER*4 CJEXT, CDISK(2)
      EQUIVALENCE (CJEXT,JEXT), (CDISK, DISK)
      DATA cJEXT,cDISK/'.cmd',2*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN COMMAND FILES (DEFAULT EXT - .cmd)
C     ------------------------------------------------------------------
C     IWD     - CONTAINS CMD FILENAME<.EXT> - INPUT
C     LCI     = LOGICAL UNIT FOR VDT INPUT  - INPUT
C     LIN     = LOGICAL UNIT FOR INPUT      - INPUT
C     LCM     = LOGICAL UNIT TO BE ASSIGNED - INPUT
C     NAMCMD  - CONTAINS FULL FILENAME      - RETURNED
C     IERR    = ERROR FLAG                  - RETURNED
C
C     IF NO ERROR, SETS LIN=LCM ON RETURN
C     ------------------------------------------------------------------
C
      IERR=0
C
      CLOSE(UNIT=LCM)
C
      CALL BILNAM(IWD,DISK,JEXT,NAMFIL,IEXT,LDOT,INEW,IERR)
C
      IF(IERR.NE.0) GO TO 200
C
      DO 10 I=1,20
      NAMCMD(I)=NAMFIL(I)
   10 CONTINUE
C
      CALL ISBYTE(0,NAMFIL,LDOT+3)
C
      OPEN(UNIT    = LCM,
     &     FILE    = CNAMFIL,
     &     STATUS  = 'OLD',
     &     ACCESS  = 'SEQUENTIAL',
     &     FORM    = 'FORMATTED',
     &     IOSTAT  = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL OPENERR(STAT)
                    GO TO 100
                    ENDIF
C
      LIN=LCM
C
      RETURN
C
  100 WRITE(CMSSG,110)
  110 FORMAT('ERROR TRYING TO OPEN NEW CMD-FILE')
      GO TO 300
C
  200 WRITE(CMSSG,210)
  210 FORMAT('SYNTAX ERROR IN COMMAND FILE SPECIFICATION')
C
  300 CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,310)
  310 FORMAT('CONTROL RETURNED TO VDT')
C
      CALL MESSLOG(LOGUT,LOGUP)
      LIN=LCI
      IERR=1
      RETURN
      END
C$PROG DIOERR
      SUBROUTINE DIOERR(KIND,IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      CHARACTER*4  KIND
      integer*4    eoftst
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
c     IF(IOS.EQ.36.OR.IOS.EQ.-1) RETURN
      IF(eoftst(ios).ne.0) RETURN
C
      WRITE(CMSSG,20)KIND,IOS
   20 FORMAT('ERROR ',A4,'ING - ZSTAT =',Z10)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG DIRIN
      SUBROUTINE DIRIN(LU,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      INTEGER*4   IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4   EOF
      CHARACTER*4 CEOF
      EQUIVALENCE (CEOF, EOF)
      DATA        CEOF/'EOF '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
C     ------------------------------------------------------------------
C     READ IN THE DIRECTORY
C     ------------------------------------------------------------------
C
      DO 152 I=1,880
      IDIR(I)=0
  152 CONTINUE
      NN=0
      NBN=0
  153 DO 160 KK=1,5
C
      CALL DREAD(LU,IWD,NBN+1,80,IOS)
      IF(IOS.EQ.EOF) RETURN
      IF(IOS.NE.0)   GO TO 200
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 200
      IF(NF.NE.16)  GO TO 200
      DO 154 J=1,16
      IF(ITYP(J).NE.2) GO TO 200
      NN=NN+1
      CALL LIMIV(LWD(1,J),-9999,99999,IDIR(NN),IERR)
      IF(IERR.NE.0) GO TO 200
  154 CONTINUE
      NBN=NBN+1
  160 CONTINUE
      NBN=NBN+960
      GO TO 153
C
  200 WRITE(CMSSG,210)
  210 FORMAT('ERROR READING IN DIRECTORY')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
C$PROG DLOUT
      SUBROUTINE DLOUT(LU,IDIR,NDX)
C
C     ------------------------------------------------------------------
      INTEGER*4 IDIR(880),IWD(20)
C
      CHARACTER*80  CIWD
      EQUIVALENCE  (CIWD,IWD)
C
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OUTPUT THE DIRECTORY LINE CONTAINING IDIR(NDX)
C     ------------------------------------------------------------------
C
      CALL LOCBAN(NDX,III,NBN,ILO,IHI)      !GET DIRECTORY REC# &
C                                           !FIRST & LAST ELEMENTS
      WRITE(CIWD,10)(IDIR(I),I=ILO,IHI)     !CONVERT TO ASCII
   10 FORMAT(16I5)
      CALL DRITE(LU,IWD,NBN+1,80,IOS)       !OUTPUT MODIFIED/NEW LINE
      RETURN
      END
C$PROG DREAD
      SUBROUTINE DREAD(LU,IBUF,IREC,NBY,IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(*)
C
      INTEGER*4    EOF
      CHARACTER*4 CEOF
      EQUIVALENCE (CEOF, EOF)
      DATA        CEOF/'EOF '/
C
      SAVE
C
      integer*4 eoftst
C     ------------------------------------------------------------------
C
      NW=NBY/4
C
      READ(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NW)
C
      CALL DIOERR('READ',IOS)
C
c     IF(IOS.EQ.36.OR.IOS.EQ.-1) IOS=EOF
      IF (eoftst(ios) .ne. 0) IOS=EOF
C
      RETURN
      END
C$PROG DRITE
      SUBROUTINE DRITE(LU,IBUF,IREC,NBY,IOS)
C
      INTEGER*4 IBUF(*)
C
      NW=NBY/4
C
      WRITE(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NW)
C
      CALL DIOERR('WRIT',IOS)
C
      RETURN
      END
C$PROG EXISTR
      SUBROUTINE EXISTR(LU,ID,LOC,IONOF)
C
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      CHARACTER*4 IONOF
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL DIRIN(LU,IERR)
      IF(IERR.NE.0) RETURN
C
      LOC=0
      IONOF='    '
C
      DO 20 I=1,880
      IF(IDIR(I).EQ.ID)  GO TO 30
      IF(IDIR(I).EQ.-ID) GO TO 40
   20 CONTINUE
      RETURN
C
   30 LOC=I
      IONOF='ON  '
      RETURN
C
   40 LOC=I
      IONOF='OFF '
      RETURN
      END
C$PROG FILGIT
      SUBROUTINE FILGIT(IWD,IA,IB,NAMFD,IERR)
C
      INTEGER*4 IWD(20),NAMFD(20)
C
      IERR=0
      IF(IB-IA.GT.23)         GO TO 100
      IF(IA.LT.1.OR.IB.GT.80) GO TO 100
      IF(IA.GT.IB)            GO TO 100
C
      DO 10 I=1,20
      NAMFD(I)=0
   10 CONTINUE
C
      CALL LODUP(IWD,IA,IB,NAMFD,1)
      RETURN
C
  100 IERR=1
      RETURN
      END
C$PROG HELPMAN
      SUBROUTINE HELPMAN
C
      INTEGER*4 IHELP(12,20)
C
      CHARACTER*48 CHELP(20)
      EQUIVALENCE (CHELP,IHELP)
C
      DATA (CHELP(J),J=1,10)/
     &'CMD  FIL     - PROCESS COMMANDS FROM FIL.CMD    ',
     &'IN   FIL.BAN - ASSIGN FIL.BAN FOR INPUT         ',
     &'OU   FIL.BAN - ASSIGN FIL.BAN FOR OUTPUT        ',
     &'NUOU FIL.BAN - CREATE+ASSIGN FIL.BAN FOR OUTPUT ',
     &'DDRI         - DISPLAY INPUT  FILE DIRECTORY    ',
     &'DDRO         - DISPLAY OUTPUT FILE DIRECTORY    ',
     &'DI   ID      - DISPLAY BANANA # ID FROM INPUT   ',
     &'DO   ID      - DISPLAY BANANA # ID FROM OUTPUT  ',
     &'IO   IDI,IDO - INPUT "IDI" OUTPUT AS "IDO"      ',
     &'OFF  ID-LST  - "TURN OFF" ID-LST ON OUTPUT FILE '/
      DATA (CHELP(J),J=11,20)/
     &'ON   ID-LST  - "TURN  ON" ID-LST ON OUTPUT FILE ',
     &'DE   ID-LST  - DELETE ID-LST FROM   OUTPUT FILE ',
     &'COPY ID-LST  - COPY ID-LST FROM INPUT TO OUTPUT ',
     &'             - ID-LST IMPLIES IDA,IDB,IDC.......',
     &'REN  IDA,IDB - RENAME IDA TO IDB ON OUTPUT FILE ',
     &'NUP ID,I,J,K - SET NUPM..JAUX FOR ID ON OUTPUT  ',
     &'PXY ID,IX,IY - SET IPX,IPY    FOR ID ON OUTPUT  ',
     &'HELP         - GETS THIS HELP-LIST AGAIN        ',
     &'LON/LOF      - TURN LOG ON/OFF                  ',
     &'END          - TERMINATES PROGRAM               '/
C
      WRITE(6,10)IHELP
   10 FORMAT(1H ,12A4)
      RETURN
      END
C$PROG LINIO
      SUBROUTINE LINIO(MODE,IWD,NBN,IERR)
C
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
C
      INTEGER*4 IWD(20)
C
      IERR=0
C
      IF(MODE.EQ.1) GO TO 10
      IF(MODE.EQ.2) GO TO 20
      RETURN
C
   10 CALL DREAD(LUO,IWD,NBN+1,80,IOS)
      RETURN
C
   20 CALL DRITE(LUO,IWD,NBN+1,80,IOS)
      RETURN
      END
C$PROG NUPM
      SUBROUTINE NUPM(KMD,ID,NUPAR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4    KPAR(9),NUPAR(3)
C
      CHARACTER*4  KMD,IONOF
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL EXISTR(LUO,ID,LOC,IONOF)
      IF(LOC.LE.0) GO TO 200
C
      CALL LOCBAN(LOC,NBN,III,III,III)
      NBN=NBN+3
      NBNN=NBN-1
      CALL DREAD(LUO,IWD,NBNN+1,80,IOS)
      IF(IOS.NE.0) RETURN
C
      LAB=IWD(1)
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 220
      IF(NF.GT.9) NF=9
      DO 40 J=1,NF
      CALL MILV(LWD(1,J),KPAR(J),XV,KIND,MERR)
      IF(MERR.NE.0) GO TO 220
   40 CONTINUE
      IF(KMD.EQ.'PXY ') GO TO 60
C
      DO 50 I=1,3
      KPAR(I+6)=NUPAR(I)
   50 CONTINUE
      GO TO 100
C
   60 DO 70 I=1,2
      KPAR(I)=NUPAR(I)
   70 CONTINUE
C
  100 WRITE(LUO,110,REC=NBN)LAB,KPAR
  110 FORMAT(A4,1X,6I5,3I10)
      RETURN
C
  200 WRITE(CMSSG,210)ID
  210 FORMAT('ID #',I5,'  DOES NOT EXIST')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  220 WRITE(CMSSG,230)
  230 FORMAT('ERROR DECODING GATE-LINE')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG ONOF
      SUBROUTINE ONOF(KIND,ID)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DDD/ IDIR(880),LUI,LUO,IOPN,OOPN
      CHARACTER*4                   IOPN,OOPN
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20),LABL(12)
C
      CHARACTER*4 CLABL(12)
      EQUIVALENCE (CLABL, LABL)
      DATA CLABL/'INP ','TIT ','GATE',9*'CXY '/
C
      CHARACTER*4   KIND,IWD1,IWD2
C
      EQUIVALENCE  (IWD1,IWD(1)),(IWD2,IWD(2))
C
      INTEGER*4     BLANK
      CHARACTER*4 CBLANK
      EQUIVALENCE (CBLANK,BLANK)
      DATA         CBLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(ID.EQ.0) GO TO 35
C
      CALL DIRIN(LUO,IERR)
      IF(IERR.NE.0) RETURN
C
      MI=-ID
C
      DO 30 I=1,880
      CALL LOCBAN(I,L,III,III,III)
      NDX=I
      IF(KIND.EQ.'OFF '.AND.ID.EQ.IDIR(I)) GO TO 100
      IF(KIND.EQ.'OFF '.AND.MI.EQ.IDIR(I)) GO TO 100
      IF(KIND.EQ.'ON  '.AND.MI.EQ.IDIR(I)) GO TO 200
      IF(KIND.EQ.'ON  '.AND.ID.EQ.IDIR(I)) GO TO 200
      IF(KIND.EQ.'DE  '.AND.ID.EQ.IDIR(I)) GO TO 300
      IF(KIND.EQ.'DE  '.AND.MI.EQ.IDIR(I)) GO TO 300
   30 CONTINUE
C
   35 WRITE(CMSSG,40)ID
   40 FORMAT('ID # ',I5,'  DOES NOT EXIST')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
  100 DO 110 I=1,12
      CALL LINIO(1,IWD,L,IERR)
      IF(IERR.NE.0) RETURN
      IWD1='OFF '
      CALL LINIO(2,IWD,L,IERR)
      IF(IERR.NE.0) RETURN
      L=L+1
  110 CONTINUE
      IDIR(NDX)=-IABS(IDIR(NDX))
      WRITE(CMSSG,120)ID
  120 FORMAT('ID #',I5,'  TURNED OFF')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 400
C
  200 DO 210 I=1,12
      CALL LINIO(1,IWD,L,IERR)
      IF(IERR.NE.0) RETURN
      IWD(1)=LABL(I)
      CALL LINIO(2,IWD,L,IERR)
      IF(IERR.NE.0) RETURN
      L=L+1
  210 CONTINUE
      IDIR(NDX)=IABS(IDIR(NDX))
      WRITE(CMSSG,220)ID
  220 FORMAT('ID #',I5,'  TURNED ON')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 400
C
  300 DO 310 I=1,20
      IWD(I)=BLANK
  310 CONTINUE
      IWD1='EMPT'
      IWD2='Y   '
      DO 320 I=1,12
      CALL LINIO(2,IWD,L,IERR)
      IF(IERR.NE.0) RETURN
      L=L+1
  320 CONTINUE
      IDIR(NDX)=0
      WRITE(CMSSG,330)ID
  330 FORMAT('ID #',I5,'  DELETED')
      CALL MESSLOG(LOGUT,LOGUP)
C
C     **************************************************************
C     WRITE OUT DIRECTORY-LINE CONTAINING MODIFIED/NEW ENTRY
C     **************************************************************
C
  400 CALL DLOUT(LUO,IDIR,NDX)              !OUTPUT APPROPIATE LINE
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
