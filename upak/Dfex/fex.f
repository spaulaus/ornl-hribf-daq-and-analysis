C$PROG FEXUX     - File-Examine program - FEX (Linux version)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/FEX2/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C
      COMMON/III/ LIN,LCM,LCI
C
      INTEGER*1   IBY(80),STAR
      DATA                STAR/Z'2A'/
C
      CHARACTER*4  IDONE
C
      CHARACTER*4  KMD,KMI
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMI,IWD(1)),(IBY,IWD)
C
      DATA LIN,LCI,LCM/5,5,9/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     *************************************************************
C     AUTOMATIC SPECTRUM ANALYSIS PROGRAM - ULTRIX VERSION
C     *************************************************************
C
      CALL FEXNIT(8)
C
      IF(IHEPF.NE.'YES ') GO TO 100
C
      WRITE(LOGUT,30)
      WRITE(LOGUT,35)
      WRITE(LOGUT,40)
   30 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
      GO TO 100
C   
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C   
  100 IF(LIN.EQ.LCI) WRITE(6,105)
  105 FORMAT(' FEX->',$)
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      IF(KMI.EQ.'    ')  GO TO 130
      IF(KMI.EQ.'COM ')  GO TO 130
      IF(IBY(1).EQ.STAR) GO TO 130
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
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
      IDONE='    '
      CALL CALLER(IDONE,IERR)
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4 IDONE
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C   
      IF(IDONE.NE.'YES ') CALL FEXR(IDONE,IERR)
C   
      RETURN
      END
C$PROG FEXR
C
      SUBROUTINE FEXR(IDONE,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/FEX1/ NAMF(20),KFIL,LREC
      CHARACTER*4           KFIL
C     ------------------------------------------------------------------
      COMMON/FEX2/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C
C     ------------------------------------------------------------------
      COMMON/CCC/  IBUF(32768),ISWAP
      CHARACTER*4              ISWAP
C     ------------------------------------------------------------------
C
      CHARACTER*4  CLWD(2,40),IDONE,KCON
C
      CHARACTER*256 CIBUF
C
      INTEGER*4 NAMCMD(20),IHELP(20,200)
C
      CHARACTER*4  KMD
      EQUIVALENCE (KMD,LWD(1,1)),(CIBUF,IBUF),(CLWD,LWD)
C
      DATA LHEP/8/
C
      DATA LU,LREC,JOF,KFIL,ISWAP/1,512,0,'FIXD','SWOF'/
C
      INTEGER*4  FSTAT,STATF,FILSIZ,STATARA(13)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
      IDONE='NO  '
C
      IF(KMD.EQ.'H   ') GO TO 130
      IF(KMD.EQ.'END ') STOP
      IF(KMD.EQ.'SWAP') GO TO 150
      IF(KMD.EQ.'SWOF') GO TO 150
      IF(KMD.EQ.'LON ') GO TO 160
      IF(KMD.EQ.'LOF ') GO TO 160
      IF(KMD.EQ.'HOFF') GO TO 170
      IF(KMD.EQ.'FOFF') GO TO 170
      IF(KMD.EQ.'CMD ') GO TO 180
      IF(KMD.EQ.'WO  ') GO TO 190
C
      IF(KMD.EQ.'IN  ') GO TO 200
      IF(KMD.EQ.'RECL') GO TO 230
      IF(KMD.EQ.'CVIF') GO TO 300
      IF(KMD.EQ.'MSKF') GO TO 350
C
      IF(KMD.EQ.'PEV ') GO TO 500
      IF(KMD.EQ.'DEV ') GO TO 500
C
      IF(KMD.EQ.'PZ  ') GO TO 500
      IF(KMD.EQ.'DZ  ') GO TO 500
      IF(KMD.EQ.'PI  ') GO TO 500
      IF(KMD.EQ.'DI  ') GO TO 500
      IF(KMD.EQ.'PIF ') GO TO 500
      IF(KMD.EQ.'DIF ') GO TO 500
      IF(KMD.EQ.'PZF ') GO TO 500
      IF(KMD.EQ.'DZF ') GO TO 500
      IF(KMD.EQ.'PF  ') GO TO 500
      IF(KMD.EQ.'DF  ') GO TO 500
C
      IF(KMD.EQ.'PA  ') GO TO 500
      IF(KMD.EQ.'DA  ') GO TO 500
C
      IF(KMD.EQ.'PL  ') GO TO 600
      IF(KMD.EQ.'DL  ') GO TO 600
C
      WRITE(6,120)
  120 FORMAT(1H ,'ILLEGAL COMMAND')
      RETURN
C
  130 CALL HELPMANU(IWD,LHEP,IHELP,200,20,IHEPF)
      GO TO 2500
C
  150 ISWAP=KMD
      GO TO 2500
C
  160 LISFLG=KMD
      GO TO 2500
C
  170 CALL IVALU(LWD(1,2),IV,IERR)
      IF(IERR.NE.0) GO TO 2000
      JOF=IV
      IF(KMD.EQ.'FOFF') JOF=2*IV
      GO TO 2500
C
  180 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2500
C
  190 WRITE(6,192)
  192 FORMAT(1H ,'WO encountered - type [RETURN] to continue ->',$)
      READ(5,194)IDUM
  194 FORMAT(A4)
      GO TO 2500
C
  200 IA=NXNB(IWDRAW,4,80)
      IF(IA.LE.0) GO TO 2000
      IB=NXBL(IWDRAW,IA,80)-1
      IF(IB.LE.0) GO TO 2000
C
      DO 210 I=1,20
      NAMF(I)=0
  210 CONTINUE
C
      CALL LODUP(IWDRAW,IA,IB,NAMF,1)
C
      CALL FILOP(LU,IERR)
      IF(IERR.NE.0) GO TO 2490
C
      STATF=FSTAT(LU,STATARA)
      IF(STATF.EQ.0) THEN
      FILSIZ=STATARA(8)
      ELSE
      FILSIZ=(-1)*STATF
      ENDIF
      WRITE(6,215)FILSIZ
  215 FORMAT(1H ,'FILESIZE =',I10,' bytes')
      GO TO 2500
C
  230 CALL LIMIV(LWD(1,2),16,65536,LREC,IERR)
      IF(IERR.NE.0) GO TO 2000
      GO TO 2500
C
  300 KCON=CLWD(1,2)
      IF(KCON.EQ.'SWAB') GO TO 310
      IF(KCON.EQ.'SWAF') GO TO 310
      GO TO 2000
  310 CALL FILCON(LU,KCON)
      GO TO 2500
C
  350 CALL HEXVAL(LWD(1,2),MSK,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL FILMSK(LU,MSK)
      GO TO 2500
C
  500 IF(KFIL.NE.'FIXD') THEN
                         KFIL='FIXD'
                         CALL FILOP(LU,IERR)
                         IF(IERR.NE.0) GO TO 2490
                         ENDIF
C
      CALL FREAD(LU,LREC,JOF,JLO,ILO,IHI,JNC,IERR)
      IF(IERR.NE.0) GO TO 2490
C
      CALL DDAT(KMD,JLO,ILO,IHI,JNC)
      GO TO 2500
C
  600 IF(KFIL.NE.'VAR ') THEN
                         KFIL='VAR '
                         CALL FILOP(LU,IERR)
                         IF(IERR.NE.0) GO TO 2490
                         ENDIF
C
      CALL DLINES(LU)
      GO TO 2500
C
 2000 WRITE(6,2005)
 2005 FORMAT(1H ,'SYNTAX ERROR OR ILLEGAL VALUE')
C
 2490 IERR=1
C
 2500 IDONE='YES '
      RETURN
      END
C$PROG FEXNIT
      SUBROUTINE FEXNIT(LHEP)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/FEX2/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
C
      INTEGER*4 NAMHEP(6)
C
      CHARACTER*24 CNAMHEP
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C
      character*4 cnamprog(2)
      equivalence (cnamprog,namprog)
      DATA cNAMPROG/'FEX ','    '/
C
      DATA CNAMHEP/'fex.hep'/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      MSGF= ' '
      CMSSG=' '
      LISFLG='LOF '
C
      LOGUT=6
      LOGUP=7
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'fex.log',
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
C     CLOSE(UNIT=LOGUP,DISP='DELETE')
      CLOSE(UNIT=LOGUP)
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'fex.log',
     &     STATUS     = 'REPLACE',
     &     IOSTAT     = IOS)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   LOGUP=0
                   WRITE(LOGUT,20)
                   ENDIF
C      
   20 FORMAT(1H ,'OUTPUT TO LOG-FILE IS DISABLED')
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      CALL CTCNIT
C
      RETURN
      END
C$PROG FREAD
C
      SUBROUTINE FREAD(LU,LREC,JOF,JLO,ILO,IHI,JNC,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C     ------------------------------------------------------------------
      COMMON/CCC/  IBUF(32768),ISWAP
      CHARACTER*4              ISWAP
C     ------------------------------------------------------------------
C
      INTEGER*4    EOFTST
C
      INTEGER*2    IBUH(65536)
C
      CHARACTER*4  KMD
      EQUIVALENCE (KMD,LWD(1,1)),(IBUH,IBUF)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      DO 10 I=1,32768
      IBUF(I)=0
   10 CONTINUE
C
      CALL IVALU(LWD(1,2),JLO,IERR)
      IF(IERR.NE.0)        GO TO 500
      CALL IVALU(LWD(1,3),JHI,IERR)
      IF(IERR.NE.0)        GO TO 500
      CALL IVALU(LWD(1,4),JNC,IERR)
      IF(IERR.NE.0)        GO TO 500
      IF(JLO.LT.1)         GO TO 500
      IF(JHI-JLO.GE.32768) GO TO 500
      IF(JHI.LT.JLO) JHI=JLO
C
      IA=JLO+JOF
      IB=JHI+JOF
      NHW=1
C
      IF(KMD.EQ.'PIF '.OR.KMD.EQ.'DIF ') THEN
                                         IA=JOF+2*(JLO-1)+1
                                         IB=JOF+2*JHI
                                         NHW=2
                                         ENDIF
C
      IF(KMD.EQ.'PZF '.OR.KMD.EQ.'DZF ') THEN
                                         IA=JOF+2*(JLO-1)+1
                                         IB=JOF+2*JHI
                                         NHW=2
                                         ENDIF
C
      IF(KMD.EQ.'PF  '.OR.KMD.EQ.'DF  ') THEN
                                         IA=JOF+2*(JLO-1)+1
                                         IB=JOF+2*JHI
                                         NHW=2
                                         ENDIF
C
      LRHW=LREC/2
C
      IREC=(IA+LRHW-1)/LRHW
      JREC=(IB+LRHW-1)/LRHW
C
      NLO=1
      NHI=NLO+LRHW-1
      NR=0
C
      DO 50 II=IREC,JREC
      NR=NR+1
      READ(LU,REC=II,IOSTAT=IOS)(IBUH(N),N=NLO,NHI)
      IF(EOFTST(IOS).NE.0) GO TO 55
      IF(IOS.NE.0)               GO TO 510
      NLO=NLO+LRHW
      NHI=NHI+LRHW
   50 CONTINUE
C
      NA=LRHW*(IREC-1)+1
      NOF=IA-NA
      N=0
C
   55 DO 60 I=IA,IB
      N=N+1
      IBUH(N)=IBUH(N+NOF)
   60 CONTINUE
      ILO=1
      IHI=N/NHW
      IHIT=NR*(LRHW/NHW)
      IF(IHI.GT.IHIT) IHI=IHIT
      IF(IOS.NE.0) GO TO 520
      RETURN
C
  500 WRITE(6,505)
  505 FORMAT(1H ,'ILLEGAL RANGE REQUEST - MAX-RANGE = 32768')
      IERR=1
      RETURN
C
  510 CALL IOFERR(IOS)
      WRITE(6,515)II,IOS
  515 FORMAT(1H ,'ERROR READING RECORD NO.',I6,'   IOS=',I4)
      IERR=1
      RETURN
C
  520 NWD=II*LRHW
      WRITE(6,525)II,NWD
  525 FORMAT(1H ,'END-OF-FILE AT REC#,16BIT-WD# =',2I8)
      WRITE(6,530)
  530 FORMAT(1H ,'Type: [RETURN] to continue -->',$)
      READ(5,535)IT
  535 FORMAT(A4)
      RETURN
      END
C$PROG FILOP
C
      SUBROUTINE FILOP(LU,IERR)
C
C     ------------------------------------------------------------------
      COMMON/FEX1/ NAMF(20),KFIL,LREC
      CHARACTER*4           KFIL
C     ------------------------------------------------------------------
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    RECLVALU
C
      CHARACTER*80 CNAMF
C
      EQUIVALENCE (CNAMF,NAMF),(KMD,LWD(1,1))
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
      CLOSE(UNIT=LU)
C
      IF(KFIL.EQ.'VAR ') GO TO 100
      IF(KFIL.EQ.'FIXD') GO TO 300
      GO TO 500
C
C     *************************************************************
C     OPEN FILE FOR SEQUENTIAL ACCESS INPUT
C     *************************************************************
C
  100 OPEN(UNIT   = LU,
     &     FILE   = CNAMF,
     &     STATUS = 'OLD',
     &     IOSTAT = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 500
      RETURN
C   
C     *************************************************************
C     OPEN FOR DIRECT ACCESS INPUT
C     *************************************************************
C   
  300 OPEN(UNIT       = LU,
     &     FILE       = CNAMF,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(LREC),
     &     FORM       = 'UNFORMATTED',
     &     IOSTAT     = IOS)
C   
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 500
      RETURN
C
  500 WRITE(6,505)
  505 FORMAT(1H ,'ERROR OPENING FILE')
      IERR=1
      RETURN
      END
C$PROG DDAT
      SUBROUTINE DDAT(KMD,JLO,ILO,IHI,JNC)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/CCC/  IBUF(32768),ISWAP
      CHARACTER*4              ISWAP
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C
      INTEGER*2 JBUF(16384),LINE(64),FFFF,X2020
C
      REAL*4    FBUF(32768),FLOBUF(5)
C
      EQUIVALENCE (JBUF(1),IBUF(1)),(FBUF(1),IBUF(1))
C
      DATA      FFFF,X2020/-1,Z'2020'/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL SWAPCK(KMD,ILO,IHI)
C
      KNC=5
C
      IF(JNC.LE.0)  JNC=10
C
      IF(KMD.EQ.'PEV ') GO TO 1000
      IF(KMD.EQ.'DEV ') GO TO 1000
C
      IF(KMD.EQ.'PZ  ') GO TO 1100
      IF(KMD.EQ.'DZ  ') GO TO 1150
C
      IF(KMD.EQ.'PI  ') GO TO 1200
      IF(KMD.EQ.'DI  ') GO TO 1250
C
      IF(KMD.EQ.'PIF ') GO TO 1300
      IF(KMD.EQ.'DIF ') GO TO 1350
C
      IF(KMD.EQ.'PZF ') GO TO 1400
      IF(KMD.EQ.'DZF ') GO TO 1450
C
      IF(KMD.EQ.'PF  ') GO TO 1500
      IF(KMD.EQ.'DF  ') GO TO 1550
C
      IF(KMD.EQ.'PA  ') GO TO 1600
      IF(KMD.EQ.'DA  ') GO TO 1600
C
C     **************************************************************
C     PRINT OR DISPLAY DATA IN "EVENT FORMAT"
C     **************************************************************
C
 1000 DO 1010 I=ILO,IHI
      IF(JBUF(I).EQ.FFFF) GO TO 1020
 1010 CONTINUE
      WRITE(CMSSG,1015)
      CALL MESSLOG(LOGUT,LOGUP)
 1015 FORMAT('NO "EVENTS" FOUND')
      RETURN
C
 1020 IB=I
 1030 IA=IB+1
      IF(IB.GT.IHI) RETURN
      IF(MSGF.NE.'    ') RETURN
      NPARR=0
      DO 1040 I=IA,IHI
      IF(JBUF(I).EQ.FFFF) GO TO 1050
      IF(JBUF(I).GE.0)    NPARR=NPARR+1
 1040 CONTINUE
      RETURN
C
 1050 IB=I
      IF(KMD.EQ.'PEV ') GO TO 1080
C
      WRITE(LOGUT,1060)IA,NPARR,(JBUF(I),I=IA,IB)
 1060 FORMAT(1H ,'I,NP=',I5,I4,' - ',10Z5/(1H ,17X,10Z5))
      GO TO 1030
C
 1080 WRITE(LOGUP,1090)IA,NPARR,(JBUF(I),I=IA,IB)
 1090 FORMAT(1H ,'I,NP=',I5,I4,' - ',20Z5/(1H ,17X,20Z5))
      GO TO 1030
C
C     **************************************************************
C     PRINT OR DISP HALF-WORD DATA IN "HEX FORMAT"
C     **************************************************************
C
 1100 JNOW=JLO
      IB=ILO-1
 1110 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUP,1115)JNOW,(JBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUP,1120)JNOW,(JBUF(I),I=IA,IB)
 1115 FORMAT(1H ,I8,'-',11Z7)
 1120 FORMAT(1H ,I8,'-',11Z7/(1H ,9X,10Z7))
      JNOW=JNOW+JNC
      GO TO 1110
C
 1150 JNOW=JLO
      IB=ILO-1
 1160 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUT,1165)JNOW,(JBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUT,1170)JNOW,(JBUF(I),I=IA,IB)
 1165 FORMAT(1H ,I8,'-',10Z7)
 1170 FORMAT(1H ,I8,'-',10Z7/(1H ,9X,10Z7))
      JNOW=JNOW+JNC
      GO TO 1160
C
C     **************************************************************
C     PRINT OR DISPLAY HALF-WORD DATA IN "INTEGER  FORMAT"
C     **************************************************************
C
 1200 JNOW=JLO
      IB=ILO-1
 1210 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUP,1215)JNOW,(JBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUP,1220)JNOW,(JBUF(I),I=IA,IB)
 1215 FORMAT(1H ,I8,'-',10I7)
 1220 FORMAT(1H ,I8,'-',10I7/(1H ,9X,10I7))
      JNOW=JNOW+JNC
      GO TO 1210
C
 1250 JNOW=JLO
      IB=ILO-1
 1260 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUT,1265)JNOW,(JBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUT,1270)JNOW,(JBUF(I),I=IA,IB)
 1265 FORMAT(1H ,I8,'-',10I7)
 1270 FORMAT(1H ,I8,'-',10I7/,(1H ,9X,10I7))
      JNOW=JNOW+JNC
      GO TO 1260
C
C     **************************************************************
C     PRINT OR DISPLAY FULL-WORD DATA IN "INTEGER  FORMAT"
C     **************************************************************
C
 1300 JNOW=JLO
      IB=ILO-1
 1310 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUP,1315)JNOW,(IBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUP,1320)JNOW,(IBUF(I),I=IA,IB)
 1315 FORMAT(1H ,I8,'-',10I11)
 1320 FORMAT(1H ,I8,'-',10I11/(1H ,9X,10I11))
      JNOW=JNOW+JNC
      GO TO 1310
C
 1350 JNOW=JLO
      IB=ILO-1
 1360 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUT,1365)JNOW,(IBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUT,1370)JNOW,(IBUF(I),I=IA,IB)
 1365 FORMAT(1H ,I8,'-',10I11)
 1370 FORMAT(1H ,I8,'-',10I11/(1H ,9X,10I11))
      JNOW=JNOW+JNC
      GO TO 1360
C
C     **************************************************************
C     PRINT OR DISPLAY FULL-WORD DATA IN "HEX  FORMAT"
C     **************************************************************
C
 1400 JNOW=JLO
      IB=ILO-1
 1410 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUP,1415)JNOW,(IBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUP,1420)JNOW,(IBUF(I),I=IA,IB)
 1415 FORMAT(1H ,I8,'-',10Z11)
 1420 FORMAT(1H ,I8,'-',10Z11/(1H ,9X,10Z11))
      JNOW=JNOW+JNC
      GO TO 1410
C
 1450 JNOW=JLO
      IB=ILO-1
 1460 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+JNC-1
      IF(IB.GT.IHI) IB=IHI
      IF(JNC.LE.10) WRITE(LOGUT,1465)JNOW,(IBUF(I),I=IA,IB)
      IF(JNC.GT.10) WRITE(LOGUT,1470)JNOW,(IBUF(I),I=IA,IB)
 1465 FORMAT(1H ,I8,'-',10Z11)
 1470 FORMAT(1H ,I8,'-',10Z11/(1H ,9X,10Z11))
      JNOW=JNOW+JNC
      GO TO 1460
C
C     **************************************************************
C     PRINT OR DISPLAY FULL-WORD DATA IN "FLOATING  FORMAT"
C     **************************************************************
C
 1500 JNOW=JLO
      IB=ILO-1
 1510 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+5-1
      IF(IB.GT.IHI) IB=IHI
      NUM=0
      DO 1515 I=IA,IB
      NUM=NUM+1
      FLOBUF(NUM)=FBUF(I)
 1515 CONTINUE
      WRITE(LOGUP,1535)JNOW,(FLOBUF(I),I=1,NUM)
 1535 FORMAT(1H ,I8,'-',1P5E15.6)
      JNOW=JNOW+5
      GO TO 1410
C
 1550 JNOW=JLO
      IB=ILO-1
 1560 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IB=IA+5-1
      IF(IB.GT.IHI) IB=IHI
      NUM=0
      DO 1565 I=IA,IB
      NUM=NUM+1
      FLOBUF(NUM)=FBUF(I)
 1565 CONTINUE
      WRITE(LOGUT,1585)JNOW,(FLOBUF(I),I=1,NUM)
 1585 FORMAT(1H ,I8,'-',1P5E15.6)
      JNOW=JNOW+5
      GO TO 1560
C
C     **************************************************************
C     PRINT OR DISPLAY DATA IN "ASCII FORMAT"
C     **************************************************************
C
 1600 IB=ILO-1
 1610 IA=IB+1
      IF(IA.GT.IHI) RETURN
      IF(MSGF.NE.'    ') RETURN
      IB=IA+39
      IF(IB.GT.IHI) IB=IHI
      N=0
C
      DO 1620 I=IA,IB
      N=N+1
      LINE(N)=JBUF(I)
 1620 CONTINUE
      NM1=N
      IF(NM1.GT.39.AND.LINE(N).EQ.X2020) NM1=N-1
C
      CALL CHEKAS(LINE,N)
C
      IF(KMD.EQ.'PA  ') WRITE(LOGUP,1630)(LINE(I),I=1,N)
      IF(KMD.EQ.'DA  ') WRITE(LOGUT,1635)(LINE(I),I=1,NM1)
 1630 FORMAT(1H ,40A2)
 1635 FORMAT(1H ,39A2)
      GO TO 1610
C
      END
C$PROG CHEKAS
C
      SUBROUTINE CHEKAS(LINE,N)
C
      INTEGER*2 LINE(*)
C
      INTEGER*4 X20,X7E,X3F
C
      DATA      X20,X7E,X37/Z'20',Z'7E',Z'3F'/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      NN=2*N
      DO 10 I=1,NN
      CALL ILBYTE(IT,LINE,I-1)
      IF(IT.GE.X20.AND.IT.LE.X7E) GO TO 10
      CALL ISBYTE(X3F,LINE,I-1)
   10 CONTINUE
      RETURN
      END
C$PROG DLINES
C
      SUBROUTINE DLINES(LU)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    LINE(30)
      INTEGER*2    LINE2(60)
      EQUIVALENCE (LINE2,LINE)
C
      CHARACTER*4  KMD
      EQUIVALENCE (KMD,LWD(1,1))
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL IVALU(LWD(1,2),JLO,IERR)
      IF(IERR.NE.0) GO TO 500
      CALL IVALU(LWD(1,3),JHI,IERR)
      IF(IERR.NE.0) GO TO 500
      IF(JLO.LT.1)  GO TO 500
      IF(JHI.LT.JLO) JHI=JLO
C
      REWIND LU
C
      DO 100 J=1,JHI
      READ(LU,50,END=520,ERR=540)LINE
   50 FORMAT(30A4)
      IF(J.LT.JLO) GO TO 100
      CALL CHEKAS(LINE2,40)
      IF(KMD.EQ.'DL  ') WRITE(LOGUT,60)J,LINE
      IF(KMD.EQ.'PL  ') WRITE(LOGUP,60)J,LINE
   60 FORMAT(1H ,I6,2X,30A4)
  100 CONTINUE 
      RETURN
C
  500 WRITE(6,505)
  505 FORMAT(1H ,'SYNTAX ERROR OR ILLEGAL VALUE IN RANGE SPECIFICATION')
      RETURN
  520 WRITE(6,525)J
  525 FORMAT(1H ,'END-OF-FILE READING LINE -',I8)
      RETURN
  540 WRITE(6,545)J
  545 FORMAT(1H ,'ERROR READING LINE -',I8)
      RETURN
      END
C$PROG SWAPCK
C
      SUBROUTINE SWAPCK(KMD,ILO,IHI)
C
C     ------------------------------------------------------------------
      COMMON/CCC/  IBUF(32768),ISWAP
      CHARACTER*4              ISWAP
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C
      INTEGER*2    JBUF(65536)
C
      EQUIVALENCE (JBUF,IBUF)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(ISWAP.NE.'SWAP') RETURN
C
      IF(KMD.EQ.'PIF ') GO TO 100
      IF(KMD.EQ.'DIF ') GO TO 100
      IF(KMD.EQ.'PZF ') GO TO 100
      IF(KMD.EQ.'DZF ') GO TO 100
C
      CALL SWAPB(JBUF,ILO,IHI)
      RETURN
C
  100 CALL SWAPF(IBUF,ILO,IHI)
      RETURN
      END
C$PROG SWAPB
      SUBROUTINE SWAPB(LIST,IA,IB)
C
      INTEGER*2 LIST(*),ITMP,JTMP
C
      BYTE IBY(2),JBY(2)
C
      EQUIVALENCE (IBY,ITMP),(JBY,JTMP)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ROUTINE TO SWAP BYTES IN HALF-WORD ARRAY "LIST"
C
      DO 10 I=IA,IB
      ITMP=LIST(I)
      JBY(1)=IBY(2)
      JBY(2)=IBY(1)
      LIST(I)=JTMP
   10 CONTINUE
      RETURN
      END
C$PROG SWAPF
      SUBROUTINE SWAPF(IBUF,IA,IB) 
C
      INTEGER*4 IBUF(*),ITMP,JTMP
C
      BYTE IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,ITMP),(JBY,JTMP)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     SWAPS BYTES FOR FULL-WORDS - IA THRU IB
C
      DO 10 I=IA,IB
      ITMP=IBUF(I)
      JBY(1)=IBY(4)
      JBY(2)=IBY(3)
      JBY(3)=IBY(2)
      JBY(4)=IBY(1)
      IBUF(I)=JTMP
   10 CONTINUE
      RETURN
      END
C$PROG FILCON
C
      SUBROUTINE FILCON(LU,KCON)
C
C     ------------------------------------------------------------------
      COMMON/CCC/ IBUF(32768),ISWAP
      CHARACTER*4             ISWAP
C     ------------------------------------------------------------------
      COMMON/FEX1/ NAMF(20),KFIL,LREC
      CHARACTER*4           KFIL
C     ------------------------------------------------------------------
      INTEGER*2    IBUF2(65536)
      EQUIVALENCE (IBUF2,IBUF)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KCON
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      KFIL='FIXD'
C
      CALL FILOP(LU,IERR)
C
      IF(IERR.NE.0) RETURN
C
      NHW=LREC/2
      NFW=LREC/4
      IREC=0
C
  100 IREC=IREC+1
      READ(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NFW)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(6,105)
                   RETURN
                   ENDIF
C
      IF(KCON.EQ.'SWAB') CALL SWAPB(IBUF2,1,NHW)
      IF(KCON.EQ.'SWAF') CALL SWAPF(IBUF, 1,NFW)
C
      WRITE(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NFW)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(6,110)
                   RETURN
                   ENDIF
C
  105 FORMAT(1H ,'READING')
  110 FORMAT(1H ,'WRITING')
C
      GO TO 100
C
      END
C$PROG FILMSK
C
      SUBROUTINE FILMSK(LU,MSK)
C
C     ------------------------------------------------------------------
      COMMON/CCC/ IBUF(32768),ISWAP
      CHARACTER*4             ISWAP
C     ------------------------------------------------------------------
      COMMON/FEX1/ NAMF(20),KFIL,LREC
      CHARACTER*4           KFIL
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      KFIL='FIXD'
C
      CALL FILOP(LU,IERR)
C
      IF(IERR.NE.0) RETURN
C
      NHW=LREC/2
      NFW=LREC/4
      IREC=0
C
  100 IREC=IREC+1
      READ(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NFW)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(6,205)
                   RETURN
                   ENDIF
C
      DO 110 N=1,NFW
      IBUF(N)=IAND(IBUF(N),MSK)
  110 CONTINUE
C
      WRITE(LU,REC=IREC,IOSTAT=IOS)(IBUF(I),I=1,NFW)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   WRITE(6,210)
                   RETURN
                   ENDIF
C
  205 FORMAT(1H ,'READING')
  210 FORMAT(1H ,'WRITING')
C
      GO TO 100
C
      END
