C$PROG LISTEM    - Lists peak fit results
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE LISTEM(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C   
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM02/COMP(2048,44),YCAL(2048),BGD(2048),DATA(2048),WT(2048)
C   
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C   
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C   
      COMMON/SM06/ NUMITS,ICON
C   
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM09/ XYP(50,2),NBXY
C   
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A,       B,       DET
C   
      COMMON/SM10/ ERR(18),ISAV,KFIT
C   
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
C   
      COMMON/SM12/ LTITL(20)
C     ------------------------------------------------------------------
      DIMENSION DAT(2048,3)
      INTEGER*4 LAT(500,15),LLWD(80),LWDL(10),LIST(78)
      INTEGER*4 TITLE
C     ------------------------------------------------------------------
C   
      COMMON/SM15/ IXC(50),YC(50),XCOR(50)
C   
      COMMON/SM16/ IHOLF(4,44),JPU(44),XOR(44)
C   
      COMMON/SM17/ IORD(500),ITEM(500),LOUXX(14,50)
C   
      COMMON/SM18/ PLIM(352),GUESS(176),PARV(176)
C   
      COMMON/SM19/ ISKIP(2,4),JSKIP(2,4),PMIN(4),PMAX(4),KVAR(4)
C   
      COMMON/SM20/ RESUL(12,44),FOUT(50),ITRITL(10)
C   
      COMMON/SM21/ NDXI(4),JDATE(3),JTIME(2),TITLE(20)
C   
      COMMON/SM22/ IDATA(2048),IDAT(2048,3),ISKF(2048)
      CHARACTER*4                           ISKF
C   
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
C   
      EQUIVALENCE (DAT,IDAT)
      EQUIVALENCE (LAT(1,1),PAT(1,1))
      EQUIVALENCE (LIST(1),LWD(1,2))
      EQUIVALENCE (KMD    ,LWD(1,1)),
     &            (LWDL(1),LWD(1,1)),
     &            (LLWD(1),LWD(1,1))
C
      CHARACTER*4  KTF
C
      SAVE
C   
C     ------------------------------------------------------------------
C     PRINT OUT ALL RELAVANT INPUT DATA WHICH WILL NOT APPEAR IN PEAK LI
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(RESOK.NE.'YES ') GO TO 3010
C
      ITPF=Z'0C202020'
      WRITE(7,580)ITPF
  580 FORMAT(1H ,A4)   
      WRITE(7,588)ILO,IHI,LTITL
  588 FORMAT(1H ,'SAM FIT RANGE =',2I6,' .... ',20A4,2X,'TIT$'/)
      WRITE(7,600)
  600 FORMAT(1H ,'       DEL    DELFAC      NMUL     XSTEP    ',
     1' DXMAX      FWLO      FWHI      FALO      FAHI'/)
      WRITE(7,605)DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI
  605 FORMAT(1H ,2F10.4,I10,6F10.4,25X,'DEL$'/)
      IF(NSKIP.NE.0) WRITE(7,610)
  610 FORMAT(1H ,'SKIP  FROM------THRU      FROM------THRU      FROM',
     &           '------THRU      FROM------THRU'/)
      IF(NSKIP.NE.0) WRITE(7,612)ISKIP
  612 FORMAT(1H ,8I10,35X,'SKP$'/)
      WRITE(7,620)
  620 FORMAT(1H ,'   EO(KEV)    KEV/CH KEV/CH**2       FWA      ',
     1' FWB       FWC      ASLO      ASHI      IFUN'/)
      WRITE(7,625)ECA,ECB,ECC,FWA,FWB,FWC,ASLO,ASHI,KFUNS
  625 FORMAT(1H ,8F10.4,I10,25X,'CAL$'/)
      WRITE(7,640)
  640 FORMAT(1H ,'        VX        VW      VALO      VAHI',
     &           '      PLOT      WOOD       NBC'/)
      WRITE(7,645)KVAR,KPPL,NWOOD,NBC
  645 FORMAT(1H ,6(6X,A4),I10,45X,'VAR$'/)
C   
C     ------------------------------------------------------------------
C     PRINT OUT PEAK LIST (SUMMARY OF FIT RESULTS)
C     ------------------------------------------------------------------
C   
      WRITE(7,670)
  670 FORMAT(1H ,30(4H----))
C
      KTF='FIT$'   
      IF(KFIT.NE.3) WRITE(7,675)                           !FOR SAM
      IF(KFIT.EQ.3) THEN                                   !FOR GASP
                    KTF='GFI$'
                    WRITE(7,680)
                    ENDIF
C   
  675 FORMAT(1H ,'     ICHAN     OCHAN    E(KEV)      AREA     GAREA',
     1'     PCE     FWI     FWF   ASLI   ASLF   ASHI   ASHF   KF',
     1'     ID'/)
C   
  680 FORMAT(1H ,'   ERRCHAN     OCHAN    E(KEV)   ERRAREA     GAREA',
     1'     PCE   ERRFW     FWF   ASLI   ASLF   ASHI   ASHF   KF',
     1'     ID'/)
C   
      WRITE(7,700)((RESUL(I,J),I=1,12),KFUN(J),ID,KTF,J=1,NPK)
  700 FORMAT(1H ,3F10.3,2F10.0,3F8.3,4F7.3,I5,I7,1X,A4/)
C
      WRITE(7,670)
C
      IF(NBXY.LT.1) GO TO 720
      WRITE(7,710)
      WRITE(7,715)(XCOR(I),YC(I),I=1,NBXY)
  710 FORMAT(1H ,'FIXED BGD - X/Y LIST'/)
  715 FORMAT(1H ,2F10.0,95X,'BGD$')
      WRITE(7,670)
C
  720 WRITE(7,725)ASUM,QFN
  725 FORMAT(1H ,'ASUM =',F10.0,6X,'QFN =',F10.2,78X,'QFN$',/)
C   
C     ==================================================================
C     DO THE PRINTER-PLOTS IF REQUESTED
C   
C     KPPL = 'NONE' SAYS DO NO PRINTER-PLOTS
C     KPPL = 'FITS' SAYS PRINTER-PLOT FIT AND DATA
C     KPPL = 'ALL ' - PLOT EACH COMP TOGETHER WITH CORRESPONDING RESIDUA
C     ==================================================================
C   
  755 IF(KPPL.EQ.'NONE') RETURN
C   
C     ------------------------------------------------------------------
C     SET UP CHANNEL SKIP FLAG ARRAY FOR "SPLOT" 
C     ------------------------------------------------------------------
C   
      DO 758 I=I1,I2
      ISKF(I)='    '
      IF(WT(I).EQ.0.0) ISKF(I)='SKIP'
  758 CONTINUE
      IF(IFBGD.EQ.1) GO TO 780
      DO 760 I=I1,I2
      IDAT(I,1)=DATA(I)
      IDAT(I,2)=YCAL(I)
      IDAT(I,3)=BGD(I)
  760 CONTINUE
      NSECT=3
      GO TO 810
  780 DO 800 I=I1,I2
      IDAT(I,1)=DATA(I)+BGD(I)
      IDAT(I,2)=YCAL(I)+BGD(I)
      IDAT(I,3)=BGD(I)
  800 CONTINUE
      NSECT=3
  810 WRITE(7,820)
  820 FORMAT(1H1)
      CALL SPLOT(IDAT,ISKF,ILO,I1,I2,NSECT)
      IF(KPPL.NE.'ALL ') RETURN
      NSECT=2
      DO 880 N=1,NPK
      DO 870 I=I1,I2
      SUM=0.0
      DO 860 J=1,NCOMP
      IF(J.EQ.N) GO TO 860
      CALL GETBETA(J,JX,BETAJ,ADUM)
      SUM=SUM+COMP(I,JX)*BETAJ
  860 CONTINUE
      IDAT(I,1)=DATA(I)-SUM
      CALL GETBETA(N,NX,BETAN,ADUM)
      IDAT(I,2)=COMP(I,NX)*BETAN
      IDAT(I,3)=0
  870 CONTINUE
      WRITE(7,820)
      CALL SPLOT(IDAT,ISKF,ILO,I1,I2,NSECT)
  880 CONTINUE
      RETURN
C   
 3010 WRITE(CMSSG,3015)
 3015 FORMAT('NO VALID FIT RESULTS')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
