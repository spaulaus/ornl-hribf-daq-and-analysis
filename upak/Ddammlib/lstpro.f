C$PROG LSTPRO    - Processes labeled lists & assignes parameter values
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE LSTPRO(IERR)
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
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
      CHARACTER*4            KINFD
      CHARACTER*4  CMINSY,CMAXSY
      EQUIVALENCE (CMINSY,MINSY),(CMAXSY,MAXSY)
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C     ------------------------------------------------------------------
      COMMON/SM09/ XYP(50,2),NBXY
C     ------------------------------------------------------------------
      COMMON/SM19/ ISKIP(2,4),JSKIP(2,4),PMIN(4),PMAX(4),KVAR(4)
      CHARACTER*4                                        KVAR
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
      INTEGER*4   LLWD(80)
      CHARACTER*4 LIST(78)
C     ------------------------------------------------------------------
C
      CHARACTER*4 KMD
C   
      EQUIVALENCE (LIST(1),LWD(1,2))
      EQUIVALENCE (KMD    ,LWD(1,1)),
     &            (LLWD(1),LWD(1,1))
C
      INTEGER*4    WSET
      character*4  cWSET
      equivalence  (cWSET,wset)
      DATA        cWSET/'SETW'/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
      IF(KMD.EQ.'ECAL') GO TO 1300
      IF(KMD.EQ.'FW  ') GO TO 1320
      IF(KMD.EQ.'ASYM') GO TO 1340
      IF(KMD.EQ.'WLIM') GO TO 1360
      IF(KMD.EQ.'ALIM') GO TO 1380
      IF(KMD.EQ.'DPX ') GO TO 1400
      IF(KMD.EQ.'DEL ') GO TO 1420
      IF(KMD.EQ.'NBC ') GO TO 1440
      IF(KMD.EQ.'FUNO') GO TO 1480
      IF(KMD.EQ.'KPPL') GO TO 1500
      IF(KMD.EQ.'LIN ') GO TO 1505
      IF(KMD.EQ.'LOG ') GO TO 1510
      IF(KMD.EQ.'VB  ') GO TO 1520
      IF(KMD.EQ.'FB  ') GO TO 1530
      IF(KMD.EQ.'VX  ') GO TO 1560
      IF(KMD.EQ.'VW  ') GO TO 1560
      IF(KMD.EQ.'VALO') GO TO 1560
      IF(KMD.EQ.'VAHI') GO TO 1560
C   
      IF(KMD.EQ.'MON ') GO TO 1592
      IF(KMD.EQ.'MOF ') GO TO 1594
      IF(KMD.EQ.'DFI ') GO TO 1584
      IF(KMD.EQ.'DPK ') GO TO 1586
      IF(KMD.EQ.'DPPB') GO TO 1587
      IF(KMD.EQ.'DMM ') GO TO 1588
C
      IF(KMD.EQ.'WOOD') GO TO 1600   
      IF(KMD.EQ.'BACK') GO TO 1660
      IF(KMD.EQ.'SKIP') GO TO 1680
      IF(KMD.EQ.'PZOT') GO TO 1700
      IF(KMD.EQ.'BZOT') GO TO 1730
C   
C   
C     ------------------------------------------------------------------
C     DECODE ALL THOSE LITTLE LISTS
C     ------------------------------------------------------------------
C   
 1300 CALL GREAD(IWD,LWD,ITYP,NF,WSET,12,NTRR)    !SET FIELD-W TO 12
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)       !DO THE GREAD BIT
      CALL GREAD(IWD,LWD,ITYP,NF,WSET,8,NTRR)     !SET FIELD-W TO 8
      IF(NTER.NE.0) GO TO 3000
C   
      CALL MILV3(LLWD(1),IV,ECA,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV3(LLWD(4),IV,ECB,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV3(LLWD(7),IV,ECC,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1320 CALL MILV(LIST(1),IV,FWA,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(3),IV,FWB,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(5),IV,FWC,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1340 CALL MILV(LIST(1),IV,ASLO,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(3),IV,ASHI,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1360 CALL MILV(LIST(1),IV,FWLO,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(3),IV,FWHI,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1380 CALL MILV(LIST(1),IV,FALO,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(3),IV,FAHI,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1400 CALL MILV(LIST(1),IV,XSTEP,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(3),IV,DXMAX,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1420 CALL MILV(LIST(1),IV,DEL,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(3),IV,DELFAC,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LIST(5),NMUL,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1440 CALL MILV(LIST(1),NBC,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1480 CALL MILV(LIST(1),KFUNS,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1500 KPPL=LIST(1)
      RETURN
C   
 1505 JYLOG=0
      JDSP='LIN '
      KDSP='LIN '
      RETURN
 1510 JYLOG=1
      JDSP='LOG '
      KDSP='LOG '
      RETURN
 1520 KINBAK='FREE'
      RETURN
 1530 KINBAK='FIX '
      RETURN
C   
 1560 IF(LIST(1).EQ.'UIND') GO TO 1570
      IF(LIST(1).EQ.'CIND') GO TO 1570
      IF(LIST(1).EQ.'ULOC') GO TO 1570
      IF(LIST(1).EQ.'CLOC') GO TO 1570
      IF(LIST(1).EQ.'FIX ') GO TO 1570
      GO TO 3000
C   
 1570 IF(KMD.EQ.'VX  ') KVAR(1)=LIST(1)
      IF(KMD.EQ.'VW  ') KVAR(2)=LIST(1)
      IF(KMD.EQ.'VALO') KVAR(3)=LIST(1)
      IF(KMD.EQ.'VAHI') KVAR(4)=LIST(1)
      RETURN
C   
 1584 KINFD='FIT '
      RETURN
 1586 KINFD='PKS '
      RETURN
 1587 KINFD='PPB '
      RETURN
C
 1588 CMINSY='VAR '
      CMAXSY='VAR '
      IF(NF.EQ.1) RETURN
      IF(NF.NE.3)   GO TO 3000
      IF(ITYP(2).EQ.1) GO TO 1590   
      CALL MILV(LIST(1),MINSY,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
 1590 IF(ITYP(3).EQ.1) RETURN
      CALL MILV(LIST(3),MAXSY,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      RETURN
C   
 1592 MARKS='ON  '
      RETURN
 1594 MARKS='OFF '
      RETURN
C
 1600 NWOOD=LIST(1)
      RETURN
C   
 1660 I=NF-1
      IF(I.LT.2) GO TO 1666
      IF(2*(I/2).NE.I) GO TO 1666
      NDO=I/2
      II=1
      DO 1664 I=1,NDO
      NBXY=NBXY+1
      IF(NBXY.GT.50) NBXY=50
C   
      DO 1662 JJ=1,2
      CALL MILV(LIST(II),IV,XYP(NBXY,JJ),KIND,IERR)
      IF(IERR.NE.0) GO TO 1670
      II=II+2
 1662 CONTINUE
 1664 CONTINUE
      RETURN
 1666 WRITE(CMSSG,1668)
 1668 FORMAT('ERROR - BGD SPEC IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C   
 1670 IF(II.EQ.1) GO TO 1666
      WRITE(CMSSG,1672)
 1672 FORMAT('ERROR ON BGD SPEC - BGD PARTLY SET')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C   
 1680 II=1
      DO 1684 J=1,4
      DO 1682 I=1,2
      CALL MILV(LIST(II),ISKIP(I,J),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1690
      II=II+2
 1682 CONTINUE
 1684 CONTINUE
      RETURN
C   
 1690 DO 1694 J=1,4
      DO 1692 I=1,2
      ISKIP(I,J)=0
 1692 CONTINUE
 1694 CONTINUE
      GO TO 3000
C   
 1700 NPAT=0
      RETURN
 1730 NBXY=0
      RETURN
C   
C   
 3000 IERR=1
      RETURN
      END
