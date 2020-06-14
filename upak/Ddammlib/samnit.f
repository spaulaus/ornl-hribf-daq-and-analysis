C$PROG SAMNIT    - Sets up default fit parameters & constants
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE SAMNIT
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
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
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
C     ------------------------------------------------------------------
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
      CHARACTER*4            KINFD
      CHARACTER*4  CMINSY,CMAXSY
      EQUIVALENCE (CMINSY,MINSY),(CMAXSY,MAXSY)
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM09/ XYP(50,2),NBXY
C   
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C
      DATA         MARFS/80*'ON  '/
C     ------------------------------------------------------------------
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A,       B,       DET
C     ------------------------------------------------------------------
      COMMON/SM15/ IXC(50),YC(50),XCOR(50)
C   
      COMMON/SM16/ IHOLF(4,44),JPU(44),XOR(44)
C   
      COMMON/SM17/ IORD(500),ITEM(500),LOUXX(14,50)
C   
      COMMON/SM18/ PLIM(352),GUESS(176),PARV(176)
C     ------------------------------------------------------------------
      COMMON/SM19/ ISKIP(2,4),JSKIP(2,4),PMIN(4),PMAX(4),KVAR(4)
      CHARACTER*4                                        KVAR
C     ------------------------------------------------------------------
      COMMON/SM20/ RESUL(12,44),FOUT(50),ITRITL(10)
C   
      COMMON/SM21/ NDXI(4),JDATE(3),JTIME(2),TITLE(20)
C   
      COMMON/SM22/ IDATA(2048),IDAT(2048,3),ISKF(2048)
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
      DIMENSION DAT(2048,3)
C
      INTEGER*4 LAT(500,15),LLWD(80),LWDL(10),LIST(78),BLANK
      character*4 cblank
      equivalence (cblank,blank)
C
      INTEGER*4 TITLE
C     ------------------------------------------------------------------
C   
      EQUIVALENCE (DAT,IDAT)
C   
      EQUIVALENCE (LAT(1,1),PAT(1,1))
C   
      EQUIVALENCE (LIST(1),LWD(1,2))
      EQUIVALENCE (KMD    ,LWD(1,1)),
     &            (LWDL(1),LWD(1,1)),
     &            (LLWD(1),LWD(1,1))
C   
      DATA         SETW,cBLANK/'SETW','    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     SET UP CONSTANTS AND DEFAULTS
C     ------------------------------------------------------------------
C   
      KFIT=1
      JYLOG=0
      JDSP='LIN '
      KDSP='LIN '
C   
      MXDL=16384
      MXFR=2048
      MAXH=128
      NOP=0
      NBC=2
      KINBAK='FREE'
C   
      JLOX=-1
      JHIX=-1
      JLOF=-1
      JHIF=-1
      SLOX=-1.0
      SHIX=-1.0
      SLOF=-1.0
      SHIF=-1.0
C   
      MARKS='ON  '
      KINFD='FIT '
      CMINSY='VAR '
      CMAXSY='VAR '
      ECA=0.0
      ECB=1.0
      ECC=0.0
      FWA=5.0
      FWB=0.0
      FWC=0.0
C   
C   
      DEL=0.05
      DELFAC=0.25
      NMUL=3
      XSTEP=0.2
      DXMAX=5.0
      DXFAC=XSTEP/DEL
      FWLO=0.5
      FWHI=2.0
      FALO=0.5
      FAHI=2.0
      ASLO=0.0
      ASHI=0.0
      MAXNC=25
      MAXPAT=499
      NPAT=0
      NUPAT=1
      NBXY=0
      MAXVP=176
      KFUNS=0
      NWOOD='OFF '
      KPPL='NONE'
C   
      ASUM=0.0
      ID=0
      NSKIP=0
      RESOK='NO  '
      ILO=0
      IHI=0
C   
      DO 10 J=1,4
      NDXI(J)=0
      KVAR(J)='FIX '
      DO 10 I=1,2
      ISKIP(I,J)=0
   10 CONTINUE
      KVAR(1)='CIND'
      KVAR(2)='CLOC'
      IVF(1)=1
      DO 12 I=2,4
      IVF(I)=0
   12 CONTINUE
C   
      DO 14 I=1,5
      MDYHMS(I)=BLANK
   14 CONTINUE
C   
      RETURN
      END
