C$PROG COMSET    - Preloads certain common blocks
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/02/98
C     ******************************************************************
C
      SUBROUTINE COMSET
C
C     ==================================================================
      IMPLICIT NONE
C     ==================================================================
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
      COMMON/SC01/ NAMCMD(20)
      INTEGER*4    NAMCMD
C     ------------------------------------------------------------------
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC04/ JCNF,IHEDN,MBFL
      INTEGER*4         IHEDN,MBFL
      CHARACTER*4  JCNF
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC06/ LIST(16384,2)                                               
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/SC07/ MILF(262144)
      INTEGER*4    MILF
C     ------------------------------------------------------------------
      INTEGER*4    CLX,  CLY
C
      PARAMETER   (CLX=2,CLY=2048)            !MBUF DIMENSIONS
C
      COMMON/SC08/ MBUF,JBN,IPO,NBC,LX,LY,LXB,LJP
C
      INTEGER*2    MBUF(0:CLX-1,0:CLY)        !PLUS A SPARE

      INTEGER*2    JBN(0:CLY-1),IPO(0:CLY-1),NBC(CLY)

      INTEGER*4    LX,LY,LXB,LJP
C     ------------------------------------------------------------------
      COMMON/SC09/ JHPC(4096)                                                  
      INTEGER*4    JHPC
C     ------------------------------------------------------------------
      INTEGER*4    DUM
C
      PARAMETER   (DUM=4)                    !DUMMY DIMENSION
C
      COMMON/SC10/ OPTR(3),OOFSET(3),OSIZE(3),OUTBUF
C
      INTEGER*4    OPTR,   OOFSET,   OSIZE
C
      INTEGER*4    OUTBUF(0:DUM)
C     ------------------------------------------------------------------
      COMMON/SC11/ FWLEN,NPAR,TOFSAVE
      INTEGER*4    FWLEN,NPAR,TOFSAVE
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C
      DATA         INTYP,LUINF/'    ',31/
C     ------------------------------------------------------------------
      COMMON/SC22/ IPCNAM(20),LDFNAM(20),TAPNAM(20),BANNAM(20)
      INTEGER*4    IPCNAM,    LDFNAM,    TAPNAM,    BANNAM
C
      CHARACTER*80 CIPCNAM,   CLDFNAM,   CTAPNAM,   CBANNAM
C
      EQUIVALENCE (CIPCNAM,IPCNAM),
     &            (CLDFNAM,LDFNAM),
     &            (CTAPNAM,TAPNAM),
     &            (CBANNAM,BANNAM)
C
      DATA         CIPCNAM/'Undefined'/
      DATA         CLDFNAM/'Undefined'/
      DATA         CTAPNAM/'Undefined'/
      DATA         CBANNAM/'Undefined'/
C     ------------------------------------------------------------------
      COMMON/SC23/ GATNAM(20),GATLO(1000),GATHI(1000),GATMX,GATOP,LUGAT
      INTEGER*4    GATNAM,    GATLO,      GATHI,      GATMX,      LUGAT
      CHARACTER*4                                           GATOP
C     ------------------------------------------------------------------
      DATA         GATLO/1000*1/
      DATA         GATHI/1000*0/
      DATA         GATMX/1000/
      DATA         GATOP/'NO  '/
      DATA         LUGAT/15/
C     ------------------------------------------------------------------
      COMMON/SC27/ BNDX
      INTEGER*4    BNDX
C
      DATA         BNDX/1/
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C
      DATA         CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM/5*0/
      DATA         VMETIMC,CLONOF/' ','CLOF'/
C     ------------------------------------------------------------------
      COMMON/SC29/ UDFNAM(20),UDFRECL,UDFNPAR
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR
C
      CHARACTER*80 CUDFNAM
C
      DATA         CUDFNAM/'Undefined'/
      DATA         UDFRECL,UDFNPAR/0,0/
C     ------------------------------------------------------------------
C
      SAVE
C
      RETURN
      END
