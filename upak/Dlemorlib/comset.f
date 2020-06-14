C$PROG COMSET    - Sets certain COMMON data for  LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE COMSET
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM02/ MBUF(4,5),JBN(4),IPO(4),NBC(4),LX,LY,LXB,LJP
      INTEGER*2    MBUF,     JBN,   IPO,   NBC
      INTEGER*4                                   LX,LY,LXB,LJP
C     ------------------------------------------------------------------
      COMMON/LM03/ LBUF(24576)
      INTEGER*4    LBUF
C     ------------------------------------------------------------------
      COMMON/LM04/ IONP,JONP(3)
      CHARACTER*4  IONP,JONP
C     ------------------------------------------------------------------
      COMMON/LM05/ IBUF(16384)
      INTEGER*4    IBUF
C     ------------------------------------------------------------------
      COMMON/LM06/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      COMMON/LM10/ FWLEN,NPARM,TOFSAVE
      INTEGER*4    FWLEN,NPARM,TOFSAVE
C     ------------------------------------------------------------------
      COMMON/LM11/ LHEP,LMSG,LLOD,LSAV,LSID,LTMP
      INTEGER*4    LHEP,LMSG,LLOD,LSAV,LSID,LTMP
C     ------------------------------------------------------------------
      COMMON/LM12/ MILF(262144)
      INTEGER*4    MILF
C     ------------------------------------------------------------------
      COMMON/LM13/ NUMINT,NUMOUT(3),NOUT(3),MTIN(6),MTOUT(6,3),NOSTR
      INTEGER*4    NUMINT,NUMOUT,   NOUT,   MTIN,   MTOUT,     NOSTR
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2                                OUBUF
C     ------------------------------------------------------------------
      COMMON/LM15/ LPHED(64),NDX(3)
      INTEGER*4    LPHED,    NDX
C     ------------------------------------------------------------------
      COMMON/LM16/ NRRED
      INTEGER*4    NRRED
C     ------------------------------------------------------------------
      COMMON/LM17/ NPARX,RESYN
      INTEGER*4    NPARX
      CHARACTER*4        RESYN
C     ------------------------------------------------------------------
      COMMON/LM18/ MOCMO
      CHARACTER*4  MOCMO
C     ------------------------------------------------------------------
      COMMON/LM19/ JCNF,IREBF
      CHARACTER*4  JCNF,IREBF
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM21/ NAMCMD(20)
      INTEGER*4    NAMCMD
C     ------------------------------------------------------------------
      COMMON/LM22/ NBRED,ICNF,INFLG
      INTEGER*4    NBRED
      CHARACTER*4        ICNF,INFLG
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM24/ LAUTO,INRECL
      CHARACTER*4  LAUTO
      INTEGER*4          INRECL
C     ------------------------------------------------------------------
      COMMON/LM25/ LUREC,OFLG
      INTEGER*4    LUREC
      CHARACTER*4        OFLG
C     ------------------------------------------------------------------
      COMMON/LM26/ LDFONAM,LDFINAM
      CHARACTER*80 LDFONAM,LDFINAM
C
      DATA         LDFONAM,LDFINAM/' ',' '/
C     ------------------------------------------------------------------
      COMMON/LM27/ EVLNAM
      CHARACTER*80 EVLNAM
C     ------------------------------------------------------------------
      COMMON/LM28/ UBUFF(2048),TOFFSET
      INTEGER*4    UBUFF,      TOFFSET
C     ------------------------------------------------------------------
      COMMON/LM29/ FMTI,FMTO
      CHARACTER*4  FMTI,FMTO
C     ------------------------------------------------------------------
      COMMON/LM30/ GATNAM(20),GATLO(1000),GATHI(1000),GATMX,GATOP,LUGAT
      INTEGER*4    GATNAM,    GATLO,      GATHI,      GATMX,      LUGAT
      CHARACTER*4                                           GATOP
C
      DATA         GATLO/1000*1/
      DATA         GATHI/1000*0/
      DATA         GATMX/1000/
      DATA         GATOP/'NO  '/
      DATA         LUGAT/18/
C     ------------------------------------------------------------------
      COMMON/LM32/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC
      INTEGER*4    CLIDHI,CLIDLO,VMETIM,VMETLO,VMETHI
      CHARACTER*16                                    VMETIMC
C
      DATA         CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM/5*0/
      DATA         VMETIMC/' '/
C     ------------------------------------------------------------------
      COMMON/LM33/ UDFNAM(20),UDFRECL,UDFNPAR,UDFRECI
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR,UDFRECI
C
      CHARACTER*80 CUDFNAM
C
      DATA         CUDFNAM/'Undefined'/
      DATA         UDFRECL,UDFNPAR,UDFRECI/0,0,0/
C     ------------------------------------------------------------------
C
      INTEGER*4    I,J
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ==================================================================
C     LOGICAL UNITS
C     ------------------------------------------------------------------
C     LU#     USE
C       1     NOT USED
C       2     NOT USED
C       3     NOT USED
C       4     NOT USED
C       5     VDT INPUT
C       6     VDT OUTPUT
C       7     LIST FILE/DEV
C       8     HELP-FILE, MIL-FILE, TEX-FILE, ETC. CLOSED AFTER USE
C       9     NOT USED
C      10     CMD-FILE
C      11     INPUT    TAPE/FILE
C      12     OUTPUT-1 TAPE/FILE
C      13     OUTPUT-2 TAPE
C      14     OUTPUT-3 TAPE
C      15     INPUT    LDF-FILE
C      16     OUTPUT   LDF-FILE
C      17     OUTPUT   RECLOG-FILE
C      18     GAT-FILE INPUT
C     ------------------------------------------------------------------
C     COMMON DEFINITIONS
C     ------------------------------------------------------------------
C     LIN     = LU FOR INPUT TAPE/FILE = 11
C     LUCI    = CHANNEL # ASSIGNED TO INPUT (IF ANY)
C     KFI     = 'TAPE'/'FILE' FOR INPUT FROM TAPE/FILE
C     LINO    = 'YES'/'NO' SAYS INPUT OPEN/NOT-OPEN
C     LUCO(I) = CHANNEL # ASSIGNED TO OUTPUT-STREAM-I (IF ANY)
C     LOU(I)  = LU FOR OUTPUT-STREAM-I = I+11
C     KFO(I)  = 'TAPE'/'FILE' FOR OUTPUT-STREAM-I TO TAPE/FILE
C     LOUO(I) = 'YES'/'NO' SAYS OUTPUT-I OPEN/NOT-OPEN
C     LTX     = LU FOR TEXT STORAGE FILE = 8
C     LML     = LU FOR MIL-FILE = 8
C     LMLO    = 'YES'/'NO' SAYS MIL-FILE OPEN/NOT-OPEN
C
C     IBUF    = INPUT BUFFER FOR INPUT TAPE/FILE
C
C     NPAR    = # OF PARMS/EVENT FROM MIL-FILE
C     LSTL    = INPUT TAPE LIST-LENGTH (HALF-WDS)
C     LNBY    = INPUT TAPE LIST-LENGTH (BYTES)
C     MAXIP   = MAX # OF INPUT PARAMETERS   (TENNECOMP ONLY)
C     NSKIP   = # WDS TO SKIP IN INPUT BUFF (TENNECOMP ONLY)
C     ITENN   = 'YES'/'NO' SAYS TENNECOMP/NOT-TENNECOMP
C
C     MSSG    = LOG-MESSAGE BUFFER
C     LISFLG  = 'LON'/'LOF' SAYS LOG/DON'T-LOG ALL DIALOG
C
C     MILF    = CONTANS MIL-FILE (I.E. CODE FROM CHIL)
C
C     MSGF    = '    '/'XXXX' SAYS INTERRUPT FLAG ON/OFF
C
C     MOCMO   = 'UPON' SAYS USER PROCESSING ON  (I.E. CALLS USERMOC)
C     MOCMO   = 'UPOF' SAYS USER PROCESSING OFF (I.E. CALLS   PVALL)
C
C     NUMINT       = BUFFER COUNTER FOR INPUT STREAM
C     NUMOUT(I)    = BUFFER COUNTER FOR OUTPUT-STREAM-I
C     NOUT(I)      = BUFFER COUNTER FOR OUTPUT-STREAM-I
C     MTIN         - CONTAINS NAME OF TAPE/FILE FOR INPUT
C     MTOUT( ,I)   - CONTAINS NAME OF TAPE/FILE FOR OUTPUT-STREAM-I
C     NOSTR        = # OF OUTPUT STREAMS
C
C     OUBUF        - PING-PONG BUFFERS FOR UP TO 3 OUTPUT STREAMS
C     OUBUF( ,1,J) = PING-BUFFER FOR OUTPUT-STREAM-J
C     OUBUF( ,2,J) = PONG-BUFFER FOR OUTPUT-STREAM-J
C
C     LPHED        - CONTAINS LAST PRIMARY HEADER RECORD
C     OUSIZ        = OUTPUT BUFFER SIZE (BYTES) - ALL STREAMS
C     OUPTR(I)     = OUTPUT BUFFER POINTER FOR STREAM-I
C     NDX(I)       = PING-PONG INDEX FOR OUTPUT STREAM-I
C
C     IWD          = COMMAND INPUT BUFFER
C     LWD          = REFORMATTED COMMAND INPUT BUFFER
C     ITYP         = FIELD-TYPE FOR LWD (REFORMATTED CMD BUFFER)
C     NAMCMD       - CONTAINS NAME OF CURRENT CMD-FILE/DEV
C     ------------------------------------------------------------------
C
      LUINF=15
      LUOUF=16
C
      INFOP='NO  '
      OUFOP='NO  '
C
      NBRED=0
      ICNF='NO  '
C
      LIN=11
      LUCI=-1
      LINO='NO  '
      IONP='NO  '
C
      DO 20 I=1,3
      LOU(I)=I+11
      LUCO(I)=-1
      LOUO(I)='NO  '
      NUMOUT(I)=0
      OUPTR(I)=0
      OOFSET(I)=-1
      JONP(I)='NO  '
      NDX(I)=0
      DO 10 J=1,6
      MTOUT(J,I)=Z'20202020'
   10 CONTINUE
   20 CONTINUE
C
      LTX=8
      LML=8
      LMLO='NO  '
      NUMINT=0
      NSKIP=0
      JCNF='NO  '
      IREBF='NO  '
      LFORM='L003'
      FMTI= 'L003'
      FMTO= 'L003'
      ISWAB='NO  '
      ISWAH='NO  '
C
      MOCMO='UPON'
      NOSTR=1
C
      LNBY=32768
      LSTL=16384
C
      CALL UPVSETUP(0)
      CALL UPRSCRUB
C
      DO 30 I=1,3
      OUPTR(I)=0
      OUSIZ(I)=16384
      OOFSET(I)=32768*(I-1)
      NDX(I)=1
   30 CONTINUE
C
      KINPUT='    '
      INRECN=0
C
      LAUTO='YES '
C     
      RETURN
      END
