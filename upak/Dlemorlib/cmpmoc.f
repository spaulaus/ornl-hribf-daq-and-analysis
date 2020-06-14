C$PROG CMPMOC    - Command processor for LEMOR - MOC
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CMPMOC(IDONE,IERR)
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
      COMMON/LM22/ NBRED,ICNF,INFLG
      INTEGER*4    NBRED
      CHARACTER*4        ICNF,INFLG
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      REAL*4       UMB,RMB,ERATE
C
      INTEGER*4    MTUNIT,LUCII,KK,NERR,KERR,LUCOO
C
      INTEGER*4    IERR,KS,LGO,I
C
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C     
C     ------------------------------------------------------------------
C     Process commands
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'STX ') GO TO 250
C
      IF(KMD.EQ.'MOC ') GO TO 2000
      IF(KMD.EQ.'MOCE') GO TO 2000
C
      IF(KMD.EQ.'INIT') GO TO 2020
      IF(KMD.EQ.'ZBUC') GO TO 2030
C
      RETURN
C
C
C     ------------------------------------------------------------------
C     DISPLAY/LOG EXABYTE STATUS INFORMATION
C     ------------------------------------------------------------------
C
  250 IF(INTYP.NE.'TAPE') GO TO 255
      CALL EXABSTAT(2,LUCI,UMB,RMB,ERATE,NERR,KERR)
      WRITE(CMSSG,252)UMB,RMB,NERR,ERATE
  252 FORMAT('MB-used, MB-left, #-ERR, ERR/MB =',2F10.3,I8,F8.2,
     &       ' - INPUT')
      CALL MESSLOG(LOGUT,LOGUP)
C
  255 IF(OUTYP.NE.'TAPE') GO TO 5500
C
      DO 260 KK=1,3
      IF(LOUO(KK).NE.'YES ') GO TO 260
      CALL EXABSTAT(2,LUCO(KK),UMB,RMB,ERATE,NERR,KERR)
      WRITE(CMSSG,257)UMB,RMB,NERR,ERATE,KK
  257 FORMAT('MB-used, MB-left, #-ERR, ERR/MB =',2F10.3,I8,F8.2,
     &       ' - OUT-',I1)
      CALL MESSLOG(LOGUT,LOGUP)
  260 CONTINUE
      GO TO 5500
C
C     ------------------------------------------------------------------
C     PROCESS - MOC & MOCE COMMANDS
C     ------------------------------------------------------------------
C
 2000 CALL MOC(ICNF,LGO)
C
      IF(LGO.EQ.0) GO TO 5500
      GO TO 6000
C
 2020 DO 2025 KS=1,3
      OUPTR(KS)=0
      OOFSET(KS)=32768*(KS-1)
      NDX(KS)=1
 2025 CONTINUE
      GO TO 5500
C
 2030 NUMINT=0
      DO 2035 I=1,3
      NUMOUT(I)=0
 2035 CONTINUE
      GO TO 5500
C
C     ------------------------------------------------------------------
C     NORMAL RETURN
C     ------------------------------------------------------------------
C
 5500 IDONE='YES '
      IERR=0
      RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURN
C     ------------------------------------------------------------------
C
 6000 IDONE='YES '
      IERR=1
      RETURN
      END
