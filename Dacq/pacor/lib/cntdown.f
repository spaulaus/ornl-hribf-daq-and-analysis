C$PROG CNTDOWN
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE CNTDOWN(IWD)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXG=100)
      PARAMETER (MXD=100)
C
      COMMON/PACJ/ GNAM(4,MXG),GTYP(MXG),PATN(MXG),GMSK(MXG),
     &             GLO(MXG),GHI(MXG),LPTR(MXG),RPTR(MXG),
     &             MPTR(MXG),GCNAF(5,MXG),NENT(MXG),NGAT,NGRED,
     &             PATNO,MSKNO
C
      COMMON/PACL/ CDPAT(MXD),CDMSK(MXD),CDCNT(MXD),NCDN
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(20),LWD(3,40),ITYP(40),NF
C
      INTEGER*4 GATNAM(4)
C
      REAL*4 XV
C
      character*4 cSETW
      equivalence(cSETW, SETW)
      DATA cSETW,NCDN/'SETW',0/
C
      SAVE
C
C     ************************************************************
C     DEFINITIONS FOR COMMON/PACL/ - KTH ENTRY
C     ************************************************************
C     CDPAT(K) = ASSOTIATED PATTERN WORD#
C     CDMSK(K) = ASSOCIATED MASK
C     CDCNT(K) = COUNT VALUE
C     NCDN     = NO. OF ENTRIES
C
C     ************************************************************
C
C     PROCESS $CDN  GATENAME(I)  COUNT
C
C     ************************************************************
C
      IF(NCDN.GE.MXD) GO TO 510                !TSTFR TABLE OVER
C
      CALL GREAD(IWD,LWD,ITYP,NF,SETW,12,NTER) !SET GREAD WIDTH=12
C
      CALL GREAD(IWD,LWD,ITYP,NF,6,80,NTER)    !REFORMAT LINE
C
      IF(NTER.NE.0) GO TO 520                  !TSTFR ERROR
C
      CALL MILV3(LWD(1,2),GDX,XV,KIND,IERR)    !GATE-NAME INDEX
      IF(IERR.NE.0) GO TO 530
      CALL MILV3(LWD(1,3),CNT,XV,KIND,IERR)    !COUNT
      IF(IERR.NE.0) GO TO 530
C
      DO 20 I=1,3
      GATNAM(I)=LWD(I,1)                       !LOAD GATE-NAME
   20 CONTINUE
      GATNAM(4)=GDX                            !LOAD GATE-NAME INDEX
C
      CALL NAMLOC(GNAM,GATNAM,NGAT,NDX)        !TSTFR EXIST
      IF(NDX.LE.0) GO TO 560
C
      NCDN=NCDN+1                              !INC COUNT-DOWN INDEX
C
      CDPAT(NCDN)=PATN(NDX)                    !LOAD PATTERN WD#
      CDMSK(NCDN)=GMSK(NDX)                    !LOAD MASK
      CDCNT(NCDN)=CNT                          !LOAD COUNT
      GO TO 1000
C
C     ************************************************************
C     SEND ANY ERROR MESSAGES
C     ************************************************************
C
  510 WRITE(CMSSG,515)NCDN
  515 FORMAT('COUNT-DOWN TABLE OVERFLOW AT - ',I6)
      GO TO 800
  520 WRITE(CMSSG,525)
  525 FORMAT('TRUNCATION ERROR FROM - GREAD')
      GO TO 800
  530 WRITE(CMSSG,535)
  535 FORMAT('SYNTAX ERROR FROM MILV3')
      GO TO 800
  560 WRITE(CMSSG,565)GATNAM
  565 FORMAT('COUNT-DOWN GATE-NAME NOT FOUND = ',3A4,I4)
      GO TO 800
C
  800 CALL ERRLOG(LOGUT,LOGUP)
C
 1000 CALL GREAD(IWD,LWD,ITYP,NF,SETW,8,NTER)    !RESET WIDTH TO 8
      RETURN
      END
