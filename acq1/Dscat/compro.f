C$PROG COMPRO
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE COMPRO
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (NSC=1024)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C     ------------------------------------------------------------------
      COMMON/SCAT0/ SCATBUF(8000,2),NSCAT,SCATDMP,SCATCLR,SCATERR
      CHARACTER*4                         SCATDMP,SCATCLR
      INTEGER*4     SCATBUF,        NSCAT,                SCATERR
C     ------------------------------------------------------------------
      COMMON/SCAT1/ LA(3,NSC),CN(NSC),SN(NSC), A(NSC), F(NSC),TY(NSC),
     &                        KI(NSC),VN(NSC),VO(NSC),VD(NSC),PV(NSC),
     &                        LO(NSC),HI(NSC),NR,NT,NORI,NORF
      REAL*4        NORF
C     ------------------------------------------------------------------
      COMMON/SCAT2/ POL(NSC),GOL(NSC),ECN(20),ESN(20),NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SCAT4/ LISI(400),SYM(3,50),OPR(50),NLS,NSY,DSPF
C     ------------------------------------------------------------------
C
      SAVE
C
C     ******************************************************************
C     ROUTINE TO PROCESS SYMBOL-OPERATOR LIST INTO POINTER-GOTO LIST
C     ******************************************************************
C
      IF(NSY.LE.0)  RETURN              !TST FOR EMPTY
      IF(NERR.NE.0) RETURN              !TST FOR ERROR STATE
C
      IF(LABNDX(SYM(1,1)).NE.0)GOTO 100 !ERROR IF DEFINED SYMBOL EXIST
C
      NT=NT+1                           !INC TOTAL # SCALERS
      IF(NT.GT.MAXT) GO TO 110          !TST FOR TABLE OVERFLOW
C
      DO 10 I=1,3                       !SAVE DEFINED SYMBOL
      LA(I,NT)=SYM(I,1)
   10 CONTINUE
      KI(NT)=DSPF
      LO(NT)=NPO+1                      !SET LO-LIMIT INDEX IN POL-GOL
C
      DO 20 I=2,NSY                     !LOOP ON REMAINING SYMBOLS
      NPO=NPO+1                         !INC POL-GOL COUNTER
      IF(NPO.GT.MAXT) GO TO 120         !TST FOR TABLE OVERFLOW
      POL(NPO)=LABNDX(SYM(1,I))         !SET SYMBOL INDEX IN POL
      IF(POL(NPO).LE.0) GO TO 130       !TST FOR NON-EXIST
      GOL(NPO)=OPR(I)                   !SET OPERATOR NO. IN GOL
   20 CONTINUE
C
      HI(NT)=NPO                        !HI-LIMIT INDEX IN POL-GOL
C
      RETURN
C
  100 WRITE(CMSSG,105)(SYM(K,1),K=1,3)
  105 FORMAT('ATTEMPT TO RE-DEFINE SCAT SYMBOL - ',3A4)
      GO TO 200
  110 WRITE(CMSSG,115)MAXT
  115 FORMAT('MORE THAN',I4,' SCAT SYMBOLS DEFINED')
      GO TO 200
  120 WRITE(CMSSG,125)MAXT
  125 FORMAT('SCAT TABLE OVERFLOW AT',I4,' ENTRIES')
      GO TO 200
  130 WRITE(CMSSG,135)(SYM(K,I),K=1,3)
  135 FORMAT('UNDEFINED SCAT SYMBOL REFERENCED = ',3A4)
      GO TO 200
C
  200 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,205)
  205 FORMAT('Scaler-dump-to-tape Disabled!!')
      CALL MESSLOG(LOGUT,LOGUP)
      SCATERR=1
      RETURN
      END
