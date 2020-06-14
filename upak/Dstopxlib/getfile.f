C$PROG GETFILE   - Gets file for Eloss storage
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE GETFILE
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
      COMMON/III/  LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/STX04/   IFILE, DEP
      LOGICAL         IFILE
      REAL*4                 DEP
C     ------------------------------------------------------------------
      CHARACTER*80    CNAMFIL
C
      INTEGER*4       IBUF(505),NAMFIL(20)
C
      INTEGER*4       I,LOUT,IERR
C
      DATA            LOUT/11/
C
      EQUIVALENCE    (CNAMFIL,NAMFIL)
C
      SAVE
C
C     ------------------------------------------------------------------
C
   12 FORMAT(1H ,19A4,A2)
C
      WRITE(LOGUT,5000)
      WRITE(LOGUP,5000)
C
 5000 FORMAT(' FILE WILL CONTAIN ENERGY LOSS INFORMATION IN THE',
     1 ' FOLLOWING FORMAT:',
     2 //,' REC#1:  TITLE',
     3 /,' REC#2:  Z1,E0,NE,DE,E-EXIT1,E-EXIT2,...,E-EXIT-NE.'
     4 /,' REC#3:  Z2,E0,NE,DE,E-EXIT1,E-EXIT2,...,E-EXIT-NE.'
     5 /,20X,'.',/,20X,'.',/,20X,'.',/,' WHERE:   Z = Z OF PROJECTILE',
     6 /,'         E0 = FIRST INCIDENT ENERGY',
     7 /,'         NE = NUMBER OF INCIDENT ENERGIES',
     8 /,'         DE = INCIDENT ENERGY STEP',/,'         E-EXIT-I =',
     9  ' EXIT ENERGY FOR PROJ. OF INCIDENT ENERGY E=E0+(I-1)*DE')
C
      WRITE(LOGUT,5001)
      WRITE(LOGUP,5001)
C
 5001 FORMAT(/,' ALL ENERGIES ARE IN KEV/A - FORMAT IS 10I8')
C
      WRITE(LOGUT,5010)
      WRITE(LOGUP,5010)
C
 5010 FORMAT(/,' ENTER NAME OF OUTPUT FILE.')
C
      READ(LIN,5100,END=500,ERR=500)NAMFIL
C
 5100 FORMAT(20A4)
C
      IF(LIN.NE.5) WRITE(LOGUT,12)NAMFIL
C
      IFILE=.FALSE.
C
      CALL FOPEN(CNAMFIL,LOUT,IERR)
C
      IF(IERR.NE.0) GO TO 500
      IFILE=.TRUE.
C
      WRITE(LOGUT,5300)
C
 5300 FORMAT(1H ,'ENTER TITLE')
C
      READ(LIN,5100,END=500,ERR=500)(IBUF(I),I=1,20)
C
      IF(LIN.NE.5) WRITE(LOGUT,12)(IBUF(I),I=1,20)
C
      WRITE(LOUT,5100)(IBUF(I),I=1,20)
C
      DO 570 I=1,20
      IBUF(I)=0
  570 CONTINUE
      GO TO 500
C
C
  500 RETURN
      END
