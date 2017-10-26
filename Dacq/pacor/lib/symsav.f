C$PROG SYMSAV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE SYMSAV(KSYM,IV,IERR)
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     ROUTINE TO SAVE AND ASSIGN ENTRIES IN SYMBOL TABLE
C     ************************************************************
C   
      IERR=0
C   
      DO 20 I=1,NSYM
      IF(KSYM.NE.ISYN(I)) GO TO 20
      GO TO 30
   20 CONTINUE
C   
      IF(NSYM.GT.99) GO TO 120
C   
      NSYM=NSYM+1
      ISYN(NSYM)=KSYM
      ISYV(NSYM)=IV
      RETURN
C   
   30 ISYV(I)=IV
      RETURN
C   
  120 WRITE(CMSSG,130)KSYM
      CALL ERRLOG(LOGUT,LOGUP)
  130 FORMAT('SYMBOL TABLE OVERFLOW (MAX=100) SAVING - ',A4)
      IERR=1
      RETURN
      END
