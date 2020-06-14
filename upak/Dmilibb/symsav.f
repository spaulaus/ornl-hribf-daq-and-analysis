C$PROG SYMSAV    - Saves a symbol name and associated value in table
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE SYMSAV(KSYM,IV,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML03/ ISYN(500),ISYV(500),NSYM
C
      CHARACTER*8  ISYN
C   
      CHARACTER*8  RSYM(22),KSYM
C
      DATA RSYM/'X   ','UIND','CIND','ULOC','CLOC','FIX ','NONE',
     &          'FITS','ALL ','M   ','N   ','O   ','P   ','Q   ',
     &          'R   ','S   ','COLR','GREY','DOTS','LIVE','BAN ',
     &          'NUID'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO SAVE AND ASSIGN ENTRIES IN SYMBOL TABLE
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      DO 10 I=1,22
      IF(KSYM.EQ.RSYM(I)) GO TO 100
   10 CONTINUE
C   
      DO 20 I=1,NSYM
      IF(KSYM.NE.ISYN(I)) GO TO 20
      GO TO 30
   20 CONTINUE
C   
      IF(NSYM.GT.499) GO TO 120
C   
      NSYM=NSYM+1
      ISYN(NSYM)=KSYM
      ISYV(NSYM)=IV
      RETURN
C   
   30 ISYV(I)=IV
      RETURN
C   
  100 WRITE(CMSSG,110)KSYM
  110 FORMAT(A4,' is a reserved symbol - CMD ignored')
      GO TO 200
C   
  120 WRITE(CMSSG,130)KSYM
  130 FORMAT('SYMBOL TABLE OVERFLOW (MAX=500) SAVING - ',A)
  200 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
