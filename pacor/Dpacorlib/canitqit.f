C$PROG CANITQIT
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CANITQIT
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER 
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      CHARACTER*4   CLWD(2,40)
C
      EQUIVALENCE (CMSSG,MSSG),(CLWD,LWD)
C
      INTEGER*4 IV(4),C,N,A,F,ICNAF
C
      BYTE BCNAF(4)
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,IWD)
C
      EQUIVALENCE (BCNAF,ICNAF)
C
      EQUIVALENCE (C,IV(1)),(N,IV(2)),(A,IV(3)),(F,IV(4))
C
      DATA NNAFI,NNAFQ,NNAFR,MXNAF/0,0,0,200/
C
      SAVE
C
C     ************************************************************
C     PROCESS $INI, $DUN & $RUN DIRECTIVES - INIT & WRAPUP CNAFS
C     ************************************************************
C
      DO 50 I=1,4
      CALL MILV(LWD(1,I+2),IV(I),XV,KIND,IERR)
   50 CONTINUE
C
      JV=0
      IF(CLWD(1,7).EQ.'    '.AND.CLWD(2,7).EQ.'    ') GO TO 55
C
      CALL HEXVAL(LWD(1,7),JV,IERR)
C
   55 CALL CKFRITE(C,N,A,F,IERR)
      IF(IERR.NE.0) RETURN
C
      JJ=4
      DO 60 I=1,4
      BCNAF(I)=IV(JJ)
      JJ=JJ-1
   60 CONTINUE
C
      IF(KMD.EQ.'$DUN') GO TO 100
      IF(KMD.EQ.'$RUN') GO TO 200
C
C     ************************************************************
C     PROCESS $INI C,N,A,F,DATA
C     ************************************************************
C
      NNAFI=NNAFI+1
C
      IF(NNAFI.GT.MXNAF) THEN
      WRITE(CMSSG,65)NNAFI
   65 FORMAT('MAX# $INI CNAFs EXCEEDED AT NNAFI =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NNAFI=MXNAF
                         ENDIF
C
      CNAFI(NNAFI)=ICNAF
      DATAI(NNAFI)=IAND(JV,'FFFF'X)
      RETURN
C
C     ************************************************************
C     PROCESS $DUN C,N,A,F,DATA
C     ************************************************************
C
  100 NNAFQ=NNAFQ+1
C
      IF(NNAFQ.GT.MXNAF) THEN
      WRITE(CMSSG,105)NNAFQ
  105 FORMAT('MAX# $DUN CNAFs EXCEEDED AT NNAFQ =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NNAFQ=MXNAF
                         ENDIF
C
      CNAFQ(NNAFQ)=ICNAF
      DATAQ(NNAFQ)=IAND(JV,'FFFF'X)
      RETURN
C
C     ************************************************************
C     PROCESS $RUN C,N,A,F,DATA
C     ************************************************************
C
  200 NNAFR=NNAFR+1
C
      IF(NNAFR.GT.MXNAF) THEN
      WRITE(CMSSG,205)NNAFR
  205 FORMAT('MAX# $RUN CNAFs EXCEEDED AT NNAFR =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      NNAFR=MXNAF
                         ENDIF
C
      CNAFR(NNAFR)=ICNAF
      DATAR(NNAFR)=IAND(JV,'FFFF'X)
      RETURN
      END
