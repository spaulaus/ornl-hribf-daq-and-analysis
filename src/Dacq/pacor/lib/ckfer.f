C$PROG CKFER
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKFER(C,N,A,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
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
C     CHECK FERA  C,N,A VALUES FOR LEGAL
C     ************************************************************
C
      IERR=0
C
      IF(C.GE. 0.AND.C.LE. 7) GO TO 200
      IF(C.GE.10.AND.C.LE.17) GO TO 200
C
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL FERRA CRATE#      =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
C
  200 IF(N.LT.1.OR.N.GT.31) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL FERRA SLOT#       =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.31) THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL FERRA SUB-ADDRESS =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      END
