C$PROG CKFAS
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKFAS(C,N,A,IERR)
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
C     CHECK FASTBUS C,N,A VALUES FOR LEGAL
C     ************************************************************
C
      IERR=0
C
      IF(C.LT.1.OR.C.GT.1) THEN
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL FASTBUS CRATE#    =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                           ENDIF
C
      IF(N.LT.0.OR.N.GT.25) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL FASTBUS SLOT#     =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.255)THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL FASTBUS SUB-ADDR  =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      END
