C$PROG CKCAM
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CKCAM(C,N,A,F,FLO,FHI,IERR)
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
C     CHECK CAMAC C,N,A,F VALUES FOR LEGAL
C     FLO,FHI GIVE MIN & MAX VALUES ALLOWED FOR FUNCTION-CODE
C     ************************************************************
C
      IERR=0
C
      IF(C.GE. 0.AND.C.LE. 7) GO TO 200
CX    IF(C.GE.10.AND.C.LE.17) GO TO 200
C
      WRITE(CMSSG,110)C
  110 FORMAT('ILLEGAL CAMAC CRATE#      =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
C
  200 IF(N.LT.1.OR.N.GT.31) THEN
      WRITE(CMSSG,210)N
  210 FORMAT('ILLEGAL CAMAC SLOT#       =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(A.LT.0.OR.A.GT.31) THEN
      WRITE(CMSSG,310)A
  310 FORMAT('ILLEGAL CAMAC SUB-ADDRESS =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                            ENDIF
C
      IF(F.LT.FLO.OR.F.GT.FHI) THEN
      WRITE(CMSSG,410)F
  410 FORMAT('ILLEGAL CAMAC FUNC CODE   =',I12)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
                               ENDIF
C
      RETURN
      END
