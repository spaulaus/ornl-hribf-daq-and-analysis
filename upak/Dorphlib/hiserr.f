C$PROG HISERR    - Displays error messages associated with his-file I/O
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HISERR(IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      INTEGER*4 MESS(10,7)
      CHARACTER CMESS(7)*40
      EQUIVALENCE (CMESS(1),MESS(1,1))
C
      DATA CMESS/'REQUESTED ID DOES NOT EXIST            ',
     2           'INVALID CHANNEL#,S REQUESTED           ',
     3           'I/O ERROR READING IN DIRECTORY         ',
     4           'I/O ERROR READING IN DATA              ',
     5           '2-D DATA READ  -  1-D DATA EXPECTED    ',
     6           '1-D DATA READ  -  2-D DATA EXPECTED    ',
     7           'UNDEFINED HISIN ERROR                  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IERR.EQ.0) RETURN
      J=IERR
      IF(J.LT.0.OR.J.GT.6) J=7
      WRITE(CMSSG,10)(MESS(I,J),I=1,10)
   10 FORMAT('HISIN ERROR - ',10A4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
