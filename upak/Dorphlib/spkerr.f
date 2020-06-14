C$PROG SPKERR    - Displays SPKIO error messages
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE SPKERR(IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      INTEGER*4 MESS(8,9)
      CHARACTER CMESS(9)*32
      EQUIVALENCE (CMESS(1),MESS(1,1))
C
      DATA CMESS/
     1'REQUESTED ID NOT FOUND          ',
     2'INVALID VALUE OF CHANNEL #      ',
     3'DIRECTORY OVERFLOW              ',
     4'I/O ERROR OF SOME SORT          ',
     5'ILLEGAL REQUEST MODE            ',
     6'ID TO BE DELETED NOT FOUND      ',
     7'ID TO BE STORED ALREADY EXISTS  ',
     8'FILE NOT OPEN                   ',
     9'ERROR CODE UNDEFINED            '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OUTPUT "SPKIO" ERROR MESSAGES
C     ------------------------------------------------------------------
C
      IF(IERR.EQ.0) RETURN
      J=IERR
      IF(J.LT.1.OR.J.GT.9) J=9
C
      WRITE(CMSSG,10)(MESS(I,J),I=1,8)
   10 FORMAT('SPKIO ERROR - ',8A4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
