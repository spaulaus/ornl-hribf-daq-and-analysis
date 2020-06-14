C$PROG IOFERR    - Displays I/O error messages
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/16/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE IOFERR(IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      EQUIVALENCE (CMSER,MSER)
C
      INTEGER*4 MSER(20)
C
      CHARACTER*80 CMSER
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
      CALL GET_Fortran_ERROR(IOS,CMSER)
C
      DO 20 I=1,20
      MSSG(I)=MSER(I)
   20 CONTINUE
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
