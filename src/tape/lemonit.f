C$PROG LEMONIT   - Initializing routine for LEMOR
*****************************************************************************
*
*      Modified LEMOR routine for data acquisition use
*
*   4/29/99
*****************************************************************************
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 12/23/98
C     ******************************************************************
C
      SUBROUTINE LEMONIT
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
      COMMON/LM06/ IHEPF
      INTEGER*4    IHEPF
C     ------------------------------------------------------------------
      character*4  cNAMPROG(2)
      equivalence  (cNAMPROG, NAMPROG)
      DATA         cNAMPROG/'FILE','OU  '/
C
      INTEGER*4    NERR,NCON,I
      integer      mlen
C

      SAVE
C
C     ------------------------------------------------------------------
      character*80 cfilename/'vme'/
      integer*4    inrecl,ierr,iargc
C
      cmssg = ' '
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      OPEN(UNIT       = 7,
     &     FILE       = '/dev/null',
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'SEQUENTIAL')
C     
      CALL CTCNIT
C     
      if (iargc() .eq. 1) call getarg(1,cfilename)
      call open_acq_ipc(cfilename,inrecl,ierr)
      if (ierr .ne. 0) then
         call acq_error(ierr,cmssg)
         CALL MESSLOG(LOGUT,LOGUP)
         call exit()
      endif
      write(cmssg,11)
 11   format('********** File Write Process Started **********')
      CALL MESSLOG(0,LOGUP)
      
c
c  Call messlog last, because it will clear the message!
      CALL MESSLOG(LOGUT,LOGUP)
C     
      RETURN
C     
      END
      
