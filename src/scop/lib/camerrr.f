C$PROG CAMERRR   - Customized CAMAC error routine
C
C     ****************************************************************
C     Customized CAMAC error routine 
C     ****************************************************************
C
      subroutine CAMERRR(c,n,a,f,stat)

      implicit none
C
C     ****************************************************************
      integer*4 NOX
      integer*4 NOQ
      integer*4 NOX_NOQ
      integer*4 NOQ_NOX
      integer*4 TIMEOUT
      integer*4 ILL_CRATE
      integer*4 ILL_MODULE
      integer*4 ILL_ADDRESS
      integer*4 ILL_FUNCTION
      integer*4 CRATE_OFF_LINE
*
*  All codes are possible for CAMAC I/O.  For FASTBUS I/O, only
*  TIMEOUT and ILL_ADDRESS are possible.
*
      parameter (NOQ = 1)                  ! X = 1, Q = 0
      parameter (NOX = 2)                  ! X = 0, Q = 1
      parameter (NOX_NOQ = 3)              ! X = 0, Q = 0
      parameter (NOQ_NOX = 3)              ! X = 0, Q = 0
      parameter (TIMEOUT = 128)            ! Timeout
      parameter (ILL_CRATE = 'c004'X)      !Illegal CAMAC crate number 
      parameter (ILL_MODULE = 'c005'X)     !Illegal CAMAC module number 
      parameter (ILL_ADDRESS = 'c006'X)    !Illegal CAMAC subaddress 
      parameter (ILL_FUNCTION = 'c009'X)   !Illegal CAMAC function code
      parameter (CRATE_OFF_LINE = 'a000'X) !CAMAC crate off line
C     ****************************************************************
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      integer*4 stat,c,n,a,f
      integer*4 i
      character*20 msg(10)/
     1 'GOOD X   BAD Q','GOOD Q   BAD X','BAD  Q   - BAD X',
     2 'Timeout',
     3 'Illegal Crate','Illegal Module','Illegal Address',
     4 'Illegal Function','Crate Off_Line','Unknown error code'/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
      IF(STAT.EQ.0)        RETURN  !Return if GOOD
C
C     X = 1 and Q = 0 is ignored in this error handler
C
      if(stat.eq.NOQ) return   !return if bad Q only
C
      if (stat .eq. NOQ) then
         i = 1
      elseif (stat .eq. NOX) then
         i = 2
      elseif (stat .eq. NOX_NOQ) then
         i = 3
      elseif (stat .eq. TIMEOUT) then
         i = 4
      elseif (stat .eq. ILL_CRATE) then
         i = 5
      elseif (stat .eq. ILL_MODULE) then
         i = 6
      elseif (stat .eq. ILL_ADDRESS) then
         i = 7
      elseif (stat .eq. ILL_FUNCTION) then
         i = 8
      elseif (stat .eq. CRATE_OFF_LINE) then
         i = 9
      else
         i = 10
      endif
C
      write(cmssg,10) c,n,a,f,msg(i)
10    format(' CAM ERROR-C,N,A,F=',4I3,'  ',A)
      call messlog(logut,logup)
      return
      end
