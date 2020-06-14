C$PROG GET_G77_ERROR - G77 I/O run-time error message routine
C
C     ******************************************************************
C     Similar to one by
c        W.T. MILNER AT HRIBF 
c        R. L. Varner 12 April 2007
C     ******************************************************************
C
      SUBROUTINE GET_G77_ERROR(IERR,MESG)
C
      IMPLICIT NONE
C
cc    G77 returns codes in IOSTAT which are between 100 and 131.  These 
CC    are the messages which correspond to these codes.  Note that we
C     have to know that these are IOSTAT codes, because they overlap with
C     the system errno list on LINUX (1 to 140).
C     ------------------------------------------------------------------
      CHARACTER*80 MSG(100),MESG
C
      INTEGER*4    IERR,NCALL,I
      integer      iosmin, iosmax, iosoff
      parameter    (iosmin=100, iosmax=131, iosoff=99)
C
      DATA         NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF (NCALL.EQ.0) THEN
C
         NCALL=1
         DO I=1,100
            MSG(I)=' '
         ENDDO

C 
         MSG( 1)='error in format'
         MSG( 2)='illegal unit number'
         MSG( 3)='formatted io not allowed'
         MSG( 4)='unformatted io not allowed'
         MSG( 5)='direct io not allowed'
         MSG( 6)='sequential io not allowed'
         MSG( 7)='cannot backspace file'
         MSG( 8)='null file name'
         MSG( 9)='cannot stat file'
         MSG(10)='unit not connected'
         MSG(11)='off end of record'
         MSG(12)='truncation failed in endfile'
         MSG(13)='incomprehensible list input'
         MSG(14)='out of free space'
         MSG(15)='unit not connected'
         MSG(16)='read unexpected character'
         MSG(17)='bad logical input field'
         MSG(18)='bad variable type'
         MSG(19)='bad namelist name'
         MSG(20)='variable not in namelist'
         MSG(21)='no end record'
         MSG(22)='variable count incorrect'
         MSG(23)='subscript for scalar variable'
         MSG(24)='invalid array section'
         MSG(25)='substring out of bounds'
         MSG(26)='subscript out of bounds'
         MSG(27)='cannot read file'
         MSG(28)='cannot write file'
         MSG(29)='"new" file exists'
         MSG(30)='cannot append to file'
         MSG(31)='non-positive record number'
         MSG(32)='I/O started while already doing I/O'

      ENDIF
C    
C     ------------------------------------------------------------------
C
C
      IF(IERR.EQ.0) RETURN
C
      if ((ierr .gt. 0) .AND. (ierr .LT. iosmin)) then
         call perror('System error')
      else IF ((IERR.GE.iosmin) .AND. (IERR.LE.iosmax) .AND.
     &    (MSG(IERR-iosoff).NE.' ')) THEN
C
          MESG='G77 runtime error' // MSG(IERR-iosoff)
      ELSE
C
          MESG=' '
          WRITE(MESG,210)IERR
  210     FORMAT('Unknown IOSTAT# =',I8)
      ENDIF
C
      RETURN
C
      END
