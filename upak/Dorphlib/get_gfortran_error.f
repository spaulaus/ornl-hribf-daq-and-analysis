C$PROG GET_Fortran_ERROR - G77 or gfortran I/O run-time error message routine
C
C     ******************************************************************
C     Similar to one by
c        W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE GET_FORTRAN_ERROR(IERR,MESG)
C
      IMPLICIT NONE
C
cc    Gfortran returns codes in IOSTAT which are between 5000 and 5018.  These 
CC    are the messages which correspond to these codes.  Since we can
c     distinguish system codes from Gfortran codes, we can handle them 
c     both.
C     ------------------------------------------------------------------
      CHARACTER*80 MSG(100),MESG
C
      INTEGER*4    IERR,NCALL,I
      integer      iosmin, iosmax, iosoff
      parameter    (iosmin=5000, iosmax=5018, iosoff=5000)
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
         MSG( 1)='Input option conflict'
         MSG( 2)='Bad input option'
         MSG( 3)='Missing input option'
         MSG( 4)='File is already open'
         MSG( 5)='Illegal unit number'
         MSG( 6)='Error in format'
         MSG( 7)='Unallowed action'
         MSG( 8)='End of file'
         MSG( 9)='Bad unit separator'
         MSG(10)='Read value error'
         MSG(11)='Read value overflow'
         MSG(12)='Error on internal read'
         MSG(13)='Unallowed internal read unit'
         MSG(14)='Error in allocation'
         MSG(15)='Off end-of-record'
         MSG(16)='Direct access unexpected end-of-record'
         MSG(17)='Direct access short record'
         MSG(18)='file corruption detected.  Repent and reform.'

      ENDIF
C    
C     ------------------------------------------------------------------
C
C
      IF(IERR.EQ.0) RETURN
C
      IF (IERR .EQ. -1) THEN          ! End of file
          MESG='END of File reached'

      else if (ierr .eq. -2) then     ! End of record ?
          mesg='End of Record reached'

c         OS error, use the system
      else if ((ierr .ge. 1) .and. (ierr .lt. iosmin)) then   
          call perror('System error ')

      else if (ierr .eq. iosmin) then   ! OS error, use the system
          call perror('System error ')

      else IF ((IERR.GT.iosmin) .AND. (IERR.LE.iosmax) .AND.
     &    (MSG(IERR-iosoff).NE.' ')) THEN  ! In the runtime list
C
          MESG='Gfortran runtime error' // MSG(IERR-iosoff)

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
