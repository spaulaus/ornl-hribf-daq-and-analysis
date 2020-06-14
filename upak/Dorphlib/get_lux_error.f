C$PROG GET_LUX_ERROR - Linux run-time error message routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE GET_LUX_ERROR(IERR,MESG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      CHARACTER*80 MSG(1050),MESG
C
      INTEGER*4    IERR,NCALL,I
C
      DATA         NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.NE.0) GO TO 100
C
      DO 10 I=1,1050
      MSG(I)=' '
   10 CONTINUE
C
      MSG(  1)='Not owner'
      MSG(  2)='No such file or directory'
      MSG(  3)='No such process'
      MSG(  4)='Interrupted system call'
      MSG(  5)='I/O error'
      MSG(  6)='No such device or address'
      MSG(  7)='Arg list too long'
      MSG(  8)='Exec format error'
      MSG(  9)='Bad file number'
      MSG( 10)='No children'
      MSG( 11)='No more processes'
      MSG( 12)='Not enough core'
      MSG( 13)='Permission denied'
      MSG( 14)='Bad address'
      MSG( 15)='Block device required'
      MSG( 16)='Mount device busy'
      MSG( 17)='File exists'
      MSG( 18)='Cross-device link'
      MSG( 19)='No such device'
      MSG( 20)='Not a directory'
      MSG( 21)='Is a directory'
      MSG( 22)='Invalid argument'
      MSG( 23)='File table overflow'
      MSG( 24)='Too many open files'
      MSG( 25)='Not a typewriter'
      MSG( 26)='Text file busy'
      MSG( 27)='File too large'
      MSG( 28)='No space left on device'
      MSG( 29)='Illegal seek'
      MSG( 30)='Read-only file system'
      MSG( 31)='Too many links'
      MSG( 32)='Broken pipe'
      MSG( 33)='Domain error: argument undefined for function'
      MSG( 34)='Result too large'
      MSG( 35)='File position error'
      MSG( 36)='Non standard error'
      MSG( 37)='Error in session, process, or thread'
      MSG( 38)='Error in extended attributes'
      MSG( 39)='Error on device or in parameters to device'
      MSG( 40)='Executable module has incorrect format'
      MSG( 41)='Data being passed has error or is in incorrect format'
      MSG( 42)='Error communicating with memory'
      MSG( 43)='System error'
      MSG(128)='File sharing error'
      MSG(129)='Network error'
      MSG(130)='Network error'
      MSG(131)='Printer error'
      MSG(132)='Pause error'
      MSG(133)='Redirection error'
      MSG(134)='Directory error'
      MSG(135)='Disk error'
      MSG(136)='DOS error'
      MSG(1000)='Error in format'
      MSG(1001)='Off end of record'
      MSG(1002)='Error in list input'
      MSG(1003)='Read unexpected character'
      MSG(1004)='Fatal I/O error'
      MSG(1005)='Out of memory'
      MSG(1006)='Buffer too large'
      MSG(1007)='Blank logical input'
      MSG(1008)='Illegal record number'
      MSG(1009)='Too many decimal digits'
      MSG(1010)='Floating point overflow'
      MSG(1011)='Expected "=" in NAMELIST input'
      MSG(1012)='Digit expected in NAMELIST input'
      MSG(1013)='Expected "*" in NAMELIST input'
      MSG(1014)='Positive constant expected in NAMELIST input'
      MSG(1015)='Illegal type conversion in NAMELIST input'
      MSG(1016)='Too many elements specified in NAMELIST input'
      MSG(1017)='Expected ")" in NAMELIST input'
      MSG(1018)='Expected "," in NAMELIST input'
      MSG(1019)='Syntax error in NAMELIST input'
      MSG(1020)='Name expected'
      MSG(1021)='No such name in NAMELIST'
      MSG(1022)='Substring lower bound must not be zero'
      MSG(1023)='Expected ":" in NAMELIST input'
      MSG(1024)='Substring upper bound out of range'
      MSG(1025)='Error in formatted input'
      MSG(1026)='No such unit'
      MSG(1027)='Cannot do direct access on this unit'
      MSG(1028)='Record too large'
      MSG(1029)='Illegal modification of attributes in open'
      MSG(1030)='File already exists'
      MSG(1031)='Bad unformatted file'
      MSG(1032)='Sequential access required'
      MSG(1033)='Formatted I/O to UNFORMATTED file'
      MSG(1034)='Unformatted I/O to FORMATTED file'
      MSG(1035)='Indexed I/O not supported'
      MSG(1036)='Attempt to write to READONLY file'
      MSG(1037)='Invalid or no RECL= specified for Direct file'
      MSG(1038)='Attempt to read from WRITEONLY file'
      MSG(1039)='Illegal attempt to open UNIT=*'
      MSG(1040)='Illegal attempt to close UNIT=*'
      MSG(1041)='Formatted I/O to BINARY file'
      MSG(1042)='Illegal backspace on binary sequential file'
      MSG(1043)='Direct access required'
      MSG(1044)='Illegal attempt to do direct access on sequential file'
      MSG(1045)='Direct formatted file does not contain newline byte'
      MSG(1046)='Attempt to open non-existing file with STATUS = OLD'
      MSG(1047)='Attempt to open new or truncated file in readonly mode'
      MSG(1048)='Attempt to open file in both readonly & writeonly mode'
      MSG(1049)='PAD="NO" and input item has fewer bytes than requested'
      MSG(1050)='ACTION & MODE both specified but contradict each other'
C
C     ------------------------------------------------------------------
C
      NCALL=1
C
  100 IF(IERR.EQ.0) RETURN
C
      IF(IERR.LT.1)        GO TO 200
C
      IF(IERR.GT.1050)     GO TO 200
C
      IF(MSG(IERR).EQ.' ') GO TO 200
C
      MESG=MSG(IERR)
C
      RETURN
C
  200 MESG=' '
C
      WRITE(MESG,210)IERR
C
  210 FORMAT('Unknown error# =',I8)
C
      RETURN
C
      END
