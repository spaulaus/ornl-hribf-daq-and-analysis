C$PROG YMDHMS    - Obsolete date/time routine - use milymdhms instead
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE YMDHMS(IAR)
C
      EXTERNAL IDATE
C
      INTEGER*4 IAR(6)
C
      INTEGER*4 IDAT(3),ITIM(3)
C
      SAVE
C
      CALL IDATE(IDAT(1),IDAT(2),IDAT(3))
C
      CALL ITIME(ITIM)
C
      IAR(1)=IDAT(3)-1900
      IAR(2)=IDAT(2)
      IAR(3)=IDAT(1)
      IAR(4)=ITIM(1)
      IAR(5)=ITIM(2)
      IAR(6)=ITIM(3)
C
      RETURN
      END
