C$PROG HISNIT    - Initializes his-files (ZOT, GET, HUP)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/29/98
C     ******************************************************************
C
      SUBROUTINE HISNIT(LU,MODE)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      INTEGER*4    LU,NBYT,IREC,NDO,IDUMMY,IERR
C
      CHARACTER*4  MODE
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      NBYT=2*NHWH
      IREC=1

C
      IF(MODE.EQ.'ZOT ') GO TO 100
      IF(MODE.EQ.'GET ') GO TO 110
      IF(MODE.EQ.'HUP ') GO TO 120
      RETURN
C   
 100  NDO=NHWH*2
      CALL MEM_ZERO_HISSPACE(NDO)
      GO TO 120
C   
*  110 CALL MEM_BUFI(LU,IDUMMY,IREC,NBYT,IERR)
 110  continue
      write(6,*) 'HUP is unnecessary in mm-scanor.'
      RETURN
C   
*  120 CALL MEM_BUFO(LU,IDUMMY,IREC,NBYT,IERR)
 120  continue
      RETURN
      END
