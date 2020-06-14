C$PROG LUGET     - Returns unit-numbers associated with file-codes
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE LUGET(KODE,LUH,LUD,KINF,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      CHARACTER*4 KODL(7),KINF,KODE
C
      INTEGER*4   LDX(7)
C
      DATA KODL/'    ','N   ','O   ','P   ','Q   ','R   ','S   '/
      DATA  LDX/ 8,     8,     10,    18,    12,    14,    16   /
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
      KINF='    '
C   
      DO 10 I=1,7
      IF(KODE.EQ.KODL(I)) GO TO 20
   10 CONTINUE
      IERR=1
      RETURN
C   
   20 NDX=LDX(I)
      IF(KFIL(NDX).NE.'HIS ') LUH=NDX
      IF(KFIL(NDX).EQ.'HIS ') LUH=LUC(NDX)
      LUD=NDX+1
      KINF=KFIL(NDX)
      RETURN
      END
