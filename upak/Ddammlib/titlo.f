C$PROG TITLO     - Loads banner title for PLOTUM1 & PLOTUM2
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE TITLO(TITL,KFLG,ID)
C
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL                 LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML4/ LABLX(3),LABLY(3),LABTIT(10)
      CHARACTER*40                  CLABTIT
      CHARACTER*12 CLABLX,CLABLY
      EQUIVALENCE (CLABLX,LABLX),(CLABLY,LABLY),(CLABTIT,LABTIT)
C
      INTEGER*4    TITL(20),TIT40(40)
C
      CHARACTER*4  CTIT40(40)
      CHARACTER*96 CTIT96
      CHARACTER*80 CTIT80
      CHARACTER*48 CTIT48
C
      EQUIVALENCE (CTIT40,TIT40)
      EQUIVALENCE (CTIT96,TIT40(17))
      EQUIVALENCE (CTIT80,TIT40(21))
      EQUIVALENCE (CTIT48,TIT40(29))
C
      INTEGER*4    BLANK
      character*4  cBLANK
      equivalence (cBLANK,blank)
C
      DATA        cBLANK/'    '/
C
      CHARACTER*4  KFLG
C
      SAVE
C
C     ==================================================================
C
      DO 10 I=1,40
      TIT40(I)=BLANK
   10 CONTINUE
C
      IF(KFLG.EQ.'M   ') GO TO 50
C
      CALL LUGET(KFLG,IDUM,J,JDUM,IERR)
C
      IF(IERR.NE.0) RETURN
C
      J=J-1
      DO 30 I=1,16
      TIT40(I)=NAMFIL(I,J)
   30 CONTINUE
      GO TO 100
C
   50 CTIT40(1)='MEMO'
      CTIT40(2)='RY-B'
      CTIT40(3)='UFFE'
      CTIT40(4)='R   '
C
  100 WRITE(CTIT96,105)ID
  105 FORMAT('-ID=',I10,'-')
      CALL SQUEZL(TIT40(17),1,15)
C
      IT=LABLX(1)
      IF(IT.NE.0.AND.IT.NE.BLANK) WRITE(CTIT80,110)LABLX,LABLY
C
      JT=LABTIT(1)
      IF(JT.EQ.0.OR.JT.EQ.BLANK) GO TO 130
C
      IF(IT.NE.0.AND.IT.NE.BLANK) THEN
      WRITE(CTIT48,120)LABTIT
      GO TO 130
                                   ENDIF
C
      WRITE(CTIT48,125)LABTIT
C
  110 FORMAT(3A4,',',3A4)
  120 FORMAT('-',10A4)
  125 FORMAT(10A4)
C
  130 CALL SQUEZLX(TIT40,1,160)
C
      DO 150 I=1,20
      TITL(I)=TIT40(I)
  150 CONTINUE
      RETURN
      END
