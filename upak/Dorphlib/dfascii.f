C$PROG DFASCII   - Displays REAL*8 numbers in ASCII (various fmts)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE DFASCII(DV,ANUM,NPL)
C
      REAL*8 DV,TV,XV
C
      REAL*8 DMMAX(2,12)
C
      INTEGER*4 TMP(3),ANUM(3),STARS(3)
C
      CHARACTER*12 CTMP
      EQUIVALENCE (CTMP,TMP)
C
      CHARACTER*4 cSTARS(3)
      EQUIVALENCE (cSTARS,STARS)
      DATA cSTARS/'****','****','****'/
C
      DATA DMMAX/
     &          .9D0,            9.0D0,           ! 1
     &          9.0D0,           99.0D0,          ! 2
     &          99.0D0,          999.0D0,         ! 3
     &          999.0D0,         9999.0D0,        ! 4
     &          9999.0D0,        99999.0D0,       ! 5
     &          99999.0D0,       999999.0D0,      ! 6
     &          999999.0D0,      9999999.0D0,     ! 7
     &          9999999.0D0,     99999999.0D0,    ! 8
     &          99999999.0D0,    999999999.0D0,   ! 9
     &          999999999.0D0,   2147483647.0D0,  ! 10
     &          2147483647.0D0,  2147483647.0D0,  ! 11
     &          2147483647.0D0,  2147483647.0D0/  ! 12
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO DISPLAY A REAL*8 FLOATING NUMBER IN ASCII
C     IN VARIOUS FORMATS (MAXIMIZING PRECISION OF DISPLAY FOR
C     DISPLAY FIELDS OF 6 TO 12 CHARACTERS)
C     DV      = REAL*8 NUMBER
C     ANUM(3) = ASCII OUTPUT
C     NPL     = NO. OF PLACES (CHARACTERS)
C     ------------------------------------------------------------------
C
      IF(NPL.LT.6.OR.NPL.GT.12) GO TO 600  !TEST FOR LEGAL REQUEST
C
      TV=DABS(DV)                          !ABSOLUTE VALUE
      IGO=NPL-5
C
      TMP(1)=Z'20202020'
      TMP(2)=Z'20202020'
      TMP(3)=Z'20202020'
C
C     ------------------------------------------------------------------
C     TRY TO DISPLAY IN INTEGER FORM;  XXXXXXXX OR -XXXXXXXX
C     ------------------------------------------------------------------
C
      IF(DV.GE.0.0.AND.DV.LE.DMMAX(2,NPL)) THEN    !TEST FOR
                                           IV=DV   !POS INTEGER
                                           GO TO 5 !DISPLAY
                                           ENDIF   !POSSIBLE
C
      IF(DV.LT.0.0.AND.TV.LE.DMMAX(1,NPL)) THEN    !TEST FOR
                                           IV=DV   !NEG INTEGER
                                           GO TO 5 !DISPLAY
                                           ENDIF   !POSSIBLE
C
      GO TO 25                                     !OTHERWISE,
C                                                  !TRY MODIFIED
C                                                  !EXP FORM
C
    5 GO TO (6,7,8,9,10,11,12) IGO
C
    6 WRITE(CTMP,16)IV
      GO TO 500
    7 WRITE(CTMP,17)IV
      GO TO 500
    8 WRITE(CTMP,18)IV
      GO TO 500
    9 WRITE(CTMP,19)IV
      GO TO 500
   10 WRITE(CTMP,20)IV
      GO TO 500
   11 WRITE(CTMP,21)IV
      GO TO 500
   12 WRITE(CTMP,22)IV
      GO TO 500
   16 FORMAT(I6)
   17 FORMAT(I7)
   18 FORMAT(I8)
   19 FORMAT(I9)
   20 FORMAT(I10)
   21 FORMAT(I11)
   22 FORMAT(I12)
C
C     ------------------------------------------------------------------
C     TRY TO DISPLAY IN MODIFIED EXPODENTIAL FORM
C     XXXXXX+E  OR -XXXXX+E
C     ------------------------------------------------------------------
C
   25 IF(DV.LT.0.0) GO TO 50
C
      JEX=0
      TV=DV
   30 IF(TV.LE.DMMAX(2,NPL-2)) GO TO 40
      TV=TV/10.0D0
      JEX=JEX+1
      IF(JEX.GT.9) GO TO 200
      GO TO 30
   40 IV=TV
      GO TO 100
C
   50 JEX=0
      XV=DV
   60 IF(TV.LE.DMMAX(1,NPL-2)) GO TO 70
      TV=TV/10.0D0
      XV=XV/10.0D0
      JEX=JEX+1
      IF(JEX.GT.9) GO TO 200
      GO TO 60
   70 IV=XV
C
  100 GO TO (106,107,108,109,110,111,112) IGO
C
  106 WRITE(CTMP,116)IV,JEX
      GO TO 500
  107 WRITE(CTMP,117)IV,JEX
      GO TO 500
  108 WRITE(CTMP,118)IV,JEX
      GO TO 500
  109 WRITE(CTMP,119)IV,JEX
      GO TO 500
  110 WRITE(CTMP,120)IV,JEX
      GO TO 500
  111 WRITE(CTMP,121)IV,JEX
      GO TO 500
  112 WRITE(CTMP,122)IV,JEX
      GO TO 500
  116 FORMAT(I4,'+',I1)
  117 FORMAT(I5,'+',I1)
  118 FORMAT(I6,'+',I1)
  119 FORMAT(I7,'+',I1)
  120 FORMAT(I8,'+',I1)
  121 FORMAT(I9,'+',I1)
  122 FORMAT(I10,'+',I1)
C
C     ------------------------------------------------------------------
C     IF ALL ELSE FAILS, DISPLAY IN DOUBLE FLOATING FORMAT
C     ------------------------------------------------------------------
C
  200 IF(DV.GE.0.0) THEN
      IF(NPL.EQ.6)  WRITE(CTMP,206)DV
      IF(NPL.EQ.7)  WRITE(CTMP,207)DV
      IF(NPL.EQ.8)  WRITE(CTMP,208)DV
      IF(NPL.EQ.9)  WRITE(CTMP,209)DV
      IF(NPL.EQ.10) WRITE(CTMP,210)DV
      IF(NPL.EQ.11) WRITE(CTMP,211)DV
      IF(NPL.EQ.12) WRITE(CTMP,212)DV
  206 FORMAT(1PD6.0)
  207 FORMAT(1PD7.1)
  208 FORMAT(1PD8.2)
  209 FORMAT(1PD9.3)
  210 FORMAT(1PD10.4)
  211 FORMAT(1PD11.5)
  212 FORMAT(1PD12.6)
      GO TO 500
      ENDIF
C
      IF(NPL.EQ.6)  GO TO 600
      IF(NPL.EQ.7)  WRITE(CTMP,307)DV
      IF(NPL.EQ.8)  WRITE(CTMP,308)DV
      IF(NPL.EQ.9)  WRITE(CTMP,309)DV
      IF(NPL.EQ.10) WRITE(CTMP,310)DV
      IF(NPL.EQ.11) WRITE(CTMP,311)DV
      IF(NPL.EQ.12) WRITE(CTMP,312)DV
  307 FORMAT(1PD7.0)
  308 FORMAT(1PD8.1)
  309 FORMAT(1PD9.2)
  310 FORMAT(1PD10.3)
  311 FORMAT(1PD11.4)
  312 FORMAT(1PD12.5)
C
  500 DO 520 I=1,3
      ANUM(I)=TMP(I)
  520 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     IF ALL TRYS FAIL, LOAD WITH STARS
C     ------------------------------------------------------------------
C
  600 DO 620 I=1,3
      ANUM(I)=STARS(I)
  620 CONTINUE
      RETURN
      END
