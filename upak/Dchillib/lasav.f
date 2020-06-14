C$PROG LASAV
      SUBROUTINE LASAV(LABEL,IVAL,IERR,MSER)
C
      INTEGER*4    LATV(2500),LABV(200),MSG(10,5),MSER(10)
C
      CHARACTER*4  LABL(200)
C
      CHARACTER*40 MSC(5)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'CHIL DEFINED LABEL TABLE OVERFLOW - XXXX',
     &         'USER DEFINED LABEL TABLE OVERFLOW - XXXX',
     &         'CHIL REFERENCED LABEL NOT DEFINED - XXXX',
     &         'USER REFERENCED LABEL NOT FOUND --- XXXX',
     &         'MULTIPLY DEFINED STATEMENT LABEL -- XXXX'/
C
      DATA LATV,LABL,LABV/2500*-1,200*'    ',200*-1/
      DATA NLAB/0/
C
      CHARACTER*4  LABEL,CLABEL
C
      INTEGER*4    ILABEL
C
      EQUIVALENCE (CLABEL,ILABEL)
C
      INTEGER*4    X40
      DATA         X40/z'40'/
C
      SAVE
C
C     **************************************************************
C     THIS ENTRY STORES VALUES ASSOCIATED WITH CHIL-DEFINED @-LABELS
C            OR  STORES VALUES ASSOCIATED WITH USER-DEFINED LABELS
C            AND STORES USER-DEFINED LABELS
C     **************************************************************
C
      IERR=0                            !RESET ERROR CODE
      CALL ILBYTE(IT,LABEL,0)           !GET 1ST BYTE OF LABEL
      IF(IT.NE.X40) GO TO 50            !TST FOR @ (PSEUDO LABEL)
      READ(LABEL,10)NDX                 !IF YES, DECODE NUMBER PART
   10 FORMAT(1X,Z3)
      IF(NDX.GT.2500) GO TO 210         !TST FOR IN RANGE
      LATV(NDX)=IVAL                    !SAVE VALUE
      RETURN
C
C     STORE USER-DEFINED VALUE [LABEL] *****************************
C
   50 DO 60 I=1,NLAB                    !LOOK FOR LABEL IN TABLE
      IF(LABEL.EQ.LABL(I)) GO TO 250    !ERROR IF MULTIPLY DEFINED
   60 CONTINUE
      NLAB=NLAB+1                       !IF NOT FOUND, INC # LABELS
      IF(NLAB.GT.200) GO TO 220         !TST FOR OVERFLOW
      I=NLAB
      LABL(I)=LABEL                     !ADD LABEL TO TABLE
      LABV(I)=IVAL                      !STORE ASSOCIATED VALUE
      RETURN
C
C     **************************************************************
C     THIS ENTRY RETURNS VALUE ASSICIATED WITH LABEL
C     **************************************************************
C
C
      ENTRY LAVAL(LABEL,IVAL,IERR,MSER)
C
      IERR=0                            !RESET ERROR CODE
      CALL ILBYTE(IT,LABEL,0)           !GET 1SR BYTE
      IF(IT.NE.X40) GO TO 100           !TST FOR @ (PSEUDO LABEL)
      READ(LABEL,10)NDX                 !IF YES, DECODE NUMBER PART
      IF(NDX.GT.2500) GO TO 230         !TST FOR IN RANGE
      IF(LATV(NDX).LT.0) GO TO 230      !TST FOR UN-DEFINED
      IVAL=LATV(NDX)                    !GET ASSOCIATED VALUE
      RETURN
C
C     GET VALUE ASSOCIATED WITH USER-DEFINED LABEL *****************
C
  100 DO 110 I=1,NLAB                   !SEARCH LIST FOR LABEL
      IF(LABEL.EQ.LABL(I)) GO TO 120    !TST FOR MATCH
  110 CONTINUE
      GO TO 240                         !ERROR IF NOT FOUND
C
  120 IVAL=LABV(I)                      !GET ASSOCIATED VALUE
      RETURN
C
C     **************************************************************
C     RETURN ERROR CODE AND MESSAGES
C     **************************************************************
C
  210 JJ=1
      GO TO 300
  220 JJ=2
      GO TO 300
  230 JJ=3
      GO TO 300
  240 JJ=4
      GO TO 300
  250 JJ=5
C
  300 IERR=JJ
      DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      CLABEL=LABEL
      MSER(10)=ILABEL
      RETURN
      END
