C$PROG GETDLIS   - Gets 1-D display list
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE GETDLIS(IDL,KODE,DNOR,NID,MXID,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
C
      INTEGER*4    IDL(*),NID,MXID,IERR
C
      CHARACTER*4  KODE(*)
C
      REAL*4       DNOR(*),NORNOW
C
      INTEGER*4    IDLIS(100),MXLIS
C
      REAL*4       NORLIS(100),DNORM
C
      INTEGER*4    NLIS,IA,IB,IC,N,I
C
      INTEGER*4    KIND,KERR,IV
C
      REAL*4       XV
C
      DATA         MXLIS/100/
C
      INTEGER*4    KODVAL,KODTST
C
      CHARACTER*4  ICODE,LISTY,KODNOW,CKODTST,KODLIS(100),CLWD(2,40)
C
      EQUIVALENCE (CLWD,LWD),(CKODTST,KODTST)
C
      SAVE
C
C
C     ------------------------------------------------------------------
C     Do some initialization
C     ------------------------------------------------------------------
C
      IERR=0                                !Zero error flag
C
      NID=0                                 !Zero number of IDs
C
      NLIS=0                                !Zero list counter
C
      LISTY='LIST'                          !Default list-type
C
      KODNOW='N   '                         !Default input cocd
C
      NORNOW=1.00                           !Default normalization
C
      DO 20 I=1,100                         !Reset ID-list
      IDLIS(I)=0
   20 CONTINUE
C
C
C     ------------------------------------------------------------------
C     LOOP on all data fields
C     ------------------------------------------------------------------
C
      DO 500 N=2,NF
C
      IF(CLWD(1,N).EQ.'TO  ') THEN         !Tst for loop type list
      LISTY='LOOP'
      GO TO 500
      ENDIF
C
C     ------------------------------------------------------------------
C
      IF(ITYP(N).EQ.1) THEN                !Tst for input code spec
      KODTST=KODVAL(LWD(1,N))
      IF(CKODTST.EQ.'    ') GO TO 1000     !Tst for error
      GO TO 100                            !If OK, go set it up
      ENDIF
C
C     ------------------------------------------------------------------
C
      CALL MILV(LWD(1,N),IV,XV,KIND,KERR)  !Get ID or NORM data
C
      IF(KERR.NE.0) GO TO 1010             !Tst for error
C
      IF(KIND.EQ.2) THEN                   !Tst for NORM data
      NORNOW=XV                            !If yes, set it up
      GO TO 500                            !and go to end of loop
      ENDIF
C
C     ------------------------------------------------------------------
C
      NLIS=NLIS+1                          !Otherwise, add to ID list
      IF(NLIS.GT.MXLIS) GO TO 1020         !Tst for overflow
      IDLIS(NLIS) =IV                      !Store ID number
      NORLIS(NLIS)=NORNOW                  !Store current normalization
      KODLIS(NLIS)=KODNOW                  !Store current input code
      GO TO 500                            !Go to end of loop
C
C     ------------------------------------------------------------------
C
  100 IF(NLIS.LE.0)       GO TO 400        !Tst for empty list
C
      IF(LISTY.EQ.'LIST') GO TO 200        !Tst for LIST type
      IF(LISTY.EQ.'LOOP') GO TO 300        !Tst for LOOP type
C
C     ------------------------------------------------------------------
C     Process LIST-type list
C     ------------------------------------------------------------------
C
  200 DO 220 I=1,NLIS                      !Loop on full list
      NID=NID+1                            !Inc no. of IDs
      IF(NID.GT.MXID) GO TO 1030           !Tst for too many
      IDL(NID) =IDLIS(I)                   !Store the ID number
      DNOR(NID)=NORLIS(I)                  !Store the normalization 
      KODE(NID)=KODLIS(I)                  !store the input code
  220 CONTINUE
      GO TO 400                            !Go set end-of-list defaults
C
C     ------------------------------------------------------------------
C     Process LOOP-type list
C     ------------------------------------------------------------------
C
  300 IA=IDLIS(1)                          !Min ID number
      IB=IDLIS(2)                          !Max ID number
      IC=IDLIS(3)                          !ID increment
      IF(IC.LT.1) IC=1                     !ID increment (default)
      DNORM=NORLIS(1)                      !Normalization
      ICODE=KODLIS(1)                      !Input code
      DO 320 I=IA,IB,IC                    !Do the LOOP
      NID=NID+1                            !Inc number of IDs
      IF(NID.GT.MXID) GO TO 1030           !tst for too many
      IDL(NID) =I                          !Store the ID number
      DNOR(NID)=DNORM                      !Store the normalization
      KODE(NID)=ICODE                      !Store the input code
  320 CONTINUE
      DO 330 I=1,3                         !Reset the LOOP values
      IDLIS(I)=0
  330 CONTINUE
C
C     ------------------------------------------------------------------
C     Set default values for end-of-list etc
C     ------------------------------------------------------------------
C
  400 LISTY='LIST'                         !Set list-type = LIST
      KODNOW=CKODTST                       !Set current input code
      NORNOW=1.00                          !Set default normalization
      NLIS=0                               !Set zero entries
C
  500 CONTINUE
C
C     ------------------------------------------------------------------
C     Clean up - process the last list
C     ------------------------------------------------------------------
C
      IF(LISTY.EQ.'LIST') GO TO 600        !Tst for LIST type
      IF(LISTY.EQ.'LOOP') GO TO 700        !Tst for LOOP type
C
C
C     ------------------------------------------------------------------
C     Cleanup - Process LIST-type list
C     ------------------------------------------------------------------
C
  600 DO 620 I=1,NLIS                      !Loop on full list
      NID=NID+1                            !Inc no. of IDs
      IF(NID.GT.MXID) GO TO 1030           !Tst for too many
      IDL(NID) =IDLIS(I)                   !Store the ID number
      DNOR(NID)=NORLIS(I)                  !Store the normalization 
      KODE(NID)=KODLIS(I)                  !store the input code
  620 CONTINUE
      RETURN
C
C
C     ------------------------------------------------------------------
C     Cleanup - Process LOOP-type list
C     ------------------------------------------------------------------
C
  700 IA=IDLIS(1)                          !Min ID number
      IB=IDLIS(2)                          !Max ID number
      IC=IDLIS(3)                          !ID increment
      IF(IC.LT.1) IC=1                     !ID increment (default)
      DNORM=NORLIS(1)                      !Normalization
      ICODE=KODLIS(1)                      !Input code
      DO 720 I=IA,IB,IC                    !Do the LOOP
      NID=NID+1                            !Inc number of IDs
      IF(NID.GT.MXID) GO TO 1030           !tst for too many
      IDL(NID) =I                          !Store the ID number
      DNOR(NID)=DNORM                      !Store the normalization
      KODE(NID)=ICODE                      !Store the input code
  720 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Illegal file-code - command ignored')
      GO TO 1500
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Syntax error from MILV - command ignored')
      GO TO 1500
C
 1020 WRITE(CMSSG,1025)MXLIS
 1025 FORMAT('More than ',I3,' IDs specified - not allowed')
      GO TO 1500
C
 1030 WRITE(CMSSG,1035)MXID
 1035 FORMAT('More than ',I3,' IDs specified - not allowed')
      GO TO 1500
C
C
 1500 CALL MESSLOG(LOGUT,LOGUP)
      NID=0
      IERR=1
      RETURN
      END
