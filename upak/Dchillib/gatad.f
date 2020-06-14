C$PROG GATAD
      SUBROUTINE GATAD(GATES,NG,LOC,LOCG,IERR,MSER)
C
      INTEGER*4 GATES(2,200)
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MILH(524288)
C
      INTEGER*4 MESS(10,2),MSER(10)
      CHARACTER*40 MSC(2)
C
      EQUIVALENCE (MSC(1),MESS(1,1))
      EQUIVALENCE (MILH(1),MILF(1))
C
      DATA  MSC/'INSUFFICIENT SPACE TO STORE GATE LIST  ',
     &          '                                       '/
C
      SAVE
C
C     **************************************************************
C     ADD THE GATES IN "GATES" TO GATE-REGION OF MIL
C     **************************************************************
C
      IERR=0                                !RESET ERROR FLAG
      CALL GASORT(GATES,NG,IERR,MSER)       !SORT AND TST FOR LEGAL
      IF(IERR.NE.0) RETURN                  !RETURN IF ERROR
C
      IF(NGATL+NG.GT.MXGATL) GO TO 110      !TST FOR SPACE TO STORE
C
      NDX=IGATOF+NGATL                      !INIT MIL INDEX
      LOCG=NDX+1                            !HALF-WD LOC IN MIL
      LOC=(LOCG+1)/2                        !FULL-WD LOC IN MIL
C
      DO 20 J=1,NG                          !LOOP ON # OF GATES
      NDX=NDX+1                             !INC MIL-INDEX
      MILH(NDX)=GATES(2,J)                  !STOR HI-LIMIT
      NDX=NDX+1                             !INC MIL-INDEX
      MILH(NDX)=GATES(1,J)                  !STOR LO-LIMIT
   20 CONTINUE
      NGATL=NGATL+2*NG                      !NEW GATE-LIST LENGTH
      RETURN
C
  110 JJ=1
      IERR=JJ
      DO 210 I=1,10
      MSER(I)=MESS(I,JJ)
  210 CONTINUE
      RETURN
      END
