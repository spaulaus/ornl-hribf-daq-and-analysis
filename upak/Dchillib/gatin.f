C$PROG GATIN
      SUBROUTINE GATIN(LU,LIBGAT,GLIB,IERR,MSG)
C
      INTEGER*4    LIBGAT(2052)
C
      INTEGER*2    GLIB(4096),JTIT(38),BLANK
C
      DATA         BLANK/Z'2020'/
C
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40)
      INTEGER*4    LIST(5)
      INTEGER*4    MESS(11,6),MSG(13)
      CHARACTER*44 MSC(6)
      CHARACTER*8  CMSSG
      INTEGER*4    IMSSG(2)
      CHARACTER*4  KWD
C
      EQUIVALENCE (CMSSG,IMSSG)
      EQUIVALENCE (MSC(1),MESS(1,1))
      EQUIVALENCE (JTIT(1),IWD(2)),
     &            (KWD    ,IWD(1))
      EQUIVALENCE (ISN    ,LIST(1)),
     &            (LPAR   ,LIST(2)),
     &            (LHIS   ,LIST(3))
C
      DATA  MSC/'ERROR READING GAF-FILE,  ILLEGAL LABEL ON - ',
     2          'ERROR READING GAF-FILE,  SET-               ',
     3          'ERROR READING GAF-FILE,  GATE-              ',
     4          'ERROR READING GAF-FILE,  ODD # GATE LIMITS- ',
     5          'ERROR READING GAF-FILE,  SUPPRISE EOF -     ',
     6          'ERROR READING GAF-FILE,  ILLEGAL GATE/S IN  '/
C
      INTEGER*4  GAF
      character*4 cgaf
      equivalence (cgaf,gaf)
      DATA       cGAF/'GAF '/
C
      SAVE
C
C     **************************************************************
C     GATE LIST STRUCTURE (INTERNAL)
C
C     LIBGAT(1) - CONTAINS 'GAF ' (NO LONGER USED)
C     LIBGAT(2) - CONTAINS NUMSET (NUMBER OF GATE SETS ON FILE)
C     LIBGAT(3) - CONTAINS MAXL   (# HALF-WDS USED FOR GATE-SETS)
C     LIBGAT(4) - CONTAINS NSEC   (NO LONGER USED)
C
C     GATE-SET
C     GATE-SET
C     --------
C     **************************************************************
C     GATE-SET STRUCTURE (INTERNAL)
C
C     SET#  - GATE-SET ID NUMBER
C     LPAR  - ORIGINAL PARAMETER LENGTH
C     LHIS  - HISTOGRAM LENGTH
C     NHWT  - NUMBER OF HALF-WORDS FOR TITLE
C     NHWG  - NUMBER OF HALF-WORDS FOR GATES
C     TITLE - TITLE (NHWT 16-BIT WORDS)
C     GATES - LO,HI (NHWG 16-BIT WORDS)
C
C     **************************************************************
C     STRUCTURE OF ASCII GAF-FILE
C
C     SET     ID      LPAR    LHIS    NWT     NWG
C     TIT ------------TITLE----------------------
C     GATE    LO,HI   LO,HI   LO,HI   ...........
C     GATE    LO,HI   LO,HI   LO,HI   ...........
C       .
C       .
C     SET     ID      LPAR    LHIS    NWT     NWG
C     TIT ------------TITLE----------------------
C     GATE    LO,HI   LO,HI   LO,HI   ...........
C     GATE    LO,HI   LO,HI   LO,HI   ...........
C       .
C     END
C     **************************************************************
C     READ GATE LIBRARY FROM ASCII GAF-FILE
C     **************************************************************
C
      IERR=0
      REWIND LU
      LIBGAT(2)=0
      LIBGAT(3)=0
      NUMSET=0
      NSET=0
      MAXL=0
      NLN=0
      N=0
C
      READ(LU,220,END=600)IWD
  220 FORMAT(20A4)
      NLN=NLN+1
  230 IF(KWD.NE.'SET ') GO TO 1010
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 1020
      IF(NF.LT.3)   GO TO 1020
C
      DO 240 J=1,3
      CALL LIMIV(LWD(1,J),1,32768,LIST(J),KERR)
      IF(KERR.NE.0) GO TO 1020
  240 CONTINUE
C
      GLIB(N+1)=ISN
      GLIB(N+2)=LPAR
      GLIB(N+3)=LHIS
      NWTI=N+4
      NWGI=N+5
      N=N+5
C
      READ(LU,220,END=1050)IWD
      NLN=NLN+1
      IF(KWD.NE.'TIT ') GO TO 1010
C
      K=38
      DO 250 I=1,38
      IF(JTIT(K).NE.BLANK) GO TO 260
      K=K-1
  250 CONTINUE
C
  260 NWT=K
      GLIB(NWTI)=NWT
      DO 270 I=1,NWT
      N=N+1
      GLIB(N)=JTIT(I)
  270 CONTINUE
      NWG=0
      NSET=NSET+1
      IAA=N+1
C
  300 READ(LU,220,END=500)IWD
      NLN=NLN+1
      IF(KWD.EQ.'GATE') GO TO 310
      IF(KWD.EQ.'SET ') GO TO 400
      IF(KWD.EQ.'END ') GO TO 400
      GO TO 1010
C
  310 CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 1030
      DO 320 J=1,NF
      CALL LIMIV(LWD(1,J),0,16383,IV,KERR)
      IF(KERR.NE.0) GO TO 1030
      N=N+1
      NWG=NWG+1
      GLIB(N)=IV
  320 CONTINUE
      GO TO 300
C
  400 GLIB(NWGI)=NWG
      IBB=N
      IF(2*(NWG/2).NE.NWG) GO TO 1040
      CALL SUBSORT(GLIB,IAA,IBB,LERR)
      IF(LERR.NE.0) GO TO 1060
C
      IF(KWD.EQ.'END ') GO TO 510
      IF(KWD.EQ.'SET ') GO TO 230
      GO TO 1010
C
  500 GLIB(NWGI)=NWG
      IF(2*(NWG/2).NE.NWG) GO TO 1040
      CALL SUBSORT(GLIB,IAA,IBB,LERR)
      IF(LERR.NE.0) GO TO 1060
C
  510 MAXL=N
      LIBGAT(3)=N
      NUMSET=NSET
      LIBGAT(2)=NSET
      LIBGAT(1)=GAF
  600 RETURN
C
 1010 JJ=1
      GO TO 1100
 1020 JJ=2
      GO TO 1100
 1030 JJ=3
      GO TO 1100
 1040 JJ=4
      GO TO 1100
 1050 JJ=5
      GO TO 1100
 1060 JJ=6
      DO 1062 I=1,11
      MSG(I)=MESS(I,JJ)
 1062 CONTINUE
      WRITE(CMSSG,1064)ISN
      MSG(12)=IMSSG(1)
      MSG(13)=IMSSG(2)
 1064 FORMAT('SET ',I4)
      IERR=JJ
      RETURN
C
 1100 DO 1110 I=1,11
      MSG(I)=MESS(I,JJ)
 1110 CONTINUE
      WRITE(CMSSG,1120)NLN
      MSG(12)=IMSSG(1)
      MSG(13)=IMSSG(2)
 1120 FORMAT('LINE',I4)
      IERR=JJ
      RETURN
      END
