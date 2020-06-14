C$PROG NUGAT
      SUBROUTINE NUGAT(IERR,MSER)
C
      INTEGER*2 ISH
C
      INTEGER*2 GSID,GNOG,GLEN,MLEN,GAPO
C
      INTEGER*2 GXNX,GXLN
C
      INTEGER*4 GLOC,GXLOC
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/GGG/ GSID(128),GFOR(128),GSOR(128),GNOG(128),GLEN(128),
     &GLOC(128),MLEN(128),MLOC(128),GAPO(128),NGSET
      CHARACTER*2           GFOR,     GSOR
C
      COMMON/III/ GXNX(128),GXLN(128),GXLOC(128),NGXN
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
C
      INTEGER*2 MIL(65536)
C
      INTEGER*2 GLIB(4096),ITIT(40)
C
      INTEGER*2 NTFND(128),NTFUL(128),TOFUL(128)
C
      CHARACTER*40  MSER
      INTEGER*4     MESS(10,4),MSG(13),NAMF(20)
      CHARACTER*40  MSC(4)
      CHARACTER*80  CNAMF
C
      INTEGER*4 LIBGAT(2052),LTIT(20)
C
      EQUIVALENCE (MIL(1),MILF(1))
C
      EQUIVALENCE (NUMSET,LIBGAT(2)),(MAXL,LIBGAT(3))
C
      EQUIVALENCE (GLIB(1),LIBGAT(5)),(ITIT(1),LTIT(1))
C
      EQUIVALENCE (MSC(1),MESS(1,1)),(CNAMF,NAMF)
C
      DATA  MSC/'SYNTAX ERROR IN GATE-FILE SPECIFICATION',
     2          'UNABLE TO OPEN GATE-FILE               ',
     3          'ILLEGAL GATE-FILE STRUCTURE            ',
     4          '                                       '/
C
      DATA LU/13/
C
      SAVE
C
C     **************************************************************
C     PROCESS  -  $GAF FILENAM.GAF/ACT
C     **************************************************************
C     PICK UP FILE NAME AND OPEN GAF-FILE
C     **************************************************************
C
      IERR=0
C
      CNAMF=' '
      MSER =' '
C
      IA=NXNB(IWD,5,80)
      IF(IA.LE.0)     GO TO 1010
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0)     GO TO 1010
      CALL LODUP(IWD,IA,IB,NAMF,1)
C
      OPEN(UNIT=LU,FILE=CNAMF,STATUS='OLD',ACCESS='SEQUENTIAL',
     &             IOSTAT=IOS)
      CALL OPERR(IOS)
      IF(IOS.NE.0)     GO TO 1020
C
C     **************************************************************
C     GATE FILE STRUCTURE
C
C     LIBGAT(1) - CONTAINS 'GAF ' FOR IDENTIFICATION
C     LIBGAT(2) - CONTAINS NUMSET (NUMBER OF GATE SETS ON FILE)
C     LIBGAT(3) - CONTAINS MAXL   (# HALF-WDS USED FOR GATE-SETS)
C     LIBGAT(4) - CONTAINS NSEC   (NUMBER OF SECTORS TO READ/WRITE)
C
C     GATE-SET
C     GATE-SET
C     --------
C     **************************************************************
C     GATE-SET STRUCTURE
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
C     READ GATE-FILE LIBRARY FROM DISK
C     **************************************************************
C
      CALL GATIN(LU,LIBGAT,GLIB,IERR,MSG)
      IF(IERR.NE.0) GO TO 1050
      IF(MAXL.LE.0) GO TO 1040
C
C     **************************************************************
C     SEARCH "LIBGAT" FOR REQUESTED ID'S AND PROCESS AS REQUIRED
C     **************************************************************
C
      NNOT=0                                !# GATE-SETS NOT FOUND
      NTOO=0                                !# GATE-SETS TOO FULL
      NUNF=0                                !# GATE-SETS UN-FULL
      DO 400 IG=1,NGSET                     !LOOP ON # OF GATE SETS
      IF(GSOR(IG).NE.'GF') GO TO 400        !TST FOR SOURCE = "FILE"
      ISN=GSID(IG)                          !GATE-SET ID
C
C     ************************************** SEARCH GLIB FOR SET#
C
      NS=0                                  !INIT GLIB SET CNTR
      N=1                                   !SET POINTER TO 1ST SET
   60 NS=NS+1                               !INC SET CNTR
      IF(NS.GT.NUMSET) GO TO 300            !TST FOR DONE
      IF(GLIB(N).EQ.ISN) GO TO 100          !TST FOR REQUIRED SET
      N=N+GLIB(N+3)+GLIB(N+4)+5             !POINT TO NEXT SET
      GO TO 60                              !GO BACK AND TST IT
C
C     ************************************** PICK UP DATA FROM GLIB
C
  100 LHIS=GLIB(N+2)                        !HIST LENGTH FROM GLIB
      GLEN(IG)=LHIS                         !SET LENGTH BASIS
      NWT=GLIB(N+3)                         !# HALF-WDS OF TITLE
      NWG=GLIB(N+4)                         !# HALF-WDS OF GATES
      N=N+NWT+5                             !POINT TO GLIB GATE-LIST
      NST=N                                 !SAVE FOR LATER
      NGA=NWG/2                             !# OF GATES IN GLIB LIST
      IF(NGA.GT.GNOG(IG)) NGA=GNOG(IG)      !TST VS "GNOG"
      IF(NGA.LT.GNOG(IG)) THEN              !TST FOR UN-FULL
                          NUNF=NUNF+1       !INC #UN-FULL
                          NTFUL(NUNF)=ISN   !STORE SET#
                          ENDIF
C
      IF(NGA.GT.GNOG(IG)) THEN              !TST FOR TOO FULL
                          NTOO=NTOO+1       !INC # TOO FULL
                          TOFUL(NTOO)=ISN   !STORE SET#
                          ENDIF
C
C     ************************************** STORE IN MIL GATE-REG
C
      L=GLOC(IG)-1                          !INIT START LOC IN MIL
      DO 110 I=1,NGA                        !LOOP ON # OF GATES
      L=L+1
      MIL(L)=GLIB(N+1)                      !HI-LIMIT
      L=L+1                                 !INC MIL-INDEX
      MIL(L)=GLIB(N)                        !LO-LIMIT
      N=N+2                                 !INC GLIB-INDEX BY 2
  110 CONTINUE
C
      IF(GFOR(IG).NE.'MA') GO TO 400
C
C    *************************************** STORE IN MIL MAP-REG
C
      LENM=MLEN(IG)
      ISH=LOGB2(LENM)-LOGB2(LHIS)
C
      IA=MLOC(IG)                           !FIRST MAP LOCATION
      LOC=IA                                !FIRST MAP LOCATION
      IB=IA+LENM-1                          !LAST  MAP LOCATION
      DO 210 I=IA,IB                        !LOOP TO ZOT MAP
      MIL(I)=-1                             !SET TO IMPOSSIBLE
  210 CONTINUE
C
      N=NST
      DO 230 K=1,NGA                        !LOOP ON # OF GATES
      IA=LOC+ISHFT(GLIB(N),ISH)             !LO-INDEX IN MIL
      IB=LOC+ISHFT(GLIB(N+1),ISH)           !HI-INDEX IN MIL
      N=N+2
      DO 220 I=IA,IB                        !LOOP ON RANGE OF GATE
      MIL(I)=K-1                            !SET TO GATE#-1
  220 CONTINUE
  230 CONTINUE
      GO TO 400
C
  300 NNOT=NNOT+1                           !INC # SETS NOT FOUND
      NTFND(NNOT)=ISN                       !STORE SET#
C
  400 CONTINUE
C
C     **************************************************************
C     LIST ANY GATE-SETS NOT FOUND, NOT FULL, TOO FULL
C     **************************************************************
C
      IF(NNOT.GT.0) THEN                    !LIST ANY SETS NOT FOUND
                    WRITE(0,410)
                    WRITE(6,410)
                    WRITE(0,440)(NTFND(I),I=1,NNOT)
                    WRITE(6,440)(NTFND(I),I=1,NNOT)
                    ENDIF
      IF(NTOO.GT.0) THEN                    !LIST ANY SETS TOO FULL
                    WRITE(0,420)
                    WRITE(6,420)
                    WRITE(0,440)(TOFUL(I),I=1,NTOO)
                    WRITE(6,440)(TOFUL(I),I=1,NTOO)
                    ENDIF
      IF(NUNF.GT.0) THEN                    !LIST ANY SETS UN-FULL
                    WRITE(0,430)
                    WRITE(6,430)
                    WRITE(0,440)(NTFUL(I),I=1,NUNF)
                    WRITE(6,440)(NTFUL(I),I=1,NUNF)
                    ENDIF
  410 FORMAT(1H ,'GATE-SETS NOT FOUND ON GATE-FILE FOLLOW')
  420 FORMAT(1H ,'GATE-SETS WITH MORE GATES THAN REQUESTED FOLLOW')
  430 FORMAT(1H ,'GATE-SETS WITH LESS GATES THAN REQUESTED FOLLOW')
  440 FORMAT(1H ,10I6)
      GO TO 2000
C
C     **************************************************************
C     SET UP ERROR MESSAGE AND ERROR CODE
C     **************************************************************
C
 1010 IERR=1
      GO TO 1100
 1020 IERR=2
      GO TO 1100
C
 1040 WRITE(0,1045)
      WRITE(6,1045)
 1045 FORMAT(1H ,'GATE-FILE IS EMPTY')
      GO TO 2000
C
 1050 WRITE(0,1052) MSG
      WRITE(6,1052) MSG
 1052 FORMAT(1H ,13A4)
      IERR=3
C
 1100 MSER=MSC(IERR)
      WRITE(0,1115)MSER
 1115 FORMAT(1H ,A)
C
 2000 CLOSE(LU)
      RETURN
      END
