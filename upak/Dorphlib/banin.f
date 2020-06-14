C$PROG BANIN     - BAN-file input routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE BANIN(LU,IBN,IX,JY,NP,KPAR,IERR,MSG)
C
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 TIT(20),FIL(6),KPAR(9),MSG(10),IX(64),JY(64)
C
      INTEGER*4 LINE(20),IDIR(880),MES(10,9),IHTONP(6),DG
C
      INTEGER*4 EOFTST
C
      CHARACTER*40 CMES(9)
C
      EQUIVALENCE (LINE,IWD),(CMES,MES)
C
      DATA NDENT/0/
C
      DATA CMES/'READ ERR - ID NOT FOUND                 ',    !1
     2          'ERROR DECODING INP LINE                 ',    !2
     3          'ERROR DECODING GATE LINE                ',    !3
     4          'ERROR DECODING X-Y LIST                 ',    !4
     5          'ERROR DOING DIRECTORY I/O               ',    !5
     6          'ERROR DOING INP-LINE I/O                ',    !6
     7          'ERROR DOING TIT-LINE I/O                ',    !7
     8          'ERROR DOING GATE-LINE I/O               ',    !8
     9          'ERROR DOING CXY-LINE I/O                '/    !9
C
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     READS BANANA# IBN FROM STANDARD BAN-FILE
C     ------------------------------------------------------------------
C     THE BAN-FILE IS COMPOSED OF 80-BYTE ASCII RECORDS
C
C     LINE-1 - DIRECTORY  (16I5)
C     LINE-2 - DIRECTORY  (16I5)
C     LINE-3 - DIRECTORY  (16I5)
C     LINE-4 - DIRECTORY  (16I5)
C     LINE-5 - DIRECTORY  (16I5)
C
C     LET N="ENTRY #" AND M=N-1
C
C     LINE-(5+12*M+1) - 'INP ',FILENAME,IH,IB,DG,NP     (7A4,2X,4I5)
C     LINE-(5+12*M+2) - 'TIT ',TITLE                    (20A4)
C     LINE-(5+12*M+3) - 'GATE ',IPX,IPY,LXD,LXG,LYD,LYG (A4,1X,6I5)
C     LINE-(5+12*M+4) - 'CXY  ',X,Y X,Y X,Y .....       (A4,1X,14I5)
C     LINE-(5+12*M+12)- 'CXY  ',X,Y X,Y X,Y .....       (A4,1X,14I5)
C     ------------------------------------------------------------------
C
C     LU   = LOGICAL UNIT # FOR BAN-FILE
C     FIL  - CONTAINS .HIS-FILE NAME
C     TIT  - CONTAINS TITLE
C     IH   = HISTOGRAM ID #
C     IBN  = BANANA    ID #
C     DG   = PROJECTION AXIS IN DEGREES
C     IX   - ARRAY CONTAINING X-COORDINATES
C     JY   - ARRAY CONTAINING Y-COORDINATES
C     NP   = # OF X,Y-POINTS
C     NID  = # OF BANANAS ON FILE
C     KPAR(I),I=1,9 - CONTAINS IPX,IPY,LXD,LXG,LYD,LYG,NUPM,IAUX,JAUX
C     IERR = ERROR FLAG
C     ------------------------------------------------------------------
C
      IERR=0
      IF(IBN.NE.0) GO TO 150
C
C     ------------------------------------------------------------------
C     READ IN DIRECTORY - INITIALIZE - FOR IBN=0 ONLY
C     ------------------------------------------------------------------
C
      DO 50 I=1,880
      IDIR(I)=0
   50 CONTINUE
      NN=0
      NBN=0
  100 DO 120 KK=1,5
      READ(LU,REC=NBN+1,IOSTAT=IOS)IWD
      IF(EOFTST(IOS).NE.0) GO TO 130
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1050
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1050
      IF(NF.NE.16)  GO TO 1050
      DO 110 J=1,16
      IF(ITYP(J).NE.2) GO TO 1050
      NN=NN+1
      CALL LIMIV(LWD(1,J),-9999,99999,IDIR(NN),IERR)
      IF(IERR.NE.0) GO TO 1050
  110 CONTINUE
      NBN=NBN+1
  120 CONTINUE
      NBN=NBN+960
      GO TO 100
C
  130 NDENT=NN
      RETURN
C
C     ------------------------------------------------------------------
C     TEST FOR EXISTANCE OF BANANA-IBN AND READ IT IN
C     ------------------------------------------------------------------
C
  150 IEXIS=0
      DO 170 I=1,NDENT
      IF(IDIR(I).EQ.IBN)  IEXIS=I
  170 CONTINUE
C
      IF(IEXIS.EQ.0) GO TO 1010             !DOES IB EXIST
      NBN=5*((IEXIS+79)/80)+12*(IEXIS-1)    !GET REC# OF BANANA
C
C     ******************************************* READ IN FIL,IH,IB,..NP
C
      READ(LU,REC=NBN+1,IOSTAT=IOS)IWD
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1060
      IA=NXNB(IWD,4,80)
      IF(IA.LE.0) GO TO 1020
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0) GO TO 1020
C
      DO 208 I=1,6
      FIL(I)=Z'20202020'
      IHTONP(I)=0
  208 CONTINUE
C
      IBB=IB
      IF(IBB.GT.28) IBB=28
      CALL LODUP(IWD,IA,IBB,FIL,1)
      IA=IB+1
      CALL GREAD(IWD,LWD,ITYP,NF,IA,80,NTER)
      IF(NTER.NE.0) GO TO 1020
      IF(NF.GT.4) NF=4
C
      DO 210 J=1,NF
      CALL MILV(LWD(1,J),IHTONP(J),XV,KIND,MERR)
      IF(MERR.NE.0) GO TO 1020
  210 CONTINUE
C
      IH=IHTONP(1)
      DG=IHTONP(3)
      NP=IHTONP(4)
C
C     ************************************************* READ IN TIT-LINE
C
      NBN=NBN+1
      READ(LU,REC=NBN+1,IOSTAT=IOS)IWD
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1070
      IFLG=IWD(1)
      DO 212 I=1,19
      TIT(I)=IWD(I+1)
  212 CONTINUE
      TIT(20)=Z'20202020'
C
C     ************************************************ READ IN GATE-LINE
C
      DO 220 I=1,9
      KPAR(I)=0
  220 CONTINUE
      NBN=NBN+1
      READ(LU,REC=NBN+1,IOSTAT=IOS)IWD
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1080
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 1030
      IF(NF.GT.9) NF=9
      DO 230 J=1,NF
      CALL MILV(LWD(1,J),KPAR(J),XV,KIND,MERR)
      IF(MERR.NE.0) GO TO 1030
  230 CONTINUE
C
C     ********************************************* READ IN BANANA-LINES
C
      N=0                                   !ZERO # PTS CNTR
      DO 260 I=1,9                          !LOOP ON 9 CXY LINES
      NBN=NBN+1
      READ(LU,REC=NBN+1,IOSTAT=IOS)LINE
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1090
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0)    GO TO 1040
      IF(NF.EQ.0)      GO TO 260
      NDO=NF/2
      IF(2*NDO.NE.NF)  GO TO 1040
      J=0
      DO 250 K=1,NDO                        !LOOP ON # X-Y PNTS
      N=N+1
      J=J+1
      IF(ITYP(J).NE.2) GO TO 1040
      CALL IVALU(LWD(1,J),IX(N),JERR)       !PICK UP X-VALUE
      IF(JERR.NE.0)    GO TO 1040
      J=J+1
      IF(ITYP(J).NE.2) GO TO 1040
      CALL IVALU(LWD(1,J),JY(N),JERR)       !PICK UP Y-VALUE
      IF(JERR.NE.0)    GO TO 1040
  250 CONTINUE
  260 CONTINUE
      NP=N
      RETURN
C
C     ------------------------------------------------------------------
C     SET UP ERROR MESSAGES AND ERROR CODE - IERR
C     ------------------------------------------------------------------
C
 1010 IERR=1
      GO TO 1200
 1020 IERR=2
      GO TO 1200
 1030 IERR=3
      GO TO 1200
 1040 IERR=4
      GO TO 1200
 1050 IERR=5
      GO TO 1200
 1060 IERR=6
      GO TO 1200
 1070 IERR=7
      GO TO 1200
 1080 IERR=8
      GO TO 1200
 1090 IERR=9
C
 1200 DO 1220 I=1,10
      MSG(I)=MES(I,IERR)
 1220 CONTINUE
      RETURN
      END
