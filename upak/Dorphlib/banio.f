C     $PROG BANIO     - BAN-file I/O routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE BANIO(MO,LU,FIL,TIT,IH,IBN,DG,IX,JY,NP,NID,KPAR,MSG,
     ,IERR)
C
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 TIT(20),FIL(6),KPAR(9),MSG(7),IX(64),JY(64)
C
      INTEGER*4 LINE(20),IDIR(880),MESS(7,14),IHTONP(6),DG
C
      INTEGER*4 IZERO(20)
C
      INTEGER*4 EOFTST
C
      CHARACTER*28 CMESS(14)
      CHARACTER*40 CZERO(2)
      CHARACTER*80 CLINE,CIWD
C
      EQUIVALENCE (CLINE,LINE),(CIWD,IWD)
C
      EQUIVALENCE (LINE,IWD),(CMESS,MESS),(CZERO,IZERO)
C
      CHARACTER*4 CLAB_TIT,CLAB_CXY,CLAB_EMPT,CLAB_Y
      INTEGER*4    LAB_TIT, LAB_CXY, LAB_EMPT, LAB_Y
C
      EQUIVALENCE (CLAB_TIT, LAB_TIT)
      EQUIVALENCE (CLAB_CXY, LAB_CXY)
      EQUIVALENCE (CLAB_EMPT,LAB_EMPT)
      EQUIVALENCE (CLAB_Y,   LAB_Y)
C
      DATA        CLAB_TIT /'TIT '/
      DATA        CLAB_CXY /'CXY '/
      DATA        CLAB_EMPT/'EMPT'/
      DATA        CLAB_Y   /'Y   '/
C
C
      DATA CZERO/'    0    0    0    0    0    0    0    0',
     &           '    0    0    0    0    0    0    0    0'/
C
      DATA CMESS/'READ ERR - ID NOT FOUND     ',    !IERR=1
     2           'STOR ERR - ID ALREADY EXISTS',    !IERR=2
     3           'REPLACE ERR - ID NOT FOUND  ',    !IERR=3
     4           'DELETE ERR - ID NOT FOUND   ',    !IERR=4
     5           'MORE THAN 63 X,Y-POINTS     ',    !IERR=5
     6           'BAN-FILE FULL (MAX # = 880) ',    !IERR=6
     7           'ERROR DECODING INP LINE     ',    !IERR=7
     8           'ERROR DECODING GATE LINE    ',    !IERR=8
     9           'ERROR DECODING X-Y LIST     ',    !IERR=9
     A           'ERROR DOING DIRECTORY I/O   ',    !IERR=10
     B           'ERROR DOING INP-LINE I/O    ',    !IERR=11
     C           'ERROR DOING TIT-LINE I/O    ',    !IERR=12
     D           'ERROR DOING GATE-LINE I/O   ',    !IERR=13
     E           'ERROR DOING CXY-LINE I/O    '/    !IERR=14
C
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO READ, STORE, REPLACE AND DELETE BANANAS
C     FROM STANDARD BAN-FILE
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
C     MSG  - ARRAY CONTAINING ERROR MESSAGE IF IERR.NE.0
C     IERR = ERROR FLAG
C
C     IEXIS = EXIST-FLAG FOR IBN (REQUESTED BAN-ID)
C     IEXIS = 0 SAYS IBN DOES NOT EXIST
C     IEXIS = K SAYS IBN DOES EXIST AT IDIR(K)
C
C     MPTY  = 0 SAYS NO EMPTY DIRECTORY SLOTS
C     MPTY  = K SAYS FIRST EMPTY DIRECTORY SLOT IS AT IDIR(K)
C
C     NDENT = NUMBER OF DIRECTORY ENTRIES ON FILE (MULTIPLE OF 80)
C     ------------------------------------------------------------------
C     MO = 0   SAYS INIT THE BAN-FILE (ZERO DIRECTORY)
C     MO = 1   SAYS READ IN BANANA (ID = IBN)
C     MO = 2   SAYS STORE   BANANA (ID = IBN)
C     MO = 3   SAYS REPLACE BANANA (ID = IBN)
C     MO = 4   SAYS DELETE  BANANA (ID = IBN)
C     MO = 5   SAYS RETURN  DIRECTORY IN ARRAY - IX
C     ------------------------------------------------------------------
C
      IERR=0
      IF(MO.NE.0) GO TO 150
C
C     ------------------------------------------------------------------
C     INITIALIZE DIRECTORY - WRITE OUT FIRST 80 DIRECTORY SLOTS
C     ------------------------------------------------------------------
C
      DO 110 I=1,80
      IDIR(I)=0
  110 CONTINUE
      NDENT=80
      NID=0
      NBN=0
      DO 130 K=1,5
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)IZERO
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1100
      NBN=NBN+1
  130 CONTINUE
C
      CALL FLUSH(LU)                        !Force the output
C
      RETURN
C
C     ------------------------------------------------------------------
C     READ IN DIRECTORY, GET NID, LOOK FOR IBN, ETC
C     ------------------------------------------------------------------
C
  150 DO 152 I=1,880
      IDIR(I)=0
  152 CONTINUE
      NN=0
      NBN=0
  153 DO 160 KK=1,5
      READ(LU,REC=NBN+1,IOSTAT=IOS)IWD
      IF(EOFTST(IOS).NE.0) GO TO 162
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1100
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1100
      IF(NF.NE.16)  GO TO 1100
      DO 154 J=1,16
      IF(ITYP(J).NE.2) GO TO 1100
      NN=NN+1
      CALL LIMIV(LWD(1,J),-9999,99999,IDIR(NN),IERR)
      IF(IERR.NE.0) GO TO 1100
  154 CONTINUE
      NBN=NBN+1
  160 CONTINUE
      NBN=NBN+960
      GO TO 153
C
  162 NDENT=NN
      NID=0
      IEXIS=0
      MPTY=0
      DO 170 I=1,NDENT
      IF(IDIR(I).NE.0) NID=NID+1
      IF(IDIR(I).EQ.0) GO TO 165
      IF(IDIR(I).EQ.IBN)  IEXIS=I
      IF(IDIR(I).EQ.-IBN) IEXIS=I
  165 IF(MPTY.NE.0) GO TO 170
      IF(IDIR(I).EQ.0) MPTY=I
  170 CONTINUE
C
      IF(MO.EQ.1) GO TO 200
      IF(MO.EQ.2) GO TO 300
      IF(MO.EQ.3) GO TO 310
      IF(MO.EQ.4) GO TO 400
      IF(MO.EQ.5) GO TO 500
C
C     ------------------------------------------------------------------
C     READ IN BANANA # IBN
C     ------------------------------------------------------------------
C
  200 IF(IEXIS.EQ.0) GO TO 1010             !DOES IBN EXIST
      CALL LOCBAN(IEXIS,NBN,III,III,III)    !GET REC# OF BANANA
C
C     ******************************************* READ IN FIL,IH,IB,..NP
C
      READ(LU,REC=NBN+1,IOSTAT=IOS)IWD
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1110
      IA=NXNB(IWD,4,80)
      IF(IA.LE.0) GO TO 1070
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0) GO TO 1070
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
      IF(NTER.NE.0) GO TO 1070
      IF(NF.GT.4) NF=4
C
      DO 210 J=1,NF
      CALL MILV(LWD(1,J),IHTONP(J),XV,KIND,MERR)
      IF(MERR.NE.0) GO TO 1070
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
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1120
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
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1130
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 1080
      IF(NF.GT.9) NF=9
      DO 230 J=1,NF
      CALL MILV(LWD(1,J),KPAR(J),XV,KIND,MERR)
      IF(MERR.NE.0) GO TO 1080
  230 CONTINUE
C
C     ********************************************* READ IN BANANA-LINES
C
      N=0                                   !ZERO # PTS CNTR
      DO 260 I=1,9                          !LOOP ON 9 CXY LINES
      NBN=NBN+1
      READ(LU,REC=NBN+1,IOSTAT=IOS)LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1140
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0)    GO TO 1090
      IF(NF.EQ.0)      GO TO 260
      NDO=NF/2
      IF(2*NDO.NE.NF)  GO TO 1090
      J=0
      DO 250 K=1,NDO                        !LOOP ON # X-Y PNTS
      N=N+1
      J=J+1
      IF(ITYP(J).NE.2) GO TO 1090
      CALL IVALU(LWD(1,J),IX(N),JERR)       !PICK UP X-VALUE
      IF(JERR.NE.0)    GO TO 1090
      J=J+1
      IF(ITYP(J).NE.2) GO TO 1090
      CALL IVALU(LWD(1,J),JY(N),JERR)       !PICK UP Y-VALUE
      IF(JERR.NE.0)    GO TO 1090
  250 CONTINUE
  260 CONTINUE
      NP=N
      RETURN
C
C     ------------------------------------------------------------------
C     STORE OR REPLACE BANANA - IBN
C     ------------------------------------------------------------------
C                                           !STORE - STORE - STORE
C     ------------------------------------------------------------------
C
  300 CONTINUE
      IF(IEXIS.NE.0) GO TO 1020             !CHECK FOR IBN EXISTS
      IF(NP.GT.63)   GO TO 1050             !CHECK FOR TOO MANY PTS
C
      IF(MPTY.NE.0)  THEN                   !TST FOR DIRECTORY SPACE
                     NTR=MPTY               !IF YES, STORE IT
                     IDIR(NTR)=IBN          !PUT IBN IN DIRECTORY
                     NID=NID+1              !BUMP # OF ID'S
                     GO TO 320              !GO WRITE IT OUT
                     ENDIF
C
      IF(NDENT.GE.880) GO TO 1060           !CHECK FOR FILE FULL
C
      NTR=NDENT+1                           !OTHERWISE,
      CALL LOCBAN(NTR,III,NBN,III,III)      !COMPUTE LOCATION OF
      DO 304 I=1,5                          !NEXT DIRECTORY BLOCK AND
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)IZERO   !ADD ANOTHER 80 ENTRIES
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1100
      NBN=NBN+1
  304 CONTINUE
      NTR=NDENT+1
      NDENT=NDENT+80
      IDIR(NTR)=IBN                         !PUT IBN IN DIRECTORY
      NID=NID+1                             !BUMP # OF ID'S
      GO TO 320                             !GO WRITE IT OUT
C
C     ------------------------------------------------------------------
C                                           !REPLACE - REPLACE
C     ------------------------------------------------------------------
C
  310 IF(IEXIS.EQ.0) GO TO 1030             !CHECK FOR IBN EXISTS
      IF(NP.GT.63)   GO TO 1050             !CHECK FOR TOO MANY PTS
      NTR=IEXIS
C
  320 CALL LOCBAN(NTR,NBN,III,III,III)      !GET BANANA LOCATION
      IBP=IABS(IBN)
      DO 322 I=1,20
      LINE(I)=Z'20202020'
  322 CONTINUE
      WRITE(CLINE,330)FIL,IH,IBP,DG,NP
  330 FORMAT('INP ',6A4,2X,4I6)
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1110
C
      LINE(1)=LAB_TIT
      DO 332 I=1,19
      LINE(I+1)=TIT(I)
  332 CONTINUE
      NBN=NBN+1
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1120
C
      DO 342 I=1,20
      LINE(I)=Z'20202020'
  342 CONTINUE
      WRITE(CLINE,350)KPAR
  350 FORMAT('GATE ',6I6,3I8)
      NBN=NBN+1
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1130
C
      IHI=0
      DO 390 K=1,9
      DO 365 I=1,20
      LINE(I)=Z'20202020'
  365 CONTINUE
      LINE(1)=LAB_CXY
      ILO=IHI+1
      IF(ILO.GT.NP) GO TO 375
      IHI=ILO+6
      IF(IHI.GT.NP) IHI=NP
      WRITE(CLINE,370)(IX(I),JY(I),I=ILO,IHI)
  370 FORMAT('CXY  ',14I5)
  375 NBN=NBN+1
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1140
  390 CONTINUE
      GO TO 450
C
C     ------------------------------------------------------------------
C     DELETE BANANA - IBN
C     ------------------------------------------------------------------
C
  400 IF(IEXIS.EQ.0) GO TO 1040
      NTR=IEXIS
      IDIR(NTR)=0
      NID=NID-1
      CALL LOCBAN(NTR,NBN,III,III,III)      !GET BANANA LOCATION
      DO 410 I=1,20
      LINE(I)=Z'20202020'
  410 CONTINUE
      LINE(1)=LAB_EMPT
      LINE(2)=LAB_Y
      DO 430 I=1,12
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1140
      NBN=NBN+1
  430 CONTINUE
      GO TO 450
C
C     ------------------------------------------------------------------
C     WRITE OUT DIRECTORY-LINE CONTAINING MODIFIED/NEW ENTRY
C     ------------------------------------------------------------------
C
  450 CALL LOCBAN(NTR,III,NBN,ILO,IHI)      !GET DIRECTORY REC# &
C                                           !FIRST & LAST ELEMENTS
      WRITE(CIWD,455)(IDIR(I),I=ILO,IHI)    !CONVERT TO ASCII
  455 FORMAT(16I5)
      WRITE(LU,REC=NBN+1,IOSTAT=IOS)IWD     !OUT MODIFIED/NEW LINE
      IF(IOS.NE.0) CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1100
C
      CALL FLUSH(LU)                        !Force the output
C
      RETURN
C
C     ------------------------------------------------------------------
C     RETURN DIRECTORY IN ARRAY - IX  (ASSUME LARGE ENOUGH)
C     ------------------------------------------------------------------
C
  500 N=0
      DO 510 I=1,NDENT
      IF(IDIR(I).EQ.0) GO TO 510
      N=N+1
      IX(N)=IDIR(I)
  510 CONTINUE
      NID=N
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
      GO TO 1200
C
 1100 IERR=10
      GO TO 1200
C
 1110 IERR=11
      GO TO 1200
C
 1120 IERR=12
      GO TO 1200
C
 1130 IERR=13
      GO TO 1200
C
 1140 IERR=14
C
 1200 DO 1220 I=1,7
      MSG(I)=MESS(I,IERR)
 1220 CONTINUE
      RETURN
      END
