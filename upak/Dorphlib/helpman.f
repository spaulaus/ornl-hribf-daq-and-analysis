C$PROG HELPMAN   - Help Manager -  Old version
C
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HELPMAN(LU,LO,JWD)
C
      INTEGER*4 IHELP(20,355),IDIR(19,50),KEYLST(50),LOC(50)
C
      INTEGER*4 IWD(20),JWD(20),STAT
C
      INTEGER*4 BLANK,DOLRS
      CHARACTER*4 cBLANK, cDOLRS
      EQUIVALENCE (cBLANK, BLANK), (cDOLRS,DOLRS)
C
      DATA      cBLANK,cDOLRS/'    ','$$$$'/
C
      CHARACTER*4 KHELP
C
      DATA NCALL,NKEY,L,KHELP/0,0,0,'NO  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     HELP MANAGER - READS ASCII FILE ASSIGNED TO LU ON FIRST CALL
C                  - AND BUILDS HELP-LIST WITH KEY-WORD DIRECTORY
C                  - SUBSEQUENTLY DISPLAYS SUBSETS OF HELP-LIST
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0)         GO TO 100
C
      N=0
      L=0
      NCALL=1
   10 READ(LU,20,END=70,ERR=200)IWD
   20 FORMAT(20A4)
      L=L+1
      IF(L.GT.355)           GO TO 50
C
      DO 30 I=1,20
      IHELP(I,L)=IWD(I)
   30 CONTINUE
C
      IF(IWD(1).NE.DOLRS)   GO TO 10
      N=N+1
      NKEY=N
      KEYLST(N)=IWD(2)
      LOC(N)=L+1
      DO 40 I=1,19
      IDIR(I,N)=IWD(I+1)
   40 CONTINUE
      GO TO 10
C
   50 WRITE(LO,60)
   60 FORMAT(1H ,'HELP LIBRARY OVERFLOW AT 355 LINES')
C
   70 CLOSE(UNIT=LU)
      KHELP='YES '
C
C     ------------------------------------------------------------------
C     DISPLAY SELECTED PORTION OF HELP-LIST DEFINED BY KEY-WORD
C     PROCESS - HELP KEYWD  (CONTAINED IN JWD)
C     ------------------------------------------------------------------
C
  100 IF(KHELP.NE.'YES ')    GO TO 200
C
      KEYWD=BLANK
      IA=NXNB(JWD,5,80)
      IF(IA.LE.0)            GO TO 300
      CALL LODUP(JWD,IA,IA+3,KEYWD,1)
C
      DO 110 I=1,NKEY
      IF(KEYWD.EQ.KEYLST(I)) GO TO 120
  110 CONTINUE
      GO TO 220
C
  120 N=LOC(I)-1
  130 N=N+1
      IF(N.GT.L) RETURN
      IF(IHELP(1,N).EQ.DOLRS) RETURN
C
      WRITE(LO,140)(IHELP(I,N),I=1,19)
  140 FORMAT(1H ,19A4)
      GO TO 130
C
  200 WRITE(LO,210)
  210 FORMAT(1H ,'ERROR READING HELP-FILE - HELP NOT AVAILABLE')
      RETURN
C
  220 WRITE(LO,230)KEYWD
  230 FORMAT(1H ,A4,' - NOT FOUND IN DIRECTORY')
      RETURN
C
  300 DO 310 J=1,NKEY
      WRITE(LO,305)(IDIR(I,J),I=1,17)
  305 FORMAT(1H ,'Type: HELP ',17A4)
  310 CONTINUE
      WRITE(LO,315)
      WRITE(LO,320)
      WRITE(LO,325)
      WRITE(LO,330)
  315 FORMAT(1H )
  320 FORMAT(1H ,'Type: LON       - To turn ON  Printer-Log')
  325 FORMAT(1H ,'Type: LOF       - To turn OFF Printer-Log')
  330 FORMAT(1H ,'Type: HELP      - To get this list again')
      RETURN
      END
