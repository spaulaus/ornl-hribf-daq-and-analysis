C$PROG HELPMANU  - Help Manager - Current version
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/20/2005 - for gnu
C     ******************************************************************
C
      SUBROUTINE HELPMANU(IWD,LU,IHELP,MAX,NDLINX,IHELPF)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40)
C
      INTEGER*4 IHELP(20,*),IDIR(19,50),KEYLST(50),LOC(50)
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,LWD)
C
      CHARACTER*4  IHELPF,KHELP
C
      INTEGER*4    BLANK,DOLRS,ALL,YES,CON
      character*4  cBLANK, cDOLRS, cALL, cCON
      equivalence (cBLANK,BLANK), (cDOLRS,DOLRS), 
     &            (cALL,ALL), (cCON,CON)
C
      DATA         cBLANK,cDOLRS/'    ','$$$$'/
      DATA         cALL,  cCON  /'ALL ','CON '/
C
      DATA NCALL,NKEY,L,KHELP/0,0,0,'NO  '/
C
      DATA IBELL/Z'07070707'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     HELP MANAGER - READS ASCII FILE ASSIGNED TO LU ON FIRST CALL
C                  - AND BUILDS HELP-LIST WITH KEY-WORD DIRECTORY
C                  - SUBSEQUENTLY DISPLAYS SUBSETS OF HELP-LIST
C     ------------------------------------------------------------------
C
      IF(IHELPF.NE.'YES ')   THEN
                             WRITE(LOGUT,5)
    5 FORMAT('HELP-FILE NOT FOUND - ON-LINE HELP UNAVAILABLE!')
                             RETURN
                             ENDIF
C
      NDLINES=NDLINX
      CALL WINSIZE(NCOLS,NROWS,IERR)
      IF(IERR.EQ.0) NDLINES=NROWS-1
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(NCALL.GT.0)         GO TO 100
C
      N=0
      L=0
      NCALL=1
   10 READ(LU,20,END=70,ERR=200)IWD
   20 FORMAT(20A4)
      L=L+1
      IF(L.GE.MAX)           GO TO 50
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
   50 WRITE(LOGUT,60)MAX
   60 FORMAT('HELP LIBRARY OVERFLOW AT ',I4,' LINES')
C
   70 CLOSE(UNIT=LU)
      KHELP='YES '
      IF(KMD.EQ.'INIT') RETURN
C
C     ------------------------------------------------------------------
C     DISPLAY SELECTED PORTION OF HELP-LIST DEFINED BY KEY-WORD
C     PROCESS - HELP KEYWD  (CONTAINED IN LWD(1,2))
C     ------------------------------------------------------------------
C
  100 IF(KHELP.NE.'YES ')    GO TO 200
C
      KEYWD=LWD(1,2)
      IF(KEYWD.EQ.ALL)   GO TO 300
      IF(KEYWD.EQ.BLANK) KEYWD=CON
C
      DO 110 I=1,NKEY
      IF(KEYWD.EQ.KEYLST(I)) GO TO 120
  110 CONTINUE
      GO TO 220
C
  120 N=LOC(I)-1
      NLN=0
  130 N=N+1
      IF(N.GT.L) RETURN
      IF(IHELP(1,N).EQ.DOLRS) RETURN
      NLN=NLN+1
      IF(NLN.GT.NDLINES) THEN
                        NLN=0
                        WRITE(LOGUT,145)IBELL
                        READ(5,150)IDUM
                        ENDIF
C
      WRITE(LOGUT,140)(IHELP(I,N),I=1,19)
  140 FORMAT(19A4)
C
  145 FORMAT(1H ,A1,'There is more - type: [RETURN] to continue',$)
  150 FORMAT(A4)
C
      GO TO 130
C
  200 WRITE(LOGUT,210)
  210 FORMAT('ERROR READING HELP-FILE - HELP NOT AVAILABLE')
      RETURN
C
  220 WRITE(LOGUT,230)KEYWD
  230 FORMAT(A4,' - NOT FOUND IN DIRECTORY')
      RETURN
C
  300 NLN=0
      DO 310 J=1,NKEY
      WRITE(LOGUT,305)(IDIR(I,J),I=1,18)
  305 FORMAT('Type: h ',18A4)
C
      IF(J.EQ.NKEY) RETURN
      NLN=NLN+1
      IF(NLN.GE.NDLINES) THEN
                        NLN=1
                        WRITE(LOGUT,145)IBELL
                        READ(5,150)IDUM
                        ENDIF
C
  310 CONTINUE
      WRITE(LOGUT,315)
  315 FORMAT('Type: h      - TO GET THIS LIST AGAIN')
      RETURN
      END
