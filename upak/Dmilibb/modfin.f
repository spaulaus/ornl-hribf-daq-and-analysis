C$PROG MODFIN    - Replaces symbol in filename with ASCII integer
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE MODFIN(IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C   
      COMMON/ML02/ IWDRAW(20)
C   
      COMMON/ML03/ ISYN(500),ISYV(500),NSYM
C
      CHARACTER*8  ISYN,CASK
C
      INTEGER*4    JWD(30),ASK(2),X22
C
      DATA         X22/Z'22'/
C
      CHARACTER*4  KMD
C
      CHARACTER*8  KSY
C
      INTEGER*4   IKSY(2)
C
      EQUIVALENCE (IKSY,KSY)
C   
      EQUIVALENCE (KMD,LWD(1,1)),(CASK,ASK)
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO REPLACE "SYM" IN FILE-NAME WITH ASCII INTEGER
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(KMD.EQ.'IN  ') GO TO 10
      IF(KMD.EQ.'OU  ') GO TO 10
      IF(KMD.EQ.'QF  ') GO TO 10
      IF(KMD.EQ.'RF  ') GO TO 10
      IF(KMD.EQ.'SF  ') GO TO 10
      IF(KMD.EQ.'BAN ') GO TO 10
      IF(KMD.EQ.'BANF') GO TO 10
      RETURN
C   
   10 IA=IFIND(IWDRAW,X22,01,80)            !LOC OF FIRST - "
      IF(IA.LE.0) RETURN                    !OK IF NOT FOUND
      IS=IA+1
      IB=IFIND(IWDRAW,X22,IS,80)            !LOC OF NEXT  - "
      IF(IB.LE.0)    GO TO 500              !ERROR IF NOT FOUND
      IF(IB-IA.LT.2) GO TO 500              !TST FOR LEGAL
      IF(IB-IA.GT.9) GO TO 500              !TST FOR LEGAL
      LN=NXBL(IWDRAW,IB,80)-1               !LOC OF LAST NAME BYTE
      IF(LN.LE.0)    GO TO 500              !TST FOR ERROR
      JA=IA+1                               !LOC OF FIRST SYM BYTE
      JB=IB-1                               !LOC OF LAST  SYM BYTE
      KSY=' '                               !ZOT SYMBOL
      CALL LODUP(IWDRAW,JA,JB,IKSY,1)       !LOAD SYMBOL
      CALL KASEUP(KSY,1,8)                  !CONVERT SYMBOL TO UPPER CASE
C   
      DO 20 I=1,NSYM                        !LOOP TO FIND SYM VALUE
      IF(KSY.EQ.ISYN(I)) GO TO 30           !TST SYM NAME
   20 CONTINUE
      GO TO 510                             !ERROR IF NOT FOUND
C   
   30 WRITE(CASK,40)ISYV(I)                 !CONVERT TO ASCII
   40 FORMAT(I8)
C   
      DO 50 I=1,30                          !ZOT TEMP BUFFER
      JWD(I)=Z'20202020'
   50 CONTINUE
C   
      KA=NXNB(ASK,1,8)                      !LOC OF FIRST NON-BLANK
C   
      CALL LODUP(IWDRAW,1,IA-1,JWD,1)       !LOAD FIRST PART
C   
      CALL LODUP(ASK,KA,8,JWD,IA)           !LOAD ASCII SYMBOL
C   
      IF(IB+1.GT.LN) GO TO 60               !TST FOR MORE TO LOAD
C   
      NX=IA+(8-KA+1)                        !NEXT JWD-BYTE TO LOAD
C   
      CALL LODUP(IWDRAW,IB+1,LN,JWD,NX)     !LOAD LAST PART
C   
   60 DO 70 I=1,20                          !LOAD IT ALL BACK IN IWD
      IWDRAW(I)=JWD(I)
   70 CONTINUE
      RETURN
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR IN FILENAME SPECIFICATION')
      GO TO 600
C
  510 WRITE(CMSSG,515)KSY
  515 FORMAT('SYMBOL FOR FILENAME SUBSTITUTION NOT FOUND = ',A4)
C   
  600 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
