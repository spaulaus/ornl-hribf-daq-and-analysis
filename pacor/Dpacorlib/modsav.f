C$PROG MODSAV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/24/2000
C     ************************************************************
C
      SUBROUTINE MODSAV
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C     ------------------------------------------------------------------
C
      CHARACTER*4  USED
C
      DATA         NUMT/0/
C
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
      SAVE
C
C     ************************************************************
C     SAVES ONE MODULE-DATA-SET PER CALL AS DEFINED BELOW
C     *************************************************************
C     KINMOD      = MODULE TYPE ($LAT, $CAM, $FER, $FAS...)
C     MODKOD      = MODULE MODEL CODE (1,2,3..) FOR FERRA & FASTBUS
C     NAMOD       = MODULE NAME (FOR EXAMPLE - ADC1) UP TO 12 BYTES
C     MODATA(1,1) = MODULE-NAME FIRST INDEX
C     MODATA(2,1) = MODULE-NAME INDEX INCREMENT
C     MODATA(1,2) = CRATE#
C     MODATA(1,3) = SLOT#
C     MODATA(1,4) = FIRST SUB-ADDRESS TO READ
C     MODATA(2,4) = LAST  SUB-ADDRESS TO READ
C     MODATA(1,5) = FUNCTION CODE FOR READ
C     MODATA(1,6) = CLASS#
C     MODATA(1,7) = DETECTOR#, FIRST VALUE
C     MODATA(2,7) = DETECTOR#, INCREMENT
C     MODATA(1,8) = ENTRY#
C     MODATA(1,9) = FUNCTION CODE FOR CLEAR
C     MODATA(1,10)= SUB-ADDRRESS FOR CLEAR
C     MODATA(1,11)= DELAY TIME BEFORE READ (no longer used *******)
C     MODATA(1,12)= ID-NUMBER, FIRST
C     MODATA(2,12)= ID-NUMBER, INCREMENT
C     *************************************************************
C
      NDO=MODATA(2,4)-MODATA(1,4)+1   !# OF SUB-ADDRESSES
C
      IA  =MODATA(1,4)                !SUB-ADDRESS, FIRST READ
      IINC=1                          !SUB-ADDRESS, INCREMENT
C
      JA  =MODATA(1,1)                !MODULE-NAME, FIRST INDEX
      JINC=MODATA(2,1)                !MODULE-NAME, INDEX INCREMENT
C
      KA  =MODATA(1,7)                !DETECTOR#,   FIRST VALUE
      KINC=MODATA(2,7)                !DETECTOR#,   INCREMENT
C
      LA  =MODATA(1,12)               !ID-NUMBER,   FIRST VALUE
      LINC=MODATA(2,12)               !ID-NUMBER,   INCREMENT
C
      N=NUMT
      DO 100 I=1,NDO
      N=N+1
C
      IF(N.GT.TMX) THEN
      WRITE(CMSSG,10)N
   10 FORMAT('/PAC2/ ARRAY OVERFLOW AT N =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
      N=TMX
      ENDIF
C
      IF(MODKOD.NE.28)        GO TO 50
      IF(IA.GE.0.AND.IA.LE.2) GO TO 50
C
      WRITE(CMSSG,20)IA,NAMOD
   20 FORMAT('Sub-addr = ',I4,' is illegal for ',3A4)
      CALL ERRLOG(LOGUT,LOGUP)
C
C
   50 NAMO(1,N)=NAMOD(1)
      NAMO(2,N)=NAMOD(2)
      NAMO(3,N)=NAMOD(3)
      NAMO(4,N)=JA
      KIMO(N)=KINMOD
      CRAT(N)=MODATA(1,2)
      SLOT(N)=MODATA(1,3)
      SUBA(N)=IA
      FRED(N)=MODATA(1,5)
      FCLR(N)=MODATA(1,9)
      ACLR(N)=MODATA(1,10)
C
      LTT=LA
      IF(LTT.NE.-1) LTT=LTT+X8000
      IDNM(N)=LTT
      MOTY(1,N)=MODKOD
      USED(N)='NO  '
C
      CALL CKUNIQUE(N)
      CALL CKMOTY(KINMOD,MOTY(1,N))
      CALL CKCNAS(N)
C
      IA=IA+IINC
      JA=JA+JINC
      KA=KA+KINC
      LA=LA+LINC
C
  100 CONTINUE
      NUMT=N
      RETURN
      END
