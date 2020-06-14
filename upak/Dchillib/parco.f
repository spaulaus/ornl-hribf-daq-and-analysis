C$PROG PARCO
      SUBROUTINE PARCO(IP,KWD)
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
      CHARACTER*4 ISYN
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      CHARACTER*4 KWD(3)
C
      INTEGER*4   X28
      DATA        X28/z'28'/
C
      SAVE
C
C     **************************************************************
C     CONVERT PARM # "IP" TO ASCII FORM - PARNAME(INDEX) IF POSSIBLE
C     OR CONVERT "IP" TO LEFT-JUSTIFIED ASCII FORM, OTHERWISE
C     **************************************************************
C
      DO 10 I=1,3
      KWD(I)='    '
   10 CONTINUE
      IF(IP.LE.0) RETURN
C
      J=IPSP(IP)
      IF(J.LE.0) GO TO 40
      KWD(1)=ISYN(1,J)
      KWD(2)=ISYN(2,J)
      CALL ISBYTE(X28,KWD(2),3)
      WRITE(KWD(3),20)IPSI(IP)
   20 FORMAT(I3,')')
      CALL SQUEZL(KWD,1,12)
      RETURN
C
   40 WRITE(KWD(1),50)IP
   50 FORMAT(I3)
      CALL SQUEZL(KWD,1,3)
      RETURN
      END
