C$PROG POBLOD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE POBLOD
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/PACI/ POB(65536),NPOB
C
      BYTE IBY(4),JBY(4)
C
      EQUIVALENCE (IBY,IWD),(JBY,JWD)
C
C     ************************************************************
C     DOWNLOAD OBJECT-CODE (TABLES) INTO FRONT-END PROCESSOR
C     ************************************************************
C
      DO 10 I=1,NPOB
      IWD=POB(I)
      JBY(4)=IBY(1)
      JBY(3)=IBY(2)
      JBY(2)=IBY(3)
      JBY(1)=IBY(4)
      POB(I)=JWD
   10 CONTINUE
C
      NBYTES=4*NPOB
C
      CALL LOAD_ACQ_PARAMS(POB,NBYTES)
C
      WRITE(6,20)NBYTES
   20 FORMAT(1H ,'CONSIDER YOURSELF LOADED WITH ',I6,'  BYTES')
      RETURN
      END
