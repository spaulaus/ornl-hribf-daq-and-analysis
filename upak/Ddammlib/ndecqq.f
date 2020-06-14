C$PROG NDECQQ    - Returnd number of digits to right of decimal
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      FUNCTION NDECQQ(IWD)
C
      INTEGER*4 IWD(2),BLANK
      character*4 cblank
      equivalence (cblank,blank)
C
      DATA IDOT,cBLANK/Z'2E','    '/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     FUNCTION TO DETERMINE NUMBER OF DIGITS TO RIGHT OF DECIMAL
C   
C     NDEC IS SET TO -1 IF FIELD IS BLANK
C     ------------------------------------------------------------------
C   
      IF(IWD(2).NE.BLANK) GO TO 10
      NDECQQ=-1
      RETURN
   10 LDEC=IFIND(IWD,IDOT,1,8)
      ND=8-LDEC
      IF(ND.EQ.8) ND=0
      NDECQQ=ND
      RETURN
      END
