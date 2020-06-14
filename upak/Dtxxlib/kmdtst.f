C$PROG KMDTST
C
      CHARACTER*4 FUNCTION KMDTST(IWD,KMD)
C
      INTEGER*4 IWD(*)
      CHARACTER*4 JFLN(44)
      INTEGER     IJFLN(44)
      EQUIVALENCE (JFLN, IJFLN)
      INTEGER*4    KMD
C
      DATA JFLN/
     1'UON$','UOF$','CHP$','TOF$','TPF$','QIT$','SP1$','SP2$','SP3$',
     2'SP4$','SP5$','SP6$','SP7$','SP8$','SP9$','SIN$','DBL$','RSH$',
     3'RND$','RPD$','CY1$','CY2$','CEN$','BLP$','INP$','IDP$','ODP$',
     4'FIP$','SEC$','CEB$','CYB$','JON$','JOF$','BON$','BOF$','COV$',
     5'COF$','NAS$','ASP$','NPL$','PGL$','NPS$','NOT$','TON$'/
C
      INTEGER*4  X24
C
      DATA       X24/'24'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      KMDTST='NO  '
      KMD   =IWD(1)
      CALL CASEUP4(KMD)
      CALL ILBYTE(IT,IWD,3)
      IF(IT.NE.X24) RETURN
C
      DO 30 I=1,44
      IF(KMD.EQ.IJFLN(I)) GO TO 40
   30 CONTINUE
      RETURN
C
   40 KMDTST='YES '
      RETURN
      END
