C$PROG GETGCO    - Returns Graphic Context associated with color
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE GETGCO(KOLR,GC)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
#if (PTRSIZE == 64)
      INTEGER*8    GCOR,   GCON                             !Alpha STAR8
#else
      INTEGER*4    GCOR,   GCON                             !Alpha STAR8
#endif
C     ------------------------------------------------------------------
#if (PTRSIZE == 64)
      INTEGER*8    GC                                       !Alpha STAR8
#else
      INTEGER*4    GC                                       !Alpha STAR8
#endif
C
      CHARACTER*4  KOLR
C
      SAVE
C
C     ------------------------------------------------------------------
C     RETURNS GRAPHICS CONTEXT (GC) ASSOCIATED WITH COLOR (KOLR)
C     ------------------------------------------------------------------
C
      GC=GCON(2)
      IF(KOLR.EQ.'COM ') GC=GCOR(1)
      IF(KOLR.EQ.'ERAS') GC=GCON(1)
      IF(KOLR.EQ.'WHIT') GC=GCON(2)
      IF(KOLR.EQ.'RED ') GC=GCON(3)
      IF(KOLR.EQ.'GREE') GC=GCON(4)
      IF(KOLR.EQ.'BLUE') GC=GCON(5)
      IF(KOLR.EQ.'RDGR') GC=GCON(6)
      IF(KOLR.EQ.'RDBL') GC=GCON(7)
      IF(KOLR.EQ.'GRBL') GC=GCON(8)
C
      IF(KOLR.EQ.'OWHI') GC=GCOR(1)
      IF(KOLR.EQ.'ORED') GC=GCOR(2)
      IF(KOLR.EQ.'OGRE') GC=GCOR(3)
      IF(KOLR.EQ.'OBLU') GC=GCOR(4)
      IF(KOLR.EQ.'ORG ') GC=GCOR(5)
      IF(KOLR.EQ.'ORB ') GC=GCOR(6)
      IF(KOLR.EQ.'OGB ') GC=GCOR(7)
C
      RETURN
      END
