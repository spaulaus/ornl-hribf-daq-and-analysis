C$PROG MARKON2   - Turns ON 2-D markers which are currently OFF
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE MARKON2(IDW)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20), MINZZ,    MAXZZ                 !/PL04
C
      INTEGER*4    ILOCX,    IHICX,    ILOCY,    IHICY,
     &             KLOCX,    KHICX,    KLOCY,    KHICY,
     &             KDDP,     MINZZ,    MAXZZ
C     ------------------------------------------------------------------
      COMMON/PL13/ MARF(4,20),MARF2(2,20)
      CHARACTER*4  MARF,      MARF2
C     ------------------------------------------------------------------
C
      INTEGER*4    IDW
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(MARF2(1,IDW).EQ.'OFF ') THEN
      CALL MARKIT2(IDW,'OGRE','LL  ',KLOCX(IDW),KLOCY(IDW))
      MARF2(1,IDW)='ON  '
      ENDIF
C
      IF(MARF2(2,IDW).EQ.'OFF ') THEN
      CALL MARKIT2(IDW,'OGRE','UR  ',KHICX(IDW),KHICY(IDW))
      MARF2(2,IDW)='ON  '
      ENDIF
C
      RETURN
      END
