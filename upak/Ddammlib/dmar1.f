C$PROG DMAR1     - Displays 1-D markers which are ON
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE DMAR1(IDW)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
C
      INTEGER*4    ILOX,    IHIX,    ILOF,    IHIF,  MOPL
      REAL*4       FLOX,    FHIX,    FLOF,    FHIF
      REAL*4       GWID,    ECAL
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
      IF(MARF(1,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,FLOX(IDW))
C
      IF(MARF(2,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,FHIX(IDW))
C
      IF(MARF(3,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,FLOF(IDW))
C
      IF(MARF(4,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,FHIF(IDW))
C
      RETURN
      END
