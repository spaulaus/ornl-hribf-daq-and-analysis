C$PROG LOCBAN    - Locates ban-file directory & banana entries
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE LOCBAN(IDX,LBAN,LDIR,ILO,IHI)
C
      SAVE
C
C     ------------------------------------------------------------------
C     IDX  = INDEX (ORDINAL) IN BAN-FILE DIRECTORY
C     LBAN = LOCATION OF ASSOCIATED BANANA    ENTRY (REC # ON FILE)
C     LDIR = LOCATION OF ASSOCIATED DIRECTORY ENTRY (REC # ON FILE)
C     ILO  = FIRST DIRECTORY INDEX ON LINE LDIR
C     IHI  = LAST  DIRECTORY INDEX ON LINE LDIR
C     ------------------------------------------------------------------
C
      LBAN=5*((IDX+79)/80)+12*(IDX-1)
C
      LDIR=(IDX-1)/16+12*80*((IDX-1)/80)
C
      ILO=16*((IDX-1)/16)+1
C
      IHI=ILO+15
C
      RETURN
      END
