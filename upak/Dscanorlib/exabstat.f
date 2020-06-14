C$PROG EXABSTAT
C
C     ****************************************************************
C     BY W. T. MILNER AT HHIRF _ LAST MODIFIED 08/04/94
C     ****************************************************************
C     Routine which displays/logs, returns EXABYTE status information
C     via Charles Thomas's C routines
C     ****************************************************************
C     MODE = 1 says display/log EXABYTE status information
C     MODE = 2 says return      EXABYTE status insormation
C     LCT  = Tape Channel#
C     TMBU = Total Megabytes Used (returned)
C     TMBR = Total Megabytes Left (returned)
C     RATE = #errors/megabyte
C     NER  = Number of errors
C     ****************************************************************
C
      SUBROUTINE EXABSTAT(MODE,LCT,TMBU,TMBR,RATE,NER,IERR)
C
      REAL*8 TMB,RMB,ERATE,UMB
C
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      WRITE(6,777) 
  777 FORMAT(1H ,'STX not implemented')
C
      RETURN
      END
