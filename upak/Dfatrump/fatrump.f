C$PROG FATRUMP
C   
C     ******************************************************************
C     ******************************************************************
C                   PROGRAM MASTER INPUT
C   
C   
C     ------------------------------------------------------------------
C                            CARD SET 1
C     IPROG                 FORMAT(A4)
C   
C     PUNCH ESP, RHO, NUFREQ OR MAGNET (LEFT-JUSTIFIED)
C     ------------------------------------------------------------------
C                            CARD SET 2
C     JRAK
C     JRAK=REACTION  (EXAMPLE  12C(16O,4HE)24MG  )
C     FORMAT(6A4)
C     -------------------------------------------------------------
C   
C                             CARD SET 3
C     DTHETA,ZOUT,EIN,FLAG,IENGY,ISOLN2,IA
C     FORMAT(4F10.0,A2,I1)                                         A  15
C   
C     DTHETA = SLIT OPENING (APERTURE)    (CM)
C     ZOUT = CHARGE OF OUTGOING PARTICLE
C     EIN = BOMBARDING ENERGY IN MEV
C     FLAG = 1.0 GIVES EXTRA OUTPUT ( USUALLY 0.0)
C     IENGY  = PUNCH Q OR EL DEPENDING ON WHERHER Q-VALUES OR ENERGY
C              LEVELS ARE READ IN ON CARD SET 4.
C     ISOLN2 = 2 IF THERE ARE TWO SOLN. TO RELKIN AND THE SECOND SOLN
C              IS DESIRED   (USUALLY BLANK)
C              IA = 0 FOR ESP,RHO,NUFREQ OPTIONS.
C              IA = 0  OR 1 FOR MAGNET. SEE NOTES AT THE END
C   
C     ------------------------------------------------------------------
C                            CARD SET 4
C     TARGT,   TBACK,TBACKT,ZTB,NORDER,ABSORB
C     ( F10.5,2A4,2X,2F10.5,I10,A4)
C   
C     TARGT  = TARGET THICKNESS (MG/SQ-CM)
C     ZT =  TARGET Z
C     TBACK = TARGET BACKING (ALPHANUMERIC,ONLY USED FOR OUTPUT)
C     TBACKT = TARGET BACKING THICKNESS (MG/SQ-CM)
C     ZTB = TARGET BACKING Z
C     NORDER = ORDER OF TARGET BACKING (1 IF FIRST),(2 IF LAST)
C     ABSORB = SOLI IF ABSORBER IS A SOLID OR GAS IF GAS
C     THIS CARD MUST BE LEFT BLANK IF NOT A (16O,4HE) REACTION
C   
C     ------------------------------------------------------------------
C                            CARD SET 5
C     Q-VALUES OR ENERGY LEVELS DEPENDING ON IENGY  (MEV)
C     FORMAT(10F8.0)
C   
C     ------------------------------------------------------------------
C                            CARD SET 6
C     ANGI, ANGFI, DEL          FORMAT(3F10.0)
C   
C     ANGI = THETA MIN  (DEG)
C     ANGFI = THETA MAX (DEG)
C     DEL = DELTA THETA (DEG)
C     IF(IPROG = MAGNET OR NUFREQ) ANGFI = DEL = 0.0
C   
C     ------------------------------------------------------------------
C                            CARD SET 7
C     NQ, INQ, RHOD               FORMAT(2I10,F10.0)
C   
C     NQ = NUMBER OF Q-VALUES (OR ENERGY LEVELS) READ IN ON CARD SET 4
C     INQ = NUMBER OF THE Q-VALUE WHICH IS TO BE USED TO SET THE FOCAL
C           PLANE BESIDES THE FIRST ONE.
C     RHOD = RHO OF MAGNET  (CM)
C     NQ MUST BE .LE. 10, INQ MUST BE .LE. NQ, INQ MUST .NE. 1
C   
C     ------------------------------------------------------------------
C                            CARD SET 8
C     DELRHO, RHOMIN       FORMAT(2F10.0)
C   
C     DELRHO = DELTA RHO (CM)
C     RHOMIN = RHO MIN   (CM)
C     RHO MAX = 90.9 CM IS SET INTERNALLY
C   
C     ------------------------------------------------------------------
C                            CARD SET 9
C     F, BA, RD1, RD2          FORMAT(4F10.0)
C   
C     F = MAGNET FREQUENCY (MEGAHERTZ)
C     BA = MAGNET FIELD (NOT NECESSARY TO READ IN)  (KILOGAUSS)
C     RD1 = DETECTOR END POINT MIN (CM)
C     RD2 = DETECTOR END POINT MAX (CM)
C   
C     ------------------------------------------------------------------
C                            CARD SET 10
C     NAP,TL, TR,EA(1), EA(2), EA(3)           FORMAT(I10,5F10.0)
C   
C     THIS CARD IS FOR CALIBRATION
C     NAP = NUMBER OF ALPHA PARTICLE ENERGIES USED IN CALIBRATION
C     TL = LEFT DIAL READING
C     TR = RIGHT DIAL READING
C     EA = ALPHA ENERGIES  (MEV)
C     NAP MUST BE LESS THAN OR EQUAL TO 3
C   
C     ------------------------------------------------------------------
C                            CARD SET 11
C     TF, TCH(1), TCH(2), TCH(3)        FORMAT(4F10.0)
C   
C     TF = MAGNET FREQUENCY  (MEGAHERTZ)
C     TCH = CHANNEL NUMBER FOR ALPHA PARTICLE
C     READ IN AS MANY CARD SETS 10 AS YOU HAVE FREQUENCIES.
C     THEN PUT A BLANK CARD AT THE END.
C   
C     ------------------------------------------------------------------
C                            CARD SET 12
C     FD, TL, TR                 FORMAT(3F10.0)
C   
C     FD = MAGNET FREQUENCY  (MEGAHERTZ)
C     TL = LEFT DIAL READING
C     TR = RIGHT DIAL READING
C   
C   
C                            DATA SET ORDER
C   
C     ------------------------------------------------------------------
C                                 ESP
C     1,2,3,4,5,6,7
C   
C     ------------------------------------------------------------------
C                                 RHO
C      1,2,3,4,5,6,8
C   
C     ------------------------------------------------------------------
C                                 NUFREQ
C     1,2,3,4,5,6,9,2,3,4,5,6
C   
C     ------------------------------------------------------------------
C                                 MAGNET
C      1,10,11,2,3,6,12,4,5
C   
C   
C                                   NOTES
C   
C     A BLANK CARD SHOULD BE PLACED AT THE END OF EACH DATA SET.
C     A BLANK CARD PROPERLY TERMINATES THE PROGRAM  .
C     THEREFORE 2 BLANK CARDS SHOULD BE AT THE END OF THE DATA.
C   
C     DATA SETS MAY BE REPEATED FOR THE SAME OPTION BY RETURNING TO
C     CARD SET 2 EXCEPT FOR IPROG = MAGNET, WHERE CARD SETS 5,11,9, AND
C     10 ARE OMITTED FOR ALL REPEATED CALCULATIONS,WITH THE SAME
C     FREQUENCY AND LEFT AND RIGHT SETTINGS. FOR THIS CASE IA OF JRACK
C     CARD MUST BE 0
C     IF YOU DO NOT OMITTE CARD SETS 5 AND 11 IA OF JRACK CARD MUST BE 1
C   
C     ******************************************************************
C     ******************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
C
      INTEGER*4    IPROG
C
      CHARACTER*4  CPROG
C
      EQUIVALENCE (CPROG,IPROG)
C
      CHARACTER*80 ARGS,NAMDAF
C
      COMMON/AAA/ Z(4), NZ(30), CONST(50), ZZ(25)
C   
      EQUIVALENCE (SOLN2 ,ZZ(1)),
     &            (ILRD  ,ZZ(2)),
     &            (IFLAG ,ZZ(3)),
     &            (IPROG ,ZZ(4)),
     &            (TWO   ,ZZ(13))
C   
      DATA TRUE,FALSE/1.0,0.0/
C
      ARGS=' '
      CALL GETARG(1,ARGS)
      NAMDAF=ARGS
C
      OPEN(UNIT      =1,
     &     FILE      ='/usr/hhirf/stope.dat',
     &     STATUS    ='OLD',
     &     ACCESS    ='SEQUENTIAL')
C
      OPEN(UNIT      =5,
     &     FILE      =NAMDAF,
     &     STATUS    ='OLD',
     &     ACCESS    ='SEQUENTIAL')
C   
      OPEN(UNIT      =7,
     &     FILE      ='fatrump.log',
     &     STATUS    ='UNKNOWN',
     &     ACCESS    ='APPEND')
C
      CALL REDSTOP(1)
C   
      CALL CONSTS
1     CONTINUE
      SOLN2=FALSE
      TWO = FALSE
      ILRD=0
      IFLAG = 0
      READ(5,2) IPROG
      IF( CPROG.EQ.'    ') STOP 766
      IF( CPROG.EQ.'END ') STOP
      IF( CPROG.EQ.'RHO ') CALL RHO
      IF (CPROG.EQ.'NUFR') CALL NUFREQ
      IF( CPROG.EQ.'ESP ') CALL ESP
      IF (CPROG.EQ.'MAGN') CALL MAGNET
      GO TO 1
C   
C   
2     FORMAT (A4)
      END
