C$PROG DOCMAN    - Displays/logs documentation & notes
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE DOCMAN(MODE,LU)
C
      CHARACTER*4  MODE
C
      WRITE(LU,10)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,20)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,30)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,40)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,50)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,60)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,70)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,80)
      IF(MODE.EQ.'LIST') WRITE(LU,100)
      WRITE(LU,90)
      WRITE(LU,100)
C
      IF(MODE.EQ.'LIST') THEN
                         WRITE(LU,100)
                         WRITE(LU,110)
                         WRITE(LU,120)
                         WRITE(LU,130)
                         WRITE(LU,100)
                         ENDIF
C
      WRITE(LU,140)
      WRITE(LU,150)
      WRITE(LU,160)
      WRITE(LU,170)
      RETURN
C
   10 FORMAT(1H ,'All stopping powers are in units of MeV*CMSQ/MG')
   20 FORMAT(1H ,'NUCL   = Nuclear stopping power - from Ziegler',
     &'                      - Ref 1')
   30 FORMAT(1H ,'WARZIG = S(ele) from Ward"s effective charge &',
     &' Ziegler"s Prot-S(ele)- Ref 1,3')
   40 FORMAT(1H ,'ZIEGL  = S(ele) from Ziegler                  ',
     &'                      - Ref 1')
   50 FORMAT(1H ,'NORTH  = S(ele) from Northcliff & Schilling',
     & ' corrected for targ mass - Ref 2')
   60 FORMAT(1H ,'CNORTH = S(ele) shell-corrected NORTH - see SHELF',
     &'                   - Ref 2,4')
   70 FORMAT(1H ,'SHELF  = Shell correction factor = S(ele,He4)WARD/',
     &'S(ele,He4)NORTH   - Ref 2,4')
   80 FORMAT(1H ,'WARHE4 = S(ele,He4) from Ward"s table for',
     & ' MeV/Amu listed            - Ref 4')
   90 FORMAT(1H ,'entries flagged by * computed from linear velocity',
     & ' dependence (0 - .10 MeV/Amu)')
  100 FORMAT(1H )
  110 FORMAT(1H ,'T-WARZIG = WARZIG+NUCL'/)
  120 FORMAT(1H ,'T-ZIEGL  = ZIEGL +NUCL'/)
  130 FORMAT(1H ,'T-CNORTH = CNORTH+NUCL'/)
C
  140 FORMAT(1H ,'1)  J. F. Ziegler, The Stopping and Ranges of Ions',
     & ' in Matter, Vols 3 & 5,'/1H ,'Pergamon Press, 1980.'/)
C
  150 FORMAT(1H ,'2)  L. C. Northcliff & R. F. Schilling, Nuc Data',
     & ' Tables, Vol 7, 1970.'/1H ,'(Range .0125 to 12.0 MeV/Amu'/)
C
  160 FORMAT(1H ,'3)  D. Ward, et al, Stopping Powers for Heavy ',
     & 'Ions, AECL-5313, Chalk River,'/1H ,'1976'/)
C
  170 FORMAT(1H ,'4)  D. Ward, et al, Compilation of Realistic,',
     & ' Stopping Powers for 4He Ions in'/1H ,'Selected Materials,',
     & ' AECL-4914, Chalk River, 1975')
C
      END
