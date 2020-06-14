C$PROG CUSSMAN   - Cursor & 1-key manager routine for cus1, cus2, cuss
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE CUSSMAN(IDW,IX,JY,KEY,KRETN)
C
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLKK/ KURTY,MBKEY(3,5)
      CHARACTER*4  KURTY,MBKEY
C     ------------------------------------------------------------------
      COMMON/XLPP/ IPROMP
      CHARACTER*4  IPROMP
C     ------------------------------------------------------------------
      CHARACTER*4  KEY,KRETN
C
      SAVE
C
C     ------------------------------------------------------------------
C     CURSOR & 1-KEY MANAGER ROUTINE
C     ------------------------------------------------------------------
C
      CALL PIXTOC(IDW,IX,JY,X,Y)
C
      KRETN='    '
C
      IF(IPROMP.EQ.'YES ') GO TO 50
C
      IF(KEY.EQ.'7   '.OR.
     &   KEY.EQ.'8   '.OR.
     &   KEY.EQ.'9   '.OR.
     &   KEY.EQ.'0   '.OR.
     &   KEY.EQ.';   '.OR.
     &   KEY.EQ.'=   '.OR.
     &   KEY.EQ.'-   ') THEN
                        CALL LABLMAN('LABL',IDW,KEY,IX,JY)
                        RETURN
                        ENDIF
C
   50 IF(WINFLC(3,IDW).EQ.'1D  ') GO TO 110
      IF(WINFLC(3,IDW).EQ.'2D  ') GO TO 120
      IF(WINFLC(3,IDW).EQ.'SAM ') GO TO 130
C
      IF(KEY.EQ.'Q    ')          GO TO 150
C
      RETURN
C
  110 IF(    KEY.EQ.'MB1 ') THEN
                            KEY=MBKEY(1,1)
      ELSEIF(KEY.EQ.'MB2 ') THEN
                            KEY=MBKEY(2,1)
      ELSEIF(KEY.EQ.'MB3 ') THEN
                            KEY=MBKEY(3,1)
                            ENDIF
      IF(    KEY.EQ.'Q   ') GO TO 150
C
      CALL CUS1(IDW,IX,JY,X,Y,KEY,KRETN)
      RETURN
C
  120 IF(    KEY.EQ.'MB1 ') THEN
                            KEY=MBKEY(1,2)
      ELSEIF(KEY.EQ.'MB2 ') THEN
                            KEY=MBKEY(2,2)
      ELSEIF(KEY.EQ.'MB3 ') THEN
                            KEY=MBKEY(3,2)
                            ENDIF
      IF(    KEY.EQ.'Q   ') GO TO 150
C
      CALL CUS2(IDW,IX,JY,X,Y,KEY,KRETN)
      RETURN
C
  130 IF(    KEY.EQ.'MB1 ') THEN
                            KEY=MBKEY(1,3)
      ELSEIF(KEY.EQ.'MB2 ') THEN
                            KEY=MBKEY(2,3)
      ELSEIF(KEY.EQ.'MB3 ') THEN
                            KEY=MBKEY(3,3)
                            ENDIF
      IF(    KEY.EQ.'Q   ') GO TO 150
C
      CALL CUSS(IDW,IX,JY,X,Y,KEY,KRETN)
      RETURN
C
  150 KRETN='RET '
      RETURN
      END
