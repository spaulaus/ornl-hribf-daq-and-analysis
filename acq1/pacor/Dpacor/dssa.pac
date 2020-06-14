$lat c01 n01 f00 a00 evtype:1  id1  fc09 ac00
;
; PSAC Energy and time (focal plane) (Recoil)
;
$cam c01 n04  f02 a01     fc09 ac00   rec-e-left:1  id169
$cam c01 n04  f02 a02     fc09 ac00   rec-e-right:1 id170
;
; Scaler
$cam c01 n14  f00 a00     scaler:1  id189 
;
$pat PAT = evtype(1)1,16
      IFS(PAT(1)) GOTO IMP
      IFS(PAT(2)) GOTO DEC
      IFS(PAT(3)) GOTO DEC
      GOTO END                                                                 
IMP   CONTINUE
      READ scaler(1)
      READ rec-e-left(1)
      CNAF 2,21,0,17,0001h
      GOTO END
DEC   CONTINUE
      READ rec-e-right(1)
      CNAF 2,21,0,17,0003h
      GOTO END
END   CONTINUE





