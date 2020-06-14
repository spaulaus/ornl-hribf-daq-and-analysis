C$PROG ZSTOP_DAT - Data required for Ziegler stopping power calculation
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE ZSTOP_DAT
C
      COMMON/QSQ001/ AA(12,92)
C
      DIMENSION      BB(12,92)
C
      DATA ((BB(I,J),I=1,12),J=1,5)/
     1 1.262   ,1.44    ,242.6   ,1.2E4     ,0.1159    ,0.0005099  ,
     2 5.436E4 ,-5.052  ,2.049   ,-0.3044   ,0.01966   ,-0.0004659 ,
     3 1.229   ,1.397   ,484.5   ,5873.     ,0.05225   ,0.00102    ,
     4 2.451E4 ,-2.158  ,0.8278  ,-0.1172   ,0.007259  ,-0.000166  ,
     5 1.411   ,1.6     ,725.6   ,3013.     ,0.04578   ,0.00153    ,
     6 2.147E4 ,-0.5831 ,0.562   ,-0.1183   ,0.009298  ,-0.0002498 ,
     7 2.248   ,2.59    ,966.    ,153.8     ,0.03475   ,0.002039   ,
     8 1.63E4  ,0.2779  ,0.1745  ,-0.05684  ,0.005155  ,-0.0001488 ,
     9 2.474   ,2.815   ,1206.   ,1060.     ,0.02855   ,0.002549   ,
     A 1.345E4 ,-2.445  ,1.283   ,-0.2205   ,0.0156    ,-0.000393  /
      DATA ((BB(I,J),I=1,12),J=6,10)/
     1 2.631   ,2.989   ,1445.   ,957.2     ,0.02819   ,0.003059   ,
     2 1.322E4 ,-4.38   ,2.044   ,-0.3283   ,0.02221   ,-0.0005417 ,
     3 2.954   ,3.35    ,1683.   ,1900.     ,0.02513   ,0.003569   ,
     4 1.179E4 ,-5.054  ,2.325   ,-0.3713   ,0.02506   ,-0.0006109 ,
     5 2.652   ,3.      ,1920.   ,2000.     ,0.0223    ,0.004079   ,
     6 1.046E4 ,-6.734  ,3.019   ,-0.4748   ,0.03171   ,-0.0007669 ,
     7 2.085   ,2.352   ,2157.   ,2634.     ,0.01816   ,0.004589   ,
     8 8517.   ,-5.571  ,2.449   ,-0.3781   ,0.02483   ,-0.0005919 ,
     9 1.951   ,2.199   ,2393.   ,2699.     ,0.01568   ,0.005099   ,
     A 7353.   ,-4.408  ,1.879   ,-0.2814   ,0.01796   ,-0.0004168 /
      DATA ((BB(I,J),I=1,12),J=11,15)/
     1 2.542   ,2.869   ,2628.   ,1854.     ,0.01472   ,0.005609   ,
     2 6905.   ,-4.959  ,2.073   ,-0.3054   ,0.01921   ,-0.0004403 ,
     3 3.792   ,4.293   ,2862.   ,1009.     ,0.01397   ,0.006118   ,
     4 6551.   ,-5.51   ,2.266   ,-0.3295   ,0.02047   ,-0.0004637 ,
     5 4.154   ,4.739   ,2766.   ,164.5     ,0.02023   ,0.006628   ,
     6 6309.   ,-6.061  ,2.46    ,-0.3535   ,0.02173   ,-0.0004871 ,
     7 4.15    ,4.7     ,3329.   ,550.      ,0.01321   ,0.007138   ,
     8 6194.   ,-6.294  ,2.538   ,-0.3628   ,0.0222    ,-0.0004956 ,
     9 3.232   ,3.647   ,3561.   ,1560.     ,0.01267   ,0.007648   ,
     A 5942.   ,-6.527  ,2.616   ,-0.3721   ,0.02267   ,-0.000504  /
      DATA ((BB(I,J),I=1,12),J=16,20)/
     1 3.447   ,3.891   ,3792.   ,1219.     ,0.01211   ,0.008158   ,
     2 5678.   ,-6.761  ,2.694   ,-0.3814   ,0.02314   ,-0.0005125 ,
     3 5.047   ,5.714   ,4023.   ,878.6     ,0.01178   ,0.008668   ,
     4 5524.   ,-6.994  ,2.773   ,-0.3907   ,0.02361   ,-0.0005209 ,
     5 5.731   ,6.5     ,4253.   ,530.      ,0.01123   ,0.009178   ,
     6 5268.   ,-7.227  ,2.851   ,-0.4      ,0.02407   ,-0.0005294 ,
     7 5.151   ,5.833   ,4482.   ,545.7     ,0.01129   ,0.009687   ,
     8 5295.   ,-7.44   ,2.923   ,-0.4094   ,0.02462   ,-0.0005411 ,
     9 5.521   ,6.252   ,4710.   ,553.3     ,0.01112   ,0.0102     ,
     A 5214.   ,-7.653  ,2.995   ,-0.4187   ,0.02516   ,-0.0005529 /
      DATA ((BB(I,J),I=1,12),J=21,25)/
     1 5.201   ,5.884   ,4938.   ,560.9     ,0.009995  ,0.01071    ,
     2 4688.   ,-8.012  ,3.123   ,-0.435    ,0.02605   ,-0.0005707 ,
     3 4.862   ,5.496   ,5165.   ,568.5     ,0.009474  ,0.01122    ,
     4 4443.   ,-8.371  ,3.251   ,-0.4513   ,0.02694   ,-0.0005886 ,
     5 4.48    ,5.055   ,5391.   ,952.3     ,0.009117  ,0.01173    ,
     6 4276.   ,-8.731  ,3.379   ,-0.4676   ,0.02783   ,-0.0006064 ,
     7 3.983   ,4.489   ,5616.   ,1336.     ,0.008413  ,0.01224    ,
     8 3946.   ,-9.09   ,3.507   ,-0.4838   ,0.02872   ,-0.0006243 ,
     9 3.469   ,3.907   ,5725.   ,1461.     ,0.008829  ,0.01275    ,
     A 3785.   ,-9.449  ,3.635   ,-0.5001   ,0.02961   ,-0.0006421 /
      DATA ((BB(I,J),I=1,12),J=26,30)/
     1 3.519   ,3.963   ,6065.   ,1243.     ,0.007782  ,0.01326    ,
     2 3650.   ,-9.809  ,3.763   ,-0.5164   ,0.0305    ,-0.00066   ,
     3 3.14    ,3.535   ,6288.   ,1372.     ,0.007361  ,0.01377    ,
     4 3453.   ,-10.17  ,3.891   ,-0.5327   ,0.03139   ,-0.0006779 ,
     5 3.553   ,4.004   ,6205.   ,555.1     ,0.008763  ,0.01428    ,
     6 3297.   ,-10.53  ,4.019   ,-0.549    ,0.03229   ,-0.0006957 ,
     7 3.696   ,4.175   ,4673.   ,387.8     ,0.02188   ,0.01479    ,
     8 3174.   ,-11.18  ,4.252   ,-0.5791   ,0.03399   ,-0.0007314 ,
     9 4.21    ,4.75    ,6953.   ,295.2     ,0.006809  ,0.0153     ,
     A 3194.   ,-11.57  ,4.394   ,-0.598    ,0.03506   ,-0.0007537 /
      DATA ((BB(I,J),I=1,12),J=31,35)/
     1 5.041   ,5.697   ,7173.   ,202.6     ,0.006725  ,0.01581    ,
     2 3154.   ,-11.95  ,4.537   ,-0.6169   ,0.03613   ,-0.0007759 ,
     3 5.554   ,6.3     ,6496.   ,110.      ,0.009689  ,0.01632    ,
     4 3097.   ,-12.34  ,4.68    ,-0.6358   ,0.03721   ,-0.0007981 ,
     5 5.323   ,6.012   ,7611.   ,292.5     ,0.006447  ,0.01683    ,
     6 3024.   ,-12.72  ,4.823   ,-0.6547   ,0.03828   ,-0.0008203 ,
     7 5.874   ,6.656   ,7395.   ,117.5     ,0.007684  ,0.01734    ,
     8 3006.   ,-13.11  ,4.965   ,-0.6735   ,0.03935   ,-0.0008425 ,
     9 5.611   ,6.335   ,8046.   ,365.2     ,0.006244  ,0.01785    ,
     A 2928.   ,-13.4   ,5.083   ,-0.6906   ,0.04042   ,-0.0008675 /
      DATA ((BB(I,J),I=1,12),J=36,40)/
     1 6.411   ,7.25    ,8262.   ,220.      ,0.006087  ,0.01836    ,
     2 2855.   ,-13.69  ,5.2     ,-0.7076   ,0.0415    ,-0.0008925 ,
     3 5.694   ,6.429   ,8478.   ,292.9     ,0.006087  ,0.01886    ,
     4 2855.   ,-13.92  ,5.266   ,-0.714    ,0.04173   ,-0.0008943 ,
     5 6.339   ,7.159   ,8693.   ,330.3     ,0.006003  ,0.01937    ,
     6 2815.   ,-14.14  ,5.331   ,-0.7205   ,0.04196   ,-0.0008962 ,
     7 6.407   ,7.234   ,8907.   ,367.8     ,0.005889  ,0.01988    ,
     8 2762.   ,-14.36  ,5.397   ,-0.7269   ,0.04219   ,-0.000898  ,
     9 6.734   ,7.603   ,9120.   ,405.2     ,0.005765  ,0.02039    ,
     A 2704.   ,-14.59  ,5.463   ,-0.7333   ,0.04242   ,-0.0008998 /
      DATA ((BB(I,J),I=1,12),J=41,45)/
     1 6.902   ,7.791   ,9333.   ,442.7     ,0.005587  ,0.0209     ,
     2 2621.   ,-16.22  ,6.094   ,-0.8225   ,0.04791   ,-0.001024  ,
     3 6.425   ,7.248   ,9545.   ,480.2     ,0.005367  ,0.02141    ,
     4 2517.   ,-17.85  ,6.725   ,-0.9116   ,0.05339   ,-0.001148  ,
     5 6.799   ,7.671   ,9756.   ,517.6     ,0.005315  ,0.02192    ,
     6 2493.   ,-17.96  ,6.752   ,-0.9135   ,0.05341   ,-0.001147  ,
     7 6.108   ,6.887   ,9966.   ,555.1     ,0.005151  ,0.02243    ,
     8 2416.   ,-18.07  ,6.779   ,-0.9154   ,0.05342   ,-0.001145  ,
     9 5.924   ,6.677   ,1.018E4 ,592.5     ,0.004919  ,0.02294    ,
     A 2307.   ,-18.18  ,6.806   ,-0.9173   ,0.05343   ,-0.001143  /
      DATA ((BB(I,J),I=1,12),J=46,50)/
     1 5.238   ,5.9     ,1.038E4 ,630.      ,0.004758  ,0.02345    ,
     2 2231.   ,-18.28  ,6.833   ,-0.9192   ,0.05345   ,-0.001142  ,
     3 5.623   ,6.354   ,7160.   ,337.6     ,0.01394   ,0.02396    ,
     4 2193.   ,-18.39  ,6.86    ,-0.9211   ,0.05346   ,-0.00114   ,
     5 5.814   ,6.554   ,1.08E4  ,355.5     ,0.004626  ,0.02447    ,
     6 2170.   ,-18.62  ,6.915   ,-0.9243   ,0.0534    ,-0.001134  ,
     7 6.23    ,7.024   ,1.101E4 ,370.9     ,0.00454   ,0.02498    ,
     8 2129.   ,-18.85  ,6.969   ,-0.9275   ,0.05335   ,-0.001127  ,
     9 6.41    ,7.227   ,1.121E4 ,386.4     ,0.004474  ,0.02549    ,
     A 2099.   ,-19.07  ,7.024   ,-0.9308   ,0.05329   ,-0.001121  /
      DATA ((BB(I,J),I=1,12),J=51,55)/
     1 7.5     ,8.48    ,8608.   ,348.      ,0.009074  ,0.026      ,
     2 2069.   ,-19.57  ,7.225   ,-0.9603   ,0.05518   ,-0.001165  ,
     3 6.979   ,7.871   ,1.162E4 ,392.4     ,0.004402  ,0.02651    ,
     4 2065.   ,-20.07  ,7.426   ,-0.9899   ,0.05707   ,-0.001209  ,
     5 7.725   ,8.716   ,1.183E4 ,394.8     ,0.004376  ,0.02702    ,
     6 2052.   ,-20.56  ,7.627   ,-1.019    ,0.05896   ,-0.001254  ,
     7 8.231   ,9.289   ,1.203E4 ,397.3     ,0.004384  ,0.02753    ,
     8 2056.   ,-21.06  ,7.828   ,-1.049    ,0.06085   ,-0.001298  ,
     9 7.287   ,8.218   ,1.223E4 ,399.7     ,0.004447  ,0.02804    ,
     A 2086.   ,-20.4   ,7.54    ,-1.004    ,0.05782   ,-0.001224  /
      DATA ((BB(I,J),I=1,12),J=56,60)/
     1 7.899   ,8.911   ,1.243E4 ,402.1     ,0.004511  ,0.02855    ,
     2 2116.   ,-19.74  ,7.252   ,-0.9588   ,0.05479   ,-0.001151  ,
     3 8.041   ,9.071   ,1.263E4 ,404.5     ,0.00454   ,0.02906    ,
     4 2129.   ,-19.08  ,6.964   ,-0.9136   ,0.05176   ,-0.001077  ,
     5 7.489   ,8.444   ,1.283E4 ,406.9     ,0.00442   ,0.02957    ,
     6 2073.   ,-18.43  ,6.677   ,-0.8684   ,0.04872   ,-0.001003  ,
     7 7.291   ,8.219   ,1.303E4 ,409.3     ,0.004298  ,0.03008    ,
     8 2016.   ,-17.77  ,6.389   ,-0.8233   ,0.04569   ,-0.0009292 ,
     9 7.098   ,8.      ,1.323E4 ,411.8     ,0.004182  ,0.03059    ,
     A 1962.   ,-17.11  ,6.101   ,-0.7781   ,0.04266   ,-0.0008553 /
      DATA ((BB(I,J),I=1,12),J=61,65)/
     1 6.91    ,7.786   ,1.343E4 ,414.2     ,0.004058  ,0.0311     ,
     2 1903.   ,-16.45  ,5.813   ,-0.733    ,0.03963   ,-0.0007815 ,
     3 6.728   ,7.58    ,1.362E4 ,416.6     ,0.003976  ,0.03161    ,
     4 1865.   ,-15.79  ,5.526   ,-0.6878   ,0.0366    ,-0.0007077 ,
     5 6.551   ,7.38    ,1.382E4 ,419.      ,0.003877  ,0.03212    ,
     6 1819.   ,-15.13  ,5.238   ,-0.6426   ,0.03357   ,-0.0006339 ,
     7 6.739   ,7.592   ,1.402E4 ,421.4     ,0.003863  ,0.03263    ,
     8 1812.   ,-14.47  ,4.95    ,-0.5975   ,0.03053   ,-0.0005601 ,
     9 6.212   ,6.996   ,1.421E4 ,423.9     ,0.003725  ,0.03314    ,
     A 1747.   ,-14.56  ,4.984   ,-0.6022   ,0.03082   ,-0.0005668 /
      DATA ((BB(I,J),I=1,12),J=66,70)/
     1 5.517   ,6.21    ,1.44E4  ,426.3     ,0.003632  ,0.03365    ,
     2 1703.   ,-14.65  ,5.018   ,-0.6069   ,0.03111   ,-0.0005734 ,
     3 5.219   ,5.874   ,1.46E4  ,428.7     ,0.003498  ,0.03416    ,
     4 1640.   ,-14.74  ,5.051   ,-0.6117   ,0.03141   ,-0.0005801 ,
     5 5.071   ,5.706   ,1.479E4 ,433.      ,0.003405  ,0.03467    ,
     6 1597.   ,-14.83  ,5.085   ,-0.6164   ,0.0317    ,-0.0005867 ,
     7 4.926   ,5.542   ,1.498E4 ,433.5     ,0.003342  ,0.03518    ,
     8 1567.   ,-14.91  ,5.119   ,-0.6211   ,0.03199   ,-0.0005933 ,
     9 4.787   ,5.386   ,1.517E4 ,435.9     ,0.003292  ,0.03569    ,
     A 1544.   ,-15.    ,5.153   ,-0.6258   ,0.03228   ,-0.0006    /
      DATA ((BB(I,J),I=1,12),J=71,75)/
     1 4.893   ,5.505   ,1.536E4 ,438.4     ,0.003243  ,0.0362     ,
     2 1521.   ,-15.09  ,5.186   ,-0.6305   ,0.03257   ,-0.0006066 ,
     3 5.028   ,5.657   ,1.555E4 ,440.8     ,0.003195  ,0.03671    ,
     4 1499.   ,-15.18  ,5.22    ,-0.6353   ,0.03286   ,-0.0006133 ,
     5 4.738   ,5.329   ,1.574E4 ,443.2     ,0.003186  ,0.03722    ,
     6 1494.   ,-15.27  ,5.254   ,-0.64     ,0.03315   ,-0.0006199 ,
     7 4.574   ,5.144   ,1.593E4 ,442.4     ,0.003144  ,0.03773    ,
     8 1475.   ,-15.67  ,5.392   ,-0.6577   ,0.03418   ,-0.0006426 ,
     9 5.2     ,5.851   ,1.612E4 ,441.6     ,0.003122  ,0.03824    ,
     A 1464.   ,-16.07  ,5.529   ,-0.6755   ,0.03521   ,-0.0006654 /
      DATA ((BB(I,J),I=1,12),J=76,80)/
     1 5.07    ,5.704   ,1.63E4  ,440.9     ,0.003082  ,0.03875    ,
     2 1446.   ,-16.47  ,5.667   ,-0.6932   ,0.03624   ,-0.0006881 ,
     3 4.945   ,5.563   ,1.649E4 ,440.1     ,0.002965  ,0.03926    ,
     4 1390.   ,-16.88  ,5.804   ,-0.711    ,0.03727   ,-0.0007109 ,
     5 4.476   ,5.034   ,1.667E4 ,439.3     ,0.002871  ,0.03977    ,
     6 1347.   ,-17.28  ,5.942   ,-0.7287   ,0.0383    ,-0.0007336 ,
     7 4.856   ,5.46    ,1.832E4 ,438.5     ,0.002542  ,0.04028    ,
     8 1354.   ,-17.02  ,5.846   ,-0.7149   ,0.0374    ,-0.0007114 ,
     9 4.308   ,4.843   ,1.704E4 ,487.8     ,0.002882  ,0.04079    ,
     A 1352.   ,-17.84  ,6.183   ,-0.7659   ,0.04076   ,-0.0007925 /
      DATA ((BB(I,J),I=1,12),J=81,85)/
     1 4.723   ,5.311   ,1.722E4 ,537.      ,0.002913  ,0.0413     ,
     2 1366.   ,-18.66  ,6.52    ,-0.8169   ,0.04411   ,-0.0008737 ,
     3 5.319   ,5.982   ,1.74E4  ,586.3     ,0.002871  ,0.04181    ,
     4 1347.   ,-19.48  ,6.857   ,-0.8678   ,0.04747   ,-0.0009548 ,
     5 5.956   ,6.7     ,1.78E4  ,677.      ,0.00266   ,0.04232    ,
     6 1336.   ,-19.55  ,6.871   ,-0.8686   ,0.04748   ,-0.0009544 ,
     7 6.158   ,6.928   ,1.777E4 ,586.3     ,0.002812  ,0.04283    ,
     8 1319.   ,-19.62  ,6.884   ,-0.8694   ,0.04748   ,-0.000954  ,
     9 6.204   ,6.979   ,1.795E4 ,586.3     ,0.002776  ,0.04334    ,
     A 1302.   ,-19.69  ,6.898   ,-0.8702   ,0.04749   ,-0.0009536 /
      DATA ((BB(I,J),I=1,12),J=86,90)/
     1 6.181   ,6.954   ,1.812E4 ,586.3     ,0.002748  ,0.04385    ,
     2 1289.   ,-19.76  ,6.912   ,-0.871    ,0.04749   ,-0.0009532 ,
     3 6.949   ,7.82    ,1.83E4  ,586.3     ,0.002737  ,0.04436    ,
     4 1284.   ,-19.83  ,6.926   ,-0.8718   ,0.0475    ,-0.0009528 ,
     5 7.506   ,8.448   ,1.848E4 ,586.3     ,0.002727  ,0.04487    ,
     6 1279.   ,-19.9   ,6.94    ,-0.8726   ,0.04751   ,-0.0009524 ,
     7 7.649   ,8.609   ,1.866E4 ,586.3     ,0.002697  ,0.04538    ,
     8 1265.   ,-19.97  ,6.953   ,-0.8733   ,0.04751   ,-0.000952  ,
     9 7.71    ,8.679   ,1.883E4 ,586.3     ,0.002641  ,0.04589    ,
     A 1239.   ,-20.04  ,6.967   ,-0.8741   ,0.04752   ,-0.0009516 /
      DATA ((BB(I,J),I=1,12),J=91,92)/
     1 7.407   ,8.336   ,1.901E4 ,586.3     ,0.002603  ,0.0464     ,
     2 1221.   ,-20.11  ,6.981   ,-0.8749   ,0.04752   ,-0.0009512 ,
     3 7.29    ,8.204   ,1.918E4 ,586.3     ,0.002573  ,0.04691    ,
     4 1207.   ,-20.18  ,6.995   ,-0.8757   ,0.04753   ,-0.0009508 /
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 J=1,92
      DO 10 I=1,12
      AA(I,J)=BB(I,J)
   10 CONTINUE
   20 CONTINUE
      RETURN
      END
