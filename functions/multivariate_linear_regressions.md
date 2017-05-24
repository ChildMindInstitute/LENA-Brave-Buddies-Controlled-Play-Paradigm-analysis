get & prep data

    library(car)
    library(hash)

    ## hash-2.2.6 provided by Decision Patterns

    library(MASS)
    library(MESS)

    ## Loading required package: geepack

    ## Loading required package: geeM

    ## Loading required package: Matrix

    library(plyr)
    library(ROCR)

    ## Loading required package: gplots

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

    data <- read.csv('../data/CPP_data_all.csv', header=TRUE)
    smq <- read.csv('../data/SMQ.csv', header=TRUE)
    smq$smq_id <- (18 - smq$smq_id) / 6
    data <- ddply(data, .(URSI, SM_dx), summarize,
                  Turn_Count=sum(Turn_Count),
                  Child_Voc_Count=sum(Child_Voc_Count),
                  Child_Voc_Duration=sum(Child_Voc_Duration),
                  Child_NonVoc_Duration=sum(Child_NonVoc_Duration),
                  Average_SignalLevel=mean(Average_SignalLevel),
                  Peak_SignalLevel=max(Peak_SignalLevel)
                  )
    data <- merge(data, smq, by="URSI", all=TRUE)
    remove(smq)
    # invert SMQ for severity
    data$smq_as <- 3 - data$smq_as
    data$smq_hf <- 3 - data$smq_hf
    data$smq_ss <- 3 - data$smq_ss
    data$smq_id <- 3 - data$smq_id
    data$SM_dx <- factor(data$SM_dx)
    smq_data <- data[complete.cases(data),]

multivariate forward linear regression for SMQ

    smq <- c("smq_as", "smq_hf", "smq_ss", "smq_id")
    fsmq_predicted <- vector()
    fsmq_actual <- vector()
    fsmq_models <- vector()
    fsmq_predictors <- hash()
    kii <- vector()
    for(k in 1:4){
      predicted <- vector()
      actual <- vector()
      models <- vector()
      predictors <- hash()
      for(i in 1:nrow(smq_data)){
        train <- smq_data[-i,]
        test <- smq_data[i,]
        model <- step(lm(get(smq[k])~1, data=train, na.action=na.omit),
                 direction='forward', scope=~Child_Voc_Count+Turn_Count+
                 Child_Voc_Duration+Child_NonVoc_Duration+Average_SignalLevel+
                 Peak_SignalLevel)
        for(j in 1:length(names(model$coefficients))){
          if(has.key(names(model$coefficients)[[j]], predictors)){
            .set(predictors, names(model$coefficients)[[j]],
                 as.numeric(predictors[[names(model$coefficients)[[j]]]])+ 1)
          }else{
            .set(predictors, names(model$coefficients)[[j]], 1)
          }
        }
        models <- c(models, model)
        predicted <- c(predicted, predict(model, test))
        actual <- c(actual, test[[smq[k]]])
        remove(j)
        remove(train)
        remove(test)
      }
      fsmq_models <- c(fsmq_models, models)
      fsmq_predicted <- c(fsmq_predicted, predicted)
      fsmq_actual <- c(fsmq_actual, actual)
      .set(fsmq_predictors, smq[k], predictors)
    }

    ## Start:  AIC=2.56
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.4218 14.150 -4.2916
    ## + Child_Voc_Count        1    7.1167 14.455 -3.8435
    ## + Child_Voc_Duration     1    5.9246 15.647 -2.1793
    ## + Child_NonVoc_Duration  1    3.2187 18.353  1.1703
    ## <none>                               21.571  2.5638
    ## + Peak_SignalLevel       1    0.4555 21.116  4.1156
    ## + Average_SignalLevel    1    0.0474 21.524  4.5176
    ## 
    ## Step:  AIC=-4.29
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.15188 10.998 -7.5835
    ## <none>                               14.150 -4.2916
    ## + Peak_SignalLevel       1   0.59965 13.550 -3.2009
    ## + Child_Voc_Count        1   0.09452 14.055 -2.4323
    ## + Child_NonVoc_Duration  1   0.02741 14.122 -2.3323
    ## + Child_Voc_Duration     1   0.02396 14.126 -2.3272
    ## 
    ## Step:  AIC=-7.58
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.998 -7.5835
    ## + Child_Voc_Count        1   0.40255 10.595 -6.3666
    ## + Peak_SignalLevel       1   0.18742 10.810 -5.9445
    ## + Child_NonVoc_Duration  1   0.17790 10.820 -5.9260
    ## + Child_Voc_Duration     1   0.03360 10.964 -5.6478
    ## Start:  AIC=-0.69
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1    5.2451 13.231 -5.7011
    ## + Turn_Count             1    5.2230 13.253 -5.6660
    ## + Child_Voc_Duration     1    4.2867 14.190 -4.2324
    ## + Child_NonVoc_Duration  1    1.8302 16.646 -0.8794
    ## <none>                               18.476 -0.6888
    ## + Average_SignalLevel    1    0.1925 18.284  1.0913
    ## + Peak_SignalLevel       1    0.1841 18.292  1.1008
    ## 
    ## Step:  AIC=-5.7
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Average_SignalLevel    1    4.7925  8.4385 -13.1460
    ## + Child_Voc_Duration     1    2.1671 11.0640  -7.4574
    ## <none>                               13.2311  -5.7011
    ## + Peak_SignalLevel       1    0.2764 12.9547  -4.1444
    ## + Turn_Count             1    0.1901 13.0409  -4.0050
    ## + Child_NonVoc_Duration  1    0.0136 13.2175  -3.7227
    ## 
    ## Step:  AIC=-13.15
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Duration     1   1.87442 6.5641 -16.421
    ## <none>                               8.4385 -13.146
    ## + Turn_Count             1   0.43643 8.0021 -12.261
    ## + Child_NonVoc_Duration  1   0.00936 8.4292 -11.169
    ## + Peak_SignalLevel       1   0.00224 8.4363 -11.152
    ## 
    ## Step:  AIC=-16.42
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.5641 -16.421
    ## + Child_NonVoc_Duration  1  0.158587 6.4055 -14.935
    ## + Peak_SignalLevel       1  0.157907 6.4062 -14.932
    ## + Turn_Count             1  0.026927 6.5372 -14.507
    ## Start:  AIC=2.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.3908 14.159 -4.2769
    ## + Child_Voc_Count        1    7.1980 14.352 -3.9928
    ## + Child_Voc_Duration     1    6.0332 15.517 -2.3542
    ## + Child_NonVoc_Duration  1    3.1475 18.403  1.2275
    ## <none>                               21.550  2.5432
    ## + Peak_SignalLevel       1    1.1056 20.445  3.4372
    ## + Average_SignalLevel    1    0.1803 21.370  4.3668
    ## 
    ## Step:  AIC=-4.28
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.08439 11.075 -7.4363
    ## <none>                               14.159 -4.2769
    ## + Peak_SignalLevel       1   0.54811 13.611 -3.1059
    ## + Child_Voc_Count        1   0.17096 13.989 -2.5320
    ## + Child_NonVoc_Duration  1   0.00133 14.158 -2.2789
    ## + Child_Voc_Duration     1   0.00026 14.159 -2.2773
    ## 
    ## Step:  AIC=-7.44
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.075 -7.4363
    ## + Child_Voc_Count        1   0.48744 10.588 -6.3815
    ## + Peak_SignalLevel       1   0.27602 10.799 -5.9663
    ## + Child_NonVoc_Duration  1   0.07913 10.996 -5.5869
    ## + Child_Voc_Duration     1   0.07509 11.000 -5.5792
    ## Start:  AIC=2.89
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.6795 14.225 -4.1796
    ## + Child_Voc_Count        1    7.4758 14.429 -3.8810
    ## + Child_Voc_Duration     1    6.2259 15.679 -2.1364
    ## + Child_NonVoc_Duration  1    3.4595 18.445  1.2760
    ## <none>                               21.905  2.8858
    ## + Peak_SignalLevel       1    0.7060 21.199  4.1979
    ## + Average_SignalLevel    1    0.1270 21.778  4.7637
    ## 
    ## Step:  AIC=-4.18
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.2658 10.959 -7.6567
    ## <none>                               14.225 -4.1796
    ## + Peak_SignalLevel       1    0.7042 13.521 -3.2458
    ## + Child_Voc_Count        1    0.1816 14.044 -2.4495
    ## + Child_Voc_Duration     1    0.0004 14.225 -2.1802
    ## + Child_NonVoc_Duration  1    0.0000 14.225 -2.1796
    ## 
    ## Step:  AIC=-7.66
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.959 -7.6567
    ## + Child_Voc_Count        1   0.54231 10.417 -6.7225
    ## + Peak_SignalLevel       1   0.26247 10.697 -6.1658
    ## + Child_Voc_Duration     1   0.09257 10.867 -5.8349
    ## + Child_NonVoc_Duration  1   0.05821 10.901 -5.7686
    ## Start:  AIC=1.12
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.6087 13.534 -5.2254
    ## + Child_Voc_Count        1    6.4282 13.715 -4.9472
    ## + Child_Voc_Duration     1    5.3123 14.831 -3.3044
    ## + Child_NonVoc_Duration  1    2.8791 17.264 -0.1142
    ## <none>                               20.143  1.1249
    ## + Peak_SignalLevel       1    0.2886 19.854  2.8218
    ## + Average_SignalLevel    1    0.0004 20.142  3.1244
    ## 
    ## Step:  AIC=-5.23
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Average_SignalLevel    1    4.0336  9.5006 -10.6566
    ## <none>                               13.5342  -5.2254
    ## + Peak_SignalLevel       1    0.8691 12.6651  -4.6192
    ## + Child_Voc_Count        1    0.1543 13.3799  -3.4661
    ## + Child_Voc_Duration     1    0.0018 13.5324  -3.2281
    ## + Child_NonVoc_Duration  1    0.0000 13.5342  -3.2254
    ## 
    ## Step:  AIC=-10.66
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               9.5006 -10.6566
    ## + Child_Voc_Count        1   0.50911 8.9915  -9.8132
    ## + Peak_SignalLevel       1   0.35159 9.1490  -9.4484
    ## + Child_Voc_Duration     1   0.09254 9.4080  -8.8621
    ## + Child_NonVoc_Duration  1   0.07221 9.4284  -8.8168
    ## Start:  AIC=2.31
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    9.2020 12.110 -7.5600
    ## + Child_Voc_Count        1    8.8352 12.477 -6.9334
    ## + Child_Voc_Duration     1    7.5051 13.807 -4.8062
    ## + Child_NonVoc_Duration  1    4.6679 16.644 -0.8816
    ## <none>                               21.312  2.3099
    ## + Peak_SignalLevel       1    1.3231 19.989  2.9639
    ## + Average_SignalLevel    1    0.6387 20.674  3.6710
    ## 
    ## Step:  AIC=-7.56
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   1.75910 10.351 -8.8561
    ## <none>                               12.110 -7.5600
    ## + Peak_SignalLevel       1   0.21326 11.897 -5.9331
    ## + Child_Voc_Count        1   0.17003 11.940 -5.8569
    ## + Child_NonVoc_Duration  1   0.05642 12.054 -5.6581
    ## + Child_Voc_Duration     1   0.00000 12.110 -5.5600
    ## 
    ## Step:  AIC=-8.86
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS     AIC
    ## <none>                               10.3511 -8.8561
    ## + Child_Voc_Count        1   0.41495  9.9361 -7.7152
    ## + Child_NonVoc_Duration  1   0.12632 10.2248 -7.1139
    ## + Peak_SignalLevel       1   0.09378 10.2573 -7.0472
    ## + Child_Voc_Duration     1   0.05561 10.2955 -6.9692
    ## Start:  AIC=2.98
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.7579 14.242 -4.1547
    ## + Child_Voc_Count        1    7.5354 14.465 -3.8292
    ## + Child_Voc_Duration     1    6.3008 15.699 -2.1092
    ## + Child_NonVoc_Duration  1    3.5982 18.402  1.2265
    ## <none>                               22.000  2.9769
    ## + Peak_SignalLevel       1    0.6631 21.337  4.3343
    ## + Average_SignalLevel    1    0.1200 21.880  4.8621
    ## 
    ## Step:  AIC=-4.15
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.1939 11.048 -7.4874
    ## <none>                               14.242 -4.1547
    ## + Peak_SignalLevel       1    0.6371 13.605 -3.1158
    ## + Child_Voc_Count        1    0.1693 14.073 -2.4058
    ## + Child_Voc_Duration     1    0.0014 14.241 -2.1568
    ## + Child_NonVoc_Duration  1    0.0000 14.242 -2.1548
    ## 
    ## Step:  AIC=-7.49
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.048 -7.4874
    ## + Child_Voc_Count        1   0.48791 10.560 -6.4359
    ## + Peak_SignalLevel       1   0.25771 10.790 -5.9830
    ## + Child_NonVoc_Duration  1   0.09410 10.954 -5.6670
    ## + Child_Voc_Duration     1   0.08533 10.963 -5.6502
    ## Start:  AIC=1.58
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.7772 12.805 -6.3888
    ## + Child_Voc_Count        1    7.2679 13.314 -5.5697
    ## + Child_Voc_Duration     1    5.9912 14.591 -3.6468
    ## + Child_NonVoc_Duration  1    5.5009 15.081 -2.9527
    ## <none>                               20.582  1.5778
    ## + Average_SignalLevel    1    0.4035 20.178  3.1620
    ## + Peak_SignalLevel       1    0.3625 20.220  3.2046
    ## 
    ## Step:  AIC=-6.39
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.29378 10.511 -8.5341
    ## + Peak_SignalLevel       1   1.25030 11.555 -6.5464
    ## <none>                               12.805 -6.3888
    ## + Child_NonVoc_Duration  1   0.29502 12.510 -4.8783
    ## + Child_Voc_Count        1   0.06613 12.739 -4.4975
    ## + Child_Voc_Duration     1   0.04247 12.762 -4.4585
    ## 
    ## Step:  AIC=-8.53
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS     AIC
    ## <none>                               10.5110 -8.5341
    ## + Peak_SignalLevel       1   0.52410  9.9869 -7.6082
    ## + Child_NonVoc_Duration  1   0.35379 10.1572 -7.2531
    ## + Child_Voc_Count        1   0.32556 10.1854 -7.1948
    ## + Child_Voc_Duration     1   0.01612 10.4949 -6.5663
    ## Start:  AIC=2.74
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    8.4699 13.287 -5.6129
    ## + Child_Voc_Count        1    8.2168 13.540 -5.2167
    ## + Child_Voc_Duration     1    7.4444 14.312 -4.0516
    ## + Child_NonVoc_Duration  1    3.2838 18.473  1.3073
    ## <none>                               21.757  2.7433
    ## + Peak_SignalLevel       1    0.9101 20.846  3.8460
    ## + Average_SignalLevel    1    0.1743 21.582  4.5744
    ## 
    ## Step:  AIC=-5.61
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.2431 10.044 -9.4893
    ## <none>                               13.287 -5.6129
    ## + Peak_SignalLevel       1    0.4511 12.836 -4.3383
    ## + Child_NonVoc_Duration  1    0.2041 13.083 -3.9380
    ## + Child_Voc_Count        1    0.1854 13.101 -3.9080
    ## + Child_Voc_Duration     1    0.0455 13.241 -3.6849
    ## 
    ## Step:  AIC=-9.49
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS     AIC
    ## <none>                               10.0436 -9.4893
    ## + Child_Voc_Count        1   0.52042  9.5232 -8.6066
    ## + Child_Voc_Duration     1   0.29661  9.7470 -8.1188
    ## + Peak_SignalLevel       1   0.09794  9.9457 -7.6951
    ## + Child_NonVoc_Duration  1   0.03852 10.0051 -7.5700
    ## Start:  AIC=-0.69
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1    5.3813 13.095 -5.9183
    ## + Turn_Count             1    5.2519 13.224 -5.7118
    ## + Child_Voc_Duration     1    4.4241 14.052 -4.4368
    ## + Child_NonVoc_Duration  1    2.0599 16.416 -1.1712
    ## <none>                               18.476 -0.6888
    ## + Average_SignalLevel    1    0.9797 17.496  0.1670
    ## + Peak_SignalLevel       1    0.2539 18.222  1.0206
    ## 
    ## Step:  AIC=-5.92
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Duration     1   2.12819 10.967 -7.6429
    ## + Average_SignalLevel    1   1.71001 11.385 -6.8570
    ## <none>                               13.095 -5.9183
    ## + Peak_SignalLevel       1   0.23784 12.857 -4.3033
    ## + Turn_Count             1   0.13968 12.955 -4.1436
    ## + Child_NonVoc_Duration  1   0.00376 13.091 -3.9244
    ## 
    ## Step:  AIC=-7.64
    ## get(smq[k]) ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Average_SignalLevel    1   2.12878  8.8379 -10.1749
    ## <none>                               10.9667  -7.6429
    ## + Child_NonVoc_Duration  1   0.35341 10.6133  -6.3308
    ## + Turn_Count             1   0.02664 10.9400  -5.6940
    ## + Peak_SignalLevel       1   0.01782 10.9489  -5.6770
    ## 
    ## Step:  AIC=-10.17
    ## get(smq[k]) ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               8.8379 -10.1749
    ## + Peak_SignalLevel       1   0.36250 8.4754  -9.0545
    ## + Turn_Count             1   0.17152 8.6664  -8.5865
    ## + Child_NonVoc_Duration  1   0.04865 8.7892  -8.2909
    ## Start:  AIC=1.58
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.8530 12.729 -6.5134
    ## + Child_Voc_Count        1    7.0565 13.525 -5.2388
    ## + Child_Voc_Duration     1    5.8430 14.739 -3.4345
    ## + Child_NonVoc_Duration  1    3.3670 17.215 -0.1736
    ## <none>                               20.582  1.5778
    ## + Peak_SignalLevel       1    1.8284 18.754  1.6241
    ## + Average_SignalLevel    1    0.1925 20.390  3.3805
    ## 
    ## Step:  AIC=-6.51
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq     RSS     AIC
    ## + Average_SignalLevel    1   2.91608  9.8129 -9.9772
    ## <none>                               12.7290 -6.5134
    ## + Child_Voc_Duration     1   0.11223 12.6168 -4.6994
    ## + Peak_SignalLevel       1   0.09722 12.6318 -4.6744
    ## + Child_Voc_Count        1   0.00763 12.7214 -4.5260
    ## + Child_NonVoc_Duration  1   0.00407 12.7250 -4.5201
    ## 
    ## Step:  AIC=-9.98
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               9.8129 -9.9772
    ## + Child_Voc_Count        1  0.159949 9.6530 -8.3223
    ## + Child_NonVoc_Duration  1  0.032512 9.7804 -8.0469
    ## + Peak_SignalLevel       1  0.000153 9.8128 -7.9775
    ## + Child_Voc_Duration     1  0.000117 9.8128 -7.9775
    ## Start:  AIC=1.99
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1    7.7749 13.220 -5.7189
    ## + Turn_Count             1    7.6613 13.333 -5.5392
    ## + Child_Voc_Duration     1    6.5549 14.440 -3.8653
    ## + Child_NonVoc_Duration  1    3.3088 17.686  0.3932
    ## <none>                               20.995  1.9947
    ## + Peak_SignalLevel       1    0.2722 20.723  3.7207
    ## + Average_SignalLevel    1    0.2659 20.729  3.7270
    ## 
    ## Step:  AIC=-5.72
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.58979 10.630 -8.2976
    ## + Child_Voc_Duration     1   2.44005 10.780 -8.0038
    ## <none>                               13.220 -5.7189
    ## + Peak_SignalLevel       1   0.83495 12.385 -5.0889
    ## + Turn_Count             1   0.20847 13.011 -4.0527
    ## + Child_NonVoc_Duration  1   0.00405 13.216 -3.7253
    ## 
    ## Step:  AIC=-8.3
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Child_Voc_Duration     1   2.42489  8.2052 -11.7349
    ## <none>                               10.6301  -8.2976
    ## + Turn_Count             1   0.55806 10.0720  -7.4300
    ## + Peak_SignalLevel       1   0.12006 10.5100  -6.5361
    ## + Child_NonVoc_Duration  1   0.03962 10.5904  -6.3760
    ## 
    ## Step:  AIC=-11.73
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               8.2052 -11.7349
    ## + Child_NonVoc_Duration  1  0.142289 8.0629 -10.1023
    ## + Turn_Count             1  0.026493 8.1787  -9.8029
    ## + Peak_SignalLevel       1  0.011257 8.1939  -9.7638
    ## Start:  AIC=1.62
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    9.6856 10.934 -9.7066
    ## + Child_Voc_Count        1    9.4665 11.152 -9.2900
    ## + Child_Voc_Duration     1    8.0921 12.527 -6.8494
    ## + Child_NonVoc_Duration  1    4.1721 16.447 -1.1320
    ## <none>                               20.619  1.6155
    ## + Peak_SignalLevel       1    0.9101 19.709  2.6675
    ## + Average_SignalLevel    1    0.6187 20.000  2.9758
    ## 
    ## Step:  AIC=-9.71
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Average_SignalLevel    1   1.91661  9.0169 -11.7539
    ## <none>                               10.9335  -9.7066
    ## + Peak_SignalLevel       1   0.69237 10.2411  -9.0804
    ## + Child_Voc_Count        1   0.24929 10.6842  -8.1909
    ## + Child_Voc_Duration     1   0.00714 10.9263  -7.7203
    ## + Child_NonVoc_Duration  1   0.00117 10.9323  -7.7088
    ## 
    ## Step:  AIC=-11.75
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               9.0169 -11.7539
    ## + Child_Voc_Count        1   0.50889 8.5080 -10.9739
    ## + Peak_SignalLevel       1   0.30910 8.7078 -10.4864
    ## + Child_Voc_Duration     1   0.09520 8.9217  -9.9768
    ## + Child_NonVoc_Duration  1   0.02984 8.9870  -9.8235
    ## Start:  AIC=2.56
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.4677 14.104 -4.3597
    ## + Child_Voc_Count        1    7.1456 14.426 -3.8855
    ## + Child_Voc_Duration     1    5.8981 15.673 -2.1438
    ## + Child_NonVoc_Duration  1    3.0812 18.490  1.3271
    ## <none>                               21.571  2.5638
    ## + Peak_SignalLevel       1    0.8565 20.715  3.7130
    ## + Average_SignalLevel    1    0.0511 21.520  4.5140
    ## 
    ## Step:  AIC=-4.36
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.6126 10.491 -8.5737
    ## <none>                               14.104 -4.3597
    ## + Peak_SignalLevel       1    0.5379 13.566 -3.1763
    ## + Child_Voc_Duration     1    0.0895 14.014 -2.4934
    ## + Child_Voc_Count        1    0.0455 14.058 -2.4276
    ## + Child_NonVoc_Duration  1    0.0195 14.084 -2.3887
    ## 
    ## Step:  AIC=-8.57
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.491 -8.5737
    ## + Child_Voc_Count        1  0.080310 10.411 -6.7351
    ## + Peak_SignalLevel       1  0.065871 10.425 -6.7060
    ## + Child_Voc_Duration     1  0.029186 10.462 -6.6322
    ## + Child_NonVoc_Duration  1  0.000107 10.491 -6.5739
    ## Start:  AIC=2.76
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.9105 13.862 -4.7228
    ## + Child_Voc_Count        1    7.4834 14.289 -4.0856
    ## + Child_Voc_Duration     1    6.2544 15.518 -2.3528
    ## + Child_NonVoc_Duration  1    3.3850 18.387  1.2101
    ## <none>                               21.773  2.7586
    ## + Peak_SignalLevel       1    0.7614 21.011  4.0111
    ## + Average_SignalLevel    1    0.1304 21.642  4.6325
    ## 
    ## Step:  AIC=-4.72
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.3215 10.540 -8.4752
    ## <none>                               13.862 -4.7228
    ## + Peak_SignalLevel       1    0.5787 13.283 -3.6183
    ## + Child_Voc_Count        1    0.0889 13.773 -2.8579
    ## + Child_Voc_Duration     1    0.0190 13.843 -2.7516
    ## + Child_NonVoc_Duration  1    0.0089 13.853 -2.7363
    ## 
    ## Step:  AIC=-8.48
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.540 -8.4752
    ## + Child_Voc_Count        1   0.32915 10.211 -7.1415
    ## + Peak_SignalLevel       1   0.15920 10.381 -6.7948
    ## + Child_Voc_Duration     1   0.02521 10.515 -6.5255
    ## + Child_NonVoc_Duration  1   0.02026 10.520 -6.5156
    ## Start:  AIC=2.56
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1    7.6982 13.873 -4.7058
    ## + Turn_Count             1    7.5625 14.009 -4.5014
    ## + Child_Voc_Duration     1    6.6036 14.968 -3.1110
    ## + Child_NonVoc_Duration  1    3.4186 18.153  0.9404
    ## <none>                               21.571  2.5638
    ## + Peak_SignalLevel       1    0.9997 20.572  3.5673
    ## + Average_SignalLevel    1    0.1688 21.403  4.3988
    ## 
    ## Step:  AIC=-4.71
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.96834 10.905 -7.7616
    ## + Child_Voc_Duration     1   2.09005 11.783 -6.1349
    ## <none>                               13.873 -4.7058
    ## + Turn_Count             1   0.18244 13.691 -2.9838
    ## + Peak_SignalLevel       1   0.05817 13.815 -2.7941
    ## + Child_NonVoc_Duration  1   0.00004 13.873 -2.7059
    ## 
    ## Step:  AIC=-7.76
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Child_Voc_Duration     1   2.11135  8.7935 -10.2807
    ## <none>                               10.9048  -7.7616
    ## + Turn_Count             1   0.52290 10.3819  -6.7935
    ## + Child_NonVoc_Duration  1   0.06181 10.8430  -5.8810
    ## + Peak_SignalLevel       1   0.05882 10.8460  -5.8752
    ## 
    ## Step:  AIC=-10.28
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               8.7935 -10.2807
    ## + Peak_SignalLevel       1  0.287238 8.5063  -8.9781
    ## + Child_NonVoc_Duration  1  0.068977 8.7245  -8.4461
    ## + Turn_Count             1  0.064692 8.7288  -8.4358
    ## Start:  AIC=2.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.3298 14.220 -4.1866
    ## + Child_Voc_Count        1    7.0801 14.470 -3.8211
    ## + Child_Voc_Duration     1    5.8525 15.698 -2.1110
    ## + Child_NonVoc_Duration  1    3.3126 18.238  1.0383
    ## <none>                               21.550  2.5432
    ## + Peak_SignalLevel       1    0.4921 21.058  4.0581
    ## + Average_SignalLevel    1    0.0291 21.521  4.5148
    ## 
    ## Step:  AIC=-4.19
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.14546 11.075 -7.4364
    ## <none>                               14.220 -4.1866
    ## + Peak_SignalLevel       1   0.62961 13.591 -3.1376
    ## + Child_Voc_Count        1   0.16223 14.058 -2.4276
    ## + Child_Voc_Duration     1   0.00164 14.219 -2.1890
    ## + Child_NonVoc_Duration  1   0.00020 14.220 -2.1869
    ## 
    ## Step:  AIC=-7.44
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.075 -7.4364
    ## + Child_Voc_Count        1   0.49344 10.582 -6.3936
    ## + Peak_SignalLevel       1   0.21095 10.864 -5.8403
    ## + Child_Voc_Duration     1   0.07925 10.996 -5.5872
    ## + Child_NonVoc_Duration  1   0.07755 10.998 -5.5840
    ## Start:  AIC=1.62
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.3923 14.227 -4.1774
    ## + Child_Voc_Count        1    6.2345 14.385 -3.9457
    ## + Child_Voc_Duration     1    4.9819 15.637 -2.1923
    ## + Child_NonVoc_Duration  1    2.5911 18.028  0.7954
    ## <none>                               20.619  1.6155
    ## + Peak_SignalLevel       1    0.3274 20.292  3.2794
    ## + Average_SignalLevel    1    0.0244 20.595  3.5907
    ## 
    ## Step:  AIC=-4.18
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.14834 11.078 -7.4300
    ## <none>                               14.227 -4.1774
    ## + Peak_SignalLevel       1   0.65356 13.573 -3.1650
    ## + Child_Voc_Count        1   0.14433 14.082 -2.3915
    ## + Child_Voc_Duration     1   0.02205 14.205 -2.2100
    ## + Child_NonVoc_Duration  1   0.00003 14.227 -2.1774
    ## 
    ## Step:  AIC=-7.43
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.078 -7.4300
    ## + Child_Voc_Count        1   0.65392 10.425 -6.7077
    ## + Peak_SignalLevel       1   0.21211 10.866 -5.8360
    ## + Child_Voc_Duration     1   0.09406 10.984 -5.6091
    ## + Child_NonVoc_Duration  1   0.06724 11.011 -5.5579
    ## Start:  AIC=1.62
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.3212 13.298 -5.5953
    ## + Child_Voc_Count        1    7.3044 13.315 -5.5687
    ## + Child_Voc_Duration     1    6.3060 14.313 -4.0503
    ## + Child_NonVoc_Duration  1    4.5241 16.095 -1.5863
    ## <none>                               20.619  1.6155
    ## + Peak_SignalLevel       1    1.0073 19.612  2.5637
    ## + Average_SignalLevel    1    0.1045 20.515  3.5089
    ## 
    ## Step:  AIC=-5.6
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.14086 10.157 -9.2535
    ## <none>                               13.298 -5.5953
    ## + Peak_SignalLevel       1   0.36737 12.931 -4.1836
    ## + Child_Voc_Count        1   0.24488 13.053 -3.9856
    ## + Child_NonVoc_Duration  1   0.11203 13.186 -3.7729
    ## + Child_Voc_Duration     1   0.01501 13.283 -3.6190
    ## 
    ## Step:  AIC=-9.25
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS     AIC
    ## <none>                               10.1570 -9.2535
    ## + Child_Voc_Count        1   0.60926  9.5478 -8.5525
    ## + Child_NonVoc_Duration  1   0.34950  9.8075 -7.9888
    ## + Child_Voc_Duration     1   0.18451  9.9725 -7.6385
    ## + Peak_SignalLevel       1   0.06532 10.0917 -7.3890
    ## Start:  AIC=2.31
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.0948 14.217 -4.1913
    ## + Child_Voc_Count        1    6.8405 14.472 -3.8190
    ## + Child_Voc_Duration     1    5.6276 15.684 -2.1288
    ## + Child_NonVoc_Duration  1    3.5837 17.729  0.4436
    ## <none>                               21.312  2.3099
    ## + Peak_SignalLevel       1    0.3824 20.930  3.9297
    ## + Average_SignalLevel    1    0.0199 21.292  4.2902
    ## 
    ## Step:  AIC=-4.19
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.14304 11.074 -7.4378
    ## <none>                               14.217 -4.1913
    ## + Peak_SignalLevel       1   0.61783 13.600 -3.1243
    ## + Child_Voc_Count        1   0.14684 14.070 -2.4093
    ## + Child_NonVoc_Duration  1   0.00402 14.213 -2.1972
    ## + Child_Voc_Duration     1   0.00271 14.215 -2.1953
    ## 
    ## Step:  AIC=-7.44
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.074 -7.4378
    ## + Child_Voc_Count        1   0.48134 10.593 -6.3710
    ## + Peak_SignalLevel       1   0.20476 10.870 -5.8298
    ## + Child_Voc_Duration     1   0.07148 11.003 -5.5738
    ## + Child_NonVoc_Duration  1   0.06317 11.011 -5.5580
    ## Start:  AIC=1.62
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.4978 14.121 -4.3337
    ## + Child_Voc_Count        1    6.3294 14.290 -4.0847
    ## + Child_Voc_Duration     1    5.3352 15.284 -2.6722
    ## + Child_NonVoc_Duration  1    2.8657 17.753  0.4731
    ## <none>                               20.619  1.6155
    ## + Peak_SignalLevel       1    0.8871 19.732  2.6920
    ## + Average_SignalLevel    1    0.0961 20.523  3.5175
    ## 
    ## Step:  AIC=-4.33
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   3.04215 11.079 -7.4288
    ## <none>                               14.121 -4.3337
    ## + Peak_SignalLevel       1   0.52312 13.598 -3.1264
    ## + Child_Voc_Count        1   0.16718 13.954 -2.5838
    ## + Child_NonVoc_Duration  1   0.00060 14.121 -2.3346
    ## + Child_Voc_Duration     1   0.00025 14.121 -2.3341
    ## 
    ## Step:  AIC=-7.43
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.079 -7.4288
    ## + Child_Voc_Count        1   0.48396 10.595 -6.3667
    ## + Peak_SignalLevel       1   0.21889 10.860 -5.8478
    ## + Child_Voc_Duration     1   0.07815 11.001 -5.5774
    ## + Child_NonVoc_Duration  1   0.06841 11.011 -5.5588
    ## Start:  AIC=2.89
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.8618 14.043 -4.4504
    ## + Child_Voc_Count        1    7.6673 14.238 -4.1615
    ## + Child_Voc_Duration     1    6.4083 15.496 -2.3821
    ## + Child_NonVoc_Duration  1    3.8241 18.081  0.8568
    ## <none>                               21.905  2.8858
    ## + Peak_SignalLevel       1    0.6286 21.276  4.2744
    ## + Average_SignalLevel    1    0.1427 21.762  4.7485
    ## 
    ## Step:  AIC=-4.45
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.1840 10.859 -7.8502
    ## <none>                               14.043 -4.4504
    ## + Peak_SignalLevel       1    0.7949 13.248 -3.6742
    ## + Child_Voc_Count        1    0.1823 13.861 -2.7248
    ## + Child_NonVoc_Duration  1    0.0086 14.034 -2.4634
    ## + Child_Voc_Duration     1    0.0007 14.042 -2.4515
    ## 
    ## Step:  AIC=-7.85
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.859 -7.8502
    ## + Child_Voc_Count        1   0.51098 10.348 -6.8624
    ## + Peak_SignalLevel       1   0.29789 10.561 -6.4344
    ## + Child_NonVoc_Duration  1   0.12029 10.739 -6.0842
    ## + Child_Voc_Duration     1   0.07845 10.780 -6.0025
    ## Start:  AIC=-18.85
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    1.7063 6.0762 -22.043
    ## + Child_Voc_Count        1    1.5940 6.1885 -21.659
    ## + Child_NonVoc_Duration  1    1.5067 6.2758 -21.364
    ## + Child_Voc_Duration     1    1.4950 6.2875 -21.325
    ## + Average_SignalLevel    1    1.1844 6.5981 -20.313
    ## <none>                               7.7825 -18.845
    ## + Peak_SignalLevel       1    0.0219 7.7606 -16.905
    ## 
    ## Step:  AIC=-22.04
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0762 -22.043
    ## + Peak_SignalLevel       1   0.32875 5.7474 -21.211
    ## + Child_NonVoc_Duration  1   0.29488 5.7813 -21.088
    ## + Average_SignalLevel    1   0.14410 5.9321 -20.547
    ## + Child_Voc_Count        1   0.00968 6.0665 -20.076
    ## + Child_Voc_Duration     1   0.00417 6.0720 -20.057
    ## Start:  AIC=-27.83
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   0.75258 4.3199 -29.207
    ## + Child_Voc_Count        1   0.74222 4.3303 -29.157
    ## + Child_Voc_Duration     1   0.71223 4.3603 -29.012
    ## + Child_NonVoc_Duration  1   0.64297 4.4295 -28.681
    ## <none>                               5.0725 -27.834
    ## + Average_SignalLevel    1   0.26680 4.8057 -26.969
    ## + Peak_SignalLevel       1   0.01056 5.0619 -25.878
    ## 
    ## Step:  AIC=-29.21
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Peak_SignalLevel       1   0.39653 3.9234 -29.229
    ## <none>                               4.3199 -29.207
    ## + Child_NonVoc_Duration  1   0.10743 4.2125 -27.736
    ## + Child_Voc_Count        1   0.02376 4.2962 -27.323
    ## + Child_Voc_Duration     1   0.02139 4.2985 -27.311
    ## + Average_SignalLevel    1   0.00061 4.3193 -27.210
    ## 
    ## Step:  AIC=-29.23
    ## get(smq[k]) ~ Turn_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               3.9234 -29.229
    ## + Child_NonVoc_Duration  1 0.0291314 3.8942 -27.386
    ## + Average_SignalLevel    1 0.0223278 3.9010 -27.349
    ## + Child_Voc_Duration     1 0.0091214 3.9143 -27.278
    ## + Child_Voc_Count        1 0.0002475 3.9231 -27.230
    ## Start:  AIC=-20.25
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.66545 5.6134 -23.707
    ## + Average_SignalLevel    1   1.54957 5.7293 -23.278
    ## + Child_Voc_Count        1   1.52991 5.7489 -23.206
    ## + Child_Voc_Duration     1   1.45722 5.8216 -22.942
    ## + Child_NonVoc_Duration  1   1.31691 5.9619 -22.442
    ## <none>                               7.2788 -20.251
    ## + Peak_SignalLevel       1   0.27531 7.0035 -19.060
    ## 
    ## Step:  AIC=-23.71
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.6134 -23.707
    ## + Average_SignalLevel    1  0.287589 5.3258 -22.811
    ## + Child_NonVoc_Duration  1  0.152448 5.4609 -22.285
    ## + Peak_SignalLevel       1  0.102005 5.5114 -22.092
    ## + Child_Voc_Count        1  0.009094 5.6043 -21.741
    ## + Child_Voc_Duration     1  0.008034 5.6053 -21.737
    ## Start:  AIC=-20.25
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.58517 5.6937 -23.409
    ## + Child_NonVoc_Duration  1   1.55375 5.7251 -23.293
    ## + Child_Voc_Count        1   1.40091 5.8779 -22.740
    ## + Child_Voc_Duration     1   1.31176 5.9671 -22.424
    ## + Average_SignalLevel    1   1.29940 5.9794 -22.380
    ## <none>                               7.2788 -20.251
    ## + Peak_SignalLevel       1   0.09068 7.1882 -18.514
    ## 
    ## Step:  AIC=-23.41
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.6937 -23.409
    ## + Child_NonVoc_Duration  1  0.293879 5.3998 -22.521
    ## + Peak_SignalLevel       1  0.228631 5.4650 -22.269
    ## + Average_SignalLevel    1  0.195934 5.4977 -22.144
    ## + Child_Voc_Count        1  0.001150 5.6925 -21.413
    ## + Child_Voc_Duration     1  0.000335 5.6933 -21.410
    ## Start:  AIC=-22.4
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.40752 5.1634 -25.462
    ## + Child_NonVoc_Duration  1   1.30014 5.2708 -25.029
    ## + Child_Voc_Count        1   1.27356 5.2973 -24.924
    ## + Child_Voc_Duration     1   1.19048 5.3804 -24.597
    ## + Average_SignalLevel    1   0.75235 5.8185 -22.953
    ## <none>                               6.5709 -22.399
    ## + Peak_SignalLevel       1   0.00003 6.5709 -20.399
    ## 
    ## Step:  AIC=-25.46
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Peak_SignalLevel       1   0.50945 4.6539 -25.643
    ## <none>                               5.1634 -25.462
    ## + Child_NonVoc_Duration  1   0.22439 4.9390 -24.395
    ## + Average_SignalLevel    1   0.03253 5.1308 -23.594
    ## + Child_Voc_Count        1   0.00464 5.1587 -23.480
    ## + Child_Voc_Duration     1   0.00199 5.1614 -23.470
    ## 
    ## Step:  AIC=-25.64
    ## get(smq[k]) ~ Turn_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               4.6539 -25.643
    ## + Average_SignalLevel    1  0.095377 4.5586 -24.078
    ## + Child_NonVoc_Duration  1  0.090380 4.5636 -24.055
    ## + Child_Voc_Count        1  0.009359 4.6446 -23.685
    ## + Child_Voc_Duration     1  0.000300 4.6536 -23.644
    ## Start:  AIC=-18.19
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   2.03596 5.9942 -22.328
    ## + Child_Voc_Count        1   1.84715 6.1830 -21.677
    ## + Child_NonVoc_Duration  1   1.80862 6.2215 -21.547
    ## + Child_Voc_Duration     1   1.73113 6.2990 -21.287
    ## + Average_SignalLevel    1   1.65911 6.3710 -21.048
    ## <none>                               8.0302 -18.188
    ## + Peak_SignalLevel       1   0.08063 7.9495 -16.400
    ## 
    ## Step:  AIC=-22.33
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.9942 -22.328
    ## + Child_NonVoc_Duration  1  0.297622 5.6966 -21.398
    ## + Average_SignalLevel    1  0.277932 5.7163 -21.325
    ## + Peak_SignalLevel       1  0.266405 5.7278 -21.283
    ## + Child_Voc_Count        1  0.007715 5.9865 -20.355
    ## + Child_Voc_Duration     1  0.004082 5.9901 -20.343
    ## Start:  AIC=-18.92
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   2.12109 5.6329 -23.634
    ## + Child_Voc_Count        1   1.93891 5.8151 -22.965
    ## + Child_Voc_Duration     1   1.76390 5.9901 -22.343
    ## + Average_SignalLevel    1   1.67890 6.0751 -22.047
    ## + Child_NonVoc_Duration  1   1.61537 6.1386 -21.828
    ## <none>                               7.7540 -18.923
    ## + Peak_SignalLevel       1   0.17704 7.5769 -17.408
    ## 
    ## Step:  AIC=-23.63
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.6329 -23.634
    ## + Average_SignalLevel    1  0.243640 5.3892 -22.562
    ## + Peak_SignalLevel       1  0.179181 5.4537 -22.313
    ## + Child_NonVoc_Duration  1  0.143327 5.4895 -22.175
    ## + Child_Voc_Count        1  0.009352 5.6235 -21.669
    ## + Child_Voc_Duration     1  0.000308 5.6326 -21.635
    ## Start:  AIC=-18.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.95340 5.9699 -22.414
    ## + Child_Voc_Count        1   1.82690 6.0964 -21.973
    ## + Child_Voc_Duration     1   1.72656 6.1967 -21.631
    ## + Child_NonVoc_Duration  1   1.60159 6.3217 -21.211
    ## + Average_SignalLevel    1   1.28058 6.6427 -20.171
    ## <none>                               7.9233 -18.469
    ## + Peak_SignalLevel       1   0.11978 7.8035 -16.789
    ## 
    ## Step:  AIC=-22.41
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.9699 -22.414
    ## + Peak_SignalLevel       1  0.257308 5.7126 -21.339
    ## + Child_NonVoc_Duration  1  0.159037 5.8108 -20.981
    ## + Average_SignalLevel    1  0.092071 5.8778 -20.740
    ## + Child_Voc_Count        1  0.016992 5.9529 -20.474
    ## + Child_Voc_Duration     1  0.011162 5.9587 -20.453
    ## Start:  AIC=-18.85
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_NonVoc_Duration  1   2.06980 5.7127 -23.338
    ## + Turn_Count             1   1.78201 6.0005 -22.306
    ## + Child_Voc_Count        1   1.61920 6.1633 -21.744
    ## + Child_Voc_Duration     1   1.45534 6.3272 -21.193
    ## + Average_SignalLevel    1   1.29034 6.4922 -20.652
    ## <none>                               7.7825 -18.845
    ## + Peak_SignalLevel       1   0.03132 7.7512 -16.930
    ## 
    ## Step:  AIC=-23.34
    ## get(smq[k]) ~ Child_NonVoc_Duration
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## <none>                             5.7127 -23.338
    ## + Average_SignalLevel  1  0.190748 5.5220 -22.052
    ## + Turn_Count           1  0.174814 5.5379 -21.991
    ## + Child_Voc_Count      1  0.114132 5.5986 -21.762
    ## + Child_Voc_Duration   1  0.066463 5.6463 -21.584
    ## + Peak_SignalLevel     1  0.040230 5.6725 -21.487
    ## Start:  AIC=-18.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   2.55238 5.3709 -24.634
    ## + Child_Voc_Count        1   2.19659 5.7267 -23.287
    ## + Child_NonVoc_Duration  1   2.08039 5.8429 -22.865
    ## + Child_Voc_Duration     1   2.02509 5.8982 -22.667
    ## + Average_SignalLevel    1   1.27449 6.6488 -20.152
    ## <none>                               7.9233 -18.469
    ## + Peak_SignalLevel       1   0.11376 7.8095 -16.773
    ## 
    ## Step:  AIC=-24.63
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.3709 -24.634
    ## + Peak_SignalLevel       1   0.32808 5.0428 -23.958
    ## + Child_NonVoc_Duration  1   0.29707 5.0738 -23.829
    ## + Average_SignalLevel    1   0.08522 5.2857 -22.970
    ## + Child_Voc_Duration     1   0.00155 5.3694 -22.640
    ## + Child_Voc_Count        1   0.00000 5.3709 -22.634
    ## Start:  AIC=-18.25
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.94917 6.0566 -22.111
    ## + Child_Voc_Count        1   1.83120 6.1746 -21.706
    ## + Child_NonVoc_Duration  1   1.72294 6.2829 -21.341
    ## + Child_Voc_Duration     1   1.71964 6.2862 -21.329
    ## + Average_SignalLevel    1   1.36431 6.6415 -20.175
    ## <none>                               8.0058 -18.251
    ## + Peak_SignalLevel       1   0.05447 7.9513 -16.395
    ## 
    ## Step:  AIC=-22.11
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0566 -22.111
    ## + Peak_SignalLevel       1   0.48006 5.5766 -21.845
    ## + Child_NonVoc_Duration  1   0.25605 5.8006 -21.018
    ## + Average_SignalLevel    1   0.13812 5.9185 -20.595
    ## + Child_Voc_Count        1   0.01659 6.0401 -20.168
    ## + Child_Voc_Duration     1   0.00878 6.0479 -20.141
    ## Start:  AIC=-18.25
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.94605 6.0598 -22.100
    ## + Child_Voc_Count        1   1.80137 6.2045 -21.604
    ## + Child_Voc_Duration     1   1.68920 6.3166 -21.228
    ## + Child_NonVoc_Duration  1   1.68527 6.3206 -21.215
    ## + Average_SignalLevel    1   1.45959 6.5462 -20.478
    ## <none>                               8.0058 -18.251
    ## + Peak_SignalLevel       1   0.05547 7.9504 -16.397
    ## 
    ## Step:  AIC=-22.1
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0598 -22.100
    ## + Peak_SignalLevel       1   0.44812 5.6116 -21.713
    ## + Child_NonVoc_Duration  1   0.23871 5.8211 -20.944
    ## + Average_SignalLevel    1   0.17009 5.8897 -20.698
    ## + Child_Voc_Count        1   0.01185 6.0479 -20.141
    ## + Child_Voc_Duration     1   0.00589 6.0539 -20.120
    ## Start:  AIC=-18.85
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    2.3695 5.4130 -24.470
    ## + Child_Voc_Count        1    2.1834 5.5991 -23.760
    ## + Child_Voc_Duration     1    2.0622 5.7204 -23.310
    ## + Average_SignalLevel    1    1.9733 5.8092 -22.987
    ## + Child_NonVoc_Duration  1    1.8979 5.8846 -22.716
    ## <none>                               7.7825 -18.845
    ## + Peak_SignalLevel       1    0.1064 7.6761 -17.134
    ## 
    ## Step:  AIC=-24.47
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.4130 -24.470
    ## + Average_SignalLevel    1   0.35125 5.0618 -23.879
    ## + Peak_SignalLevel       1   0.34070 5.0723 -23.835
    ## + Child_NonVoc_Duration  1   0.22530 5.1877 -23.363
    ## + Child_Voc_Count        1   0.01618 5.3968 -22.533
    ## + Child_Voc_Duration     1   0.01190 5.4011 -22.516
    ## Start:  AIC=-18.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.88069 6.0426 -22.160
    ## + Child_Voc_Count        1   1.68764 6.2356 -21.499
    ## + Child_NonVoc_Duration  1   1.61378 6.3095 -21.252
    ## + Child_Voc_Duration     1   1.57277 6.3505 -21.116
    ## + Average_SignalLevel    1   1.27803 6.6452 -20.163
    ## <none>                               7.9233 -18.469
    ## + Peak_SignalLevel       1   0.10227 7.8210 -16.742
    ## 
    ## Step:  AIC=-22.16
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0426 -22.160
    ## + Peak_SignalLevel       1  0.294584 5.7480 -21.209
    ## + Child_NonVoc_Duration  1  0.212518 5.8301 -20.911
    ## + Average_SignalLevel    1  0.122232 5.9204 -20.589
    ## + Child_Voc_Duration     1  0.002979 6.0396 -20.170
    ## + Child_Voc_Count        1  0.001289 6.0413 -20.164
    ## Start:  AIC=-18.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.99807 5.9252 -22.571
    ## + Child_Voc_Count        1   1.76111 6.1622 -21.748
    ## + Child_Voc_Duration     1   1.64449 6.2788 -21.354
    ## + Child_NonVoc_Duration  1   1.63768 6.2856 -21.331
    ## + Average_SignalLevel    1   1.36644 6.5568 -20.444
    ## <none>                               7.9233 -18.469
    ## + Peak_SignalLevel       1   0.09093 7.8324 -16.712
    ## 
    ## Step:  AIC=-22.57
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.9252 -22.571
    ## + Peak_SignalLevel       1  0.296866 5.6283 -21.651
    ## + Child_NonVoc_Duration  1  0.190167 5.7350 -21.256
    ## + Average_SignalLevel    1  0.127449 5.7978 -21.028
    ## + Child_Voc_Count        1  0.000228 5.9250 -20.572
    ## + Child_Voc_Duration     1  0.000064 5.9251 -20.572
    ## Start:  AIC=-18.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.90336 6.0199 -22.238
    ## + Child_Voc_Count        1   1.81757 6.1057 -21.941
    ## + Child_Voc_Duration     1   1.73848 6.1848 -21.671
    ## + Child_NonVoc_Duration  1   1.66630 6.2570 -21.427
    ## + Average_SignalLevel    1   1.43098 6.4923 -20.652
    ## <none>                               7.9233 -18.469
    ## + Peak_SignalLevel       1   0.12521 7.7981 -16.804
    ## 
    ## Step:  AIC=-22.24
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0199 -22.238
    ## + Peak_SignalLevel       1  0.273378 5.7465 -21.214
    ## + Child_NonVoc_Duration  1  0.244168 5.7758 -21.108
    ## + Average_SignalLevel    1  0.170985 5.8489 -20.843
    ## + Child_Voc_Count        1  0.024667 5.9953 -20.325
    ## + Child_Voc_Duration     1  0.018874 6.0010 -20.304
    ## Start:  AIC=-18.41
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   2.45285 5.4916 -24.167
    ## + Child_Voc_Count        1   2.17028 5.7742 -23.114
    ## + Child_Voc_Duration     1   2.00741 5.9370 -22.530
    ## + Child_NonVoc_Duration  1   1.80048 6.1440 -21.810
    ## + Average_SignalLevel    1   1.75524 6.1892 -21.656
    ## <none>                               7.9444 -18.413
    ## + Peak_SignalLevel       1   0.11703 7.8274 -16.725
    ## 
    ## Step:  AIC=-24.17
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.4916 -24.167
    ## + Peak_SignalLevel       1  0.284240 5.2074 -23.283
    ## + Average_SignalLevel    1  0.244500 5.2471 -23.124
    ## + Child_NonVoc_Duration  1  0.147504 5.3441 -22.739
    ## + Child_Voc_Count        1  0.003228 5.4884 -22.180
    ## + Child_Voc_Duration     1  0.000271 5.4913 -22.168
    ## Start:  AIC=-18.85
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.70672 6.0758 -22.044
    ## + Child_Voc_Count        1   1.59209 6.1905 -21.652
    ## + Child_Voc_Duration     1   1.49000 6.2925 -21.308
    ## + Child_NonVoc_Duration  1   1.47422 6.3083 -21.256
    ## + Average_SignalLevel    1   1.21552 6.5670 -20.412
    ## <none>                               7.7825 -18.845
    ## + Peak_SignalLevel       1   0.02702 7.7555 -16.919
    ## 
    ## Step:  AIC=-22.04
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0758 -22.044
    ## + Peak_SignalLevel       1   0.32448 5.7513 -21.197
    ## + Child_NonVoc_Duration  1   0.24793 5.8279 -20.919
    ## + Average_SignalLevel    1   0.14374 5.9321 -20.547
    ## + Child_Voc_Count        1   0.01366 6.0622 -20.092
    ## + Child_Voc_Duration     1   0.00698 6.0688 -20.069
    ## Start:  AIC=-18.85
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_NonVoc_Duration  1    2.0095 5.7731 -23.118
    ## + Turn_Count             1    1.8608 5.9218 -22.584
    ## + Child_Voc_Count        1    1.7329 6.0496 -22.135
    ## + Child_Voc_Duration     1    1.6646 6.1179 -21.899
    ## + Average_SignalLevel    1    1.3361 6.4464 -20.801
    ## <none>                               7.7825 -18.845
    ## + Peak_SignalLevel       1    0.1202 7.6623 -17.172
    ## 
    ## Step:  AIC=-23.12
    ## get(smq[k]) ~ Child_NonVoc_Duration
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## <none>                             5.7731 -23.118
    ## + Turn_Count           1  0.274445 5.4986 -22.140
    ## + Child_Voc_Duration   1  0.253535 5.5195 -22.061
    ## + Average_SignalLevel  1  0.243837 5.5292 -22.024
    ## + Child_Voc_Count      1  0.226235 5.5468 -21.957
    ## + Peak_SignalLevel     1  0.007334 5.7657 -21.144
    ## Start:  AIC=-18.25
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   2.07683 5.9290 -22.558
    ## + Child_Voc_Count        1   1.81437 6.1915 -21.648
    ## + Child_NonVoc_Duration  1   1.71092 6.2949 -21.300
    ## + Child_Voc_Duration     1   1.69653 6.3093 -21.253
    ## + Average_SignalLevel    1   1.38743 6.6184 -20.248
    ## <none>                               8.0058 -18.251
    ## + Peak_SignalLevel       1   0.05762 7.9482 -16.403
    ## 
    ## Step:  AIC=-22.56
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               5.9290 -22.558
    ## + Peak_SignalLevel       1  0.284789 5.6442 -21.592
    ## + Average_SignalLevel    1  0.164358 5.7646 -21.148
    ## + Child_NonVoc_Duration  1  0.143688 5.7853 -21.073
    ## + Child_Voc_Count        1  0.000577 5.9284 -20.560
    ## + Child_Voc_Duration     1  0.000234 5.9288 -20.559
    ## Start:  AIC=-18.85
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.71287 6.0697 -22.066
    ## + Child_Voc_Count        1   1.55134 6.2312 -21.514
    ## + Child_NonVoc_Duration  1   1.52683 6.2557 -21.432
    ## + Child_Voc_Duration     1   1.46833 6.3142 -21.236
    ## + Average_SignalLevel    1   1.32450 6.4580 -20.763
    ## <none>                               7.7825 -18.845
    ## + Peak_SignalLevel       1   0.10315 7.6794 -17.126
    ## 
    ## Step:  AIC=-22.07
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0697 -22.066
    ## + Peak_SignalLevel       1   0.32925 5.7404 -21.237
    ## + Child_NonVoc_Duration  1   0.24866 5.8210 -20.944
    ## + Average_SignalLevel    1   0.16144 5.9082 -20.632
    ## + Child_Voc_Count        1   0.00759 6.0621 -20.092
    ## + Child_Voc_Duration     1   0.00468 6.0650 -20.082
    ## Start:  AIC=-18.28
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1   1.97771 6.0186 -22.243
    ## + Child_NonVoc_Duration  1   1.81974 6.1766 -21.699
    ## + Child_Voc_Count        1   1.81234 6.1840 -21.674
    ## + Child_Voc_Duration     1   1.69113 6.3052 -21.266
    ## + Average_SignalLevel    1   1.39000 6.6063 -20.287
    ## <none>                               7.9963 -18.276
    ## + Peak_SignalLevel       1   0.06441 7.9319 -16.446
    ## 
    ## Step:  AIC=-22.24
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               6.0186 -22.243
    ## + Peak_SignalLevel       1   0.38345 5.6351 -21.625
    ## + Child_NonVoc_Duration  1   0.29629 5.7223 -21.303
    ## + Average_SignalLevel    1   0.14308 5.8755 -20.748
    ## + Child_Voc_Count        1   0.00926 6.0093 -20.275
    ## + Child_Voc_Duration     1   0.00360 6.0150 -20.256
    ## Start:  AIC=-1.97
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    5.6149 11.768 -8.1620
    ## + Child_Voc_Count        1    4.9199 12.463 -6.9570
    ## + Child_Voc_Duration     1    3.9125 13.470 -5.3247
    ## + Child_NonVoc_Duration  1    1.9600 15.423 -2.4821
    ## <none>                               17.383 -1.9698
    ## + Average_SignalLevel    1    1.1457 16.237 -1.4016
    ## + Peak_SignalLevel       1    0.7005 16.682 -0.8336
    ## 
    ## Step:  AIC=-8.16
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.768 -8.1620
    ## + Child_Voc_Duration     1  0.232159 11.536 -6.5805
    ## + Average_SignalLevel    1  0.195499 11.572 -6.5138
    ## + Peak_SignalLevel       1  0.148932 11.619 -6.4295
    ## + Child_NonVoc_Duration  1  0.003436 11.764 -6.1682
    ## + Child_Voc_Count        1  0.001221 11.767 -6.1642
    ## Start:  AIC=-5.09
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Turn_Count             1    4.3785 10.604 -10.3484
    ## + Child_Voc_Count        1    3.6513 11.332  -8.9555
    ## + Child_Voc_Duration     1    2.8166 12.166  -7.4629
    ## + Child_NonVoc_Duration  1    1.5546 13.428  -5.3905
    ## <none>                               14.983  -5.0899
    ## + Peak_SignalLevel       1    0.4998 14.483  -3.8025
    ## + Average_SignalLevel    1    0.3271 14.656  -3.5535
    ## 
    ## Step:  AIC=-10.35
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               10.604 -10.3484
    ## + Average_SignalLevel    1   0.58039 10.024  -9.5304
    ## + Child_Voc_Duration     1   0.21450 10.390  -8.7776
    ## + Peak_SignalLevel       1   0.16895 10.435  -8.6857
    ## + Child_NonVoc_Duration  1   0.00914 10.595  -8.3665
    ## + Child_Voc_Count        1   0.00507 10.599  -8.3585
    ## Start:  AIC=-2.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.0657 10.906 -9.7599
    ## + Child_Voc_Count        1    5.0734 11.898 -7.9312
    ## + Child_Voc_Duration     1    4.0941 12.877 -6.2702
    ## + Child_NonVoc_Duration  1    2.4013 14.570 -3.6766
    ## + Peak_SignalLevel       1    2.2944 14.677 -3.5231
    ## + Average_SignalLevel    1    2.0001 14.971 -3.1061
    ## <none>                               16.971 -2.4728
    ## 
    ## Step:  AIC=-9.76
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.906 -9.7599
    ## + Child_Voc_Duration     1  0.241356 10.664 -8.2299
    ## + Average_SignalLevel    1  0.057129 10.849 -7.8702
    ## + Child_Voc_Count        1  0.013303 10.892 -7.7855
    ## + Child_NonVoc_Duration  1  0.012092 10.894 -7.7832
    ## + Peak_SignalLevel       1  0.001194 10.905 -7.7622
    ## Start:  AIC=-1.87
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.0409 11.426 -8.7817
    ## + Child_Voc_Count        1    4.9259 12.541 -6.8263
    ## + Child_Voc_Duration     1    3.8931 13.574 -5.1643
    ## + Child_NonVoc_Duration  1    2.9511 14.516 -3.7553
    ## + Average_SignalLevel    1    1.6001 15.867 -1.8865
    ## <none>                               17.467 -1.8688
    ## + Peak_SignalLevel       1    1.3092 16.157 -1.5049
    ## 
    ## Step:  AIC=-8.78
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.426 -8.7817
    ## + Child_Voc_Duration     1   0.33310 11.093 -7.4031
    ## + Average_SignalLevel    1   0.14195 11.284 -7.0443
    ## + Peak_SignalLevel       1   0.07001 11.356 -6.9108
    ## + Child_Voc_Count        1   0.03315 11.393 -6.8428
    ## + Child_NonVoc_Duration  1   0.00878 11.417 -6.7979
    ## Start:  AIC=-1.87
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.0145 11.452 -8.7333
    ## + Child_Voc_Count        1    4.9734 12.493 -6.9060
    ## + Child_Voc_Duration     1    3.9316 13.535 -5.2240
    ## + Child_NonVoc_Duration  1    2.7160 14.751 -3.4179
    ## <none>                               17.467 -1.8688
    ## + Average_SignalLevel    1    1.0775 16.389 -1.2059
    ## + Peak_SignalLevel       1    0.7836 16.683 -0.8328
    ## 
    ## Step:  AIC=-8.73
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.452 -8.7333
    ## + Average_SignalLevel    1   0.33714 11.115 -7.3608
    ## + Child_Voc_Duration     1   0.29611 11.156 -7.2834
    ## + Peak_SignalLevel       1   0.20548 11.247 -7.1135
    ## + Child_Voc_Count        1   0.01931 11.433 -6.7687
    ## + Child_NonVoc_Duration  1   0.00112 11.451 -6.7353
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.8558 11.754 -8.1875
    ## + Child_Voc_Count        1    5.7039 12.906 -6.2240
    ## + Child_Voc_Duration     1    4.5630 14.046 -4.4451
    ## + Child_NonVoc_Duration  1    3.1589 15.451 -2.4444
    ## + Average_SignalLevel    1    1.7156 16.894 -0.5689
    ## <none>                               18.610 -0.5378
    ## + Peak_SignalLevel       1    1.1511 17.458  0.1212
    ## 
    ## Step:  AIC=-8.19
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.754 -8.1875
    ## + Child_Voc_Duration     1  0.280919 11.473 -6.6955
    ## + Average_SignalLevel    1  0.131348 11.622 -6.4235
    ## + Peak_SignalLevel       1  0.093359 11.660 -6.3549
    ## + Child_Voc_Count        1  0.015917 11.738 -6.2159
    ## + Child_NonVoc_Duration  1  0.008000 11.746 -6.2018
    ## Start:  AIC=-0.56
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.7656 11.821 -8.0674
    ## + Child_Voc_Count        1    5.6550 12.932 -6.1817
    ## + Child_Voc_Duration     1    4.5552 14.031 -4.4676
    ## + Child_NonVoc_Duration  1    3.3166 15.270 -2.6912
    ## <none>                               18.587 -0.5636
    ## + Average_SignalLevel    1    1.6167 16.970 -0.4747
    ## + Peak_SignalLevel       1    1.1268 17.460  0.1231
    ## 
    ## Step:  AIC=-8.07
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.821 -8.0674
    ## + Child_Voc_Duration     1  0.283669 11.537 -6.5774
    ## + Average_SignalLevel    1  0.206354 11.615 -6.4372
    ## + Peak_SignalLevel       1  0.151445 11.670 -6.3381
    ## + Child_Voc_Count        1  0.016286 11.805 -6.0963
    ## + Child_NonVoc_Duration  1  0.003839 11.817 -6.0742
    ## Start:  AIC=-2.47
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Turn_Count             1    6.8720 10.099 -11.3730
    ## + Child_Voc_Count        1    5.4797 11.492  -8.6608
    ## + Child_NonVoc_Duration  1    5.2367 11.735  -8.2213
    ## + Child_Voc_Duration     1    4.3085 12.663  -6.6227
    ## + Average_SignalLevel    1    2.6215 14.350  -3.9964
    ## <none>                               16.971  -2.4728
    ## + Peak_SignalLevel       1    0.7496 16.222  -1.4214
    ## 
    ## Step:  AIC=-11.37
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## <none>                               10.0994 -11.3730
    ## + Child_Voc_Duration     1   0.53689  9.5625 -10.5201
    ## + Peak_SignalLevel       1   0.47872  9.6207 -10.3928
    ## + Child_NonVoc_Duration  1   0.39456  9.7048 -10.2099
    ## + Child_Voc_Count        1   0.08882 10.0106  -9.5585
    ## + Average_SignalLevel    1   0.00784 10.0916  -9.3893
    ## Start:  AIC=-1
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Turn_Count             1    7.6725 10.533 -10.4898
    ## + Child_Voc_Count        1    6.4567 11.749  -8.1959
    ## + Child_Voc_Duration     1    5.7481 12.458  -6.9660
    ## + Child_NonVoc_Duration  1    2.8408 15.365  -2.5612
    ## + Average_SignalLevel    1    1.8933 16.312  -1.3045
    ## <none>                               18.206  -0.9985
    ## + Peak_SignalLevel       1    1.6311 16.575  -0.9697
    ## 
    ## Step:  AIC=-10.49
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               10.533 -10.4898
    ## + Child_NonVoc_Duration  1  0.237440 10.296  -8.9686
    ## + Average_SignalLevel    1  0.210753 10.322  -8.9142
    ## + Child_Voc_Duration     1  0.072554 10.461  -8.6349
    ## + Peak_SignalLevel       1  0.042309 10.491  -8.5743
    ## + Child_Voc_Count        1  0.010557 10.523  -8.5109
    ## Start:  AIC=-0.45
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.5573 11.128 -9.3355
    ## + Child_Voc_Count        1    6.1024 12.583 -6.7552
    ## + Child_Voc_Duration     1    4.8513 13.835 -4.7646
    ## + Child_NonVoc_Duration  1    3.3692 15.316 -2.6275
    ## + Average_SignalLevel    1    1.9645 16.721 -0.7847
    ## <none>                               18.686 -0.4520
    ## + Peak_SignalLevel       1    1.2367 17.449  0.1100
    ## 
    ## Step:  AIC=-9.34
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Average_SignalLevel    1   2.04906  9.0793 -11.6090
    ## <none>                               11.1284  -9.3355
    ## + Child_Voc_Duration     1   0.40467 10.7237  -8.1134
    ## + Peak_SignalLevel       1   0.13258 10.9958  -7.5872
    ## + Child_Voc_Count        1   0.04627 11.0821  -7.4230
    ## + Child_NonVoc_Duration  1   0.00918 11.1192  -7.3528
    ## 
    ## Step:  AIC=-11.61
    ## get(smq[k]) ~ Turn_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               9.0793 -11.6090
    ## + Child_NonVoc_Duration  1  0.217817 8.8615 -10.1189
    ## + Child_Voc_Duration     1  0.212466 8.8669 -10.1063
    ## + Child_Voc_Count        1  0.002336 9.0770  -9.6144
    ## + Peak_SignalLevel       1  0.000880 9.0784  -9.6110
    ## Start:  AIC=-0.56
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.8476 11.739 -8.2135
    ## + Child_Voc_Count        1    5.9259 12.661 -6.6263
    ## + Child_Voc_Duration     1    4.7908 13.796 -4.8231
    ## + Child_NonVoc_Duration  1    3.2652 15.322 -2.6206
    ## <none>                               18.587 -0.5636
    ## + Average_SignalLevel    1    1.6736 16.913 -0.5452
    ## + Peak_SignalLevel       1    1.1308 17.456  0.1182
    ## 
    ## Step:  AIC=-8.21
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.739 -8.2135
    ## + Peak_SignalLevel       1  0.285641 11.454 -6.7308
    ## + Child_Voc_Duration     1  0.229770 11.509 -6.6286
    ## + Average_SignalLevel    1  0.208359 11.531 -6.5896
    ## + Child_NonVoc_Duration  1  0.004444 11.735 -6.2215
    ## + Child_Voc_Count        1  0.002578 11.736 -6.2181
    ## Start:  AIC=-0.76
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.8136 11.598 -8.4678
    ## + Child_Voc_Count        1    5.8569 12.555 -6.8033
    ## + Child_Voc_Duration     1    4.7299 13.681 -4.9980
    ## + Child_NonVoc_Duration  1    3.1070 15.305 -2.6440
    ## + Average_SignalLevel    1    1.9500 16.461 -1.1135
    ## <none>                               18.411 -0.7626
    ## + Peak_SignalLevel       1    0.9790 17.432  0.0900
    ## 
    ## Step:  AIC=-8.47
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.598 -8.4678
    ## + Peak_SignalLevel       1   0.34526 11.253 -7.1024
    ## + Child_Voc_Duration     1   0.22313 11.375 -6.8757
    ## + Average_SignalLevel    1   0.12334 11.475 -6.6923
    ## + Child_Voc_Count        1   0.00274 11.595 -6.4727
    ## + Child_NonVoc_Duration  1   0.00024 11.598 -6.4682
    ## Start:  AIC=-1.97
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Turn_Count             1    8.5949  8.788 -14.2939
    ## + Child_Voc_Count        1    7.3410 10.042 -11.4929
    ## + Child_Voc_Duration     1    6.0586 11.324  -8.9691
    ## + Child_NonVoc_Duration  1    3.8002 13.583  -5.1503
    ## + Average_SignalLevel    1    3.1074 14.275  -4.1056
    ## <none>                               17.383  -1.9698
    ## + Peak_SignalLevel       1    1.5167 15.866  -1.8870
    ## 
    ## Step:  AIC=-14.29
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               8.7880 -14.294
    ## + Child_Voc_Duration     1  0.182171 8.6058 -12.734
    ## + Peak_SignalLevel       1  0.152498 8.6355 -12.662
    ## + Child_Voc_Count        1  0.001736 8.7862 -12.298
    ## + Average_SignalLevel    1  0.000362 8.7876 -12.295
    ## + Child_NonVoc_Duration  1  0.000002 8.7880 -12.294
    ## Start:  AIC=-0.76
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.6534 11.758 -8.1797
    ## + Child_Voc_Count        1    5.5360 12.875 -6.2732
    ## + Child_Voc_Duration     1    4.3790 14.032 -4.4662
    ## + Child_NonVoc_Duration  1    2.9415 15.470 -2.4181
    ## <none>                               18.411 -0.7626
    ## + Average_SignalLevel    1    1.4945 16.917 -0.5404
    ## + Peak_SignalLevel       1    1.4141 16.997 -0.4408
    ## 
    ## Step:  AIC=-8.18
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.758 -8.1797
    ## + Child_Voc_Duration     1   0.67721 11.081 -7.4254
    ## + Average_SignalLevel    1   0.25077 11.507 -6.6324
    ## + Child_Voc_Count        1   0.13919 11.619 -6.4298
    ## + Peak_SignalLevel       1   0.09676 11.661 -6.3532
    ## + Child_NonVoc_Duration  1   0.00274 11.755 -6.1846
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.8140 11.796 -8.1128
    ## + Child_Voc_Count        1    5.7929 12.817 -6.3693
    ## + Child_Voc_Duration     1    4.6689 13.941 -4.6040
    ## + Child_NonVoc_Duration  1    3.3483 15.261 -2.7033
    ## + Average_SignalLevel    1    1.7310 16.878 -0.5881
    ## <none>                               18.610 -0.5378
    ## + Peak_SignalLevel       1    1.1872 17.422  0.0778
    ## 
    ## Step:  AIC=-8.11
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.796 -8.1128
    ## + Child_Voc_Duration     1  0.264014 11.532 -6.5881
    ## + Average_SignalLevel    1  0.182710 11.613 -6.4406
    ## + Peak_SignalLevel       1  0.139933 11.656 -6.3634
    ## + Child_Voc_Count        1  0.009181 11.786 -6.1291
    ## + Child_NonVoc_Duration  1  0.006229 11.789 -6.1239
    ## Start:  AIC=-1.46
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.5796 11.230 -9.1448
    ## + Child_Voc_Count        1    5.9264 11.883 -7.9575
    ## + Child_Voc_Duration     1    4.9415 12.868 -6.2853
    ## + Child_NonVoc_Duration  1    3.0568 14.753 -3.4149
    ## + Average_SignalLevel    1    1.8698 15.940 -1.7898
    ## + Peak_SignalLevel       1    1.8123 15.997 -1.7143
    ## <none>                               17.809 -1.4606
    ## 
    ## Step:  AIC=-9.14
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.230 -9.1448
    ## + Average_SignalLevel    1  0.116305 11.114 -7.3635
    ## + Child_Voc_Duration     1  0.098079 11.132 -7.3290
    ## + Peak_SignalLevel       1  0.018382 11.211 -7.1792
    ## + Child_Voc_Count        1  0.007120 11.223 -7.1581
    ## + Child_NonVoc_Duration  1  0.001702 11.228 -7.1480
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.0883 11.521 -8.6069
    ## + Child_Voc_Count        1    5.8193 12.790 -6.4127
    ## + Child_Voc_Duration     1    4.6207 13.989 -4.5315
    ## + Child_NonVoc_Duration  1    3.1433 15.466 -2.4232
    ## <none>                               18.610 -0.5378
    ## + Average_SignalLevel    1    1.6451 16.964 -0.4815
    ## + Peak_SignalLevel       1    1.1511 17.458  0.1213
    ## 
    ## Step:  AIC=-8.61
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.521 -8.6069
    ## + Child_Voc_Duration     1   0.32071 11.200 -7.1998
    ## + Average_SignalLevel    1   0.13083 11.390 -6.8468
    ## + Peak_SignalLevel       1   0.11201 11.409 -6.8121
    ## + Child_Voc_Count        1   0.02215 11.499 -6.6473
    ## + Child_NonVoc_Duration  1   0.00134 11.520 -6.6094
    ## Start:  AIC=-1.97
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    5.5923 11.791 -8.1216
    ## + Child_Voc_Count        1    4.4603 12.923 -6.1965
    ## + Child_Voc_Duration     1    3.3188 14.064 -4.4188
    ## + Child_NonVoc_Duration  1    2.3442 15.039 -3.0119
    ## <none>                               17.383 -1.9698
    ## + Average_SignalLevel    1    1.2530 16.130 -1.5409
    ## + Peak_SignalLevel       1    0.7571 16.626 -0.9049
    ## 
    ## Step:  AIC=-8.12
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.791 -8.1216
    ## + Child_Voc_Duration     1   0.59585 11.195 -7.2106
    ## + Average_SignalLevel    1   0.18074 11.610 -6.4460
    ## + Peak_SignalLevel       1   0.13597 11.655 -6.3652
    ## + Child_Voc_Count        1   0.07256 11.718 -6.2512
    ## + Child_NonVoc_Duration  1   0.00147 11.789 -6.1242
    ## Start:  AIC=-1.97
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.4570 10.926 -9.7213
    ## + Child_Voc_Count        1    5.5433 11.840 -8.0346
    ## + Child_Voc_Duration     1    4.6078 12.775 -6.4376
    ## + Child_NonVoc_Duration  1    4.1236 13.259 -5.6564
    ## + Peak_SignalLevel       1    1.6406 15.742 -2.0517
    ## + Average_SignalLevel    1    1.6001 15.783 -1.9977
    ## <none>                               17.383 -1.9698
    ## 
    ## Step:  AIC=-9.72
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               10.926 -9.7213
    ## + Average_SignalLevel    1  0.183173 10.743 -8.0763
    ## + Child_Voc_Duration     1  0.154884 10.771 -8.0211
    ## + Child_NonVoc_Duration  1  0.130024 10.796 -7.9727
    ## + Peak_SignalLevel       1  0.026205 10.900 -7.7717
    ## + Child_Voc_Count        1  0.002120 10.924 -7.7253
    ## Start:  AIC=-1.06
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    6.3864 11.766 -8.1655
    ## + Child_Voc_Count        1    5.2141 12.938 -6.1710
    ## + Child_Voc_Duration     1    4.0987 14.054 -4.4343
    ## + Child_NonVoc_Duration  1    3.2549 14.897 -3.2100
    ## <none>                               18.152 -1.0601
    ## + Average_SignalLevel    1    1.3246 16.828 -0.6513
    ## + Peak_SignalLevel       1    0.8875 17.265 -0.1129
    ## 
    ## Step:  AIC=-8.17
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.766 -8.1655
    ## + Child_Voc_Duration     1   0.32067 11.445 -6.7457
    ## + Average_SignalLevel    1   0.17822 11.588 -6.4860
    ## + Peak_SignalLevel       1   0.11436 11.652 -6.3706
    ## + Child_Voc_Count        1   0.02876 11.737 -6.2168
    ## + Child_NonVoc_Duration  1   0.00234 11.764 -6.1696
    ## Start:  AIC=-1.97
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    5.6967 11.686 -8.3084
    ## + Child_Voc_Count        1    4.6818 12.701 -6.5596
    ## + Child_Voc_Duration     1    3.7841 13.599 -5.1254
    ## + Child_NonVoc_Duration  1    2.5980 14.785 -3.3693
    ## <none>                               17.383 -1.9698
    ## + Average_SignalLevel    1    1.5691 15.814 -1.9565
    ## + Peak_SignalLevel       1    1.4873 15.896 -1.8481
    ## 
    ## Step:  AIC=-8.31
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.686 -8.3084
    ## + Child_Voc_Duration     1  0.242485 11.444 -6.7487
    ## + Average_SignalLevel    1  0.139992 11.546 -6.5615
    ## + Peak_SignalLevel       1  0.065688 11.620 -6.4268
    ## + Child_Voc_Count        1  0.016277 11.670 -6.3377
    ## + Child_NonVoc_Duration  1  0.003424 11.683 -6.3146
    ## Start:  AIC=-1
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Turn_Count             1    7.0236 11.182 -9.2344
    ## + Child_Voc_Count        1    5.9362 12.270 -7.2856
    ## + Child_Voc_Duration     1    4.7544 13.451 -5.3543
    ## + Child_NonVoc_Duration  1    3.6888 14.517 -3.7534
    ## + Average_SignalLevel    1    1.7540 16.452 -1.1259
    ## <none>                               18.206 -0.9985
    ## + Peak_SignalLevel       1    1.0671 17.139 -0.2669
    ## 
    ## Step:  AIC=-9.23
    ## get(smq[k]) ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.182 -9.2344
    ## + Child_Voc_Duration     1  0.278624 10.903 -7.7643
    ## + Peak_SignalLevel       1  0.253496 10.929 -7.7159
    ## + Average_SignalLevel    1  0.195295 10.987 -7.6044
    ## + Child_NonVoc_Duration  1  0.034434 11.148 -7.2991
    ## + Child_Voc_Count        1  0.009466 11.173 -7.2522
    ## Start:  AIC=-4.79
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1    4.0367 11.164 -9.2677
    ## + Turn_Count             1    3.6318 11.569 -8.5196
    ## + Child_Voc_Duration     1    3.4658 11.735 -8.2205
    ## + Child_NonVoc_Duration  1    2.6607 12.540 -6.8269
    ## <none>                               15.201 -4.7863
    ## + Average_SignalLevel    1    0.1257 15.075 -2.9607
    ## + Peak_SignalLevel       1    0.0065 15.195 -2.7952
    ## 
    ## Step:  AIC=-9.27
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq     RSS     AIC
    ## + Average_SignalLevel    1   1.23800  9.9263 -9.7359
    ## <none>                               11.1643 -9.2677
    ## + Child_Voc_Duration     1   0.96874 10.1956 -9.1739
    ## + Peak_SignalLevel       1   0.96266 10.2017 -9.1614
    ## + Child_NonVoc_Duration  1   0.13919 11.0251 -7.5312
    ## + Turn_Count             1   0.00197 11.1624 -7.2714
    ## 
    ## Step:  AIC=-9.74
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Duration     1   0.91559 9.0108 -9.7682
    ## <none>                               9.9263 -9.7359
    ## + Peak_SignalLevel       1   0.51844 9.4079 -8.8624
    ## + Child_NonVoc_Duration  1   0.20743 9.7189 -8.1794
    ## + Turn_Count             1   0.02020 9.9062 -7.7787
    ## 
    ## Step:  AIC=-9.77
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               9.0108 -9.7682
    ## + Peak_SignalLevel       1  0.232805 8.7780 -8.3179
    ## + Turn_Count             1  0.054960 8.9558 -7.8966
    ## + Child_NonVoc_Duration  1  0.016314 8.9944 -7.8062
    ## Start:  AIC=-1.62
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   1.85641 15.818 -1.95059
    ## <none>                               17.675 -1.62027
    ## + Child_Voc_Duration     1   1.58791 16.087 -1.59713
    ## + Turn_Count             1   0.68786 16.987 -0.45388
    ## + Peak_SignalLevel       1   0.66450 17.010 -0.42501
    ## + Average_SignalLevel    1   0.32037 17.354 -0.00441
    ## + Child_NonVoc_Duration  1   0.00591 17.669  0.37271
    ## 
    ## Step:  AIC=-1.95
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.81645 13.002 -4.0682
    ## + Peak_SignalLevel       1   2.18915 13.629 -3.0787
    ## <none>                               15.818 -1.9506
    ## + Turn_Count             1   1.31195 14.506 -1.7688
    ## + Child_NonVoc_Duration  1   1.01393 14.804 -1.3417
    ## + Child_Voc_Duration     1   0.44306 15.375 -0.5472
    ## 
    ## Step:  AIC=-4.07
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               13.002 -4.0682
    ## + Peak_SignalLevel       1   1.16410 11.838 -4.0380
    ## + Turn_Count             1   0.96205 12.040 -3.6826
    ## + Child_NonVoc_Duration  1   0.71870 12.283 -3.2624
    ## + Child_Voc_Duration     1   0.34356 12.658 -2.6306
    ## Start:  AIC=-3.96
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1   1.97163 13.841 -4.7553
    ## + Child_Voc_Duration     1   1.75193 14.060 -4.4246
    ## <none>                               15.812 -3.9586
    ## + Turn_Count             1   0.80927 15.003 -3.0619
    ## + Peak_SignalLevel       1   0.01480 15.797 -1.9783
    ## + Child_NonVoc_Duration  1   0.00127 15.811 -1.9603
    ## + Average_SignalLevel    1   0.00107 15.811 -1.9600
    ## 
    ## Step:  AIC=-4.76
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_NonVoc_Duration  1   1.39623 12.444 -4.9884
    ## + Average_SignalLevel    1   1.32148 12.519 -4.8627
    ## + Turn_Count             1   1.26252 12.578 -4.7640
    ## <none>                               13.841 -4.7553
    ## + Peak_SignalLevel       1   0.86448 12.976 -4.1097
    ## + Child_Voc_Duration     1   0.27786 13.563 -3.1812
    ## 
    ## Step:  AIC=-4.99
    ## get(smq[k]) ~ Child_Voc_Count + Child_NonVoc_Duration
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## <none>                             12.444 -4.9884
    ## + Peak_SignalLevel     1   1.02562 11.419 -4.7947
    ## + Child_Voc_Duration   1   0.95880 11.486 -4.6722
    ## + Average_SignalLevel  1   0.90157 11.543 -4.5678
    ## + Turn_Count           1   0.85881 11.585 -4.4901
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.46230 16.143 -1.52308
    ## + Child_Voc_Duration     1   2.12549 16.480 -1.08945
    ## <none>                               18.606 -0.54201
    ## + Turn_Count             1   1.17093 17.435  0.09297
    ## + Peak_SignalLevel       1   0.31640 18.289  1.09780
    ## + Child_NonVoc_Duration  1   0.13968 18.466  1.29974
    ## + Average_SignalLevel    1   0.01254 18.593  1.44383
    ## 
    ## Step:  AIC=-1.52
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Average_SignalLevel    1   2.07151 14.072 -2.40703
    ## + Peak_SignalLevel       1   2.02996 14.114 -2.34511
    ## <none>                               16.143 -1.52308
    ## + Turn_Count             1   1.05794 15.086 -0.94644
    ## + Child_NonVoc_Duration  1   0.81031 15.333 -0.60453
    ## + Child_Voc_Duration     1   0.53572 15.608 -0.23178
    ## 
    ## Step:  AIC=-2.41
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               14.072 -2.4070
    ## + Peak_SignalLevel       1   1.16572 12.906 -2.2230
    ## + Turn_Count             1   0.68352 13.389 -1.4527
    ## + Child_Voc_Duration     1   0.53273 13.539 -1.2175
    ## + Child_NonVoc_Duration  1   0.51219 13.560 -1.1857
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   1.97847 15.318 -2.62561
    ## + Child_Voc_Duration     1   1.69401 15.602 -2.23921
    ## <none>                               17.296 -2.07463
    ## + Turn_Count             1   0.83284 16.463 -1.11097
    ## + Peak_SignalLevel       1   0.73948 16.557 -0.99222
    ## + Average_SignalLevel    1   0.20785 17.088 -0.32852
    ## + Child_NonVoc_Duration  1   0.05021 17.246 -0.13568
    ## 
    ## Step:  AIC=-2.63
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.78623 12.532 -4.8416
    ## + Peak_SignalLevel       1   2.49072 12.827 -4.3522
    ## <none>                               15.318 -2.6256
    ## + Turn_Count             1   1.16148 14.156 -2.2815
    ## + Child_NonVoc_Duration  1   0.85749 14.460 -1.8354
    ## + Child_Voc_Duration     1   0.48630 14.832 -1.3031
    ## 
    ## Step:  AIC=-4.84
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Peak_SignalLevel       1   1.42780 11.104 -5.3819
    ## <none>                               12.532 -4.8416
    ## + Turn_Count             1   0.72925 11.802 -4.1007
    ## + Child_NonVoc_Duration  1   0.49353 12.038 -3.6854
    ## + Child_Voc_Duration     1   0.44819 12.083 -3.6065
    ## 
    ## Step:  AIC=-5.38
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               11.104 -5.3819
    ## + Child_NonVoc_Duration  1   0.85551 10.248 -5.0656
    ## + Turn_Count             1   0.17886 10.925 -3.7230
    ## + Child_Voc_Duration     1   0.14874 10.955 -3.6651
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.91133 15.694 -2.11547
    ## + Child_Voc_Duration     1   2.54974 16.056 -1.63713
    ## <none>                               18.606 -0.54201
    ## + Turn_Count             1   1.50248 17.103 -0.31023
    ## + Child_NonVoc_Duration  1   0.24854 18.357  1.17557
    ## + Peak_SignalLevel       1   0.24091 18.365  1.18431
    ## + Average_SignalLevel    1   0.00251 18.603  1.45516
    ## 
    ## Step:  AIC=-2.12
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Peak_SignalLevel       1   1.65020 14.044 -2.44844
    ## + Average_SignalLevel    1   1.61533 14.079 -2.39636
    ## <none>                               15.694 -2.11547
    ## + Turn_Count             1   0.96176 14.733 -1.44347
    ## + Child_NonVoc_Duration  1   0.63908 15.055 -0.98849
    ## + Child_Voc_Duration     1   0.50026 15.194 -0.79574
    ## 
    ## Step:  AIC=-2.45
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               14.044 -2.44844
    ## + Child_NonVoc_Duration  1   1.08396 12.960 -2.13522
    ## + Average_SignalLevel    1   1.06982 12.975 -2.11232
    ## + Turn_Count             1   0.25329 13.791 -0.83064
    ## + Child_Voc_Duration     1   0.17616 13.868 -0.71352
    ## Start:  AIC=-0.44
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1   2.65627 16.037 -1.6623
    ## + Child_Voc_Duration     1   2.28109 16.412 -1.1767
    ## <none>                               18.693 -0.4437
    ## + Turn_Count             1   1.31979 17.373  0.0187
    ## + Peak_SignalLevel       1   0.29970 18.393  1.2169
    ## + Child_NonVoc_Duration  1   0.14899 18.544  1.3882
    ## + Average_SignalLevel    1   0.00450 18.689  1.5512
    ## 
    ## Step:  AIC=-1.66
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Average_SignalLevel    1   1.95146 14.085 -2.38706
    ## + Peak_SignalLevel       1   1.89131 14.146 -2.29758
    ## <none>                               16.037 -1.66230
    ## + Turn_Count             1   1.04366 14.993 -1.07545
    ## + Child_NonVoc_Duration  1   0.94947 15.087 -0.94394
    ## + Child_Voc_Duration     1   0.70157 15.335 -0.60170
    ## 
    ## Step:  AIC=-2.39
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               14.085 -2.3871
    ## + Peak_SignalLevel       1   1.12217 12.963 -2.1305
    ## + Turn_Count             1   0.68477 13.401 -1.4336
    ## + Child_Voc_Duration     1   0.59048 13.495 -1.2864
    ## + Child_NonVoc_Duration  1   0.54732 13.538 -1.2193
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.53397 16.072 -1.61652
    ## + Child_Voc_Duration     1   2.18627 16.419 -1.16705
    ## <none>                               18.606 -0.54201
    ## + Turn_Count             1   1.26977 17.336 -0.02642
    ## + Peak_SignalLevel       1   0.42729 18.178  0.97009
    ## + Child_NonVoc_Duration  1   0.27334 18.332  1.14719
    ## + Average_SignalLevel    1   0.00074 18.605  1.45716
    ## 
    ## Step:  AIC=-1.62
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Peak_SignalLevel       1   2.24053 13.831 -2.76934
    ## + Average_SignalLevel    1   2.00934 14.062 -2.42121
    ## <none>                               16.072 -1.61652
    ## + Turn_Count             1   1.00741 15.064 -0.97590
    ## + Child_NonVoc_Duration  1   0.75474 15.317 -0.62660
    ## + Child_Voc_Duration     1   0.58784 15.484 -0.39902
    ## 
    ## Step:  AIC=-2.77
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               13.831 -2.76934
    ## + Average_SignalLevel    1   0.87870 12.953 -2.14773
    ## + Child_NonVoc_Duration  1   0.87266 12.959 -2.13793
    ## + Child_Voc_Duration     1   0.20412 13.627 -1.08157
    ## + Turn_Count             1   0.14098 13.690 -0.98448
    ## Start:  AIC=-0.41
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.64488 16.077 -1.60934
    ## + Child_Voc_Duration     1   2.38492 16.337 -1.27250
    ## <none>                               18.722 -0.41104
    ## + Turn_Count             1   1.30332 17.419  0.07370
    ## + Peak_SignalLevel       1   0.34213 18.380  1.20166
    ## + Child_NonVoc_Duration  1   0.16480 18.557  1.40329
    ## + Average_SignalLevel    1   0.00962 18.713  1.57817
    ## 
    ## Step:  AIC=-1.61
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Average_SignalLevel    1   2.06218 14.015 -2.49204
    ## + Peak_SignalLevel       1   1.93386 14.143 -2.30064
    ## <none>                               16.077 -1.60934
    ## + Child_NonVoc_Duration  1   1.23289 14.845 -1.28482
    ## + Turn_Count             1   1.03892 15.038 -1.01219
    ## + Child_Voc_Duration     1   0.59363 15.484 -0.39941
    ## 
    ## Step:  AIC=-2.49
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               14.015 -2.4920
    ## + Peak_SignalLevel       1   1.05045 12.965 -2.1281
    ## + Child_NonVoc_Duration  1   0.82435 13.191 -1.7650
    ## + Turn_Count             1   0.66609 13.349 -1.5146
    ## + Child_Voc_Duration     1   0.53466 13.480 -1.3088
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.47284 16.133 -1.53680
    ## + Child_Voc_Duration     1   2.12471 16.481 -1.08846
    ## <none>                               18.606 -0.54201
    ## + Turn_Count             1   1.15404 17.452  0.11330
    ## + Peak_SignalLevel       1   0.41376 18.192  0.98572
    ## + Child_NonVoc_Duration  1   0.10162 18.504  1.34298
    ## + Average_SignalLevel    1   0.00004 18.606  1.45795
    ## 
    ## Step:  AIC=-1.54
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1    3.7948 12.338 -5.1682
    ## + Peak_SignalLevel       1    1.9875 14.146 -2.2977
    ## <none>                               16.133 -1.5368
    ## + Turn_Count             1    1.0633 15.070 -0.9686
    ## + Child_NonVoc_Duration  1    0.7957 15.337 -0.5990
    ## + Child_Voc_Duration     1    0.5647 15.568 -0.2851
    ## 
    ## Step:  AIC=-5.17
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               12.338 -5.1682
    ## + Child_Voc_Duration     1   0.90006 11.438 -4.7589
    ## + Peak_SignalLevel       1   0.43028 11.908 -3.9137
    ## + Child_NonVoc_Duration  1   0.13262 12.206 -3.3952
    ## + Turn_Count             1   0.09437 12.244 -3.3295
    ## Start:  AIC=-1.24
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.37022 15.624 -2.20935
    ## + Child_Voc_Duration     1   2.03224 15.963 -1.75992
    ## <none>                               17.995 -1.24334
    ## + Turn_Count             1   1.29228 16.702 -0.80833
    ## + Child_NonVoc_Duration  1   0.13281 17.862  0.60109
    ## + Peak_SignalLevel       1   0.08117 17.913  0.66173
    ## + Average_SignalLevel    1   0.00249 17.992  0.75375
    ## 
    ## Step:  AIC=-2.21
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   1.80439 13.820 -2.7864
    ## + Peak_SignalLevel       1   1.49028 14.134 -2.3144
    ## <none>                               15.624 -2.2094
    ## + Child_NonVoc_Duration  1   0.77752 14.847 -1.2813
    ## + Turn_Count             1   0.76709 14.857 -1.2665
    ## + Child_Voc_Duration     1   0.59083 15.034 -1.0189
    ## 
    ## Step:  AIC=-2.79
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               13.820 -2.7864
    ## + Peak_SignalLevel       1   0.84974 12.970 -2.1190
    ## + Child_Voc_Duration     1   0.55981 13.260 -1.6547
    ## + Turn_Count             1   0.52490 13.295 -1.5995
    ## + Child_NonVoc_Duration  1   0.49202 13.328 -1.5476
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1   2.72929 14.567 -3.6810
    ## + Child_Voc_Duration     1   2.39585 14.900 -3.2058
    ## <none>                               17.296 -2.0746
    ## + Turn_Count             1   1.21592 16.080 -1.6054
    ## + Peak_SignalLevel       1   1.05442 16.242 -1.3955
    ## + Child_NonVoc_Duration  1   0.10342 17.193 -0.2006
    ## + Average_SignalLevel    1   0.00458 17.292 -0.0802
    ## 
    ## Step:  AIC=-3.68
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Peak_SignalLevel       1    4.2432 10.324 -8.9115
    ## + Average_SignalLevel    1    1.5888 12.978 -4.1063
    ## + Turn_Count             1    1.5288 13.038 -4.0095
    ## <none>                               14.567 -3.6810
    ## + Child_NonVoc_Duration  1    1.1140 13.453 -3.3516
    ## + Child_Voc_Duration     1    0.4745 14.092 -2.3764
    ## 
    ## Step:  AIC=-8.91
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq     RSS      AIC
    ## + Child_NonVoc_Duration  1   2.15491  8.1689 -11.8280
    ## <none>                               10.3238  -8.9115
    ## + Average_SignalLevel    1   0.29291 10.0309  -7.5159
    ## + Turn_Count             1   0.23142 10.0924  -7.3876
    ## + Child_Voc_Duration     1   0.01972 10.3041  -6.9517
    ## 
    ## Step:  AIC=-11.83
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel + Child_NonVoc_Duration
    ## 
    ##                       Df Sum of Sq    RSS      AIC
    ## <none>                             8.1689 -11.8280
    ## + Child_Voc_Duration   1   0.40522 7.7637 -10.8964
    ## + Average_SignalLevel  1   0.03112 8.1378  -9.9082
    ## + Turn_Count           1   0.00796 8.1609  -9.8485
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1    3.6430 13.653 -5.0414
    ## + Child_Voc_Duration     1    3.2472 14.049 -4.4413
    ## + Turn_Count             1    1.9980 15.298 -2.6525
    ## <none>                               17.296 -2.0746
    ## + Child_NonVoc_Duration  1    0.3015 16.995 -0.4440
    ## + Peak_SignalLevel       1    0.2063 17.090 -0.3267
    ## + Average_SignalLevel    1    0.0873 17.209 -0.1809
    ## 
    ## Step:  AIC=-5.04
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Peak_SignalLevel       1   1.96215 11.691 -6.2995
    ## <none>                               13.653 -5.0414
    ## + Average_SignalLevel    1   1.18209 12.471 -4.9431
    ## + Turn_Count             1   0.95412 12.699 -4.5627
    ## + Child_NonVoc_Duration  1   0.88673 12.767 -4.4516
    ## + Child_Voc_Duration     1   0.44736 13.206 -3.7410
    ## 
    ## Step:  AIC=-6.3
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_NonVoc_Duration  1   1.26837 10.423 -6.7111
    ## <none>                               11.691 -6.2995
    ## + Average_SignalLevel    1   0.51602 11.175 -5.2475
    ## + Turn_Count             1   0.19475 11.496 -4.6523
    ## + Child_Voc_Duration     1   0.12285 11.568 -4.5213
    ## 
    ## Step:  AIC=-6.71
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel + Child_NonVoc_Duration
    ## 
    ##                       Df Sum of Sq     RSS     AIC
    ## <none>                             10.4228 -6.7111
    ## + Child_Voc_Duration   1   0.59049  9.8323 -5.9359
    ## + Average_SignalLevel  1   0.25599 10.1668 -5.2333
    ## + Turn_Count           1   0.02208 10.4007 -4.7556
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   1.66412 15.632 -2.19901
    ## <none>                               17.296 -2.07463
    ## + Child_Voc_Duration     1   1.36634 15.930 -1.80274
    ## + Turn_Count             1   1.01683 16.279 -1.34698
    ## + Peak_SignalLevel       1   0.17626 17.120 -0.28973
    ## + Average_SignalLevel    1   0.13917 17.157 -0.24429
    ## + Child_NonVoc_Duration  1   0.00017 17.296 -0.07483
    ## 
    ## Step:  AIC=-2.2
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.10590 13.526 -3.2377
    ## + Peak_SignalLevel       1   1.56478 14.067 -2.4139
    ## <none>                               15.632 -2.1990
    ## + Child_NonVoc_Duration  1   1.09360 14.539 -1.7220
    ## + Child_Voc_Duration     1   0.62176 15.010 -1.0513
    ## + Turn_Count             1   0.56910 15.063 -0.9778
    ## 
    ## Step:  AIC=-3.24
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               13.526 -3.2377
    ## + Child_NonVoc_Duration  1   0.72727 12.799 -2.3982
    ## + Peak_SignalLevel       1   0.72525 12.801 -2.3949
    ## + Child_Voc_Duration     1   0.60808 12.918 -2.2036
    ## + Turn_Count             1   0.22621 13.300 -1.5918
    ## Start:  AIC=-0.54
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.60944 15.996 -1.71536
    ## + Child_Voc_Duration     1   2.27352 16.332 -1.27893
    ## <none>                               18.606 -0.54201
    ## + Turn_Count             1   1.23819 17.368  0.01180
    ## + Peak_SignalLevel       1   0.36279 18.243  1.04447
    ## + Child_NonVoc_Duration  1   0.18717 18.419  1.24567
    ## + Average_SignalLevel    1   0.00832 18.598  1.44860
    ## 
    ## Step:  AIC=-1.72
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Peak_SignalLevel       1   2.13350 13.863 -2.72146
    ## + Average_SignalLevel    1   2.04589 13.950 -2.58916
    ## <none>                               15.996 -1.71536
    ## + Turn_Count             1   1.25852 14.738 -1.43615
    ## + Child_NonVoc_Duration  1   0.74286 15.254 -0.71395
    ## + Child_Voc_Duration     1   0.51466 15.482 -0.40211
    ## 
    ## Step:  AIC=-2.72
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               13.863 -2.72146
    ## + Average_SignalLevel    1   1.13168 12.731 -2.50980
    ## + Child_NonVoc_Duration  1   1.07771 12.785 -2.42096
    ## + Turn_Count             1   0.34422 13.519 -1.24948
    ## + Child_Voc_Duration     1   0.14309 13.720 -0.93935
    ## Start:  AIC=-0.71
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.64262 15.818 -1.95126
    ## + Child_Voc_Duration     1   2.36111 16.099 -1.58080
    ## <none>                               18.460 -0.70688
    ## + Turn_Count             1   1.20076 17.260 -0.11929
    ## + Peak_SignalLevel       1   0.22347 18.237  1.03736
    ## + Child_NonVoc_Duration  1   0.13859 18.322  1.13487
    ## + Average_SignalLevel    1   0.00460 18.456  1.28789
    ## 
    ## Step:  AIC=-1.95
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Average_SignalLevel    1   1.97919 13.838 -2.75841
    ## + Peak_SignalLevel       1   1.74004 14.078 -2.39860
    ## + Turn_Count             1   1.51881 14.299 -2.07115
    ## <none>                               15.818 -1.95126
    ## + Child_NonVoc_Duration  1   0.92067 14.897 -1.21058
    ## + Child_Voc_Duration     1   0.36208 15.456 -0.43755
    ## 
    ## Step:  AIC=-2.76
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               13.838 -2.7584
    ## + Turn_Count             1   1.02606 12.812 -2.3762
    ## + Peak_SignalLevel       1   0.93940 12.899 -2.2347
    ## + Child_NonVoc_Duration  1   0.57994 13.259 -1.6575
    ## + Child_Voc_Duration     1   0.36933 13.469 -1.3265
    ## Start:  AIC=-0.44
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.95649 15.737 -2.05916
    ## + Child_Voc_Duration     1   2.54584 16.147 -1.51818
    ## <none>                               18.693 -0.44370
    ## + Turn_Count             1   1.53951 17.154 -0.24858
    ## + Peak_SignalLevel       1   0.30184 18.391  1.21444
    ## + Child_NonVoc_Duration  1   0.17352 18.520  1.36046
    ## + Average_SignalLevel    1   0.00236 18.691  1.55365
    ## 
    ## Step:  AIC=-2.06
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Peak_SignalLevel       1   1.84018 13.896 -2.67066
    ## + Average_SignalLevel    1   1.80173 13.935 -2.61264
    ## <none>                               15.737 -2.05916
    ## + Child_NonVoc_Duration  1   0.95678 14.780 -1.37641
    ## + Turn_Count             1   0.88731 14.849 -1.27794
    ## + Child_Voc_Duration     1   0.64741 15.089 -0.94138
    ## 
    ## Step:  AIC=-2.67
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_NonVoc_Duration  1   1.30486 12.592 -2.74135
    ## <none>                               13.896 -2.67066
    ## + Average_SignalLevel    1   1.03556 12.861 -2.29696
    ## + Child_Voc_Duration     1   0.23387 13.663 -1.02710
    ## + Turn_Count             1   0.18831 13.708 -0.95719
    ## 
    ## Step:  AIC=-2.74
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel + Child_NonVoc_Duration
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## <none>                             12.592 -2.7414
    ## + Child_Voc_Duration   1   0.89597 11.696 -2.2915
    ## + Average_SignalLevel  1   0.64272 11.949 -1.8416
    ## + Turn_Count           1   0.01411 12.578 -0.7649
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## <none>                               17.296 -2.07463
    ## + Child_Voc_Count        1   1.32283 15.973 -1.74547
    ## + Child_Voc_Duration     1   1.00557 16.291 -1.33245
    ## + Peak_SignalLevel       1   0.75649 16.540 -1.01380
    ## + Turn_Count             1   0.54304 16.753 -0.74453
    ## + Average_SignalLevel    1   0.10346 17.193 -0.20062
    ## + Child_NonVoc_Duration  1   0.00370 17.293 -0.07912
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Child_Voc_Count        1   2.43115 14.865 -3.2556
    ## + Child_Voc_Duration     1   2.22648 15.070 -2.9684
    ## <none>                               17.296 -2.0746
    ## + Turn_Count             1   1.07976 16.216 -1.4283
    ## + Child_NonVoc_Duration  1   0.38576 16.910 -0.5483
    ## + Peak_SignalLevel       1   0.16875 17.128 -0.2805
    ## + Average_SignalLevel    1   0.02126 17.275 -0.1005
    ## 
    ## Step:  AIC=-3.26
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Average_SignalLevel    1   2.11292 12.752 -4.4751
    ## + Peak_SignalLevel       1   1.52698 13.338 -3.5318
    ## <none>                               14.865 -3.2556
    ## + Turn_Count             1   1.33575 13.529 -3.2328
    ## + Child_NonVoc_Duration  1   0.37214 14.493 -1.7880
    ## + Child_Voc_Duration     1   0.18999 14.675 -1.5257
    ## 
    ## Step:  AIC=-4.48
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               12.752 -4.4751
    ## + Turn_Count             1   0.91040 11.842 -4.0306
    ## + Peak_SignalLevel       1   0.73212 12.020 -3.7168
    ## + Child_Voc_Duration     1   0.17279 12.579 -2.7616
    ## + Child_NonVoc_Duration  1   0.15454 12.598 -2.7312
    ## Start:  AIC=-0.71
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.31941 16.141 -1.52648
    ## + Child_Voc_Duration     1   1.98398 16.476 -1.09455
    ## <none>                               18.460 -0.70688
    ## + Turn_Count             1   1.01836 17.442  0.10148
    ## + Peak_SignalLevel       1   0.55424 17.906  0.65298
    ## + Child_NonVoc_Duration  1   0.16400 18.296  1.10572
    ## + Average_SignalLevel    1   0.06244 18.398  1.22197
    ## 
    ## Step:  AIC=-1.53
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Average_SignalLevel    1   2.10506 14.036 -2.46107
    ## + Peak_SignalLevel       1   2.09080 14.050 -2.43973
    ## <none>                               16.141 -1.52648
    ## + Turn_Count             1   1.18894 14.952 -1.13328
    ## + Child_NonVoc_Duration  1   0.84955 15.291 -0.66193
    ## + Child_Voc_Duration     1   0.53951 15.601 -0.24040
    ## 
    ## Step:  AIC=-2.46
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               14.036 -2.4611
    ## + Peak_SignalLevel       1   1.22108 12.815 -2.3724
    ## + Turn_Count             1   0.86422 13.172 -1.7956
    ## + Child_Voc_Duration     1   0.53431 13.502 -1.2761
    ## + Child_NonVoc_Duration  1   0.43619 13.600 -1.1240
    ## Start:  AIC=-2.07
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   1.75100 15.545 -2.31605
    ## + Child_Voc_Duration     1   1.59519 15.701 -2.10662
    ## <none>                               17.296 -2.07463
    ## + Turn_Count             1   0.64760 16.649 -0.87600
    ## + Peak_SignalLevel       1   0.21637 17.080 -0.33899
    ## + Child_NonVoc_Duration  1   0.03361 17.263 -0.11548
    ## + Average_SignalLevel    1   0.02541 17.271 -0.10551
    ## 
    ## Step:  AIC=-2.32
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Average_SignalLevel    1   1.74149 13.804 -2.81114
    ## + Peak_SignalLevel       1   1.59019 13.955 -2.58222
    ## <none>                               15.545 -2.31605
    ## + Turn_Count             1   1.25703 14.288 -2.08676
    ## + Child_NonVoc_Duration  1   0.79609 14.749 -1.42000
    ## + Child_Voc_Duration     1   0.18135 15.364 -0.56247
    ## 
    ## Step:  AIC=-2.81
    ## get(smq[k]) ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               13.804 -2.8111
    ## + Peak_SignalLevel       1   0.93007 12.874 -2.2760
    ## + Turn_Count             1   0.83022 12.974 -2.1138
    ## + Child_NonVoc_Duration  1   0.50718 13.297 -1.5973
    ## + Child_Voc_Duration     1   0.28234 13.521 -1.2451
    ## Start:  AIC=-0.71
    ## get(smq[k]) ~ 1
    ## 
    ##                         Df Sum of Sq    RSS      AIC
    ## + Child_Voc_Count        1   2.66834 15.792 -1.98543
    ## + Child_Voc_Duration     1   2.30487 16.155 -1.50757
    ## <none>                               18.460 -0.70688
    ## + Turn_Count             1   1.31738 17.143 -0.26166
    ## + Peak_SignalLevel       1   0.40818 18.052  0.82357
    ## + Child_NonVoc_Duration  1   0.23061 18.230  1.02913
    ## + Average_SignalLevel    1   0.00762 18.453  1.28445
    ## 
    ## Step:  AIC=-1.99
    ## get(smq[k]) ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## + Peak_SignalLevel       1   2.32985 13.462 -3.3375
    ## + Average_SignalLevel    1   2.07460 13.717 -2.9430
    ## <none>                               15.792 -1.9854
    ## + Turn_Count             1   1.08297 14.709 -1.4773
    ## + Child_NonVoc_Duration  1   0.66346 15.129 -0.8868
    ## + Child_Voc_Duration     1   0.60424 15.188 -0.8047
    ## 
    ## Step:  AIC=-3.34
    ## get(smq[k]) ~ Child_Voc_Count + Peak_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS     AIC
    ## <none>                               13.462 -3.3375
    ## + Average_SignalLevel    1   1.11463 12.348 -3.1525
    ## + Child_NonVoc_Duration  1   0.95503 12.507 -2.8828
    ## + Turn_Count             1   0.20408 13.258 -1.6583
    ## + Child_Voc_Duration     1   0.19677 13.265 -1.6467

    print(fsmq_predictors)

    ## <hash> containing 4 key-value pair(s).
    ##   smq_as : <hash> containing 5 key-value pair(s).
    ##       (Intercept) : 22
    ##       Average_SignalLevel : 22
    ##       Child_Voc_Count : 4
    ##       Child_Voc_Duration : 4
    ##       Turn_Count : 18
    ##   smq_hf : <hash> containing 4 key-value pair(s).
    ##       (Intercept) : 22
    ##       Child_NonVoc_Duration : 2
    ##       Peak_SignalLevel : 2
    ##       Turn_Count : 20
    ##   smq_id : <hash> containing 6 key-value pair(s).
    ##       (Intercept) : 22
    ##       Average_SignalLevel : 13
    ##       Child_NonVoc_Duration : 4
    ##       Child_Voc_Count : 21
    ##       Child_Voc_Duration : 1
    ##       Peak_SignalLevel : 8
    ##   smq_ss : <hash> containing 3 key-value pair(s).
    ##       (Intercept) : 22
    ##       Average_SignalLevel : 1
    ##       Turn_Count : 22

    remove(i)

    par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(2,2), pty="s")
    plot(matrix(fsmq_predicted, ncol=4)[,1],matrix(fsmq_actual, ncol=4)[,1], type='p',
         xlab="", ylab=paste("R\u00b2=", 1-(sum((matrix(fsmq_actual, ncol=4)[,1]-matrix(fsmq_predicted, ncol=4)[,1])^2)/sum((matrix(fsmq_actual, ncol=4)[,1]-mean(matrix(fsmq_actual, ncol=4)[,1]))^2)), "\nP's r=", cor(matrix(fsmq_predicted, ncol=4)[,1], matrix(fsmq_actual, ncol=4)[,1])), xlim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,1], matrix(fsmq_actual, ncol=4)[,1]))), ylim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,1], matrix(fsmq_actual, ncol=4)[,1]))), asp=1, main="at school")
    abline(lm(as.numeric(matrix(fsmq_actual, ncol=4)[,1])~as.numeric(matrix(fsmq_predicted, ncol=4)[,1])))
    plot(matrix(fsmq_predicted, ncol=4)[,2],matrix(fsmq_actual, ncol=4)[,2], type='p',
         xlab="", xlim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,2], matrix(fsmq_actual, ncol=4)[,2]))), ylim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,2], matrix(fsmq_actual, ncol=4)[,2]))), asp=1, main="home / family", ylab=paste("R\u00b2=", 1-(sum((matrix(fsmq_actual, ncol=4)[,2]-matrix(fsmq_predicted, ncol=4)[,2])^2)/sum((matrix(fsmq_actual, ncol=4)[,2]-mean(matrix(fsmq_actual, ncol=4)[,2]))^2)), "\nP's r=", cor(matrix(fsmq_predicted, ncol=4)[,2], matrix(fsmq_actual, ncol=4)[,2])))
    abline(lm(as.numeric(matrix(fsmq_actual, ncol=4)[,2])~as.numeric(matrix(fsmq_predicted, ncol=4)[,2])))
    plot(matrix(fsmq_predicted, ncol=4)[,3],matrix(fsmq_actual, ncol=4)[,3], type='p',
         xlab="", xlim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,3], matrix(fsmq_actual, ncol=4)[,3]))), ylim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,3], matrix(fsmq_actual, ncol=4)[,3]))), asp=1, main="in social situations\n(outside of school)", ylab=paste("R\u00b2=", 1-(sum((matrix(fsmq_actual, ncol=4)[,3]-matrix(fsmq_predicted, ncol=4)[,3])^2)/sum((matrix(fsmq_actual, ncol=4)[,3]-mean(matrix(fsmq_actual, ncol=4)[,3]))^2)), "\nP's r=", cor(matrix(fsmq_predicted, ncol=4)[,3], matrix(fsmq_actual, ncol=4)[,3])))
    abline(lm(as.numeric(matrix(fsmq_actual, ncol=4)[,3])~as.numeric(matrix(fsmq_predicted, ncol=4)[,3])))
    plot(matrix(fsmq_predicted, ncol=4)[,4],matrix(fsmq_actual, ncol=4)[,4], type='p',
         xlab="", xlim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,4], matrix(fsmq_actual, ncol=4)[,4]))), ylim=c(0,max(c(matrix(fsmq_predicted, ncol=4)[,4], matrix(fsmq_actual, ncol=4)[,4]))), asp=1, main="interference / distress", ylab=paste("R\u00b2=", 1-(sum((matrix(fsmq_actual, ncol=4)[,4]-matrix(fsmq_predicted, ncol=4)[,4])^2)/sum((matrix(fsmq_actual, ncol=4)[,4]-mean(matrix(fsmq_actual, ncol=4)[,4]))^2)), "\nP's r=", cor(matrix(fsmq_predicted, ncol=4)[,4], matrix(fsmq_actual, ncol=4)[,4])))
    abline(lm(as.numeric(matrix(fsmq_actual, ncol=4)[,4])~as.numeric(matrix(fsmq_predicted, ncol=4)[,4])))
    mtext(text="Predicted SMQ symptom severity",side=1,line=0,outer=TRUE)
    mtext(text="Reported SMQ symptom severity",side=2,line=0,outer=TRUE)

![](multivariate_linear_regressions_files/figure-markdown_strict/plot%20for%20forward%20regression%20predicting%20SMQ-1.png)
