get & prep data

    data <- read.csv('../data/CPP_data_all.csv', header=TRUE)
    smq <- read.csv('../data/SMQ.csv', header=TRUE)
    smq$smq_id <- (18 - smq$smq_id) / 6
    library(plyr)
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
    data$SM_dx <- factor(data$SM_dx)

subset train/test

    set.seed(2)
    train_ind <- sample(seq_len(nrow(data)), size=19)

    train <- data[train_ind, ]
    test <- data[-train_ind, ]

logistical regression predicting SM dx

    overfit.model <- glm(SM_dx ~ Child_Voc_Count+Turn_Count+Child_Voc_Duration+Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel, family=binomial(link='logit'), data=data, na.action=na.pass)

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    summary(overfit.model)

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = data, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15227  -0.00001   0.00958   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68975  130.87658   0.212    0.832
    ## Child_Voc_Count        -0.34036    0.27246  -1.249    0.212
    ## Turn_Count              0.08218    0.10353   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25499   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44383    2.42805   1.418    0.156
    ## Peak_SignalLevel       -2.86856    2.63568  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 33.271  on 23  degrees of freedom
    ## Residual deviance:  6.929  on 17  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9

    anova(overfit.model, test="Chisq")

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                     23     33.271            
    ## Child_Voc_Count        1   9.0331        22     24.238 0.002651 **
    ## Turn_Count             1   0.0007        21     24.237 0.978189   
    ## Child_Voc_Duration     1   7.5150        20     16.722 0.006119 **
    ## Child_NonVoc_Duration  1   1.8870        19     14.835 0.169544   
    ## Average_SignalLevel    1   5.6737        18      9.162 0.017221 * 
    ## Peak_SignalLevel       1   2.2326        17      6.929 0.135127   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

ROC for logistical regression

    library('ROCR')

    ## Loading required package: gplots

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

    overfit.p <- predict(overfit.model, newdata=data, type="response")
    overfit.pr <- prediction(overfit.p, data$SM_dx)
    overfit.prf <- performance(overfit.pr, measure="tpr", x.measure="fpr")
    plot(overfit.prf)

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/ROC%20for%20logistical%20regression,%20overfitting-1.png)

AUC for ROC

    overfit.auc <- performance(overfit.pr, measure="auc")
    overfit.auc <- overfit.auc@y.values[[1]]
    overfit.auc

    ## [1] 0.9861111

logistical regression predicting SM dx

    model <- glm(SM_dx ~ Child_Voc_Count+Turn_Count+Child_Voc_Duration+Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel, family=binomial(link='logit'), data=train, na.action=na.pass)

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    summary(model)

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -7.069e-05  -2.100e-08   2.100e-08   2.100e-08   7.666e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)              9365.13 3498867.55   0.003    0.998
    ## Child_Voc_Count           -31.77   11005.62  -0.003    0.998
    ## Turn_Count                 13.67    4763.53   0.003    0.998
    ## Child_Voc_Duration         30.76   10676.82   0.003    0.998
    ## Child_NonVoc_Duration      23.28    8568.59   0.003    0.998
    ## Average_SignalLevel        31.11   12815.23   0.002    0.998
    ## Peak_SignalLevel         -132.80   48024.30  -0.003    0.998
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2.6287e+01  on 18  degrees of freedom
    ## Residual deviance: 1.6644e-08  on 12  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    anova(model, test="Chisq")

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                     18    26.2869            
    ## Child_Voc_Count        1   6.5216        17    19.7653 0.010657 * 
    ## Turn_Count             1   0.0379        16    19.7274 0.845612   
    ## Child_Voc_Duration     1   7.1420        15    12.5854 0.007530 **
    ## Child_NonVoc_Duration  1   1.3874        14    11.1980 0.238847   
    ## Average_SignalLevel    1   4.0002        13     7.1978 0.045494 * 
    ## Peak_SignalLevel       1   7.1978        12     0.0000 0.007299 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    p <- predict(model, newdata=test, type="response")
    pr <- prediction(p, test$SM_dx)
    prf <- performance(pr, measure="tpr", x.measure="fpr")
    plot(prf)

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/ROC%20curve-1.png)

    auc <- performance(pr, measure="auc")
    auc <- auc@y.values[[1]]
    auc

    ## [1] 0.6666667

forward logistical regression

    library("MASS")
    fwd.overfit.model <- stepAIC(glm(SM_dx ~ 1, family=binomial(link='logit'), data=data, na.action=na.pass), direction='forward',
                         scope=~Child_Voc_Count+Turn_Count+Child_Voc_Duration+Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel)

    ## Start:  AIC=35.27
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   24.238 28.238
    ## + Turn_Count             1   25.232 29.232
    ## + Child_Voc_Duration     1   26.216 30.216
    ## + Child_NonVoc_Duration  1   28.549 32.549
    ## <none>                       33.271 35.271
    ## + Peak_SignalLevel       1   32.795 36.795
    ## + Average_SignalLevel    1   32.904 36.904
    ## 
    ## Step:  AIC=28.24
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.887 23.887
    ## + Average_SignalLevel    1   18.588 24.588
    ## <none>                       24.238 28.238
    ## + Peak_SignalLevel       1   23.779 29.779
    ## + Child_NonVoc_Duration  1   24.172 30.172
    ## + Turn_Count             1   24.237 30.237
    ## 
    ## Step:  AIC=23.89
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2246 17.225
    ## + Child_NonVoc_Duration  1  15.7369 23.737
    ## <none>                      17.8870 23.887
    ## + Turn_Count             1  16.7222 24.722
    ## + Peak_SignalLevel       1  17.8614 25.861
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2246 17.225
    ## + Peak_SignalLevel       1   7.8506 17.851
    ## + Child_NonVoc_Duration  1   9.1616 19.162
    ## + Turn_Count             1   9.2213 19.221

    fwd.overfit.model$anova

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## SM_dx ~ 1
    ## 
    ## Final Model:
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ## 
    ##                    Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                                          23  33.271065 35.27106
    ## 2     + Child_Voc_Count  1 9.033119        22  24.237946 28.23795
    ## 3  + Child_Voc_Duration  1 6.350912        21  17.887033 23.88703
    ## 4 + Average_SignalLevel  1 8.662386        20   9.224648 17.22465

ROC, AUC

    fwd.overfit.p <- predict(fwd.overfit.model, newdata=data, type="response")
    fwd.overfit.pr <- prediction(fwd.overfit.p, data$SM_dx)
    fwd.overfit.prf <- performance(fwd.overfit.pr, measure="tpr", x.measure="fpr")
    plot(fwd.overfit.prf)

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/ROC,%20AUC%20for%20forward%20logistical%20regression,%20overfitting-1.png)

    fwd.overfit.auc <- performance(fwd.overfit.pr, measure="auc")
    fwd.overfit.auc <- fwd.overfit.auc@y.values[[1]]
    fwd.overfit.auc

    ## [1] 0.9861111

forward logistical regression without overfitting

    fwd.model <- stepAIC(glm(SM_dx ~ 1, family=binomial(link='logit'), data=train, na.action=na.pass), direction='forward', scope=~Child_Voc_Count+Turn_Count+Child_Voc_Duration+Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel)

    ## Start:  AIC=28.29
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   19.765 23.765
    ## + Turn_Count             1   20.959 24.959
    ## + Child_Voc_Duration     1   21.555 25.555
    ## + Child_NonVoc_Duration  1   22.776 26.776
    ## <none>                       26.287 28.287
    ## + Average_SignalLevel    1   25.771 29.771
    ## + Peak_SignalLevel       1   26.278 30.278
    ## 
    ## Step:  AIC=23.77
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   14.194 20.194
    ## + Average_SignalLevel    1   16.105 22.105
    ## <none>                       19.765 23.765
    ## + Peak_SignalLevel       1   19.569 25.569
    ## + Child_NonVoc_Duration  1   19.593 25.593
    ## + Turn_Count             1   19.727 25.727
    ## 
    ## Step:  AIC=20.19
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   7.6476 15.648
    ## <none>                      14.1936 20.194
    ## + Turn_Count             1  12.5854 20.585
    ## + Child_NonVoc_Duration  1  13.2794 21.279
    ## + Peak_SignalLevel       1  14.1165 22.116
    ## 
    ## Step:  AIC=15.65
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       7.6476 15.648
    ## + Peak_SignalLevel       1   6.9896 16.990
    ## + Turn_Count             1   7.3164 17.316
    ## + Child_NonVoc_Duration  1   7.4197 17.420

    fwd.model$anova

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## SM_dx ~ 1
    ## 
    ## Final Model:
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ## 
    ##                    Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                                          18  26.286937 28.28694
    ## 2     + Child_Voc_Count  1 6.521642        17  19.765295 23.76529
    ## 3  + Child_Voc_Duration  1 5.571680        16  14.193615 20.19362
    ## 4 + Average_SignalLevel  1 6.545970        15   7.647645 15.64765

ROC, AUC

    fwd.p <- predict(fwd.model, newdata=test, type="response")
    fwd.pr <- prediction(fwd.p, test$SM_dx)
    fwd.prf <- performance(fwd.pr, measure="tpr", x.measure="fpr")
    plot(fwd.prf)

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/ROC,%20AUC%20for%20forward%20logistic%20regression%20for%20sm%20dx%20without%20overfitting-1.png)

    fwd.auc <- performance(fwd.pr, measure="auc")
    fwd.auc <- fwd.auc@y.values[[1]]
    fwd.auc

    ## [1] 1

forward regression for SMQ

    fwd.overfit.smq.model <- step(lm(smq_as+smq_hf+smq_ss+smq_id ~ 1, data=data[complete.cases(data),], na.action=na.omit), direction='forward', scope=~Child_Voc_Count+Turn_Count+Child_Voc_Duration+Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel)

    ## Start:  AIC=49.98
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    65.406 129.43 42.986
    ## + Turn_Count             1    62.929 131.91 43.403
    ## + Child_Voc_Duration     1    55.569 139.27 44.598
    ## + Child_NonVoc_Duration  1    28.902 165.94 48.452
    ## <none>                               194.84 49.985
    ## + Average_SignalLevel    1     7.606 187.23 51.109
    ## + Peak_SignalLevel       1     2.689 192.15 51.679
    ## 
    ## Step:  AIC=42.99
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   18.6022 110.83 41.573
    ## <none>                               129.43 42.986
    ## + Average_SignalLevel    1    9.1278 120.30 43.378
    ## + Peak_SignalLevel       1    4.3794 125.05 44.229
    ## + Turn_Count             1    1.1685 128.26 44.787
    ## + Child_NonVoc_Duration  1    0.0000 129.43 44.986
    ## 
    ## Step:  AIC=41.57
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               110.83 41.573
    ## + Average_SignalLevel    1    8.9200 101.91 41.727
    ## + Child_NonVoc_Duration  1    2.6187 108.21 43.047
    ## + Peak_SignalLevel       1    1.2385 109.59 43.326
    ## + Turn_Count             1    0.3488 110.48 43.504

    fwd.overfit.smq.model$anova

    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        21   194.8376 49.98472
    ## 2    + Child_Voc_Count -1 65.40560        20   129.4320 42.98649
    ## 3 + Child_Voc_Duration -1 18.60218        19   110.8298 41.57297

    summary(fwd.overfit.smq.model)

    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = data[complete.cases(data), ], 
    ##     na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5451 -1.8877  0.1258  1.4153  5.4717 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         5.05397    0.94859   5.328 3.84e-05 ***
    ## Child_Voc_Count     0.04496    0.02036   2.208   0.0397 *  
    ## Child_Voc_Duration -0.04324    0.02422  -1.786   0.0901 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.415 on 19 degrees of freedom
    ## Multiple R-squared:  0.4312, Adjusted R-squared:  0.3713 
    ## F-statistic: 7.201 on 2 and 19 DF,  p-value: 0.004703

partial regression plot

    library("car")
    av.plots(fwd.overfit.smq.model)

    ## Warning: 'av.plots' is deprecated.
    ## Use 'avPlots' instead.
    ## See help("Deprecated") and help("car-deprecated").

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    plot(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Count, data=data[complete.cases(data),])
    abline(lm(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Count, data=data[complete.cases(data),]))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    plot(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Duration, data=data[complete.cases(data),])
    abline(lm(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Duration, data=data[complete.cases(data),]))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    plot(smq_as+smq_hf+smq_ss+smq_id ~ Average_SignalLevel, data=data[complete.cases(data),])
    abline(lm(smq_as+smq_hf+smq_ss+smq_id ~ Average_SignalLevel, data=data[complete.cases(data),]))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-4-1.png)
