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

logistical regression predicting SM dx

    lreg_fp <- vector()
    lreg_tp <- vector()
    lreg_predictors <- hash()
    for(i in 1:nrow(data)){
      train <- data[-i,]
      test <- data[i,]
      lreg_model <- glm(SM_dx~Child_Voc_Count+Turn_Count+Child_Voc_Duration+Child_NonVoc_Duration+
                    Average_SignalLevel+Peak_SignalLevel, family=binomial(link='logit'), data=train,
                    na.action=na.pass)
      lreg_p <- predict(lreg_model, test, "response")
      lreg_pr <- prediction(lreg_p, test$SM_dx)
      lreg_fp <- c(lreg_fp, lreg_pr@fp[[1]][[2]])
      lreg_tp <- c(lreg_tp, lreg_pr@tp[[1]][[2]])
      for(j in 1:length(names(lreg_model$coefficients))){
        if(has.key(names(lreg_model$coefficients)[[j]], lreg_predictors)){
          .set(lreg_predictors, names(lreg_model$coefficients)[[j]],
               as.numeric(lreg_predictors[[names(lreg_model$coefficients)[[j]]]]) + 1)
        }else{
          .set(lreg_predictors, names(lreg_model$coefficients)[[j]], 1)
        }
      }
      remove(j)
      print(summary(lreg_model))
      print(anova(lreg_model, test="Chisq"))
      remove(train)
      remove(test)
    }

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -7.890e-05  -2.100e-08   2.100e-08   2.100e-08   8.336e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)              8195.45 2997660.99   0.003    0.998
    ## Child_Voc_Count           -34.14   11034.87  -0.003    0.998
    ## Turn_Count                 13.86    4501.27   0.003    0.998
    ## Child_Voc_Duration         31.92   10366.89   0.003    0.998
    ## Child_NonVoc_Duration      37.05   12145.07   0.003    0.998
    ## Average_SignalLevel        53.30   18874.68   0.003    0.998
    ## Peak_SignalLevel         -135.89   46026.58  -0.003    0.998
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.8780e-08  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.9615        21     23.880 0.004778 **
    ## Turn_Count             1   0.0478        20     23.832 0.826849   
    ## Child_Voc_Duration     1   7.6179        19     16.214 0.005779 **
    ## Child_NonVoc_Duration  1   3.8530        18     12.361 0.049656 * 
    ## Average_SignalLevel    1   3.8113        17      8.550 0.050909 . 
    ## Peak_SignalLevel       1   8.5497        16      0.000 0.003456 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.30676  -0.16143  -0.00003   0.00719   1.86367  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            19.39054  129.68364   0.150    0.881
    ## Child_Voc_Count        -0.31906    0.27394  -1.165    0.244
    ## Turn_Count              0.07499    0.10361   0.724    0.469
    ## Child_Voc_Duration      0.27710    0.25704   1.078    0.281
    ## Child_NonVoc_Duration   0.01605    0.21887   0.073    0.942
    ## Average_SignalLevel     3.36601    2.44556   1.376    0.169
    ## Peak_SignalLevel       -2.71801    2.63411  -1.032    0.302
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.8557  on 16  degrees of freedom
    ## AIC: 20.856
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.8660        21     23.975 0.005037 **
    ## Turn_Count             1   0.0065        20     23.969 0.935842   
    ## Child_Voc_Duration     1   7.3566        19     16.612 0.006682 **
    ## Child_NonVoc_Duration  1   1.8555        18     14.757 0.173142   
    ## Average_SignalLevel    1   5.7921        17      8.965 0.016099 * 
    ## Peak_SignalLevel       1   2.1090        16      6.856 0.146439   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31441  -0.15752  -0.00002   0.00509   1.88545  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68992  130.81788   0.212    0.832
    ## Child_Voc_Count        -0.34021    0.27259  -1.248    0.212
    ## Turn_Count              0.08214    0.10354   0.793    0.428
    ## Child_Voc_Duration      0.29749    0.25509   1.166    0.244
    ## Child_NonVoc_Duration   0.01196    0.21905   0.055    0.956
    ## Average_SignalLevel     3.44206    2.43116   1.416    0.157
    ## Peak_SignalLevel       -2.86725    2.63669  -1.087    0.277
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9287  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.4394        21     23.402 0.003672 **
    ## Turn_Count             1   0.0135        20     23.388 0.907573   
    ## Child_Voc_Duration     1   6.9313        19     16.457 0.008470 **
    ## Child_NonVoc_Duration  1   1.9407        18     14.516 0.163589   
    ## Average_SignalLevel    1   5.3563        17      9.160 0.020648 * 
    ## Peak_SignalLevel       1   2.2315        16      6.929 0.135220   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15742  -0.00002   0.01215   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68809  130.88136   0.212    0.832
    ## Child_Voc_Count        -0.34035    0.27248  -1.249    0.212
    ## Turn_Count              0.08217    0.10354   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25501   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44379    2.42820   1.418    0.156
    ## Peak_SignalLevel       -2.86852    2.63589  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance:  6.929  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.6934        21     23.148 0.003194 **
    ## Turn_Count             1   0.0691        20     23.079 0.792656   
    ## Child_Voc_Duration     1   6.5275        19     16.551 0.010622 * 
    ## Child_NonVoc_Duration  1   1.9870        18     14.564 0.158653   
    ## Average_SignalLevel    1   5.4406        17      9.124 0.019674 * 
    ## Peak_SignalLevel       1   2.1947        16      6.929 0.138487   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31460  -0.15744  -0.00002   0.01021   1.88556  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68447  130.86904   0.212    0.832
    ## Child_Voc_Count        -0.34032    0.27251  -1.249    0.212
    ## Turn_Count              0.08217    0.10354   0.794    0.427
    ## Child_Voc_Duration      0.29758    0.25503   1.167    0.243
    ## Child_NonVoc_Duration   0.01191    0.21903   0.054    0.957
    ## Average_SignalLevel     3.44345    2.42897   1.418    0.156
    ## Peak_SignalLevel       -2.86822    2.63631  -1.088    0.277
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9289  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.1077        21     23.734 0.004408 **
    ## Turn_Count             1   0.0005        20     23.733 0.982634   
    ## Child_Voc_Duration     1   7.2590        19     16.474 0.007055 **
    ## Child_NonVoc_Duration  1   1.7345        18     14.740 0.187838   
    ## Average_SignalLevel    1   5.5807        17      9.159 0.018160 * 
    ## Peak_SignalLevel       1   2.2300        16      6.929 0.135351   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.28276  -0.16771  -0.00009   0.01918   1.86820  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)           -10.06405  133.93581  -0.075    0.940
    ## Child_Voc_Count        -0.26505    0.24984  -1.061    0.289
    ## Turn_Count              0.05390    0.10006   0.539    0.590
    ## Child_Voc_Duration      0.22553    0.23490   0.960    0.337
    ## Child_NonVoc_Duration   0.01928    0.22185   0.087    0.931
    ## Average_SignalLevel     3.16954    2.40980   1.315    0.188
    ## Peak_SignalLevel       -2.23556    2.55992  -0.873    0.383
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.5914  on 16  degrees of freedom
    ## AIC: 20.591
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.1630        21     23.678 0.004275 **
    ## Turn_Count             1   0.0050        20     23.673 0.943847   
    ## Child_Voc_Duration     1   7.4037        19     16.270 0.006509 **
    ## Child_NonVoc_Duration  1   1.8347        18     14.435 0.175572   
    ## Average_SignalLevel    1   6.4313        17      8.004 0.011213 * 
    ## Peak_SignalLevel       1   1.4123        16      6.591 0.234681   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.30521  -0.09120   0.00000   0.01386   1.87747  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            28.63027  129.34246   0.221    0.825
    ## Child_Voc_Count        -0.33254    0.27331  -1.217    0.224
    ## Turn_Count              0.08050    0.10259   0.785    0.433
    ## Child_Voc_Duration      0.29159    0.25428   1.147    0.252
    ## Child_NonVoc_Duration   0.00809    0.21170   0.038    0.970
    ## Average_SignalLevel     3.31072    2.53719   1.305    0.192
    ## Peak_SignalLevel       -2.77895    2.64980  -1.049    0.294
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.8971  on 16  degrees of freedom
    ## AIC: 20.897
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1  10.9570        21     20.884 0.0009325 ***
    ## Turn_Count             1   0.0398        20     20.845 0.8419688    
    ## Child_Voc_Duration     1   6.5447        19     14.300 0.0105199 *  
    ## Child_NonVoc_Duration  1   0.7107        18     13.589 0.3992118    
    ## Average_SignalLevel    1   4.5192        17      9.070 0.0335168 *  
    ## Peak_SignalLevel       1   2.1729        16      6.897 0.1404603    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31485  -0.15791   0.00000   0.01218   1.88500  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.42370  131.58608   0.208    0.835
    ## Child_Voc_Count        -0.33973    0.27416  -1.239    0.215
    ## Turn_Count              0.08195    0.10410   0.787    0.431
    ## Child_Voc_Duration      0.29703    0.25656   1.158    0.247
    ## Child_NonVoc_Duration   0.01174    0.21902   0.054    0.957
    ## Average_SignalLevel     3.44059    2.43195   1.415    0.157
    ## Peak_SignalLevel       -2.86308    2.64914  -1.081    0.280
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9283  on 16  degrees of freedom
    ## AIC: 20.928
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.2904        21     23.551 0.003986 **
    ## Turn_Count             1   0.0007        20     23.550 0.978966   
    ## Child_Voc_Duration     1   6.8599        19     16.690 0.008815 **
    ## Child_NonVoc_Duration  1   1.8558        18     14.835 0.173107   
    ## Average_SignalLevel    1   5.8779        17      8.957 0.015332 * 
    ## Peak_SignalLevel       1   2.0283        16      6.928 0.154397   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15742  -0.00002   0.01215   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68895  130.87531   0.212    0.832
    ## Child_Voc_Count        -0.34035    0.27247  -1.249    0.212
    ## Turn_Count              0.08217    0.10353   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25500   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44376    2.42827   1.418    0.156
    ## Peak_SignalLevel       -2.86850    2.63583  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance:  6.929  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.9419        21     23.899 0.004830 **
    ## Turn_Count             1   0.0015        20     23.898 0.969471   
    ## Child_Voc_Duration     1   7.4011        19     16.497 0.006518 **
    ## Child_NonVoc_Duration  1   1.8927        18     14.604 0.168900   
    ## Average_SignalLevel    1   5.4436        17      9.161 0.019640 * 
    ## Peak_SignalLevel       1   2.2316        16      6.929 0.135215   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15741  -0.00002   0.01215   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68935  130.87718   0.212    0.832
    ## Child_Voc_Count        -0.34036    0.27246  -1.249    0.212
    ## Turn_Count              0.08218    0.10353   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25500   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44381    2.42816   1.418    0.156
    ## Peak_SignalLevel       -2.86854    2.63578  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance:  6.929  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.9388        21     22.902 0.002792 **
    ## Turn_Count             1   0.0057        20     22.897 0.940071   
    ## Child_Voc_Duration     1   9.2856        19     13.611 0.002310 **
    ## Child_NonVoc_Duration  1   0.3580        18     13.253 0.549613   
    ## Average_SignalLevel    1   4.1063        17      9.147 0.042724 * 
    ## Peak_SignalLevel       1   2.2180        16      6.929 0.136412   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15742  -0.00002   0.01215   1.88558  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68289  130.90936   0.211    0.833
    ## Child_Voc_Count        -0.34034    0.27256  -1.249    0.212
    ## Turn_Count              0.08217    0.10357   0.793    0.428
    ## Child_Voc_Duration      0.29760    0.25510   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21908   0.054    0.957
    ## Average_SignalLevel     3.44375    2.42823   1.418    0.156
    ## Peak_SignalLevel       -2.86843    2.63626  -1.088    0.277
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9289  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1  11.4856        21     20.356 0.0007014 ***
    ## Turn_Count             1   0.0963        20     20.259 0.7563371    
    ## Child_Voc_Duration     1   3.5399        19     16.720 0.0599091 .  
    ## Child_NonVoc_Duration  1   1.8847        18     14.835 0.1698026    
    ## Average_SignalLevel    1   5.6849        17      9.150 0.0171115 *  
    ## Peak_SignalLevel       1   2.2210        16      6.929 0.1361477    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15741  -0.00002   0.01215   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68975  130.87729   0.212    0.832
    ## Child_Voc_Count        -0.34036    0.27246  -1.249    0.212
    ## Turn_Count              0.08218    0.10353   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25499   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44383    2.42807   1.418    0.156
    ## Peak_SignalLevel       -2.86856    2.63570  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance:  6.929  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.9138        21     23.927 0.004906 **
    ## Turn_Count             1   0.0128        20     23.915 0.909796   
    ## Child_Voc_Duration     1   7.3453        19     16.569 0.006724 **
    ## Child_NonVoc_Duration  1   1.8138        18     14.756 0.178056   
    ## Average_SignalLevel    1   5.5941        17      9.162 0.018022 * 
    ## Peak_SignalLevel       1   2.2326        16      6.929 0.135127   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.30982  -0.10235   0.00000   0.02521   1.69579  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)           287.5504   372.7189   0.771    0.440
    ## Child_Voc_Count        -0.5250     0.5356  -0.980    0.327
    ## Turn_Count              0.1496     0.1978   0.756    0.450
    ## Child_Voc_Duration      0.5025     0.5303   0.948    0.343
    ## Child_NonVoc_Duration   0.0893     0.2982   0.299    0.765
    ## Average_SignalLevel     3.3461     2.9849   1.121    0.262
    ## Peak_SignalLevel       -5.8297     6.2598  -0.931    0.352
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  5.5785  on 16  degrees of freedom
    ## AIC: 19.578
    ## 
    ## Number of Fisher Scoring iterations: 10
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.6718        21     23.169 0.003232 **
    ## Turn_Count             1   0.0575        20     23.112 0.810430   
    ## Child_Voc_Duration     1   7.3056        19     15.806 0.006874 **
    ## Child_NonVoc_Duration  1   2.7476        18     13.059 0.097402 . 
    ## Average_SignalLevel    1   3.9887        17      9.070 0.045807 * 
    ## Peak_SignalLevel       1   3.4916        16      5.578 0.061681 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31456  -0.15753   0.00000   0.01216   1.88549  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.63978  130.94400   0.211    0.833
    ## Child_Voc_Count        -0.34021    0.27276  -1.247    0.212
    ## Turn_Count              0.08213    0.10360   0.793    0.428
    ## Child_Voc_Duration      0.29748    0.25522   1.166    0.244
    ## Child_NonVoc_Duration   0.01192    0.21897   0.054    0.957
    ## Average_SignalLevel     3.44222    2.43234   1.415    0.157
    ## Peak_SignalLevel       -2.86678    2.64067  -1.086    0.278
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9288  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   9.1143        21     22.727 0.002536 **
    ## Turn_Count             1   0.0349        20     22.692 0.851909   
    ## Child_Voc_Duration     1   7.6150        19     15.077 0.005788 **
    ## Child_NonVoc_Duration  1   1.8791        18     13.198 0.170436   
    ## Average_SignalLevel    1   4.3255        17      8.873 0.037545 * 
    ## Peak_SignalLevel       1   1.9438        16      6.929 0.163259   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.30507  -0.09188   0.00001   0.04374   1.81583  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)           -16.52017  120.34251  -0.137    0.891
    ## Child_Voc_Count        -0.22627    0.19379  -1.168    0.243
    ## Turn_Count              0.03777    0.08064   0.468    0.640
    ## Child_Voc_Duration      0.18955    0.17962   1.055    0.291
    ## Child_NonVoc_Duration   0.08639    0.29957   0.288    0.773
    ## Average_SignalLevel     2.32861    1.94029   1.200    0.230
    ## Peak_SignalLevel       -1.50785    2.18349  -0.691    0.490
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.3346  on 16  degrees of freedom
    ## AIC: 20.335
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1  11.2627        21     20.579 0.0007908 ***
    ## Turn_Count             1   0.0071        20     20.571 0.9329017    
    ## Child_Voc_Duration     1   6.7600        19     13.811 0.0093223 ** 
    ## Child_NonVoc_Duration  1   2.0839        18     11.728 0.1488552    
    ## Average_SignalLevel    1   4.4304        17      7.297 0.0353041 *  
    ## Peak_SignalLevel       1   0.9625        16      6.335 0.3265598    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31532  -0.15767   0.00000   0.01218   1.88479  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.59817  130.88976   0.211    0.833
    ## Child_Voc_Count        -0.34001    0.27300  -1.245    0.213
    ## Turn_Count              0.08203    0.10381   0.790    0.429
    ## Child_Voc_Duration      0.29732    0.25536   1.164    0.244
    ## Child_NonVoc_Duration   0.01191    0.21887   0.054    0.957
    ## Average_SignalLevel     3.44119    2.43169   1.415    0.157
    ## Peak_SignalLevel       -2.86554    2.64007  -1.085    0.278
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9284  on 16  degrees of freedom
    ## AIC: 20.928
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.7462        21     24.095 0.005383 **
    ## Turn_Count             1   0.0180        20     24.077 0.893377   
    ## Child_Voc_Duration     1   7.3738        19     16.703 0.006618 **
    ## Child_NonVoc_Duration  1   1.9007        18     14.803 0.168001   
    ## Average_SignalLevel    1   5.6635        17      9.139 0.017321 * 
    ## Peak_SignalLevel       1   2.2107        16      6.928 0.137060   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -3.547e-05  -2.100e-08   2.100e-08   2.100e-08   5.055e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            1.436e+03  1.224e+07   0.000    1.000
    ## Child_Voc_Count       -7.556e+00  9.566e+03  -0.001    0.999
    ## Turn_Count             1.158e+00  4.271e+03   0.000    1.000
    ## Child_Voc_Duration     7.251e+00  1.073e+04   0.001    0.999
    ## Child_NonVoc_Duration  9.190e-01  7.942e+03   0.000    1.000
    ## Average_SignalLevel    6.781e+01  7.851e+04   0.001    0.999
    ## Peak_SignalLevel      -6.642e+01  9.962e+04  -0.001    0.999
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 5.3720e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   8.6546        21     23.187 0.0032625 ** 
    ## Turn_Count             1   0.0816        20     23.105 0.7751128    
    ## Child_Voc_Duration     1   7.1382        19     15.967 0.0075458 ** 
    ## Child_NonVoc_Duration  1   2.3356        18     13.631 0.1264471    
    ## Average_SignalLevel    1  13.6313        17      0.000 0.0002224 ***
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9997978    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -7.570e-05  -2.100e-08   2.100e-08   2.100e-08   9.002e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            7.937e+03  2.418e+06   0.003    0.997
    ## Child_Voc_Count       -1.307e+01  3.807e+03  -0.003    0.997
    ## Turn_Count             6.177e+00  1.823e+03   0.003    0.997
    ## Child_Voc_Duration     8.830e+00  2.569e+03   0.003    0.997
    ## Child_NonVoc_Duration  4.428e+00  1.539e+03   0.003    0.998
    ## Average_SignalLevel    1.751e+02  5.466e+04   0.003    0.997
    ## Peak_SignalLevel      -2.224e+02  6.649e+04  -0.003    0.997
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 2.3431e-08  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   9.1050        21     22.736 0.002549 **
    ## Turn_Count             1   0.1561        20     22.580 0.692770   
    ## Child_Voc_Duration     1   6.1125        19     16.468 0.013423 * 
    ## Child_NonVoc_Duration  1   1.6668        18     14.801 0.196682   
    ## Average_SignalLevel    1   5.7052        17      9.096 0.016914 * 
    ## Peak_SignalLevel       1   9.0956        16      0.000 0.002562 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31364  -0.15841  -0.00002   0.00517   1.88489  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.37014  130.81379   0.209    0.834
    ## Child_Voc_Count        -0.33919    0.27349  -1.240    0.215
    ## Turn_Count              0.08184    0.10364   0.790    0.430
    ## Child_Voc_Duration      0.29658    0.25573   1.160    0.246
    ## Child_NonVoc_Duration   0.01153    0.21837   0.053    0.958
    ## Average_SignalLevel     3.43451    2.43723   1.409    0.159
    ## Peak_SignalLevel       -2.85798    2.64583  -1.080    0.280
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9267  on 16  degrees of freedom
    ## AIC: 20.927
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.9113        21     23.930 0.004913 **
    ## Turn_Count             1   0.0070        20     23.923 0.933423   
    ## Child_Voc_Duration     1   7.3505        19     16.572 0.006704 **
    ## Child_NonVoc_Duration  1   1.7629        18     14.810 0.184259   
    ## Average_SignalLevel    1   5.6810        17      9.129 0.017150 * 
    ## Peak_SignalLevel       1   2.2019        16      6.927 0.137840   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15741   0.00000   0.01215   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68975  130.87701   0.212    0.832
    ## Child_Voc_Count        -0.34036    0.27246  -1.249    0.212
    ## Turn_Count              0.08218    0.10353   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25499   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44383    2.42806   1.418    0.156
    ## Peak_SignalLevel       -2.86856    2.63569  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance:  6.929  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.6342        21     24.207 0.005727 **
    ## Turn_Count             1   0.0000        20     24.207 0.997383   
    ## Child_Voc_Duration     1   7.5721        19     16.635 0.005928 **
    ## Child_NonVoc_Duration  1   1.8660        18     14.769 0.171933   
    ## Average_SignalLevel    1   5.6075        17      9.161 0.017883 * 
    ## Peak_SignalLevel       1   2.2325        16      6.929 0.135135   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31742  -0.10308   0.00000   0.01272   1.87109  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            24.238688 131.954988   0.184    0.854
    ## Child_Voc_Count        -0.328899   0.281641  -1.168    0.243
    ## Turn_Count              0.078839   0.104849   0.752    0.452
    ## Child_Voc_Duration      0.286864   0.263677   1.088    0.277
    ## Child_NonVoc_Duration   0.002303   0.227108   0.010    0.992
    ## Average_SignalLevel     3.401716   2.401618   1.416    0.157
    ## Peak_SignalLevel       -2.797550   2.635936  -1.061    0.289
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.9039  on 16  degrees of freedom
    ## AIC: 20.904
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   8.5740        21     23.267  0.00341 **
    ## Turn_Count             1   0.0043        20     23.263  0.94762   
    ## Child_Voc_Duration     1   6.5925        19     16.671  0.01024 * 
    ## Child_NonVoc_Duration  1   1.8359        18     14.835  0.17543   
    ## Average_SignalLevel    1   5.6951        17      9.140  0.01701 * 
    ## Peak_SignalLevel       1   2.2356        16      6.904  0.13487   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.29974  -0.15826   0.00000   0.01372   1.88941  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            57.5384   161.0682   0.357    0.721
    ## Child_Voc_Count        -0.3888     0.3196  -1.217    0.224
    ## Turn_Count              0.1153     0.1425   0.809    0.418
    ## Child_Voc_Duration      0.3632     0.3274   1.109    0.267
    ## Child_NonVoc_Duration  -0.1444     0.3929  -0.368    0.713
    ## Average_SignalLevel     3.0733     2.3484   1.309    0.191
    ## Peak_SignalLevel       -2.9340     2.8273  -1.038    0.299
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.8413  on 22  degrees of freedom
    ## Residual deviance:  6.7235  on 16  degrees of freedom
    ## AIC: 20.723
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.7883        21     24.053 0.005259 **
    ## Turn_Count             1   0.0040        20     24.049 0.949533   
    ## Child_Voc_Duration     1   7.9641        19     16.085 0.004771 **
    ## Child_NonVoc_Duration  1   1.2867        18     14.798 0.256653   
    ## Average_SignalLevel    1   5.7659        17      9.032 0.016340 * 
    ## Peak_SignalLevel       1   2.3088        16      6.723 0.128642   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.31463  -0.15741   0.00000   0.01215   1.88559  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            27.68975  130.87710   0.212    0.832
    ## Child_Voc_Count        -0.34036    0.27246  -1.249    0.212
    ## Turn_Count              0.08218    0.10353   0.794    0.427
    ## Child_Voc_Duration      0.29761    0.25499   1.167    0.243
    ## Child_NonVoc_Duration   0.01192    0.21905   0.054    0.957
    ## Average_SignalLevel     3.44383    2.42807   1.418    0.156
    ## Peak_SignalLevel       -2.86856    2.63570  -1.088    0.276
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance:  6.929  on 16  degrees of freedom
    ## AIC: 20.929
    ## 
    ## Number of Fisher Scoring iterations: 9
    ## 
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
    ## NULL                                     22     31.841            
    ## Child_Voc_Count        1   7.7961        21     24.045 0.005236 **
    ## Turn_Count             1   0.0003        20     24.045 0.985273   
    ## Child_Voc_Duration     1   7.3235        19     16.721 0.006806 **
    ## Child_NonVoc_Duration  1   1.8861        18     14.835 0.169638   
    ## Average_SignalLevel    1   5.6737        17      9.162 0.017221 * 
    ## Peak_SignalLevel       1   2.2326        16      6.929 0.135127   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    remove(i)
    print(lreg_predictors)

    ## <hash> containing 7 key-value pair(s).
    ##   (Intercept) : 24
    ##   Average_SignalLevel : 24
    ##   Child_NonVoc_Duration : 24
    ##   Child_Voc_Count : 24
    ##   Child_Voc_Duration : 24
    ##   Peak_SignalLevel : 24
    ##   Turn_Count : 24

    par(pty="s")
    plot(c(0,cumsum(lreg_fp)/sum(lreg_fp),1), c(0,cumsum(lreg_tp)/sum(lreg_tp),1), type='l', xlab=
         "False positive rate", ylab="True positive rate", xlim=c(0,1), ylim=c(0,1), asp=1)
    text(0.6, 0.2, labels=paste("auc: ", auc(cumsum(lreg_fp)/sum(lreg_fp), cumsum(lreg_tp)/sum(lreg_tp))))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/ROC%20for%20logistical%20regression%20predicting%20SM%20dx-1.png)

logistical forward regression predicting SM dx

    lfreg_fp <- vector()
    lfreg_tp <- vector()
    lfreg_predictors <- hash()
    for(i in 1:nrow(data)){
      train <- data[-i,]
      test <- data[i,]
      lfreg_model <- stepAIC(glm(SM_dx ~ 1, family=binomial(link='logit'), data=train, na.action=na.pass),
                    direction='forward', scope=~Child_Voc_Count+Turn_Count+Child_Voc_Duration+
                    Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel)
      lfreg_p <- predict(lfreg_model, test, "response")
      lfreg_pr <- prediction(lfreg_p, test$SM_dx)
      lfreg_fp <- c(lfreg_fp, lfreg_pr@fp[[1]][[2]])
      lfreg_tp <- c(lfreg_tp, lfreg_pr@tp[[1]][[2]])
      print(summary(lreg_model))
      print(anova(lreg_model, test="Chisq"))
      for(j in 1:length(names(lfreg_model$coefficients))){
        if(has.key(names(lfreg_model$coefficients)[[j]], lfreg_predictors)){
          .set(lfreg_predictors, names(lfreg_model$coefficients)[[j]],
               as.numeric(lfreg_predictors[[names(lfreg_model$coefficients)[[j]]]]) + 1)
        }else{
          .set(lfreg_predictors, names(lfreg_model$coefficients)[[j]], 1)
        }
      }
      remove(j)
      remove(train)
      remove(test)
    }

    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.880 27.880
    ## + Turn_Count             1   25.053 29.053
    ## + Child_Voc_Duration     1   25.714 29.714
    ## + Child_NonVoc_Duration  1   28.398 32.398
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.655 35.655
    ## + Average_SignalLevel    1   31.702 35.702
    ## 
    ## Step:  AIC=27.88
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.759 23.759
    ## + Average_SignalLevel    1   18.110 24.110
    ## <none>                       23.880 27.880
    ## + Peak_SignalLevel       1   23.315 29.315
    ## + Turn_Count             1   23.832 29.832
    ## + Child_NonVoc_Duration  1   23.877 29.877
    ## 
    ## Step:  AIC=23.76
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.0712 17.071
    ## + Child_NonVoc_Duration  1  14.9943 22.994
    ## <none>                      17.7593 23.759
    ## + Turn_Count             1  16.2140 24.214
    ## + Peak_SignalLevel       1  17.7215 25.721
    ## 
    ## Step:  AIC=17.07
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.0712 17.071
    ## + Peak_SignalLevel       1   7.8244 17.824
    ## + Child_NonVoc_Duration  1   8.7421 18.742
    ## + Turn_Count             1   9.0085 19.009
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.975 27.975
    ## + Turn_Count             1   24.999 28.999
    ## + Child_Voc_Duration     1   25.826 29.826
    ## + Child_NonVoc_Duration  1   28.070 32.070
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.605 35.605
    ## + Average_SignalLevel    1   31.798 35.798
    ## 
    ## Step:  AIC=27.98
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.800 23.800
    ## + Average_SignalLevel    1   17.815 23.815
    ## <none>                       23.975 27.975
    ## + Peak_SignalLevel       1   23.481 29.481
    ## + Child_NonVoc_Duration  1   23.930 29.930
    ## + Turn_Count             1   23.969 29.969
    ## 
    ## Step:  AIC=23.8
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.0566 17.057
    ## + Child_NonVoc_Duration  1  15.6686 23.669
    ## <none>                      17.8003 23.800
    ## + Turn_Count             1  16.6123 24.612
    ## + Peak_SignalLevel       1  17.7692 25.769
    ## 
    ## Step:  AIC=17.06
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.0566 17.057
    ## + Peak_SignalLevel       1   7.7160 17.716
    ## + Child_NonVoc_Duration  1   8.9649 18.965
    ## + Turn_Count             1   9.0532 19.053
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.402 27.402
    ## + Turn_Count             1   24.468 28.468
    ## + Child_Voc_Duration     1   25.217 29.217
    ## + Child_NonVoc_Duration  1   27.821 31.821
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   30.818 34.818
    ## + Average_SignalLevel    1   31.368 35.368
    ## 
    ## Step:  AIC=27.4
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.589 23.589
    ## + Average_SignalLevel    1   18.490 24.490
    ## <none>                       23.402 27.402
    ## + Peak_SignalLevel       1   23.217 29.217
    ## + Turn_Count             1   23.388 29.388
    ## + Child_NonVoc_Duration  1   23.389 29.389
    ## 
    ## Step:  AIC=23.59
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2237 17.224
    ## + Child_NonVoc_Duration  1  15.3285 23.328
    ## <none>                      17.5887 23.589
    ## + Turn_Count             1  16.4572 24.457
    ## + Peak_SignalLevel       1  17.5866 25.587
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2237 17.224
    ## + Peak_SignalLevel       1   7.8501 17.850
    ## + Child_NonVoc_Duration  1   9.1602 19.160
    ## + Turn_Count             1   9.2203 19.220
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.148 27.148
    ## + Turn_Count             1   23.517 27.517
    ## + Child_Voc_Duration     1   25.008 29.008
    ## + Child_NonVoc_Duration  1   27.590 31.590
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   31.454 35.454
    ## + Peak_SignalLevel       1   31.517 35.517
    ## 
    ## Step:  AIC=27.15
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.324 23.324
    ## + Average_SignalLevel    1   18.139 24.139
    ## <none>                       23.148 27.148
    ## + Peak_SignalLevel       1   22.515 28.515
    ## + Turn_Count             1   23.079 29.079
    ## + Child_NonVoc_Duration  1   23.128 29.128
    ## 
    ## Step:  AIC=23.32
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.1962 17.196
    ## + Child_NonVoc_Duration  1  14.9442 22.944
    ## <none>                      17.3244 23.324
    ## + Turn_Count             1  16.5513 24.551
    ## + Peak_SignalLevel       1  17.2582 25.258
    ## 
    ## Step:  AIC=17.2
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.1962 17.196
    ## + Peak_SignalLevel       1   7.8491 17.849
    ## + Child_NonVoc_Duration  1   9.1252 19.125
    ## + Turn_Count             1   9.1953 19.195
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.734 27.734
    ## + Turn_Count             1   24.646 28.646
    ## + Child_Voc_Duration     1   25.587 29.587
    ## + Child_NonVoc_Duration  1   27.322 31.322
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.308 35.308
    ## + Average_SignalLevel    1   31.522 35.522
    ## 
    ## Step:  AIC=27.73
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.582 23.582
    ## + Average_SignalLevel    1   18.509 24.509
    ## <none>                       23.734 27.734
    ## + Peak_SignalLevel       1   23.360 29.360
    ## + Child_NonVoc_Duration  1   23.639 29.639
    ## + Turn_Count             1   23.733 29.733
    ## 
    ## Step:  AIC=23.58
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2218 17.222
    ## <none>                      17.5817 23.582
    ## + Child_NonVoc_Duration  1  15.6300 23.630
    ## + Turn_Count             1  16.4741 24.474
    ## + Peak_SignalLevel       1  17.5665 25.567
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2218 17.222
    ## + Peak_SignalLevel       1   7.8503 17.850
    ## + Child_NonVoc_Duration  1   9.1589 19.159
    ## + Turn_Count             1   9.2185 19.218
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.678 27.678
    ## + Turn_Count             1   24.673 28.673
    ## + Child_Voc_Duration     1   25.541 29.541
    ## + Child_NonVoc_Duration  1   27.617 31.617
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.609 35.609
    ## + Average_SignalLevel    1   31.732 35.732
    ## 
    ## Step:  AIC=27.68
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   17.229 23.229
    ## + Child_Voc_Duration     1   17.486 23.486
    ## <none>                       23.678 27.678
    ## + Peak_SignalLevel       1   23.077 29.077
    ## + Child_NonVoc_Duration  1   23.620 29.620
    ## + Turn_Count             1   23.673 29.673
    ## 
    ## Step:  AIC=23.23
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   8.0978 16.098
    ## <none>                      17.2286 23.229
    ## + Child_NonVoc_Duration  1  16.3923 24.392
    ## + Turn_Count             1  16.7821 24.782
    ## + Peak_SignalLevel       1  17.2284 25.228
    ## 
    ## Step:  AIC=16.1
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       8.0978 16.098
    ## + Peak_SignalLevel       1   7.1100 17.110
    ## + Child_NonVoc_Duration  1   8.0130 18.013
    ## + Turn_Count             1   8.0971 18.097
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   20.884 24.884
    ## + Turn_Count             1   21.732 25.732
    ## + Child_Voc_Duration     1   23.191 27.191
    ## + Child_NonVoc_Duration  1   25.526 29.526
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   30.523 34.523
    ## + Peak_SignalLevel       1   30.732 34.732
    ## 
    ## Step:  AIC=24.88
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   14.807 20.807
    ## + Average_SignalLevel    1   17.914 23.914
    ## <none>                       20.884 24.884
    ## + Child_NonVoc_Duration  1   20.525 26.525
    ## + Turn_Count             1   20.845 26.845
    ## + Peak_SignalLevel       1   20.849 26.849
    ## 
    ## Step:  AIC=20.81
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.1152 17.115
    ## <none>                      14.8069 20.807
    ## + Child_NonVoc_Duration  1  14.0901 22.090
    ## + Turn_Count             1  14.2999 22.300
    ## + Peak_SignalLevel       1  14.5862 22.586
    ## 
    ## Step:  AIC=17.12
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.1152 17.115
    ## + Peak_SignalLevel       1   7.7963 17.796
    ## + Child_NonVoc_Duration  1   9.0701 19.070
    ## + Turn_Count             1   9.1127 19.113
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.551 27.551
    ## + Turn_Count             1   24.419 28.419
    ## + Child_Voc_Duration     1   25.293 29.293
    ## + Child_NonVoc_Duration  1   26.867 30.867
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.646 35.646
    ## + Average_SignalLevel    1   31.664 35.664
    ## 
    ## Step:  AIC=27.55
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   16.993 22.993
    ## + Child_Voc_Duration     1   17.797 23.797
    ## <none>                       23.551 27.551
    ## + Peak_SignalLevel       1   22.832 28.832
    ## + Child_NonVoc_Duration  1   23.395 29.395
    ## + Turn_Count             1   23.550 29.550
    ## 
    ## Step:  AIC=22.99
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   8.9638 16.964
    ## <none>                      16.9928 22.993
    ## + Child_NonVoc_Duration  1  15.1950 23.195
    ## + Turn_Count             1  16.0193 24.019
    ## + Peak_SignalLevel       1  16.9654 24.965
    ## 
    ## Step:  AIC=16.96
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       8.9638 16.964
    ## + Peak_SignalLevel       1   7.7823 17.782
    ## + Turn_Count             1   8.9569 18.957
    ## + Child_NonVoc_Duration  1   8.9636 18.964
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.899 27.899
    ## + Turn_Count             1   24.849 28.849
    ## + Child_Voc_Duration     1   25.765 29.765
    ## + Child_NonVoc_Duration  1   27.900 31.900
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.377 35.377
    ## + Average_SignalLevel    1   31.547 35.547
    ## 
    ## Step:  AIC=27.9
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.662 23.662
    ## + Average_SignalLevel    1   18.556 24.556
    ## <none>                       23.899 27.899
    ## + Peak_SignalLevel       1   23.494 29.494
    ## + Child_NonVoc_Duration  1   23.849 29.849
    ## + Turn_Count             1   23.898 29.898
    ## 
    ## Step:  AIC=23.66
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2238 17.224
    ## + Child_NonVoc_Duration  1  15.5055 23.506
    ## <none>                      17.6624 23.662
    ## + Turn_Count             1  16.4968 24.497
    ## + Peak_SignalLevel       1  17.6432 25.643
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2238 17.224
    ## + Peak_SignalLevel       1   7.8506 17.851
    ## + Child_NonVoc_Duration  1   9.1605 19.160
    ## + Turn_Count             1   9.2204 19.220
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   22.902 26.902
    ## + Turn_Count             1   23.670 27.670
    ## + Child_NonVoc_Duration  1   24.187 28.187
    ## + Child_Voc_Duration     1   24.965 28.965
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   31.133 35.133
    ## + Peak_SignalLevel       1   31.587 35.587
    ## 
    ## Step:  AIC=26.9
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   14.993 20.993
    ## + Average_SignalLevel    1   18.482 24.482
    ## <none>                       22.902 26.902
    ## + Child_NonVoc_Duration  1   21.972 27.972
    ## + Peak_SignalLevel       1   22.000 28.000
    ## + Turn_Count             1   22.897 28.897
    ## 
    ## Step:  AIC=20.99
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2017 17.202
    ## <none>                      14.9931 20.993
    ## + Turn_Count             1  13.6112 21.611
    ## + Peak_SignalLevel       1  14.5382 22.538
    ## + Child_NonVoc_Duration  1  14.6213 22.621
    ## 
    ## Step:  AIC=17.2
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2017 17.202
    ## + Peak_SignalLevel       1   7.8505 17.851
    ## + Child_NonVoc_Duration  1   9.1470 19.147
    ## + Turn_Count             1   9.1986 19.199
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   20.356 24.356
    ## + Child_Voc_Duration     1   21.321 25.321
    ## + Turn_Count             1   21.986 25.986
    ## + Child_NonVoc_Duration  1   27.802 31.802
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   30.983 34.983
    ## + Average_SignalLevel    1   31.351 35.351
    ## 
    ## Step:  AIC=24.36
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   11.654 17.654
    ## + Child_Voc_Duration     1   17.857 23.857
    ## <none>                       20.356 24.356
    ## + Child_NonVoc_Duration  1   19.593 25.593
    ## + Peak_SignalLevel       1   20.158 26.158
    ## + Turn_Count             1   20.259 26.259
    ## 
    ## Step:  AIC=17.65
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1    9.219 17.219
    ## <none>                       11.654 17.654
    ## + Peak_SignalLevel       1   10.669 18.669
    ## + Child_NonVoc_Duration  1   11.271 19.271
    ## + Turn_Count             1   11.289 19.289
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2190 17.219
    ## + Peak_SignalLevel       1   7.7948 17.795
    ## + Child_NonVoc_Duration  1   9.1505 19.151
    ## + Turn_Count             1   9.2163 19.216
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.927 27.927
    ## + Turn_Count             1   24.991 28.991
    ## + Child_Voc_Duration     1   25.768 29.768
    ## + Child_NonVoc_Duration  1   27.952 31.952
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   30.905 34.905
    ## + Peak_SignalLevel       1   31.566 35.566
    ## 
    ## Step:  AIC=27.93
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.788 23.788
    ## + Average_SignalLevel    1   18.587 24.587
    ## <none>                       23.927 27.927
    ## + Peak_SignalLevel       1   23.444 29.444
    ## + Child_NonVoc_Duration  1   23.878 29.878
    ## + Turn_Count             1   23.915 29.915
    ## 
    ## Step:  AIC=23.79
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2246 17.225
    ## + Child_NonVoc_Duration  1  15.6798 23.680
    ## <none>                      17.7876 23.788
    ## + Turn_Count             1  16.5694 24.569
    ## + Peak_SignalLevel       1  17.7572 25.757
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2246 17.225
    ## + Peak_SignalLevel       1   7.8506 17.851
    ## + Child_NonVoc_Duration  1   9.1616 19.162
    ## + Turn_Count             1   9.2213 19.221
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.169 27.169
    ## + Turn_Count             1   23.584 27.584
    ## + Child_Voc_Duration     1   25.135 29.135
    ## + Child_NonVoc_Duration  1   27.169 31.169
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   30.522 34.522
    ## + Average_SignalLevel    1   31.397 35.397
    ## 
    ## Step:  AIC=27.17
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   16.363 22.363
    ## + Average_SignalLevel    1   18.316 24.316
    ## <none>                       23.169 27.169
    ## + Peak_SignalLevel       1   23.078 29.078
    ## + Child_NonVoc_Duration  1   23.090 29.090
    ## + Turn_Count             1   23.112 29.112
    ## 
    ## Step:  AIC=22.36
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.1452 17.145
    ## + Child_NonVoc_Duration  1  13.2769 21.277
    ## <none>                      16.3632 22.363
    ## + Turn_Count             1  15.8063 23.806
    ## + Peak_SignalLevel       1  16.0468 24.047
    ## 
    ## Step:  AIC=17.15
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## + Peak_SignalLevel       1   6.8932 16.893
    ## <none>                       9.1452 17.145
    ## + Child_NonVoc_Duration  1   9.0729 19.073
    ## + Turn_Count             1   9.1452 19.145
    ## 
    ## Step:  AIC=16.89
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel + 
    ##     Peak_SignalLevel

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ##                         Df Deviance    AIC
    ## <none>                       6.8932 16.893
    ## + Turn_Count             1   5.6850 17.685
    ## + Child_NonVoc_Duration  1   6.8550 18.855
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   22.727 26.727
    ## + Turn_Count             1   24.012 28.012
    ## + Child_Voc_Duration     1   24.685 28.685
    ## + Child_NonVoc_Duration  1   27.502 31.502
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   31.272 35.272
    ## + Peak_SignalLevel       1   31.706 35.706
    ## 
    ## Step:  AIC=26.73
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   16.660 22.660
    ## + Average_SignalLevel    1   17.975 23.975
    ## <none>                       22.727 26.727
    ## + Peak_SignalLevel       1   21.035 27.035
    ## + Turn_Count             1   22.692 28.692
    ## + Child_NonVoc_Duration  1   22.715 28.715
    ## 
    ## Step:  AIC=22.66
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   8.9779 16.978
    ## + Child_NonVoc_Duration  1  14.3998 22.400
    ## <none>                      16.6603 22.660
    ## + Turn_Count             1  15.0772 23.077
    ## + Peak_SignalLevel       1  16.1467 24.147
    ## 
    ## Step:  AIC=16.98
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       8.9779 16.978
    ## + Peak_SignalLevel       1   7.8488 17.849
    ## + Child_NonVoc_Duration  1   8.8837 18.884
    ## + Turn_Count             1   8.9507 18.951
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   20.579 24.579
    ## + Turn_Count             1   21.692 25.692
    ## + Child_Voc_Duration     1   22.944 26.944
    ## + Child_NonVoc_Duration  1   26.549 30.549
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   30.872 34.872
    ## + Peak_SignalLevel       1   31.216 35.216
    ## 
    ## Step:  AIC=24.58
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   14.514 20.515
    ## + Average_SignalLevel    1   16.442 22.442
    ## <none>                       20.579 24.579
    ## + Peak_SignalLevel       1   20.122 26.122
    ## + Child_NonVoc_Duration  1   20.537 26.537
    ## + Turn_Count             1   20.572 26.572
    ## 
    ## Step:  AIC=20.51
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   7.4871 15.487
    ## + Child_NonVoc_Duration  1  12.1383 20.138
    ## <none>                      14.5145 20.515
    ## + Turn_Count             1  13.8115 21.811
    ## + Peak_SignalLevel       1  14.5068 22.507
    ## 
    ## Step:  AIC=15.49
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       7.4871 15.487
    ## + Peak_SignalLevel       1   6.8483 16.848
    ## + Child_NonVoc_Duration  1   7.2972 17.297
    ## + Turn_Count             1   7.4736 17.474
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   24.095 28.095
    ## + Turn_Count             1   24.443 28.443
    ## + Child_Voc_Duration     1   26.008 30.008
    ## + Child_NonVoc_Duration  1   28.202 32.202
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.162 35.162
    ## + Average_SignalLevel    1   31.692 35.692
    ## 
    ## Step:  AIC=28.1
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.669 23.669
    ## + Average_SignalLevel    1   18.554 24.554
    ## <none>                       24.095 28.095
    ## + Peak_SignalLevel       1   23.695 29.695
    ## + Child_NonVoc_Duration  1   24.042 30.042
    ## + Turn_Count             1   24.077 30.077
    ## 
    ## Step:  AIC=23.67
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1    9.205 17.205
    ## + Child_NonVoc_Duration  1   15.480 23.480
    ## <none>                       17.669 23.669
    ## + Turn_Count             1   16.703 24.703
    ## + Peak_SignalLevel       1   17.664 25.664
    ## 
    ## Step:  AIC=17.2
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2050 17.205
    ## + Peak_SignalLevel       1   7.8219 17.822
    ## + Child_NonVoc_Duration  1   9.1406 19.141
    ## + Turn_Count             1   9.2044 19.204
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.187 27.187
    ## + Turn_Count             1   23.564 27.564
    ## + Child_Voc_Duration     1   25.133 29.133
    ## + Child_NonVoc_Duration  1   27.623 31.623
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.251 35.251
    ## + Average_SignalLevel    1   31.499 35.499
    ## 
    ## Step:  AIC=27.19
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   16.550 22.550
    ## + Average_SignalLevel    1   17.645 23.645
    ## <none>                       23.187 27.187
    ## + Peak_SignalLevel       1   22.864 28.864
    ## + Turn_Count             1   23.105 29.105
    ## + Child_NonVoc_Duration  1   23.163 29.163
    ## 
    ## Step:  AIC=22.55
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   6.8294 14.829
    ## + Child_NonVoc_Duration  1  13.9236 21.924
    ## <none>                      16.5503 22.550
    ## + Turn_Count             1  15.9669 23.967
    ## + Peak_SignalLevel       1  16.5502 24.550

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Step:  AIC=14.83
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ##                         Df Deviance    AIC
    ## + Peak_SignalLevel       1   0.0000 10.000
    ## <none>                       6.8294 14.829
    ## + Turn_Count             1   5.9587 15.959
    ## + Child_NonVoc_Duration  1   6.5752 16.575

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Step:  AIC=10
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel + 
    ##     Peak_SignalLevel

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ##                         Df   Deviance AIC
    ## <none>                     6.3098e-09  10
    ## + Child_NonVoc_Duration  1 5.7772e-09  12
    ## + Turn_Count             1 5.8292e-09  12
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   22.736 26.736
    ## + Turn_Count             1   24.261 28.261
    ## + Child_Voc_Duration     1   24.471 28.471
    ## + Child_NonVoc_Duration  1   27.448 31.448
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   30.981 34.981
    ## + Average_SignalLevel    1   31.398 35.398
    ## 
    ## Step:  AIC=26.74
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   17.342 23.342
    ## + Child_Voc_Duration     1   17.655 23.655
    ## <none>                       22.736 26.736
    ## + Peak_SignalLevel       1   22.575 28.575
    ## + Turn_Count             1   22.580 28.580
    ## + Child_NonVoc_Duration  1   22.719 28.719
    ## 
    ## Step:  AIC=23.34
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   9.1605 17.160
    ## <none>                      17.3422 23.342
    ## + Child_NonVoc_Duration  1  16.7952 24.795
    ## + Peak_SignalLevel       1  17.1315 25.131
    ## + Turn_Count             1  17.2352 25.235
    ## 
    ## Step:  AIC=17.16
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.1605 17.160
    ## + Peak_SignalLevel       1   7.5274 17.527
    ## + Child_NonVoc_Duration  1   9.0960 19.096
    ## + Turn_Count             1   9.1536 19.154
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.930 27.930
    ## + Turn_Count             1   24.955 28.955
    ## + Child_Voc_Duration     1   25.778 29.778
    ## + Child_NonVoc_Duration  1   27.443 31.443
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.595 35.595
    ## + Average_SignalLevel    1   31.743 35.743
    ## 
    ## Step:  AIC=27.93
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.768 23.768
    ## + Average_SignalLevel    1   18.235 24.235
    ## <none>                       23.930 27.930
    ## + Peak_SignalLevel       1   23.429 29.429
    ## + Child_NonVoc_Duration  1   23.840 29.840
    ## + Turn_Count             1   23.923 29.923
    ## 
    ## Step:  AIC=23.77
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.1847 17.185
    ## + Child_NonVoc_Duration  1  15.7183 23.718
    ## <none>                      17.7683 23.768
    ## + Turn_Count             1  16.5725 24.573
    ## + Peak_SignalLevel       1  17.7353 25.735
    ## 
    ## Step:  AIC=17.18
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.1847 17.185
    ## + Peak_SignalLevel       1   7.8420 17.842
    ## + Child_NonVoc_Duration  1   9.1286 19.129
    ## + Turn_Count             1   9.1813 19.181
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   24.207 28.207
    ## + Turn_Count             1   25.075 29.075
    ## + Child_Voc_Duration     1   26.166 30.166
    ## + Child_NonVoc_Duration  1   28.103 32.103
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.626 35.626
    ## + Average_SignalLevel    1   31.662 35.662
    ## 
    ## Step:  AIC=28.21
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.629 23.629
    ## + Average_SignalLevel    1   18.587 24.587
    ## <none>                       24.207 28.207
    ## + Peak_SignalLevel       1   23.752 29.752
    ## + Child_NonVoc_Duration  1   24.139 30.139
    ## + Turn_Count             1   24.207 30.207
    ## 
    ## Step:  AIC=23.63
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2245 17.224
    ## <none>                      17.6287 23.629
    ## + Child_NonVoc_Duration  1  15.6358 23.636
    ## + Turn_Count             1  16.6350 24.635
    ## + Peak_SignalLevel       1  17.6072 25.607
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2245 17.224
    ## + Peak_SignalLevel       1   7.8506 17.851
    ## + Child_NonVoc_Duration  1   9.1615 19.162
    ## + Turn_Count             1   9.2212 19.221
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   23.267 27.267
    ## + Turn_Count             1   24.316 28.316
    ## + Child_Voc_Duration     1   24.978 28.978
    ## + Child_NonVoc_Duration  1   26.148 30.148
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.147 35.147
    ## + Average_SignalLevel    1   31.524 35.524
    ## 
    ## Step:  AIC=27.27
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   17.662 23.662
    ## + Child_Voc_Duration     1   17.776 23.776
    ## <none>                       23.267 27.267
    ## + Child_NonVoc_Duration  1   22.974 28.974
    ## + Peak_SignalLevel       1   23.001 29.001
    ## + Turn_Count             1   23.263 29.263
    ## 
    ## Step:  AIC=23.66
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   9.1877 17.188
    ## <none>                      17.6617 23.662
    ## + Child_NonVoc_Duration  1  15.9623 23.962
    ## + Turn_Count             1  17.2447 25.245
    ## + Peak_SignalLevel       1  17.5277 25.528
    ## 
    ## Step:  AIC=17.19
    ## SM_dx ~ Child_Voc_Count + Average_SignalLevel + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.1877 17.188
    ## + Peak_SignalLevel       1   7.7293 17.729
    ## + Child_NonVoc_Duration  1   9.1395 19.140
    ## + Turn_Count             1   9.1853 19.185
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   24.053 28.053
    ## + Turn_Count             1   25.072 29.072
    ## + Child_Voc_Duration     1   25.953 29.953
    ## + Child_NonVoc_Duration  1   27.166 31.166
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.666 35.666
    ## + Average_SignalLevel    1   31.731 35.731
    ## 
    ## Step:  AIC=28.05
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.669 23.669
    ## + Average_SignalLevel    1   18.460 24.460
    ## <none>                       24.053 28.053
    ## + Peak_SignalLevel       1   23.553 29.553
    ## + Child_NonVoc_Duration  1   23.947 29.947
    ## + Turn_Count             1   24.049 30.049
    ## 
    ## Step:  AIC=23.67
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.0674 17.067
    ## <none>                      17.6686 23.669
    ## + Child_NonVoc_Duration  1  15.7264 23.726
    ## + Turn_Count             1  16.0849 24.085
    ## + Peak_SignalLevel       1  17.6265 25.627
    ## 
    ## Step:  AIC=17.07
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.0674 17.067
    ## + Peak_SignalLevel       1   7.8381 17.838
    ## + Turn_Count             1   9.0349 19.035
    ## + Child_NonVoc_Duration  1   9.0618 19.062
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   24.045 28.045
    ## + Turn_Count             1   24.986 28.986
    ## + Child_Voc_Duration     1   25.823 29.823
    ## + Child_NonVoc_Duration  1   27.876 31.876
    ## <none>                       31.841 33.841
    ## + Peak_SignalLevel       1   31.232 35.232
    ## + Average_SignalLevel    1   31.536 35.536
    ## 
    ## Step:  AIC=28.05
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   17.883 23.883
    ## + Average_SignalLevel    1   18.577 24.577
    ## <none>                       24.045 28.045
    ## + Peak_SignalLevel       1   23.653 29.653
    ## + Child_NonVoc_Duration  1   23.978 29.978
    ## + Turn_Count             1   24.045 30.045
    ## 
    ## Step:  AIC=23.88
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1   9.2246 17.225
    ## + Child_NonVoc_Duration  1  15.7369 23.737
    ## <none>                      17.8832 23.883
    ## + Turn_Count             1  16.7214 24.721
    ## + Peak_SignalLevel       1  17.8578 25.858
    ## 
    ## Step:  AIC=17.22
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel
    ## 
    ##                         Df Deviance    AIC
    ## <none>                       9.2246 17.225
    ## + Peak_SignalLevel       1   7.8506 17.851
    ## + Child_NonVoc_Duration  1   9.1616 19.162
    ## + Turn_Count             1   9.2213 19.221
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Start:  AIC=33.84
    ## SM_dx ~ 1
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Count        1   21.980 25.980
    ## + Turn_Count             1   23.352 27.352
    ## + Child_Voc_Duration     1   24.211 28.211
    ## + Child_NonVoc_Duration  1   25.883 29.883
    ## <none>                       31.841 33.841
    ## + Average_SignalLevel    1   31.446 35.446
    ## + Peak_SignalLevel       1   31.507 35.507
    ## 
    ## Step:  AIC=25.98
    ## SM_dx ~ Child_Voc_Count
    ## 
    ##                         Df Deviance    AIC
    ## + Child_Voc_Duration     1   14.276 20.276
    ## + Average_SignalLevel    1   15.520 21.520
    ## <none>                       21.980 25.980
    ## + Peak_SignalLevel       1   20.988 26.988
    ## + Child_NonVoc_Duration  1   21.688 27.688
    ## + Turn_Count             1   21.912 27.912
    ## 
    ## Step:  AIC=20.28
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ##                         Df Deviance    AIC
    ## + Average_SignalLevel    1    0.000  8.000
    ## + Turn_Count             1   10.752 18.752
    ## + Child_NonVoc_Duration  1   11.116 19.116
    ## <none>                       14.276 20.276
    ## + Peak_SignalLevel       1   13.912 21.912

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## Step:  AIC=8
    ## SM_dx ~ Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ##                         Df   Deviance AIC
    ## <none>                     4.0412e-09   8
    ## + Turn_Count             1 2.9383e-09  10
    ## + Peak_SignalLevel       1 3.8514e-09  10
    ## + Child_NonVoc_Duration  1 4.0305e-09  10
    ## 
    ## Call:
    ## glm(formula = SM_dx ~ Child_Voc_Count + Turn_Count + Child_Voc_Duration + 
    ##     Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, 
    ##     family = binomial(link = "logit"), data = train, na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.559e-05  -2.110e-08  -2.110e-08   2.110e-08   2.512e-05  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            4.415e+02  3.083e+06   0.000        1
    ## Child_Voc_Count       -4.268e+00  6.964e+03  -0.001        1
    ## Turn_Count             1.440e+00  4.006e+03   0.000        1
    ## Child_Voc_Duration     3.811e+00  7.237e+03   0.001        1
    ## Child_NonVoc_Duration -5.595e-01  9.473e+03   0.000        1
    ## Average_SignalLevel    2.823e+01  5.496e+04   0.001        1
    ## Peak_SignalLevel      -2.586e+01  4.979e+04  -0.001        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3.1841e+01  on 22  degrees of freedom
    ## Residual deviance: 1.7984e-09  on 16  degrees of freedom
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: SM_dx
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                       Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                     22     31.841              
    ## Child_Voc_Count        1   9.8612        21     21.980 0.0016880 ** 
    ## Turn_Count             1   0.0679        20     21.912 0.7944618    
    ## Child_Voc_Duration     1  11.1601        19     10.752 0.0008358 ***
    ## Child_NonVoc_Duration  1  10.7522        18      0.000 0.0010416 ** 
    ## Average_SignalLevel    1   0.0000        17      0.000 0.9997551    
    ## Peak_SignalLevel       1   0.0000        16      0.000 0.9999761    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    remove(i)
    print(lfreg_predictors)

    ## <hash> containing 5 key-value pair(s).
    ##   (Intercept) : 24
    ##   Average_SignalLevel : 24
    ##   Child_Voc_Count : 24
    ##   Child_Voc_Duration : 24
    ##   Peak_SignalLevel : 2

    par(pty="s")
    plot(c(0,cumsum(lfreg_fp)/sum(lfreg_fp),1), c(0,cumsum(lfreg_tp)/sum(lfreg_tp),1), type='l', xlab=
         "False positive rate", ylab="True positive rate", asp=1)
    text(0.6, 0.2, labels=paste("auc: ", auc(cumsum(lfreg_fp)/sum(lfreg_fp), cumsum(lfreg_tp)/sum(lfreg_tp))))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/ROC%20for%20logistical%20forward%20regression%20predicting%20SM%20dx-1.png)

forward regression for SMQ

    fsmq_predicted <- vector()
    fsmq_actual <- vector()
    fsmq_models <- vector()
    fsmq_predictors <- hash()
    for(i in 1:nrow(smq_data)){
      train <- smq_data[-i,]
      test <- smq_data[i,]
      fsmq_model <- step(lm(smq_as+smq_hf+smq_ss+smq_id~1, data=train, na.action=na.omit),
                    direction='forward', scope=~Child_Voc_Count+Turn_Count+Child_Voc_Duration+
                    Child_NonVoc_Duration+Average_SignalLevel+Peak_SignalLevel)
      print(summary(fsmq_model))
      print(fsmq_model$anova)
      avPlots(fsmq_model)
      fsmq_predicted <- c(fsmq_predicted, predict(fsmq_model, test))
      fsmq_actual <- c(fsmq_actual, test$smq_as+test$smq_hf+test$smq_ss+test$smq_id)
      fsmq_models <- c(fsmq_models, fsmq_model)
      for(j in 1:length(names(fsmq_model$coefficients))){
        if(has.key(names(fsmq_model$coefficients)[[j]], fsmq_predictors)){
          .set(fsmq_predictors, names(fsmq_model$coefficients)[[j]],
               as.numeric(fsmq_predictors[[names(fsmq_model$coefficients)[[j]]]]) + 1)
        }else{
          .set(fsmq_predictors, names(fsmq_model$coefficients)[[j]], 1)
        }
      }
      remove(j)
      remove(train)
      remove(test)
    }

    ## Start:  AIC=48.76
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Turn_Count             1    68.988 125.66 41.570
    ## + Child_Voc_Count        1    66.545 128.10 41.974
    ## + Child_Voc_Duration     1    56.197 138.45 43.605
    ## + Child_NonVoc_Duration  1    36.635 158.01 46.381
    ## <none>                               194.64 48.760
    ## + Average_SignalLevel    1     7.458 187.19 49.939
    ## + Peak_SignalLevel       1     2.495 192.15 50.489
    ## 
    ## Step:  AIC=41.57
    ## smq_as + smq_hf + smq_ss + smq_id ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               125.66 41.570
    ## + Peak_SignalLevel       1    8.4747 117.18 42.103
    ## + Average_SignalLevel    1    7.9913 117.67 42.190
    ## + Child_NonVoc_Duration  1    1.5392 124.12 43.311
    ## + Child_Voc_Count        1    1.0176 124.64 43.399
    ## + Child_Voc_Duration     1    0.0804 125.58 43.556
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Turn_Count, 
    ##     data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6527 -1.7126 -0.4513  2.1708  3.5976 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.928254   1.079254   6.419 3.73e-06 ***
    ## Turn_Count  -0.024067   0.007452  -3.230  0.00441 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.572 on 19 degrees of freedom
    ## Multiple R-squared:  0.3544, Adjusted R-squared:  0.3205 
    ## F-statistic: 10.43 on 1 and 19 DF,  p-value: 0.00441
    ## 
    ##           Step Df Deviance Resid. Df Resid. Dev     AIC
    ## 1              NA       NA        20   194.6436 48.7596
    ## 2 + Turn_Count -1 68.98779        19   125.6558 41.5695

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-1.png)

    ## Start:  AIC=43.55
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    41.282 110.61 38.890
    ## + Turn_Count             1    36.903 114.98 39.706
    ## + Child_Voc_Duration     1    34.255 117.63 40.184
    ## <none>                               151.89 43.551
    ## + Child_NonVoc_Duration  1    12.100 139.79 43.808
    ## + Peak_SignalLevel       1     0.048 151.84 45.544
    ## + Average_SignalLevel    1     0.007 151.88 45.550
    ## 
    ## Step:  AIC=38.89
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## + Average_SignalLevel    1   20.4484  90.156 36.597
    ## + Child_Voc_Duration     1   14.4545  96.150 37.949
    ## <none>                               110.605 38.890
    ## + Peak_SignalLevel       1    6.4986 104.106 39.619
    ## + Child_NonVoc_Duration  1    0.5405 110.064 40.787
    ## + Turn_Count             1    0.1096 110.495 40.869
    ## 
    ## Step:  AIC=36.6
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   12.8883 77.268 35.358
    ## <none>                               90.156 36.597
    ## + Peak_SignalLevel       1    2.0109 88.145 38.124
    ## + Turn_Count             1    0.6274 89.529 38.451
    ## + Child_NonVoc_Duration  1    0.0881 90.068 38.577
    ## 
    ## Step:  AIC=35.36
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Average_SignalLevel + 
    ##     Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               77.268 35.358
    ## + Child_NonVoc_Duration  1   2.65882 74.609 36.623
    ## + Turn_Count             1   0.33907 76.929 37.266
    ## + Peak_SignalLevel       1   0.30183 76.966 37.276
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Average_SignalLevel + Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8617 -0.8363 -0.0230  0.8773  4.6616 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)         -23.43755   14.63253  -1.602   0.1276  
    ## Child_Voc_Count      -0.04147    0.01824  -2.273   0.0363 *
    ## Average_SignalLevel   0.43876    0.21526   2.038   0.0574 .
    ## Child_Voc_Duration    0.03632    0.02157   1.684   0.1105  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.132 on 17 degrees of freedom
    ## Multiple R-squared:  0.4913, Adjusted R-squared:  0.4015 
    ## F-statistic: 5.472 on 3 and 17 DF,  p-value: 0.008099
    ## 
    ##                    Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                       NA       NA        20  151.88646 43.55083
    ## 2     + Child_Voc_Count -1 41.28170        19  110.60476 38.89025
    ## 3 + Average_SignalLevel -1 20.44838        18   90.15638 36.59749
    ## 4  + Child_Voc_Duration -1 12.88828        17   77.26810 35.35793

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-2.png)

    ## Start:  AIC=46.39
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    57.401 116.51 39.982
    ## + Turn_Count             1    54.340 119.57 40.527
    ## + Child_Voc_Duration     1    49.146 124.76 41.420
    ## + Child_NonVoc_Duration  1    20.313 153.59 45.786
    ## <none>                               173.91 46.394
    ## + Average_SignalLevel    1     9.711 164.20 47.187
    ## + Peak_SignalLevel       1     8.817 165.09 47.302
    ## 
    ## Step:  AIC=39.98
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   14.8510 101.66 39.118
    ## <none>                               116.51 39.982
    ## + Average_SignalLevel    1    5.5015 111.00 40.966
    ## + Turn_Count             1    0.7441 115.76 41.847
    ## + Peak_SignalLevel       1    0.7336 115.77 41.849
    ## + Child_NonVoc_Duration  1    0.4121 116.09 41.908
    ## 
    ## Step:  AIC=39.12
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               101.656 39.118
    ## + Average_SignalLevel    1    5.8780  95.778 39.868
    ## + Child_NonVoc_Duration  1    4.2040  97.452 40.232
    ## + Turn_Count             1    0.3851 101.271 41.039
    ## + Peak_SignalLevel       1    0.0077 101.648 41.117
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2634 -1.5226 -0.0504  1.5893  3.8494 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.63027    0.96570   6.866 2.01e-06 ***
    ## Child_Voc_Count    -0.04101    0.02028  -2.023   0.0582 .  
    ## Child_Voc_Duration  0.03901    0.02406   1.622   0.1223    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.376 on 18 degrees of freedom
    ## Multiple R-squared:  0.4155, Adjusted R-squared:  0.3505 
    ## F-statistic: 6.397 on 2 and 18 DF,  p-value: 0.007967
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   173.9080 46.39409
    ## 2    + Child_Voc_Count -1 57.40130        19   116.5067 39.98196
    ## 3 + Child_Voc_Duration -1 14.85099        18   101.6558 39.11846

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-3.png)

    ## Start:  AIC=48
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    59.389 128.35 42.015
    ## + Turn_Count             1    57.307 130.43 42.353
    ## + Child_Voc_Duration     1    50.006 137.73 43.497
    ## + Child_NonVoc_Duration  1    27.020 160.72 46.738
    ## <none>                               187.74 48.001
    ## + Average_SignalLevel    1     7.019 180.72 49.201
    ## + Peak_SignalLevel       1     2.969 184.77 49.666
    ## 
    ## Step:  AIC=42.02
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   18.1463 110.20 40.814
    ## <none>                               128.35 42.015
    ## + Average_SignalLevel    1    8.5353 119.81 42.570
    ## + Peak_SignalLevel       1    3.8799 124.47 43.370
    ## + Turn_Count             1    1.1976 127.15 43.818
    ## + Child_NonVoc_Duration  1    0.0076 128.34 44.014
    ## 
    ## Step:  AIC=40.81
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               110.20 40.814
    ## + Average_SignalLevel    1    8.4999 101.70 41.128
    ## + Child_NonVoc_Duration  1    2.3643 107.84 42.359
    ## + Peak_SignalLevel       1    1.0433 109.16 42.614
    ## + Turn_Count             1    0.3151 109.89 42.754
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4011 -1.5389 -0.1135  2.0069  3.6361 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.85258    1.01485   6.752 2.51e-06 ***
    ## Child_Voc_Count    -0.04440    0.02094  -2.120   0.0481 *  
    ## Child_Voc_Duration  0.04278    0.02485   1.722   0.1023    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.474 on 18 degrees of freedom
    ## Multiple R-squared:  0.413,  Adjusted R-squared:  0.3478 
    ## F-statistic: 6.332 on 2 and 18 DF,  p-value: 0.008275
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   187.7392 48.00115
    ## 2    + Child_Voc_Count -1 59.38866        19   128.3505 42.01509
    ## 3 + Child_Voc_Duration -1 18.14635        18   110.2041 40.81405

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-4.png)

    ## Start:  AIC=46.04
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    53.299 117.70 40.197
    ## + Turn_Count             1    50.725 120.28 40.651
    ## + Child_Voc_Duration     1    44.626 126.38 41.690
    ## + Child_NonVoc_Duration  1    22.176 148.83 45.124
    ## <none>                               171.00 46.040
    ## + Average_SignalLevel    1     2.043 168.96 47.788
    ## + Peak_SignalLevel       1     0.323 170.68 48.001
    ## 
    ## Step:  AIC=40.2
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   17.5170 100.19 38.813
    ## + Average_SignalLevel    1   14.8887 102.82 39.357
    ## <none>                               117.70 40.197
    ## + Peak_SignalLevel       1    7.1363 110.57 40.883
    ## + Turn_Count             1    0.8153 116.89 42.051
    ## + Child_NonVoc_Duration  1    0.0115 117.69 42.195
    ## 
    ## Step:  AIC=38.81
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## + Average_SignalLevel    1   14.3563  85.832 37.565
    ## <none>                               100.188 38.813
    ## + Peak_SignalLevel       1    2.8992  97.289 40.196
    ## + Child_NonVoc_Duration  1    2.8140  97.374 40.215
    ## + Turn_Count             1    0.5255  99.663 40.703
    ## 
    ## Step:  AIC=37.57
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration + 
    ##     Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               85.832 37.565
    ## + Child_NonVoc_Duration  1   1.22993 84.602 39.262
    ## + Peak_SignalLevel       1   0.57123 85.261 39.425
    ## + Turn_Count             1   0.01747 85.815 39.561
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration + Average_SignalLevel, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2371 -1.0619 -0.3046  1.0368  5.0711 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)         -18.67383   15.00831  -1.244   0.2303  
    ## Child_Voc_Count      -0.04600    0.01905  -2.414   0.0273 *
    ## Child_Voc_Duration    0.04136    0.02255   1.834   0.0842 .
    ## Average_SignalLevel   0.37373    0.22164   1.686   0.1100  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.247 on 17 degrees of freedom
    ## Multiple R-squared:  0.4981, Adjusted R-squared:  0.4095 
    ## F-statistic: 5.623 on 3 and 17 DF,  p-value: 0.007265
    ## 
    ##                    Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                       NA       NA        20   171.0042 46.04048
    ## 2     + Child_Voc_Count -1 53.29878        19   117.7054 40.19692
    ## 3  + Child_Voc_Duration -1 17.51703        18   100.1884 38.81313
    ## 4 + Average_SignalLevel -1 14.35631        17    85.8321 37.56528

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-5.png)

    ## Start:  AIC=48.7
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    70.998 123.06 41.131
    ## + Turn_Count             1    68.964 125.09 41.475
    ## + Child_Voc_Duration     1    60.656 133.40 42.825
    ## + Child_NonVoc_Duration  1    33.423 160.63 46.727
    ## <none>                               194.06 48.696
    ## + Average_SignalLevel    1    11.883 182.17 49.369
    ## + Peak_SignalLevel       1     4.066 189.99 50.251
    ## 
    ## Step:  AIC=41.13
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   17.8314 105.23 39.843
    ## <none>                               123.06 41.131
    ## + Average_SignalLevel    1    4.9029 118.16 42.277
    ## + Peak_SignalLevel       1    2.2351 120.82 42.746
    ## + Turn_Count             1    1.6280 121.43 42.851
    ## + Child_NonVoc_Duration  1    0.1713 122.89 43.102
    ## 
    ## Step:  AIC=39.84
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               105.23 39.843
    ## + Average_SignalLevel    1    5.0319 100.19 40.814
    ## + Child_NonVoc_Duration  1    1.4263 103.80 41.557
    ## + Peak_SignalLevel       1    0.2770 104.95 41.788
    ## + Turn_Count             1    0.1316 105.09 41.817
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6878 -0.9910 -0.0787  1.9073  3.3043 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         7.18996    0.98177   7.323 8.42e-07 ***
    ## Child_Voc_Count    -0.04476    0.02039  -2.195   0.0415 *  
    ## Child_Voc_Duration  0.04237    0.02426   1.746   0.0978 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.418 on 18 degrees of freedom
    ## Multiple R-squared:  0.4578, Adjusted R-squared:  0.3975 
    ## F-statistic: 7.598 on 2 and 18 DF,  p-value: 0.004053
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   194.0562 48.69613
    ## 2    + Child_Voc_Count -1 70.99842        19   123.0578 41.13076
    ## 3 + Child_Voc_Duration -1 17.83140        18   105.2264 39.84342

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-6.png)

    ## Start:  AIC=48.78
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    66.347 128.45 42.031
    ## + Turn_Count             1    63.866 130.93 42.433
    ## + Child_Voc_Duration     1    55.993 138.80 43.659
    ## + Child_NonVoc_Duration  1    28.891 165.90 47.405
    ## <none>                               194.79 48.776
    ## + Average_SignalLevel    1     8.103 186.69 49.884
    ## + Peak_SignalLevel       1     3.059 191.74 50.443
    ## 
    ## Step:  AIC=42.03
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   22.1006 106.35 40.066
    ## <none>                               128.45 42.031
    ## + Average_SignalLevel    1    8.4009 120.05 42.611
    ## + Peak_SignalLevel       1    3.6592 124.79 43.424
    ## + Turn_Count             1    1.2177 127.23 43.831
    ## + Child_NonVoc_Duration  1    0.0355 128.41 44.025
    ## 
    ## Step:  AIC=40.07
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               106.347 40.066
    ## + Average_SignalLevel    1    7.1062  99.241 40.614
    ## + Child_NonVoc_Duration  1    5.5335 100.813 40.944
    ## + Turn_Count             1    0.5759 105.771 41.952
    ## + Peak_SignalLevel       1    0.2287 106.118 42.021
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -5.394 -1.337 -0.061  1.763  3.523 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.97974    0.95545   7.305 8.72e-07 ***
    ## Child_Voc_Count    -0.04969    0.02120  -2.344   0.0308 *  
    ## Child_Voc_Duration  0.04863    0.02514   1.934   0.0690 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.431 on 18 degrees of freedom
    ## Multiple R-squared:  0.4541, Adjusted R-squared:  0.3934 
    ## F-statistic: 7.485 on 2 and 18 DF,  p-value: 0.004309
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   194.7944 48.77587
    ## 2    + Child_Voc_Count -1 66.34682        19   128.4476 42.03097
    ## 3 + Child_Voc_Duration -1 22.10060        18   106.3470 40.06588

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-7.png)

    ## Start:  AIC=48.08
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    63.684 124.76 41.420
    ## + Turn_Count             1    62.960 125.49 41.541
    ## + Child_Voc_Duration     1    53.523 134.92 43.064
    ## + Child_NonVoc_Duration  1    41.244 147.20 44.893
    ## <none>                               188.45 48.080
    ## + Average_SignalLevel    1    11.282 177.17 48.784
    ## + Peak_SignalLevel       1     1.346 187.10 49.930
    ## 
    ## Step:  AIC=41.42
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   21.0336 103.73 39.543
    ## <none>                               124.76 41.420
    ## + Peak_SignalLevel       1    6.6260 118.14 42.274
    ## + Average_SignalLevel    1    6.1436 118.62 42.359
    ## + Turn_Count             1    1.8245 122.94 43.110
    ## + Child_NonVoc_Duration  1    1.1158 123.65 43.231
    ## 
    ## Step:  AIC=39.54
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               103.730 39.543
    ## + Average_SignalLevel    1    5.1727  98.557 40.468
    ## + Peak_SignalLevel       1    2.6074 101.122 41.008
    ## + Child_NonVoc_Duration  1    0.2715 103.458 41.488
    ## + Turn_Count             1    0.1346 103.595 41.515
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3094 -1.3629 -0.0381  1.6973  3.6705 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.82482    0.94915   7.190 1.08e-06 ***
    ## Child_Voc_Count    -0.04736    0.02036  -2.327   0.0319 *  
    ## Child_Voc_Duration  0.04628    0.02422   1.910   0.0721 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.401 on 18 degrees of freedom
    ## Multiple R-squared:  0.4496, Adjusted R-squared:  0.3884 
    ## F-statistic:  7.35 on 2 and 18 DF,  p-value: 0.004639
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   188.4477 48.08026
    ## 2    + Child_Voc_Count -1 63.68446        19   124.7633 41.41981
    ## 3 + Child_Voc_Duration -1 21.03358        18   103.7297 39.54258

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-8.png)

    ## Start:  AIC=48.72
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    68.995 125.33 41.514
    ## + Turn_Count             1    66.533 127.79 41.923
    ## + Child_Voc_Duration     1    62.042 132.28 42.648
    ## + Child_NonVoc_Duration  1    28.539 165.78 47.389
    ## <none>                               194.32 48.725
    ## + Average_SignalLevel    1     8.016 186.30 49.840
    ## + Peak_SignalLevel       1     3.324 191.00 50.362
    ## 
    ## Step:  AIC=41.51
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   16.3855 108.94 40.572
    ## <none>                               125.33 41.514
    ## + Average_SignalLevel    1    9.3363 115.99 41.888
    ## + Peak_SignalLevel       1    3.2718 122.05 42.959
    ## + Turn_Count             1    1.3502 123.97 43.287
    ## + Child_NonVoc_Duration  1    0.8961 124.43 43.364
    ## 
    ## Step:  AIC=40.57
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               108.94 40.572
    ## + Average_SignalLevel    1    8.6740 100.27 40.829
    ## + Turn_Count             1    1.8776 107.06 42.207
    ## + Child_NonVoc_Duration  1    1.6936 107.25 42.243
    ## + Peak_SignalLevel       1    1.1808 107.76 42.343
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -5.446 -1.454 -0.297  2.197  3.389 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         7.12355    1.01712   7.004 1.54e-06 ***
    ## Child_Voc_Count    -0.05583    0.02843  -1.964   0.0652 .  
    ## Child_Voc_Duration  0.05668    0.03445   1.645   0.1172    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.46 on 18 degrees of freedom
    ## Multiple R-squared:  0.4394, Adjusted R-squared:  0.3771 
    ## F-statistic: 7.054 on 2 and 18 DF,  p-value: 0.00547
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   194.3198 48.72464
    ## 2    + Child_Voc_Count -1 68.99450        19   125.3253 41.51419
    ## 3 + Child_Voc_Duration -1 16.38552        18   108.9398 40.57173

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-9.png)

    ## Start:  AIC=48.35
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    61.539 129.37 42.181
    ## + Turn_Count             1    59.485 131.43 42.512
    ## + Child_Voc_Duration     1    51.647 139.26 43.729
    ## + Child_NonVoc_Duration  1    25.320 165.59 47.365
    ## <none>                               190.91 48.353
    ## + Average_SignalLevel    1    12.437 178.47 48.938
    ## + Peak_SignalLevel       1     1.716 189.19 50.163
    ## 
    ## Step:  AIC=42.18
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   19.2095 110.16 40.806
    ## + Average_SignalLevel    1   17.1039 112.27 41.203
    ## <none>                               129.37 42.181
    ## + Peak_SignalLevel       1    4.3299 125.04 43.466
    ## + Turn_Count             1    1.3556 128.01 43.960
    ## + Child_NonVoc_Duration  1    0.0006 129.37 44.181
    ## 
    ## Step:  AIC=40.81
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## + Average_SignalLevel    1   21.0814  89.079 38.345
    ## <none>                               110.161 40.806
    ## + Child_NonVoc_Duration  1    2.4458 107.715 42.334
    ## + Peak_SignalLevel       1    1.0735 109.087 42.600
    ## + Turn_Count             1    0.1989 109.962 42.768
    ## 
    ## Step:  AIC=38.35
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration + 
    ##     Average_SignalLevel
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               89.079 38.345
    ## + Turn_Count             1   1.89426 87.185 39.894
    ## + Peak_SignalLevel       1   1.44370 87.636 40.002
    ## + Child_NonVoc_Duration  1   0.14151 88.938 40.312
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration + Average_SignalLevel, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6008 -1.2548 -0.3813  1.0808  4.5276 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)         -31.23521   19.12237  -1.633   0.1208  
    ## Child_Voc_Count      -0.05657    0.02025  -2.793   0.0125 *
    ## Child_Voc_Duration    0.04894    0.02326   2.104   0.0506 .
    ## Average_SignalLevel   0.57764    0.28799   2.006   0.0611 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.289 on 17 degrees of freedom
    ## Multiple R-squared:  0.5334, Adjusted R-squared:  0.4511 
    ## F-statistic: 6.478 on 3 and 17 DF,  p-value: 0.004018
    ## 
    ##                    Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                       NA       NA        20  190.90952 48.35282
    ## 2     + Child_Voc_Count -1 61.53919        19  129.37033 42.18129
    ## 3  + Child_Voc_Duration -1 19.20948        18  110.16086 40.80580
    ## 4 + Average_SignalLevel -1 21.08140        17   89.07945 38.34513

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-10.png)

    ## Start:  AIC=48.51
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    63.736 128.63 42.061
    ## + Turn_Count             1    63.234 129.13 42.142
    ## + Child_Voc_Duration     1    53.919 138.45 43.605
    ## + Child_NonVoc_Duration  1    28.291 164.07 47.172
    ## <none>                               192.37 48.512
    ## + Average_SignalLevel    1     8.126 184.24 49.606
    ## + Peak_SignalLevel       1     5.589 186.78 49.893
    ## 
    ## Step:  AIC=42.06
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   19.0399 109.59 40.696
    ## <none>                               128.63 42.061
    ## + Average_SignalLevel    1    8.5498 120.08 42.616
    ## + Peak_SignalLevel       1    3.5771 125.05 43.468
    ## + Turn_Count             1    1.8201 126.81 43.761
    ## + Child_NonVoc_Duration  1    0.0003 128.63 44.061
    ## 
    ## Step:  AIC=40.7
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               109.59 40.696
    ## + Average_SignalLevel    1    8.1659 101.42 41.070
    ## + Child_NonVoc_Duration  1    2.5983 106.99 42.193
    ## + Peak_SignalLevel       1    0.4487 109.14 42.610
    ## + Turn_Count             1    0.1032 109.49 42.677
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -5.398 -1.501 -0.101  2.052  3.618 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.87313    0.98247   6.996 1.57e-06 ***
    ## Child_Voc_Count    -0.04533    0.02082  -2.177   0.0430 *  
    ## Child_Voc_Duration  0.04381    0.02477   1.768   0.0939 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.467 on 18 degrees of freedom
    ## Multiple R-squared:  0.4303, Adjusted R-squared:  0.367 
    ## F-statistic: 6.798 on 2 and 18 DF,  p-value: 0.006321
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   192.3648 48.51229
    ## 2    + Child_Voc_Count -1 63.73608        19   128.6287 42.06055
    ## 3 + Child_Voc_Duration -1 19.03991        18   109.5888 40.69646

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-11.png)

    ## Start:  AIC=47.86
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    67.284 119.16 40.455
    ## + Turn_Count             1    62.029 124.42 41.361
    ## + Child_Voc_Duration     1    57.496 128.95 42.113
    ## + Child_NonVoc_Duration  1    27.055 159.39 46.564
    ## <none>                               186.45 47.856
    ## + Average_SignalLevel    1    10.162 176.28 48.679
    ## + Peak_SignalLevel       1     0.518 185.93 49.798
    ## 
    ## Step:  AIC=40.46
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   17.6858 101.48 39.081
    ## + Peak_SignalLevel       1   13.0632 106.10 40.017
    ## <none>                               119.16 40.455
    ## + Average_SignalLevel    1    6.6447 112.52 41.250
    ## + Turn_Count             1    0.3543 118.81 42.393
    ## + Child_NonVoc_Duration  1    0.1511 119.01 42.429
    ## 
    ## Step:  AIC=39.08
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               101.476 39.081
    ## + Peak_SignalLevel       1    6.6514  94.825 39.658
    ## + Average_SignalLevel    1    6.5793  94.897 39.674
    ## + Child_NonVoc_Duration  1    3.9280  97.548 40.252
    ## + Turn_Count             1    1.1413 100.335 40.844
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6426 -1.0034 -0.2089  1.8391  3.3811 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         7.10926    0.94112   7.554 5.49e-07 ***
    ## Child_Voc_Count    -0.04421    0.02003  -2.208   0.0405 *  
    ## Child_Voc_Duration  0.04219    0.02382   1.771   0.0935 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.374 on 18 degrees of freedom
    ## Multiple R-squared:  0.4557, Adjusted R-squared:  0.3953 
    ## F-statistic: 7.536 on 2 and 18 DF,  p-value: 0.004191
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   186.4455 47.85595
    ## 2    + Child_Voc_Count -1 67.28368        19   119.1618 40.45516
    ## 3 + Child_Voc_Duration -1 17.68579        18   101.4760 39.08131

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-12.png)

    ## Start:  AIC=46.96
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## + Child_Voc_Count        1    84.135  94.520 35.590
    ## + Turn_Count             1    80.941  97.714 36.288
    ## + Child_Voc_Duration     1    73.002 105.653 37.928
    ## + Child_NonVoc_Duration  1    35.031 143.623 44.376
    ## + Average_SignalLevel    1    18.058 160.596 46.722
    ## <none>                               178.655 46.960
    ## + Peak_SignalLevel       1     4.233 174.422 48.456
    ## 
    ## Step:  AIC=35.59
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   16.6428 77.877 33.523
    ## <none>                               94.520 35.590
    ## + Peak_SignalLevel       1    4.1807 90.339 36.640
    ## + Average_SignalLevel    1    2.7657 91.754 36.966
    ## + Turn_Count             1    1.6331 92.887 37.224
    ## + Child_NonVoc_Duration  1    0.0274 94.493 37.584
    ## 
    ## Step:  AIC=33.52
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               77.877 33.523
    ## + Child_NonVoc_Duration  1   2.89142 74.986 34.728
    ## + Average_SignalLevel    1   2.78662 75.091 34.758
    ## + Peak_SignalLevel       1   1.25504 76.622 35.182
    ## + Turn_Count             1   0.08474 77.792 35.500
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3125 -1.3482 -0.0113  1.7867  2.9278 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         7.57174    0.84783   8.931 4.94e-08 ***
    ## Child_Voc_Count    -0.04444    0.01754  -2.534   0.0208 *  
    ## Child_Voc_Duration  0.04094    0.02087   1.961   0.0655 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.08 on 18 degrees of freedom
    ## Multiple R-squared:  0.5641, Adjusted R-squared:  0.5157 
    ## F-statistic: 11.65 on 2 and 18 DF,  p-value: 0.0005683
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20  178.65471 46.95958
    ## 2    + Child_Voc_Count -1 84.13474        19   94.51996 35.59006
    ## 3 + Child_Voc_Duration -1 16.64275        18   77.87721 33.52283

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-13.png)

    ## Start:  AIC=47.96
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Turn_Count             1    59.165 128.24 41.997
    ## + Child_Voc_Count        1    57.990 129.41 42.189
    ## + Child_Voc_Duration     1    48.222 139.18 43.717
    ## + Child_NonVoc_Duration  1    22.353 165.05 47.297
    ## <none>                               187.41 47.964
    ## + Average_SignalLevel    1     4.866 182.54 49.411
    ## + Peak_SignalLevel       1     4.059 183.35 49.504
    ## 
    ## Step:  AIC=42
    ## smq_as + smq_hf + smq_ss + smq_id ~ Turn_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               128.24 41.997
    ## + Average_SignalLevel    1   11.1436 117.10 42.088
    ## + Peak_SignalLevel       1    7.7143 120.53 42.694
    ## + Child_Voc_Count        1    0.8232 127.42 43.862
    ## + Child_NonVoc_Duration  1    0.4666 127.77 43.920
    ## + Child_Voc_Duration     1    0.3016 127.94 43.947
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Turn_Count, 
    ##     data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6300 -1.9312  0.4892  1.9130  3.7590 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.742244   1.069524   6.304 4.74e-06 ***
    ## Turn_Count  -0.020985   0.007088  -2.961  0.00803 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.598 on 19 degrees of freedom
    ## Multiple R-squared:  0.3157, Adjusted R-squared:  0.2797 
    ## F-statistic: 8.766 on 1 and 19 DF,  p-value: 0.00803
    ## 
    ##           Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1              NA       NA        20   187.4048 47.96372
    ## 2 + Turn_Count -1 59.16506        19   128.2397 41.99695

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-14.png)

    ## Start:  AIC=48.78
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    65.365 129.43 42.191
    ## + Turn_Count             1    63.190 131.61 42.541
    ## + Child_Voc_Duration     1    55.530 139.27 43.729
    ## + Child_NonVoc_Duration  1    28.966 165.83 47.395
    ## <none>                               194.80 48.776
    ## + Average_SignalLevel    1     7.587 187.21 49.942
    ## + Peak_SignalLevel       1     2.760 192.04 50.476
    ## 
    ## Step:  AIC=42.19
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   18.6252 110.81 40.929
    ## <none>                               129.43 42.191
    ## + Average_SignalLevel    1    9.1276 120.30 42.656
    ## + Peak_SignalLevel       1    4.4315 125.00 43.460
    ## + Turn_Count             1    1.2067 128.22 43.995
    ## + Child_NonVoc_Duration  1    0.0000 129.43 44.191
    ## 
    ## Step:  AIC=40.93
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               110.81 40.929
    ## + Average_SignalLevel    1    8.9225 101.88 41.166
    ## + Child_NonVoc_Duration  1    2.7277 108.08 42.405
    ## + Peak_SignalLevel       1    1.2159 109.59 42.697
    ## + Turn_Count             1    0.3273 110.48 42.866
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4777 -1.5759 -0.1115  1.9897  3.5385 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.95269    0.98050   7.091 1.31e-06 ***
    ## Child_Voc_Count    -0.04501    0.02093  -2.150   0.0454 *  
    ## Child_Voc_Duration  0.04330    0.02490   1.739   0.0990 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.481 on 18 degrees of freedom
    ## Multiple R-squared:  0.4312, Adjusted R-squared:  0.368 
    ## F-statistic: 6.822 on 2 and 18 DF,  p-value: 0.006235
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   194.7969 48.77614
    ## 2    + Child_Voc_Count -1 65.36512        19   129.4318 42.19127
    ## 3 + Child_Voc_Duration -1 18.62519        18   110.8066 40.92855

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-15.png)

    ## Start:  AIC=48.12
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    66.958 121.86 40.925
    ## + Turn_Count             1    60.692 128.13 41.978
    ## + Child_Voc_Duration     1    58.489 130.33 42.336
    ## + Child_NonVoc_Duration  1    27.672 161.15 46.794
    ## <none>                               188.82 48.122
    ## + Average_SignalLevel    1     8.449 180.37 49.160
    ## + Peak_SignalLevel       1     4.960 183.86 49.562
    ## 
    ## Step:  AIC=40.93
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   13.8137 108.05 40.399
    ## <none>                               121.86 40.925
    ## + Average_SignalLevel    1    8.3995 113.46 41.426
    ## + Peak_SignalLevel       1    2.2927 119.57 42.527
    ## + Turn_Count             1    0.1154 121.75 42.906
    ## + Child_NonVoc_Duration  1    0.0774 121.78 42.912
    ## 
    ## Step:  AIC=40.4
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               108.047 40.399
    ## + Average_SignalLevel    1    8.4917  99.555 40.680
    ## + Child_NonVoc_Duration  1    2.6574 105.390 41.876
    ## + Turn_Count             1    0.8719 107.175 42.229
    ## + Peak_SignalLevel       1    0.7004 107.346 42.262
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5832 -0.8032 -0.1474  1.8116  3.4993 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.98416    0.96390   7.246 9.75e-07 ***
    ## Child_Voc_Count    -0.04120    0.02138  -1.927    0.070 .  
    ## Child_Voc_Duration  0.03865    0.02548   1.517    0.147    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.45 on 18 degrees of freedom
    ## Multiple R-squared:  0.4278, Adjusted R-squared:  0.3642 
    ## F-statistic: 6.728 on 2 and 18 DF,  p-value: 0.006578
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   188.8185 48.12154
    ## 2    + Child_Voc_Count -1 66.95794        19   121.8606 40.92546
    ## 3 + Child_Voc_Duration -1 13.81366        18   108.0469 40.39890

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-16.png)

    ## Start:  AIC=48.75
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    68.323 126.25 41.668
    ## + Turn_Count             1    66.858 127.72 41.911
    ## + Child_Voc_Duration     1    57.474 137.10 43.400
    ## + Child_NonVoc_Duration  1    28.637 165.94 47.409
    ## <none>                               194.57 48.752
    ## + Average_SignalLevel    1     7.451 187.12 49.932
    ## + Peak_SignalLevel       1     2.456 192.12 50.485
    ## 
    ## Step:  AIC=41.67
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   20.5255 105.72 39.942
    ## <none>                               126.25 41.668
    ## + Average_SignalLevel    1    7.6015 118.65 42.364
    ## + Peak_SignalLevel       1    3.7012 122.55 43.044
    ## + Turn_Count             1    1.8789 124.37 43.354
    ## + Child_NonVoc_Duration  1    0.0443 126.20 43.661
    ## 
    ## Step:  AIC=39.94
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               105.724 39.942
    ## + Average_SignalLevel    1    6.9493  98.774 40.515
    ## + Child_NonVoc_Duration  1    4.0673 101.656 41.119
    ## + Peak_SignalLevel       1    0.6842 105.039 41.806
    ## + Turn_Count             1    0.0998 105.624 41.923
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.7100 -1.1228 -0.0401  2.0055  3.2168 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         7.28519    1.01900   7.149 1.17e-06 ***
    ## Child_Voc_Count    -0.04772    0.02065  -2.311   0.0329 *  
    ## Child_Voc_Duration  0.04569    0.02444   1.869   0.0779 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.424 on 18 degrees of freedom
    ## Multiple R-squared:  0.4566, Adjusted R-squared:  0.3963 
    ## F-statistic: 7.564 on 2 and 18 DF,  p-value: 0.004129
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   194.5725 48.75193
    ## 2    + Child_Voc_Count -1 68.32339        19   126.2491 41.66842
    ## 3 + Child_Voc_Duration -1 20.52554        18   105.7236 39.94241

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-17.png)

    ## Start:  AIC=46.96
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    49.291 129.36 42.180
    ## + Turn_Count             1    48.114 130.54 42.370
    ## + Child_Voc_Duration     1    39.403 139.25 43.727
    ## + Child_NonVoc_Duration  1    19.499 159.16 46.533
    ## <none>                               178.66 46.960
    ## + Average_SignalLevel    1     4.229 174.43 48.457
    ## + Peak_SignalLevel       1     0.543 178.11 48.896
    ## 
    ## Step:  AIC=42.18
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   19.7331 109.63 40.704
    ## <none>                               129.36 42.180
    ## + Average_SignalLevel    1    9.9882 119.38 42.493
    ## + Peak_SignalLevel       1    4.3785 124.98 43.457
    ## + Turn_Count             1    1.1300 128.23 43.996
    ## + Child_NonVoc_Duration  1    0.0009 129.36 44.180
    ## 
    ## Step:  AIC=40.7
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               109.63 40.704
    ## + Average_SignalLevel    1    7.8774 101.75 41.139
    ## + Child_NonVoc_Duration  1    2.6323 107.00 42.194
    ## + Peak_SignalLevel       1    1.0670 108.56 42.499
    ## + Turn_Count             1    0.0690 109.56 42.691
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3717 -1.5180 -0.2427  1.9493  3.6483 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.84198    0.99726   6.861 2.03e-06 ***
    ## Child_Voc_Count    -0.04709    0.02135  -2.205   0.0407 *  
    ## Child_Voc_Duration  0.04673    0.02596   1.800   0.0886 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.468 on 18 degrees of freedom
    ## Multiple R-squared:  0.3864, Adjusted R-squared:  0.3182 
    ## F-statistic: 5.666 on 2 and 18 DF,  p-value: 0.01234
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   178.6547 46.95958
    ## 2    + Child_Voc_Count -1 49.29104        19   129.3637 42.18021
    ## 3 + Child_Voc_Duration -1 19.73309        18   109.6306 40.70447

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-18.png)

    ## Start:  AIC=46.96
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    62.928 115.73 39.841
    ## + Turn_Count             1    58.523 120.13 40.625
    ## + Child_Voc_Duration     1    55.355 123.30 41.172
    ## + Child_NonVoc_Duration  1    38.394 140.26 43.879
    ## <none>                               178.66 46.960
    ## + Average_SignalLevel    1     6.751 171.90 48.151
    ## + Peak_SignalLevel       1     4.930 173.72 48.372
    ## 
    ## Step:  AIC=39.84
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   11.7158 104.01 39.600
    ## <none>                               115.73 39.841
    ## + Average_SignalLevel    1    9.5677 106.16 40.029
    ## + Peak_SignalLevel       1    2.1644 113.56 41.444
    ## + Child_NonVoc_Duration  1    1.1189 114.61 41.637
    ## + Turn_Count             1    0.4766 115.25 41.754
    ## 
    ## Step:  AIC=39.6
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               104.011 39.600
    ## + Average_SignalLevel    1    9.2770  94.734 39.638
    ## + Peak_SignalLevel       1    0.6616 103.350 41.466
    ## + Turn_Count             1    0.3564 103.655 41.527
    ## + Child_NonVoc_Duration  1    0.3485 103.663 41.529
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6040 -0.8612 -0.2498  1.6794  3.5376 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.93892    0.94415   7.349 8.03e-07 ***
    ## Child_Voc_Count    -0.03856    0.02111  -1.827   0.0843 .  
    ## Child_Voc_Duration  0.03571    0.02508   1.424   0.1716    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.404 on 18 degrees of freedom
    ## Multiple R-squared:  0.4178, Adjusted R-squared:  0.3531 
    ## F-statistic: 6.459 on 2 and 18 DF,  p-value: 0.007684
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   178.6547 46.95958
    ## 2    + Child_Voc_Count -1 62.92761        19   115.7271 39.84096
    ## 3 + Child_Voc_Duration -1 11.71576        18   104.0113 39.59953

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-19.png)

    ## Start:  AIC=48.23
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    60.355 129.43 42.190
    ## + Turn_Count             1    58.385 131.40 42.508
    ## + Child_Voc_Duration     1    50.521 139.26 43.728
    ## + Child_NonVoc_Duration  1    29.270 160.51 46.711
    ## <none>                               189.78 48.228
    ## + Average_SignalLevel    1     4.929 184.85 49.676
    ## + Peak_SignalLevel       1     1.115 188.67 50.105
    ## 
    ## Step:  AIC=42.19
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   18.6086 110.82 40.931
    ## <none>                               129.43 42.190
    ## + Average_SignalLevel    1    9.2256 120.20 42.637
    ## + Peak_SignalLevel       1    4.4641 124.96 43.453
    ## + Turn_Count             1    1.3265 128.10 43.974
    ## + Child_NonVoc_Duration  1    0.0007 129.43 44.190
    ## 
    ## Step:  AIC=40.93
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               110.82 40.931
    ## + Average_SignalLevel    1    9.1617 101.66 41.119
    ## + Child_NonVoc_Duration  1    2.7491 108.07 42.403
    ## + Peak_SignalLevel       1    1.3204 109.50 42.679
    ## + Turn_Count             1    0.4408 110.38 42.847
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4699 -1.5734 -0.1549  1.9884  3.5488 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.94207    0.97861   7.094  1.3e-06 ***
    ## Child_Voc_Count    -0.04497    0.02092  -2.149   0.0455 *  
    ## Child_Voc_Duration  0.04329    0.02490   1.739   0.0992 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.481 on 18 degrees of freedom
    ## Multiple R-squared:  0.4161, Adjusted R-squared:  0.3512 
    ## F-statistic: 6.413 on 2 and 18 DF,  p-value: 0.007892
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   189.7811 48.22832
    ## 2    + Child_Voc_Count -1 60.35478        19   129.4263 42.19037
    ## 3 + Child_Voc_Duration -1 18.60864        18   110.8176 40.93063

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-20.png)

    ## Start:  AIC=46.96
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    52.539 126.12 41.646
    ## + Turn_Count             1    49.693 128.96 42.115
    ## + Child_Voc_Duration     1    45.291 133.36 42.820
    ## + Child_NonVoc_Duration  1    22.313 156.34 46.158
    ## <none>                               178.66 46.960
    ## + Average_SignalLevel    1     6.523 172.13 48.178
    ## + Peak_SignalLevel       1     4.070 174.59 48.476
    ## 
    ## Step:  AIC=41.65
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   15.3125 110.80 40.928
    ## <none>                               126.12 41.646
    ## + Average_SignalLevel    1    7.5779 118.54 42.345
    ## + Peak_SignalLevel       1    2.8689 123.25 43.163
    ## + Turn_Count             1    0.7830 125.33 43.515
    ## + Child_NonVoc_Duration  1    0.0001 126.12 43.646
    ## 
    ## Step:  AIC=40.93
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## <none>                               110.80 40.928
    ## + Average_SignalLevel    1    9.4268 101.38 41.061
    ## + Child_NonVoc_Duration  1    2.8392 107.96 42.383
    ## + Peak_SignalLevel       1    1.3337 109.47 42.674
    ## + Turn_Count             1    0.3651 110.44 42.859
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4710 -1.5421 -0.1414  2.0294  3.5287 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.96441    1.01432   6.866 2.01e-06 ***
    ## Child_Voc_Count    -0.04572    0.02388  -1.914   0.0716 .  
    ## Child_Voc_Duration  0.04408    0.02795   1.577   0.1322    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.481 on 18 degrees of freedom
    ## Multiple R-squared:  0.3798, Adjusted R-squared:  0.3109 
    ## F-statistic: 5.511 on 2 and 18 DF,  p-value: 0.01358
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   178.6547 46.95958
    ## 2    + Child_Voc_Count -1 52.53868        19   126.1160 41.64628
    ## 3 + Child_Voc_Duration -1 15.31247        18   110.8036 40.92797

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-21.png)

    ## Start:  AIC=48.45
    ## smq_as + smq_hf + smq_ss + smq_id ~ 1
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Count        1    66.997 124.80 41.426
    ## + Turn_Count             1    64.131 127.67 41.903
    ## + Child_Voc_Duration     1    56.709 135.09 43.090
    ## + Child_NonVoc_Duration  1    32.551 159.25 46.545
    ## <none>                               191.80 48.450
    ## + Average_SignalLevel    1     7.806 183.99 49.578
    ## + Peak_SignalLevel       1     2.076 189.72 50.222
    ## 
    ## Step:  AIC=41.43
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count
    ## 
    ##                         Df Sum of Sq    RSS    AIC
    ## + Child_Voc_Duration     1   20.1030 104.70 39.738
    ## <none>                               124.80 41.426
    ## + Average_SignalLevel    1    9.3209 115.48 41.796
    ## + Peak_SignalLevel       1    6.0685 118.73 42.380
    ## + Turn_Count             1    1.0783 123.72 43.244
    ## + Child_NonVoc_Duration  1    0.1163 124.69 43.407
    ## 
    ## Step:  AIC=39.74
    ## smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + Child_Voc_Duration
    ## 
    ##                         Df Sum of Sq     RSS    AIC
    ## <none>                               104.699 39.738
    ## + Average_SignalLevel    1    9.1311  95.568 39.822
    ## + Peak_SignalLevel       1    2.2138 102.486 41.289
    ## + Child_NonVoc_Duration  1    1.6398 103.060 41.406
    ## + Turn_Count             1    0.5174 104.182 41.634
    ## 
    ## Call:
    ## lm(formula = smq_as + smq_hf + smq_ss + smq_id ~ Child_Voc_Count + 
    ##     Child_Voc_Duration, data = train, na.action = na.omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3754 -1.4120 -0.0365  1.7223  3.6031 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.89250    0.94868   7.265 9.39e-07 ***
    ## Child_Voc_Count    -0.04663    0.02040  -2.286   0.0346 *  
    ## Child_Voc_Duration  0.04508    0.02425   1.859   0.0794 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.412 on 18 degrees of freedom
    ## Multiple R-squared:  0.4541, Adjusted R-squared:  0.3935 
    ## F-statistic: 7.487 on 2 and 18 DF,  p-value: 0.004304
    ## 
    ##                   Step Df Deviance Resid. Df Resid. Dev      AIC
    ## 1                      NA       NA        20   191.7992 48.45045
    ## 2    + Child_Voc_Count -1 66.99677        19   124.8024 41.42639
    ## 3 + Child_Voc_Duration -1 20.10298        18   104.6994 39.73799

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/forward%20regression%20for%20SMQ-22.png)

    print(fsmq_predictors)

    ## <hash> containing 5 key-value pair(s).
    ##   (Intercept) : 22
    ##   Average_SignalLevel : 3
    ##   Child_Voc_Count : 20
    ##   Child_Voc_Duration : 20
    ##   Turn_Count : 2

    remove(i)

    par(pty="s")
    plot(fsmq_predicted, fsmq_actual, type='p',
         xlab="Predicted SMQ symptom severity",ylab="Reported SMQ symptom severity", asp=1)
    abline(lm(as.numeric(fsmq_actual)~as.numeric(fsmq_predicted)))
    text(0.6*max(fsmq_predicted), 0.05*max(fsmq_actual), labels=paste("R\u00b2: ", 1-(sum((fsmq_actual-fsmq_predicted)^2)/sum((fsmq_actual-mean(fsmq_actual))^2))))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/plot%20for%20forward%20regression%20predicting%20SMQ-1.png)

    plot(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Count, data=smq_data)
    abline(lm(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Count, data=smq_data))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    plot(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Duration, data=smq_data)
    abline(lm(smq_as+smq_hf+smq_ss+smq_id ~ Child_Voc_Duration, data=smq_data))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    plot(smq_as+smq_hf+smq_ss+smq_id ~ Average_SignalLevel, data=smq_data)
    abline(lm(smq_as+smq_hf+smq_ss+smq_id ~ Average_SignalLevel, data=smq_data))

![](logistical_forward_regression_with_ROC_curve_files/figure-markdown_strict/unnamed-chunk-3-1.png)
