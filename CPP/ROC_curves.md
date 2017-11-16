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

    SM_DX_ROC_plot <- function(data, formula, plotlabel) {
      plotlabel <- "SM diagnosis by vocalization count"
      lreg_p <- vector()
      lreg_l <- vector()
      lreg_fp <- vector()
      lreg_tp <- vector()
      for(i in 1:nrow(data)){
        train <- data[-i,]
        test <- data[i,]
        gl_model <- glm(formula=formula, family=binomial(link='logit'), data=train,
                      na.action=na.pass)
        lreg_p <- c(lreg_p, predict(gl_model, test, "response"))
        lreg_l <- c(lreg_l, (test$SM_dx))
        print(summary(gl_model))
        print(anova(gl_model, test="Chisq"))
        remove(train)
        remove(test)
      }
      remove(i)
      pred <- prediction(lreg_p, lreg_l)
      perf <- performance(pred, "tpr", "fpr")
      auc <- sprintf("%.4f", round(as.numeric(performance(pred, "auc")@y.values), 4))
      par(pty = "s")
      plot(perf,
           main=paste("ROC curve: ", plotlabel),
           xlim=c(0,1), ylim=c(0,1), asp=1)
      text(0.6, 0.2,
           labels=paste("auc: ", auc))
    }

predicting SM dx by vocalization counts

      SM_DX_ROC_plot(data, "SM_dx~Child_Voc_Count", "SM diagnosis by vocalization count")

    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7869  -0.9163   0.5141   0.8133   1.7551  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.029364   0.964305   2.104   0.0353 *
    ## Child_Voc_Count -0.008040   0.003632  -2.214   0.0269 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.880  on 21  degrees of freedom
    ## AIC: 27.88
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.9615        21     23.880 0.004778 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7609  -0.8852  -0.1870   0.8355   1.7965  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.979712   0.997032   1.986   0.0471 *
    ## Child_Voc_Count -0.008143   0.003660  -2.225   0.0261 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.975  on 21  degrees of freedom
    ## AIC: 27.975
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1    7.866        21     23.975 0.005037 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7409  -0.8629  -0.1789   0.7518   1.8264  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.940612   0.966964   2.007   0.0448 *
    ## Child_Voc_Count -0.008211   0.003585  -2.291   0.0220 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.402  on 21  degrees of freedom
    ## AIC: 27.402
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.4394        21     23.402 0.003672 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7420  -0.8500  -0.1703   0.7519   1.8479  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.955684   0.963031   2.031   0.0423 *
    ## Child_Voc_Count -0.008365   0.003617  -2.313   0.0207 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.148  on 21  degrees of freedom
    ## AIC: 27.148
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.6934        21     23.148 0.003194 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7471  -0.8776  -0.1865   0.8456   1.8044  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.945536   0.981655   1.982   0.0475 *
    ## Child_Voc_Count -0.008103   0.003601  -2.251   0.0244 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.734  on 21  degrees of freedom
    ## AIC: 27.734
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.1077        21     23.734 0.004408 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7452  -0.8754  -0.1857   0.8287   1.8074  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.942095   0.978466   1.985   0.0472 *
    ## Child_Voc_Count -0.008111   0.003593  -2.258   0.0240 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.678  on 21  degrees of freedom
    ## AIC: 27.678
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1    8.163        21     23.678 0.004275 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0167  -0.6758   0.3709   0.6835   1.8505  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.734644   1.188117   2.302   0.0214 *
    ## Child_Voc_Count -0.010261   0.004269  -2.404   0.0162 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 20.884  on 21  degrees of freedom
    ## AIC: 24.884
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                               22     31.841              
    ## Child_Voc_Count  1   10.957        21     20.884 0.0009325 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8002  -0.8163   0.5072   0.8028   1.7399  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.058256   0.959128   2.146   0.0319 *
    ## Child_Voc_Count -0.008027   0.003544  -2.265   0.0235 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.551  on 21  degrees of freedom
    ## AIC: 27.551
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.2904        21     23.551 0.003986 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7553  -0.8833  -0.1875   0.8393   1.7978  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.964900   0.992303   1.980   0.0477 *
    ## Child_Voc_Count -0.008115   0.003638  -2.231   0.0257 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.899  on 21  degrees of freedom
    ## AIC: 27.899
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.9419        21     23.899  0.00483 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7463  -0.8367  -0.1610   0.7500   1.8713  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.980491   0.964568   2.053     0.04 *
    ## Child_Voc_Count -0.008552   0.003675  -2.327     0.02 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 22.902  on 21  degrees of freedom
    ## AIC: 26.902
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.9388        21     22.902 0.002792 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.87158  -0.69089  -0.07168   0.67791   1.68455  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.499842   1.140691   2.192   0.0284 *
    ## Child_Voc_Count -0.011452   0.004863  -2.355   0.0185 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 20.356  on 21  degrees of freedom
    ## AIC: 24.356
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                               22     31.841              
    ## Child_Voc_Count  1   11.486        21     20.356 0.0007014 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7572  -0.8841  -0.1874   0.8380   1.7972  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.969856   0.994118   1.982   0.0475 *
    ## Child_Voc_Count -0.008123   0.003646  -2.228   0.0259 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.927  on 21  degrees of freedom
    ## AIC: 27.927
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.9138        21     23.927 0.004906 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7418  -0.8511  -0.1711   0.7520   1.8460  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.953935   0.963134   2.029   0.0425 *
    ## Child_Voc_Count -0.008350   0.003613  -2.311   0.0208 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.169  on 21  degrees of freedom
    ## AIC: 27.169
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.6718        21     23.169 0.003232 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8578  -0.7371   0.4698   0.7674   1.7483  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.224462   1.004233   2.215   0.0268 *
    ## Child_Voc_Count -0.008474   0.003594  -2.358   0.0184 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 22.727  on 21  degrees of freedom
    ## AIC: 26.727
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   9.1143        21     22.727 0.002536 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9656  -0.6628   0.3556   0.6704   1.8726  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.824461   1.219714   2.316   0.0206 *
    ## Child_Voc_Count -0.010598   0.004402  -2.407   0.0161 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 20.579  on 21  degrees of freedom
    ## AIC: 24.579
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                               22     31.841              
    ## Child_Voc_Count  1   11.263        21     20.579 0.0007908 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7870  -0.9011   0.5112   0.8163   1.7793  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.042876   0.978687   2.087   0.0369 *
    ## Child_Voc_Count -0.008203   0.003710  -2.211   0.0270 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.095  on 21  degrees of freedom
    ## AIC: 28.095
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.7462        21     24.095 0.005383 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8231  -0.7427   0.4926   0.7880   1.7383  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.121480   0.972080   2.182   0.0291 *
    ## Child_Voc_Count -0.008171   0.003528  -2.316   0.0206 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.187  on 21  degrees of freedom
    ## AIC: 27.187
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   8.6546        21     23.187 0.003262 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8570  -0.7373   0.4703   0.7679   1.7480  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.222218   1.003474   2.215   0.0268 *
    ## Child_Voc_Count -0.008467   0.003592  -2.357   0.0184 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 22.736  on 21  degrees of freedom
    ## AIC: 26.736
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1    9.105        21     22.736 0.002549 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7574  -0.8841  -0.1874   0.8379   1.7971  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.970315   0.994273   1.982   0.0475 *
    ## Child_Voc_Count -0.008124   0.003646  -2.228   0.0259 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.930  on 21  degrees of freedom
    ## AIC: 27.93
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.9113        21     23.930 0.004913 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7942  -0.8884   0.5038   0.8146   1.8020  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.075641   0.982932   2.112   0.0347 *
    ## Child_Voc_Count -0.008405   0.003697  -2.274   0.0230 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.207  on 21  degrees of freedom
    ## AIC: 28.207
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.6342        21     24.207 0.005727 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8176  -0.7429   0.4962   0.7914   1.7377  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.105686   0.967998   2.175   0.0296 *
    ## Child_Voc_Count -0.008130   0.003526  -2.306   0.0211 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.267  on 21  degrees of freedom
    ## AIC: 27.267
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1    8.574        21     23.267  0.00341 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7860  -0.9048   0.5126   0.8162   1.7732  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.036713   0.975505   2.088   0.0368 *
    ## Child_Voc_Count -0.008155   0.003697  -2.206   0.0274 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.053  on 21  degrees of freedom
    ## AIC: 28.053
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.7883        21     24.053 0.005259 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7859  -0.9054   0.5128   0.8161   1.7721  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.035811   0.974913   2.088   0.0368 *
    ## Child_Voc_Count -0.008147   0.003695  -2.205   0.0275 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.045  on 21  degrees of freedom
    ## AIC: 28.045
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   7.7961        21     24.045 0.005236 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7795  -0.7835  -0.1238   0.7312   1.9719  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      2.130009   1.004977   2.119   0.0341 *
    ## Child_Voc_Count -0.009468   0.004047  -2.340   0.0193 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 21.980  on 21  degrees of freedom
    ## AIC: 25.98
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                               22     31.841            
    ## Child_Voc_Count  1   9.8612        21     21.980 0.001688 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](ROC_curves_files/figure-markdown_strict/predicting%20SM%20dx%20by%20vocalization%20counts-1.png)

predicting SM dx by vocalization duration

      SM_DX_ROC_plot(data, "SM_dx~Child_Voc_Duration", "ROC Curve: SM diagnosis by vocalization duration")

    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7040  -1.0141   0.6213   0.8874   1.8816  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.592467   0.837434   1.902   0.0572 .
    ## Child_Voc_Duration -0.007961   0.003925  -2.028   0.0425 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.714  on 21  degrees of freedom
    ## AIC: 29.714
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.1273        21     25.714  0.01331 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6683  -0.9747  -0.2384   0.9190   1.9327  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.518490   0.869882   1.746   0.0809 .
    ## Child_Voc_Duration -0.008067   0.003979  -2.027   0.0426 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.826  on 21  degrees of freedom
    ## AIC: 29.826
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.0152        21     25.826  0.01418 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6597  -0.9493  -0.2208   0.8288   1.9768  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.511501   0.845306   1.788   0.0738 .
    ## Child_Voc_Duration -0.008303   0.003938  -2.108   0.0350 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.217  on 21  degrees of freedom
    ## AIC: 29.217
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.6242        21     25.217  0.01006 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6630  -0.9391  -0.2109   0.8277   1.9997  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.527802   0.843719   1.811   0.0702 .
    ## Child_Voc_Duration -0.008477   0.003983  -2.128   0.0333 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.008  on 21  degrees of freedom
    ## AIC: 29.008
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   6.8335        21     25.008 0.008946 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6604  -0.9659  -0.2345   0.9261   1.9445  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.502464   0.856808   1.754   0.0795 .
    ## Child_Voc_Duration -0.008094   0.003928  -2.061   0.0393 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.587  on 21  degrees of freedom
    ## AIC: 29.587
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.2546        21     25.587  0.01239 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6598  -0.9640  -0.2332   0.9132   1.9478  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.501779   0.854741   1.757   0.0789 .
    ## Child_Voc_Duration -0.008112   0.003923  -2.067   0.0387 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.541  on 21  degrees of freedom
    ## AIC: 29.541
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.3005        21     25.541  0.01207 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8933  -0.8113   0.4870   0.7837   2.0305  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         2.130131   0.985200   2.162   0.0306 *
    ## Child_Voc_Duration -0.010165   0.004473  -2.273   0.0231 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.191  on 21  degrees of freedom
    ## AIC: 27.191
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   8.6506        21     23.191  0.00327 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7259  -0.9000   0.6061   0.8722   1.8785  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.647345   0.840666   1.960   0.0500 .
    ## Child_Voc_Duration -0.008081   0.003842  -2.103   0.0354 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.293  on 21  degrees of freedom
    ## AIC: 29.293
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.5485        21     25.293   0.0105 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6655  -0.9727  -0.2380   0.9212   1.9347  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.512079   0.866324   1.745   0.0809 .
    ## Child_Voc_Duration -0.008062   0.003961  -2.035   0.0418 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.765  on 21  degrees of freedom
    ## AIC: 29.765
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.0759        21     25.765   0.0137 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6640  -0.9369  -0.2088   0.8273   2.0048  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.531912   0.843788   1.816   0.0694 .
    ## Child_Voc_Duration -0.008516   0.003995  -2.132   0.0330 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.965  on 21  degrees of freedom
    ## AIC: 28.965
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   6.8767        21     24.965 0.008732 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.86142  -0.74231  -0.06167   0.71919   1.65620  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         2.230116   1.058235   2.107   0.0351 *
    ## Child_Voc_Duration -0.013529   0.005946  -2.275   0.0229 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 21.321  on 21  degrees of freedom
    ## AIC: 25.321
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1    10.52        21     21.321 0.001181 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6656  -0.9728  -0.2380   0.9211   1.9346  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.512315   0.866475   1.745   0.0809 .
    ## Child_Voc_Duration -0.008062   0.003962  -2.035   0.0419 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.768  on 21  degrees of freedom
    ## AIC: 29.768
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.0733        21     25.768  0.01372 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6608  -0.9454  -0.2171   0.8285   1.9855  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.517132   0.844290   1.797   0.0723 .
    ## Child_Voc_Duration -0.008368   0.003953  -2.117   0.0343 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.135  on 21  degrees of freedom
    ## AIC: 29.135
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   6.7063        21     25.135 0.009607 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7694  -0.8557   0.5743   0.8469   1.9047  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.767378   0.871619   2.028   0.0426 *
    ## Child_Voc_Duration -0.008531   0.003910  -2.182   0.0291 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.685  on 21  degrees of freedom
    ## AIC: 28.685
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1    7.156        21     24.685 0.007471 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8472  -0.8018   0.4733   0.7740   2.0553  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         2.192609   1.004181   2.183   0.0290 *
    ## Child_Voc_Duration -0.010467   0.004585  -2.283   0.0224 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 22.944  on 21  degrees of freedom
    ## AIC: 26.944
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1    8.897        21     22.944 0.002856 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6994  -0.9989   0.6223   0.8942   1.9080  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.589591   0.851325   1.867   0.0619 .
    ## Child_Voc_Duration -0.008104   0.004058  -1.997   0.0458 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 26.008  on 21  degrees of freedom
    ## AIC: 30.008
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1    5.833        21     26.008  0.01573 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7365  -0.8610   0.5985   0.8658   1.8829  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.675797   0.846716   1.979   0.0478 *
    ## Child_Voc_Duration -0.008177   0.003843  -2.128   0.0333 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.133  on 21  degrees of freedom
    ## AIC: 29.133
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   6.7087        21     25.133 0.009594 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7863  -0.8514   0.5620   0.8377   1.9188  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.815675   0.886274   2.049   0.0405 *
    ## Child_Voc_Duration -0.008732   0.003967  -2.201   0.0277 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.471  on 21  degrees of freedom
    ## AIC: 28.471
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   7.3706        21     24.471  0.00663 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6661  -0.9731  -0.2381   0.9208   1.9342  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.513269   0.867066   1.745   0.0809 .
    ## Child_Voc_Duration -0.008063   0.003965  -2.034   0.0420 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.778  on 21  degrees of freedom
    ## AIC: 29.778
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.0632        21     25.778   0.0138 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7070  -0.9844   0.6141   0.8938   1.9419  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.620359   0.858586   1.887   0.0591 .
    ## Child_Voc_Duration -0.008375   0.004070  -2.058   0.0396 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 26.166  on 21  degrees of freedom
    ## AIC: 30.166
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   5.6751        21     26.166  0.01721 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7473  -0.8599   0.5905   0.8594   1.8891  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.705696   0.854198   1.997   0.0458 *
    ## Child_Voc_Duration -0.008287   0.003856  -2.149   0.0316 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.978  on 21  degrees of freedom
    ## AIC: 28.978
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   6.8631        21     24.978 0.008799 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6991  -1.0026   0.6232   0.8934   1.9004  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.586070   0.847713   1.871   0.0613 .
    ## Child_Voc_Duration -0.008052   0.004032  -1.997   0.0458 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.953  on 21  degrees of freedom
    ## AIC: 29.953
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   5.8883        21     25.953  0.01524 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7007  -1.0096   0.6231   0.8905   1.8879  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.586100   0.840865   1.886   0.0593 .
    ## Child_Voc_Duration -0.007981   0.003970  -2.010   0.0444 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.823  on 21  degrees of freedom
    ## AIC: 29.823
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
    ## NULL                                  22     31.841           
    ## Child_Voc_Duration  1   6.0184        21     25.823  0.01416 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6902  -0.8966  -0.1693   0.8138   2.1041  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         1.632914   0.862726   1.893   0.0584 .
    ## Child_Voc_Duration -0.009352   0.004315  -2.167   0.0302 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.211  on 21  degrees of freedom
    ## AIC: 28.211
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##                    Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                                  22     31.841            
    ## Child_Voc_Duration  1   7.6308        21     24.211 0.005738 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](ROC_curves_files/figure-markdown_strict/predicting%20SM%20dx%20by%20vocalization%20duration-1.png)

predicting SM dx by conversational turns

      SM_DX_ROC_plot(data, "SM_dx~Turn_Count", "ROC Curve: SM diagnosis by conversational turn count")

    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7507  -0.9263   0.4837   0.8059   1.6672  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.228245   1.102450   2.021   0.0433 *
    ## Turn_Count  -0.017721   0.008178  -2.167   0.0302 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.053  on 21  degrees of freedom
    ## AIC: 29.053
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.7878        21     25.053 0.009178 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7267  -0.9094  -0.4031   0.8240   1.6865  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.170487   1.126838   1.926   0.0541 .
    ## Turn_Count  -0.017642   0.008126  -2.171   0.0299 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.999  on 21  degrees of freedom
    ## AIC: 28.999
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1    6.842        21     24.999 0.008904 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7034  -0.8915  -0.3945   0.7801   1.7079  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.117029   1.087339   1.947   0.0515 .
    ## Turn_Count  -0.017610   0.007869  -2.238   0.0252 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.468  on 21  degrees of freedom
    ## AIC: 28.468
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1    7.373        21     24.468 0.006621 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7199  -0.8468  -0.3453   0.7720   1.7813  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.232418   1.093344   2.042   0.0412 *
    ## Turn_Count  -0.019095   0.008214  -2.325   0.0201 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.517  on 21  degrees of freedom
    ## AIC: 27.517
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   8.3241        21     23.517 0.003912 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7066  -0.8986  -0.4004   0.8299   1.6978  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.11816    1.09761   1.930   0.0536 .
    ## Turn_Count  -0.01750    0.00791  -2.212   0.0270 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.646  on 21  degrees of freedom
    ## AIC: 28.646
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   7.1957        21     24.646 0.007308 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7074  -0.8997  -0.4011   0.8380   1.6964  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.119448   1.099568   1.928   0.0539 .
    ## Turn_Count  -0.017488   0.007921  -2.208   0.0273 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.673  on 21  degrees of freedom
    ## AIC: 28.673
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   7.1683        21     24.673  0.00742 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0147  -0.8845   0.3219   0.6487   1.7322  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  3.119872   1.411903   2.210   0.0271 *
    ## Turn_Count  -0.023233   0.009921  -2.342   0.0192 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 21.732  on 21  degrees of freedom
    ## AIC: 25.732
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   10.109        21     21.732 0.001476 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7731  -0.9442   0.4738   0.7880   1.6364  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.271299   1.085433   2.093   0.0364 *
    ## Turn_Count  -0.017586   0.007857  -2.238   0.0252 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.419  on 21  degrees of freedom
    ## AIC: 28.419
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   7.4222        21     24.419 0.006442 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7151  -0.9056  -0.4040   0.8320   1.6893  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.137169   1.114052   1.918   0.0551 .
    ## Turn_Count  -0.017497   0.008016  -2.183   0.0291 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.849  on 21  degrees of freedom
    ## AIC: 28.849
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.9927        21     24.849 0.008184 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7147  -0.8543  -0.3542   0.7750   1.7682  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.204985   1.086883   2.029   0.0425 *
    ## Turn_Count  -0.018793   0.008113  -2.316   0.0205 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.670  on 21  degrees of freedom
    ## AIC: 27.67
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   8.1711        21     23.670 0.004256 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7970  -0.7721  -0.2595   0.7258   1.4733  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.59599    1.21790   2.132   0.0330 *
    ## Turn_Count  -0.02270    0.00956  -2.375   0.0176 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 21.986  on 21  degrees of freedom
    ## AIC: 25.986
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   9.8557        21     21.986 0.001693 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7259  -0.9093  -0.4033   0.8245   1.6865  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.16805    1.12623   1.925   0.0542 .
    ## Turn_Count  -0.01763    0.00812  -2.171   0.0299 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.991  on 21  degrees of freedom
    ## AIC: 28.991
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.8503        21     24.991 0.008863 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7175  -0.8501  -0.3492   0.7734   1.7755  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.220102   1.090310   2.036   0.0417 *
    ## Turn_Count  -0.018961   0.008168  -2.321   0.0203 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.584  on 21  degrees of freedom
    ## AIC: 27.584
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   8.2571        21     23.584 0.004059 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8026  -0.9205   0.4542   0.7691   1.6377  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.36440    1.11124   2.128   0.0334 *
    ## Turn_Count  -0.01810    0.00792  -2.285   0.0223 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.012  on 21  degrees of freedom
    ## AIC: 28.012
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   7.8293        21     24.012 0.005141 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0085  -0.8834   0.3197   0.6466   1.7346  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  3.134679   1.417865   2.211   0.0270 *
    ## Turn_Count  -0.023339   0.009966  -2.342   0.0192 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 21.692  on 21  degrees of freedom
    ## AIC: 25.692
    ## 
    ## Number of Fisher Scoring iterations: 5
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   10.149        21     21.692 0.001444 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7716  -0.9504   0.4748   0.7890   1.6366  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.266740   1.084583   2.090   0.0366 *
    ## Turn_Count  -0.017565   0.007859  -2.235   0.0254 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.443  on 21  degrees of freedom
    ## AIC: 28.443
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   7.3981        21     24.443 0.006529 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8402  -0.9188   0.4289   0.7462   1.6475  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.490739   1.156879   2.153   0.0313 *
    ## Turn_Count  -0.018884   0.008156  -2.315   0.0206 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.564  on 21  degrees of freedom
    ## AIC: 27.564
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1    8.277        21     23.564 0.004015 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7838  -0.9192   0.4667   0.7810   1.6358  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.30414    1.09309   2.108   0.0350 *
    ## Turn_Count  -0.01775    0.00786  -2.259   0.0239 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.261  on 21  degrees of freedom
    ## AIC: 28.261
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   7.5802        21     24.261 0.005901 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7226  -0.9085  -0.4038   0.8267   1.6869  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.158373   1.123375   1.921   0.0547 .
    ## Turn_Count  -0.017582   0.008093  -2.172   0.0298 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.955  on 21  degrees of freedom
    ## AIC: 28.955
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.8864        21     24.955 0.008685 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7511  -0.9247   0.4830   0.8059   1.6697  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.23197    1.10432   2.021   0.0433 *
    ## Turn_Count  -0.01777    0.00819  -2.170   0.0300 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.075  on 21  degrees of freedom
    ## AIC: 29.075
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.7667        21     25.075 0.009287 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7800  -0.9187   0.4693   0.7835   1.6358  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.292106   1.090024   2.103   0.0355 *
    ## Turn_Count  -0.017690   0.007856  -2.252   0.0243 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.316  on 21  degrees of freedom
    ## AIC: 28.316
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1    7.525        21     24.316 0.006085 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7511  -0.9249   0.4831   0.8059   1.6694  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.231427   1.104072   2.021   0.0433 *
    ## Turn_Count  -0.017763   0.008188  -2.169   0.0301 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 25.072  on 21  degrees of freedom
    ## AIC: 29.072
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.7696        21     25.072 0.009273 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7501  -0.9308   0.4853   0.8056   1.6602  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.22033    1.09657   2.025   0.0429 *
    ## Turn_Count  -0.01760    0.00813  -2.164   0.0304 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 24.986  on 21  degrees of freedom
    ## AIC: 28.986
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   6.8551        21     24.986 0.008839 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call:
    ## glm(formula = formula, family = binomial(link = "logit"), data = train, 
    ##     na.action = na.pass)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7261  -0.8386  -0.3357   0.7683   1.7957  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  2.264587   1.102066   2.055   0.0399 *
    ## Turn_Count  -0.019439   0.008335  -2.332   0.0197 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 31.841  on 22  degrees of freedom
    ## Residual deviance: 23.352  on 21  degrees of freedom
    ## AIC: 27.352
    ## 
    ## Number of Fisher Scoring iterations: 4
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
    ##            Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
    ## NULL                          22     31.841            
    ## Turn_Count  1   8.4893        21     23.352 0.003572 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](ROC_curves_files/figure-markdown_strict/predicting%20SM%20dx%20by%20conversational%20turns-1.png)
