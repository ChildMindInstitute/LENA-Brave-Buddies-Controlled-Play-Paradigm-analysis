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
        #print(summary(gl_model))
        #print(anova(gl_model, test="Chisq"))
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

![](ROC_curves_files/figure-markdown_strict/predicting%20SM%20dx%20by%20vocalization%20counts-1.png)

predicting SM dx by vocalization duration

      SM_DX_ROC_plot(data, "SM_dx~Child_Voc_Duration", "SM diagnosis by vocalization duration")

![](ROC_curves_files/figure-markdown_strict/predicting%20SM%20dx%20by%20vocalization%20duration-1.png)

predicting SM dx by conversational turns

      SM_DX_ROC_plot(data, "SM_dx~Turn_Count", "SM diagnosis by conversational turn count")

![](ROC_curves_files/figure-markdown_strict/predicting%20SM%20dx%20by%20conversational%20turns-1.png)
