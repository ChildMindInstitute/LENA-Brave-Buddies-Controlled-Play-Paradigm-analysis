attach(LENA_Coding_Counts_v2)
par(pty="s")
plot(LENA.count, Avg.Coding.count, main="LENA v. Rater Vocalization Counts", 
     xlab="LENA Counts", ylab="Rater Counts", pch=19, xlim=c(0, 200), ylim=c(0, 200))
abline(lm(Avg.Coding.count~LENA.count), col="red") # regression line (y~x)
abline(0,1, untf=FALSE, col="green")
#linear fitting
linear.model <-lm(Avg.Coding.count ~ LENA.count)
summary(linear.model) #explains 25% of variance
#quadratic fitting
LENA.count2 <- LENA.count^2
quadratic.model <- lm(Avg.Coding.count ~ LENA.count+LENA.count2)
summary(quadratic.model) #explains 28% of variance
xvalues <- seq(0, 200, 25)
predictedy <- predict(quadratic.model, list(LENA.count=xvalues, LENA.count2=xvalues^2))
plot(LENA.count, Avg.Coding.count, pch=16, xlab = "LENA Counts", ylab = " Human Counts", cex.lab = 1.3, col = "blue",
     xlim=c(0, 200), ylim=c(0, 200))
lines(xvalues, predictedy, col="darkgreen")
#smooth line fitting
ggplot(LENA_Coding_Counts_v2, aes(LENA.count,Avg.Coding.count)) + geom_point() +geom_smooth()
#switch variables
ggplot(LENA_Coding_Counts_v2, aes(Avg.Coding.count, LENA.count)) + geom_point() +geom_smooth()

#A1 block only
attach(LENA_Rater_Counts_A1only)
plot(LENA.count, Avg.Coding.count, main="LENA v. Rater Vocalization Counts: A1 Block", 
     xlab="LENA Counts", ylab="Rater Counts", pch=19, col="darkblue", xlim=c(0, 150), ylim=c(0,150))
abline(lm(Avg.Coding.count~LENA.count), col="orange") # regression line (y~x)
abline(0, 1, untf=FALSE, col="green")

require(ggplot2)
qplot(LENA.count, Avg.Coding.count, data = LENA_Coding_Counts_v2, color=LENA_Coding_Counts_v2$Block,
      xlim=c(0, 200), ylim=c(0, 200), main="LENA v. Rater Counts by Block")

qplot(LENA.count, Avg.Coding.count, data = LENA_Rater_Coding_bydx, color=LENA_Rater_Coding_bydx$DX.status,
      xlim=c(0, 200), ylim=c(0, 200), main="LENA v. Rater Counts by Dx")

#scatter of means
means <- LENA_Coding_Counts_v2[, c(1, 8, 10)]
means <- na.omit(means)
means[,4] <- means$Mean.LENA-means$Mean.Coder
means[,5] <- 1
names(means) <- c("ID", "Mean.LENA", "Mean.Coder", "Diff.Means", "category")
plot(means$Diff.Means~means$category, main="Difference between mean LENA count and Coder count", 
     xlab="Participants", ylab="Mean LENA count - Mean Coder count", xaxt='n')
abline(0, 0, col="blue")

