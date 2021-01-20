#Install neccessary packages
install.packages("dplyr")
install.packages("e1071")
install.packages("DAAG")

#Import neccessary packages
library(dplyr)
library(e1071)
library(DAAG)

data(Seatbelts)
dat <-Seatbelts
#View structure of data
print(dat)

#Limit variables to two columns of interest
df <-data.frame(Seatbelts[,c("DriversKilled","kms")])

#Scatter Plot analysis
scatter.smooth(x=df$kms, y=df$DriversKilled, main="kms ~ DriversKilled" )

#BoxPlot - Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
# box plot for 'DriversKilled'
boxplot(df$DriversKilled, main="DriversKilled", sub=paste("Outlier rows: ", 
                                                          boxplot.stats(df$DriversKilled)$out))
# box plot for 'kms'
boxplot(df$kms, main="kms", sub=paste("Outlier rows: ", boxplot.stats(df$kms)$out)) 

#Density plot - Correlation
# divide graph area in 2 columns
par(mfrow=c(1, 2)) 
# density plot for 'kms'
plot(density(df$kms), main="Density Plot: kms", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$kms), 2)))  
polygon(density(df$kms), col="red")

# density plot for 'DriversKilled'
plot(density(df$DriversKilled), main="Density Plot: DriversKilled", 
  ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$DriversKilled), 2)))  
polygon(density(df$DriversKilled), col="red")



## calculate correlation between DriversKilled and kms
cor(df$kms, df$DriversKilled)  

## build linear regression model on full data
linearMod <- lm(DriversKilled ~ kms, data=Seatbelts)  
print(linearMod)


#Linear Regression Diagnostics
summary(linearMod)  # model summary

#The p Value: Checking for statistical significance
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
print(modelCoeffs)
beta.estimate <- modelCoeffs["kms", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["kms", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(Seatbelts)-ncol(Seatbelts))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

print(t_value)
print(p_value)
print(f_statistic)
print(model_p)

#AIC and BIC
AIC(linearMod)  
BIC(linearMod)  

#Making predictions
# Step 1 Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(Seatbelts), 0.8*nrow(Seatbelts))  # row indices for training data
trainingData <- as.data.frame.matrix(Seatbelts[trainingRowIndex, ])  # model training data
testData  <- as.data.frame.matrix(Seatbelts[-trainingRowIndex, ])   # test data

#Step 2 Build the model on training data 
lmMod <- lm(DriversKilled ~ kms, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance


#Step 3: Review diagnostic measures.
summary (lmMod)

#Step 4 Calculate prediction accuracy and error rates
# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=testData$DriversKilled, predicteds=distPred))  
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

#Min Max accuracy and MAPE:
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
print(min_max_accuracy) # min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
print(mape) # mean absolute percentage deviation


