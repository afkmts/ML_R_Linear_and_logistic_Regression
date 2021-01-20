#install neccessary packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rms",dependencies = TRUE)

#import neccessary packages
library(dplyr)
library(ggplot2)
library(rms)
require(sqldf)
library(popbio)
#===========================================================================================
#data(storms)
str(storms) #to view structure of data
dat <-storms

#1 Limit data to two variables
df <-data.frame(sqldf("select name,long  from dat  where name='Amy' OR name='Bob'"))
str(df) 

#1 t-test statistics 
ggplot(df, aes(name, long)) +
  geom_boxplot()

# Assign numeric values to classes(Amy,Bob)
df$name <-ifelse(df$name=="Amy",1,0)
str(df)
name_code <-df$name
Longitude <-df$long

#2 plotting graph
plot(df$long,jitter(df$name,0.15),pch=19,xlab="long",ylab="name")

#3 Building the model
model <-glm(name~long,data=df,family=binomial)
model

#4 making predictions
#create a sequence of test data set
sprintf("Min Longuitude:%f Maximum Loguitude:%f",min(Longitude),max(Longitude))

xv <-seq(min(Longitude),max(Longitude),0.01)
yv <-predict(model,list(long=xv),type="response")

plot(Longitude,jitter(name_code,0.15),pch=19,xlab="Longuitude",ylab="Storm Name (0 - Bob, 1 - Amy)")
lines(xv,yv,col="red")

logi.hist.plot(Longitude,name_code,boxp=FALSE,type="count",col="gray"
               ,xlabel="Longitude",ylabel="Storm Name (0 - Bob, 1 - Amy)")

#Assessment of logistic regression
model$null.deviance
model$deviance
modelChi <- model$null.deviance - model$deviance
pseudo.R2 <- modelChi / model$null.deviance
pseudo.R2

#Alternative pseudo R2
lrm(name ~ long, df)

#Assessing model significance
Chidf <- model$df.null - model$df.residual
chisq.prob <- 1 - pchisq(modelChi, Chidf)
chisq.prob

#Assessing parameter significance
summary(model)
#===========================================================================================
#Multi-variable model adding new variable pressure
# Grab variables of interest
df2 <-data.frame(sqldf("select name,long,lat  from dat  where name='Amy' OR name='Bob'"))
df2$name <-ifelse(df$name=="Amy",1,0)
str(df2) 

# Run regression model
model2 <-glm(name ~ long + lat, df2,family=binomial,control = list(maxit = 50))


# Compute pseudo R-square
modelChi <- model2$null.deviance - model2$deviance
pseudo.R2 <- modelChi / model2$null.deviance
pseudo.R2

# Compute the pseudo p-value
Chidf <- model2$df.null - model2$df.residual
modelChi <-model2$null.deviance - model2$deviance
modelChi
1 - pchisq(modelChi, Chidf)


# Assess each parameter's significance
summary(model2)
#============================================================================================


