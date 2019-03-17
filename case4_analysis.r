setwd("~/Desktop/math189/case4")
data <- read.table("data.txt", header=TRUE)
options(scipen=999)

# Aim of this lab is to provide a simple procedure for converting gain 
# into density when the gauge is in operation. Keep in mind that the 
# experiment was conducted by varying density and measuring the response in gain, 
# but when the gauge is ultimately in use, the snow-pack density is to be 
# estimated from the measured gain.

# Part A [Fitting]
# Use the data to fit the gain, or a transformation of gain to density. Try sketching the least
# squares line on a scatter plot.
# a. Do the residuals indicate any problems with the fit ?
# b. If the densities f the polyethylene blocks are not reported exactly, how might this affect the fit ?
# c. What if the blocks of polyethylene were not measured in random order?

# Part B [Predicting]
# Ultimately we are interested in answering questions such as: Given a gain reading of 38.6,
# what is the density of the snow-pack? or Given a gain reading of 426.7, what is the density of 
# snow-pack? These two numeric values, 38.6 and 426.7, were chosen because they are the average gains 
# for the 0.508 and 0.001 densities, respectively.
# a. Develop a procedure for adding bands around your least squares line that can be used to 
# make interval estimates for the snow-pack density from gain measurements. Keep in mind how the 
# data were collected: several measurements of gain were taken for polyenthylene blocks of known density.

# Part C [Cross-Validation]
# To check how well your procedure works,omit the set of measurements 
# corresponding to the block of density 0.508, apply your ”estimation”/calibration 
# procedure to the remaining data, and provide an interval estimate for the density 
# of a block with an average reading of 38.6. Where does the actual density fall 
# in the interval? Try the same test, for the set f measurements at the 0.001 density.

# The data available here consists of 10 measurements for each of 9 densities in grams per 
# cubic centimeter of polyethylene.

data$gain_log=log(data$gain)
fit=lm(density~gain_log,data)
plot(data$gain_log,data$density,main="Plotting log(gain) and density",xlab="Logged Gain",ylab="Density")
abline(fit,col="red")

plot(fit)
plot(fit$fitted.values,fit$residuals)
qqnorm(abs(fit$residuals),main="Residuals in Absolute Value")
qqline(abs(fit$residuals))
max(fit$residuals)
hist(fit$residuals,main="Histogram of Residuals",xlab="Residuals")
summary(fit)

min(log(data$gain))

plot(data$gain_log,round(data$density,1),main="Rounded Density",xlab="Logged Gain",ylab="Rounded Density",col="red")
fit_rounded=lm(round(density,1)~gain_log,data)
abline(fit,col='black')
abline(fit_rounded,col="red")
points(data$gain_log,data$density,col="black")


n=length(data$density)
model=lm(density ~ gain_log,data)
b0=model$coefficients[1]
b1=model$coefficients[2]

t=qt(0.975,n-2)
x_s=1:max(data$gain_log)
y_f=b1*x_s+b0
se=sqrt(sum((data$density-y_f)^2)/(n-2))*sqrt(1/n+(data$gain_log-mean(data$gain_log))^2/sum((data$gain_log-mean(data$gain_log))^2))
x_s2=1:max(data$gain_log+1)
y_f2=b1*x_s2+b0
upper=y_f2+t*se;lower=y_f2-t*se
bands=data.frame(Lower_Confidence_Bound=lower,Upper_Confidence_Bound=upper)
plot(data$gain_log,data$density,main="Fit Plot",xlab="Logged Gain",ylab="Density")
lines(y_f2);lines(bands$Lower_Confidence_Bound,col='red');lines(bands$Upper_Confidence_Bound,col='red')

data2=subset(data,data$density!=0.508)
d=data.frame(gain_log=log(38.6))
fit2=lm(density~gain_log,data2)
predict(fit2,newdata=d,interval='prediction')
# fit       lwr      upr
# 1 0.5083037 0.4771654 0.539442
data3=subset(data,data$density!=0.001)
d=data.frame(gain_log=log(38.6))
fit3=lm(density~gain_log,data3)
predict(fit3,newdata=d,interval='prediction')
# fit       lwr       upr
# 1 0.5086094 0.4793015 0.5379172

# additional hypothesis 
d=read.table("FR_20190301.dat",sep=";",header=TRUE)
# clear NA's
d=subset(d,abs(d$Ts)<999)

max(d$Lat)# 87.669
subset(d$BuoyID, d$Lat==87.669) #300234061872720
min(d$Lat)# 25.775
subset(d$BuoyID, d$Lat==25.775) 

A=subset(d, d$BuoyID==300234062854730)
summary(A$Ts)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.58   22.59   22.61   22.66   22.72   22.79 
B=subset(d, d$BuoyID==300234061872720)
summary(B$Ts)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -23.99  -23.70  -23.40  -23.09  -22.51  -21.72 
plot(A$Hour,A$Ts,col="red",main="A's Hourly Surface Temperature",xlab="Hour",ylab="Temperature")
plot(B$Ts,col="blue",main="B's Hourly Surface Temperature",xlab="Hour",ylab="Temperature")

length(A$Ts);length(B$Ts)
A_tc=numeric(23);B_tc=numeric(23)
for(i in 2:24){
  A_tc[i-1]=A$Ts[i]-A$Ts[i-1]
  B_tc[i-1]=B$Ts[i]-B$Ts[i-1]
}
A_mtc=mean(A_tc)
B_mtc=mean(B_tc)


