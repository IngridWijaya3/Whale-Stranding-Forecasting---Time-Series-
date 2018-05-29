require(xts)
library(zoo)
library(forecast)
library(plotly)
library(reshape2)
library(tseries)
library(tscount)
library(TSA)
library(timeSeries)
library(smooth)
library(Mcomp)
library(splines)
library(parallel)
library(lubridate)
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
setDefaultCluster(cl)
par(mfrow=c(1,1)) 
weeklywhalestranding <- read.csv("combinecountsweekly.csv",header=FALSE, sep=",")
weeklywhalestranding$V1 <- as.Date(weeklywhalestranding$V1, format= "%Y-%m-%d")
weeklywhalestranding<-weeklywhalestranding[(weeklywhalestranding$V1 < '2017-01-01'),]
weeklytimseries <- ts(weeklywhalestranding$V2,start=c(1840,01,06),end = c(2016,12,26) , frequency=52)
#365.25/7 --> better for the plot 

weeklytimseries2<-ts(weeklywhalestranding$V2, start=decimal_date(ymd("1840-01-06")) ,frequency=365.25/7) 
weeklywhalestrandingxts<-xts(weeklywhalestranding[-1], order.by = as.Date(weeklywhalestranding[,1], "%Y-%m-%d"))


hw<-HoltWinters( weeklytimseries ,beta=FALSE,  seasonal = c("additive", "multiplicative"))

plot(hw)
plot(fitted(hw))
hw
plot.ts( weeklytimseries)
plot.ts( weeklytimseries2)
v = filter(weeklywhalestranding$X1.0, sides=2, rep(1/4,4),method="convolution") 
lines(v,lty=2)
plot.xts(weeklywhalestrandingxts)
plot.xts( weeklywhalestrandingafter1918xts)
plot(weeklytimseries2after1918)
weeklyeacf<-eacf(weeklywhalestranding$X1.0,100,100)

pacf(weeklywhalestranding$X1.0,lag.max = 200)
pacfresult<-pacf(weeklywhalestranding$X1.0,lag.max = 400)
acf(weeklywhalestranding$X1.0,lag.max = 400)
ar(weeklywhalestranding$X1.0)

which(pacfresult$acf>0.025)
pacfresult$acf>0.025
plot(decompose(weeklytimseries2))
plot(stl(weeklytimseries2, s.window = "periodic") )

#small p value indicates the possibility of non-zero autocorrelation
Box.test(weeklywhalestranding$X1.0,lag=2,type='Ljung') 
Box.test(weeklywhalestranding$X1.0,lag=5,type='Ljung') 
adf.test(weeklywhalestranding$X1.0, k=11)
adf.test(weeklywhalestranding$X1.0, k=28)
kpss.test(weeklywhalestranding$X1.0, null="Trend")


weeklystarndingfit <- tsglm(ts=weeklytimseries2, model=list(past_obs=c(2,3,4,5,11,96)),distr = "poisson")
summary(weeklystarndingfit)
weeklystarndingfitpastmean <- tsglm(ts=weeklytimseries2, model=list(past_obs=c(1),past_mean=c(2,3,4,5,11,23,26,96)),distr = "poisson")
summary(weeklystarndingfitpastmean)


weeklystarndingfitnegativebinomial<- tsglm(ts=weeklytimseries2, model=list(past_obs=c(2,3)),distr = "nbinom")
summary(weeklystarndingfitnegativebinomial)

testlagandAICResultdf <- data.frame(past_obs=integer(),past_mean=double(),AIC=double(), BIC=double(),AIC_pastmean=double() ,  BIC_pastmean=double()) 

tsglmresult<-tsglm(ts=weeklytimseries2, model=list(past_obs=c(2)),distr = "nbinom")
tsglmresult<-tsglm(ts=weeklytimseries2, model=list(past_obs=c(2)),distr = "poisson")
summary(tsglmresult)
for(i in seq(from=1, to=110, by=1)){
  tsglmresult<-tsglm(ts=weeklytimseries2, model=list(past_obs=c(i)),distr = "nbinom")
  tsglmresult2<-tsglm(ts=weeklytimseries2, model=list(past_obs=c(1),past_mean=c(i)),distr = "nbinom")
  print(summary(tsglmresult))
   testlagandAICResultdf <- rbind(testlagandAICResultdf, c(i,AIC(tsglmresult), BIC(tsglmresult), i, AIC(tsglmresult2) , BIC(tsglmresult2)))
}
colnames(testlagandAICResultdf) <- c("Past Observation", "AIC","BIC","Past Mean","AIC","BIC")
write.csv(testlagandAICResultdf,"WhaleStranding/pastobservationaAICandBICResult2.csv")
stopCluster(cl)
weeklywhalestranding[(weeklywhalestranding$V2 > 100),]


weeklystarndingfitnegativebinomial<- tsglm(ts=weeklytimseries2, model=list(past_obs=c(2),past_mean=c(103)),distr = "nbinom")
summary(weeklystarndingfitnegativebinomial)

interventiondetecttest <- list()
listoftaus <- c()
for (indexintervention in which(weeklywhalestranding$V2 > 100)){

  resulttest1<-interv_test(weeklystarndingfitnegativebinomial, tau =indexintervention, delta = 0.8, est_interv = TRUE)
  resulttest2<-interv_test(weeklystarndingfitnegativebinomial, tau =indexintervention, delta = 1, est_interv = TRUE)
  if(resulttest1$p_value<0.05)
  {
  interventiondetecttest[[length(interventiondetecttest)+1]] <- resulttest1
  }
  if(resulttest2$p_value<0.05)
  {
  interventiondetecttest[[length(interventiondetecttest)+1]] <- resulttest2
  }
  if(resulttest1$p_value<0.05 || resulttest2$p_value<0.05)
  {
    listoftaus<- append(listoftaus, indexintervention)
  }
}

listofAICs=list()
indextau<-1
for( fittingresult in interventiondetecttest)
{
  #print(fittingresult$fit_interv)
 
 # if((indextau%%2) != 0)
 # {
    print(indextau%%2)
    aicandtaun<-list()
    aicandtaun$AIC<-AIC(fittingresult$fit_interv)
    aicandtaun$BIC<- BIC(fittingresult$fit_interv)
    print(indextau%%2)
    fittingresult2<-interventiondetecttest[[indextau+1]]
    aicandtaun$AIC2<-AIC(fittingresult2$fit_interv)
    aicandtaun$BIC2<-BIC(fittingresult2$fit_interv)
    aicandtaun$TAU<-listoftaus[indextau]
    listofAICs[[length(listofAICs)+1]] <- aicandtaun
    print(aicandtaun$TAU)
 # }
  
  indextau<-indextau+1
  
  #fittingresult$p_value<0.05
}


sortedaicandbicresult<-c()
minaicandbic<-5000000
mintau<-0
#indexofitem<-1
for( resultforaicandbic2 in listofAICs)
{
  minaicandbic<-5000000
  mintau<-0
  for( resultforaicandbic in listofAICs)
  {
    if ((resultforaicandbic$TAU %in% sortedaicandbicresult)==FALSE)
    {
      if(resultforaicandbic$AIC > resultforaicandbic$AIC2)
      {
        tempAIC=resultforaicandbic$AIC2
      }
      else 
      {
        tempAIC=resultforaicandbic$AIC
      }
      if(minaicandbic>tempAIC  )
      {
        minaicandbic=tempAIC
        mintau<-resultforaicandbic$TAU
      }
      #if(resultforaicandbic[[]])
      #sortedaicandbicresult
      print(resultforaicandbic)
    }
  }
  
  sortedaicandbicresult <- append(sortedaicandbicresult, mintau)
}
AIC: 20722.47 
BIC: 20815.24 
QIC: 181576.4 

AIC: 20690.05 
BIC: 20775.69 
QIC: 177548.7 

AIC: 21719.95 
BIC: 21798.45 
QIC: 185504.5 

AIC: 20663.59 
BIC: 20742.09 
QIC: 174764.2 

AIC: 20542.49 
BIC: 20606.72 
QIC: 162696.1
#4070, 5388,7213,7597,8027,8159 99 93 83 89  68, 75,77,82 68,75,82,99)

regressors <- cbind(linearTrend = seq(along = weeklywhalestranding$X1.0)/(365.25/7))
regressors_1982 <- window(regressors, start = 1840, end = 1840 + 51/(365.25/7))
interventions <- interv_covariate(n = length(weeklywhalestranding$X1.0), tau = c(4070, 5388,7213,7597),delta = c(0.9,0.9,0.8,0.8))
#interventions 21,31
#interventions <- interv_covariate(n = length(after1918$X1.0), tau = c(5388,7213,7597),delta = c(0.9,0.9,0.8,0.8))
weeklystarndingfitnegativebinomial<- tsglm(ts=weeklytimseries2, model=list( past_obs=c(2,3,7)),distr = "nbinom")
summary(weeklystarndingfitnegativebinomial)
interv_test(weeklystarndingfitnegativebinomial, tau =4070, delta = 0.8, est_interv = TRUE)
#s2 s20 s21 s25 s13 s18 s16 s7
#s3 s7 s47 s2 s52 s36 s50 s41 s27 s1 s49  s8 s23 s14 s16 s21 s19
weeklystarndingfitnegativebinomialwithinterventions<- tsglm(ts=weeklytimseries2, model=list( past_obs=c(2,3,7) ),distr = "nbinom",xreg =interventions)
summary(weeklystarndingfitnegativebinomialwithinterventions)

acf(residuals(weeklystarndingfitnegativebinomialwithinterventions), main = "ACF of response residuals")
adf.test(residuals(weeklystarndingfitnegativebinomialwithinterventions))

predicitons<-predict(weeklystarndingfitnegativebinomialwithinterventions, n.ahead = 12, level = 0.9, global = TRUE,  B = 2000)
predicitons$pred
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
weeklywhalestranding2017 <- read.csv("WhaleStranding/combinecountsweekly.csv",header=TRUE, sep=",")
weeklywhalestranding2017$X1840.01.06 <- as.Date(weeklywhalestranding2017$X1840.01.06, format= "%Y-%m-%d")
weeklywhalestranding2017<-weeklywhalestranding2017[(weeklywhalestranding2017$X1840.01.06 > '2017-01-01'),]
actual<-weeklywhalestranding2017$X1.0[1:12]
error <- actual - c(predicitons$pred[1],predicitons$pred[2],predicitons$pred[3],predicitons$pred[4],predicitons$pred[5],predicitons$pred[6],predicitons$pred[7],predicitons$pred[8],predicitons$pred[9],predicitons$pred[10],predicitons$pred[11],predicitons$pred[12])
error
predicitons$pred
actual

rmse(error)
mae(error)

weeklywhalestranding[(weeklywhalestranding$X1.0 > 200),]
weeklywhalestranding[((weeklywhalestranding$X1.0 > 100) & (weeklywhalestranding$X1.0 < 200)),]


plot(weeklytimseries2)
lines(meanf(dj2,h=42)$mean, col=4)

beer3 <- window(ausbeer, start=2006)
accuracy(beerfit1, beer3)
dj2 <- window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="", xlab="Day", xlim=c(2,290))
lines(meanf(dj2,h=42)$mean, col=4)
lines(rwf(dj2,h=42)$mean, col=2)
lines(rwf(dj2,drift=TRUE,h=42)$mean, col=3)
legend("topleft", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
lines(dj)