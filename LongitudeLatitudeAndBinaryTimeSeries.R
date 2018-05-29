######################vlmc
la<-read.csv("data/weeklycountandbinary.csv")
testt<-la[4384:9183,3]
trainn<-la[9184:9288,3]

m2<-vlmc(testt)
p2<-predict(m2,trainn)
nn<-apply(p2,1, which.max)
result<-as.numeric(nn)-1
j<-0
for (i in 2:length(result))
{
  if(result[i]==trainn[i])
    j<-j+1
}
j/length(result)

############################bsts
library(bsts)
############################################

yy<-read.csv("data/weeklycountandbinary.csv")

testy<-as.data.frame(la[4384:9180,-1])
trainy<-as.data.frame(la[9181:9288,-1])
ss <- AddLocalLevel(list(),sigma.prior = SdPrior(sigma.guess = 0.7071068,sample.size = 1,upper.limit = 1),initial.state.prior = NormalPrior(0, 10))
ts.model <- bsts(StrandingHappened ~ ., ss, data = testy, niter = 5000,family = "logit", expected.model.size = 1)
pred1 <- predict(ts.model,trainy[,1])
#pred1$mean[pred1$mean>0.2]<-1
#pred1$mean[pred1$mean<=0.2]<-0
plot(pred1)

j<-0
for (i in 1:length(pred1$median))
{
  if(pred1$median[i]==trainy[i,2])
    j<-j+1
}
j/length(pred1$median)

################################latutude and longitude
library(RCurl)
library(forecast)
y<-read.csv("data/latlongdata.csv")

y$Lat[is.na(y$Lat)] <- 0 #Mode(y$Lat[!is.na(y$Lat)])
y$Long[is.na(y$Long)] <- 0#mean(y$Long[!is.na(y$Long)])
##############################            
adf.test(y[,4])
m1<-arima(y[28508:length(y[,4]),4], order=c(4,1,0))
m2<-arima(y[28508:length(y[,4]),3], order=c(1,1,0))
predict(m1,n.ahead = 100)
predict(m2,n.ahead = 100)
acf(m1$residuals)