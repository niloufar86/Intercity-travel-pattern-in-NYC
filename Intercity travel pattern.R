setwd("C:/Users/Niloofar/Advanced data analysis CE G1101/Project/5-19-2017")

library(maptools)
library(rgdal)
library(ggplot2)
library(corrplot)
library(biwavelet)

data=read.csv("matrix1.csv",header=TRUE)
data1=as.matrix(as.numeric(data1))
data_oct11=read.csv("significant-counties-afteroct10.csv",header=TRUE)
data_oct11=as.matrix(data_oct11)

plot.ts(data[,6],col='red',lwd=2, main="Manhattan")

M<-cor(data)
b=princomp(covmat=M)
loading=b$loadings
loading1=b$loadings[1:15,1]
score1=b$scores=as.matrix(data)%*%loading1
score2=as.matrix(data)%*%loading[,2]

corrplot(M, method="square")

c=princomp(data)
loading.data=c$loadings
score.data=c$scores
c<-lable #find
summary(c)
plot(c$loadings[,1],pch=16,,xlab='Borough',main='PC1- loadings',col='skyblue3')
lines(c$loadings[,1])
plot(c$loadings[,2],pch=16,xlab='Borough',main='PC2- loadings',col='skyblue4')
lines(c$loadings[,2])
plot(b,main='PCA')


plot.ts(score1,col='red',lwd=2)
biplot(b) #find

spec.ar(c$loadings[,1])
spec.ar(score.data[,1])
ar(score.data[,1])
spec.ar(c$loadings[,2])
spec.ar(score.data[,2])
ar(score.data[,2])

spec.pgram(score.data[,1])
k = kernel("daniell", 2)
y=spec.pgram(score.data[,1], k, taper=0, log = "no")

spec.pgram(data[,6])
ar(data1)
k = kernel("daniell", 2)
y=spec.pgram(data[,6], k, taper=0, log = "no")

data1=read.csv("significant-counties.csv",header=TRUE)
plot.ts(data1,col='red',lwd=2, main="Significant Counties")

par(mfrow=c(1,1))
y = spec.pgram(data_oct11,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(data_oct11,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

par(mfrow=c(1,1))
acf(loading.data[,1])

par(mfrow=c(1,1))
acf(data1)
pacf(data1)

library("xts")
library("TTR")

dataSMA <- SMA(data1,n=2)
plot.ts(dataSMA,lwd=3,col="deeppink")
plot.ts(data1,lwd=2,lty=5,ylab="App Usage")
lines(dataSMA,lwd=3,col="deeppink")
legend("topright", c("Moving Average Line","Actual Usage"), col = c('deeppink', 'black'),
       text.col = "black", lty = c(1,2), lwd= c(2,3),
       merge = F, bg = "gray90")

data2=score1
data_t= ts(data_oct11[,1], frequency=3, start=c(1,1))
a <- decompose(data_t)
summary(a)
par(mfrow=c(4,1))
plot(data_t,col='red2',ylab="App Usage")
plot(a$trend,ylab="trend")
plot(a$seasonal,ylab="seasonal")
plot(a$random,ylab="random")


par(mfrow=c(4,2))
plot(data_t)
plot(data_t2)
plot(a$trend)
plot(a2$trend)
plot(a$seasonal)
plot(a2$seasonal)
plot(a$random)
plot(a2$random)

par(mfrow=c(1,1))
a_trend<-data_t-(a$seasonal)
plot(a_trend)
plot(a_trend,lwd=2,col='red',ylim=c(-1000,3500))
lines(a$seasonal,pch=16,lwd=2,lty=2,col='grey')

par(mfrow=c(1,2))
plot(data_t)
plot(a_trend)

x = data1
qqnorm(data1$total4)
qqline(data1$total4)

before=data1[1:11,]
after=data1[11:31,]
wilcox.test(before,after)

x=c(1:21)
x=as.matrix(x)
y=data_oct11[,1]
y=as.matrix(y)
model1=lm(y~x)
summary(model1)
par(mfrow=c(1,1))
plot(x,model1$residuals,main="Residuals From Best Linear Fit",ylab="Residuals")
plot(acf(model1$residuals,plot=F), main="Autocorrelation Function-Residuals",lwd=2)

model1res=lm(model1$residuals~x)
summary(model1res)

q <- seq(from=1, to=21)
v=lm(y~poly(q,15))
summary(v)

yy=y[2:21,]
yyy=y[1:20,]
xx=x[2:21,]
model2=lm(yy~xx+yyy)
summary(model2)
plot(xx,model2$residuals,main="Residuals From Best Linear Fit",ylab="Residuals")
plot(acf(model2$residuals,plot=F), main="Autocorrelation Function-Residuals",lwd=2)

model2res=lm(model2$residuals~xx)
summary(model2res)

model2a=lm(yy~yyy)
summary(model2a)
plot(xx,model2a$residuals,main="Residuals From Best Linear Fit",ylab="Residuals")
plot(acf(model2a$residuals,plot=F), main="Autocorrelation Function-Residuals",lwd=2)
model2ares=lm(model2a$residuals~xx)
summary(model2ares)

modelc=lm(yy~sin(8*(pi/180)*xx)+yyy)
summary(modelc)

modelc1=lm(y~sin(8*(pi/180)*x))
summary(modelc1)

modelc1a=lm(y~cos(8*(pi/180)*x)+x)
summary(modelc1a)

modelc1b=lm(y~cos(8*(pi/180)*x))
summary(modelc1b)
modelc1bres=lm(modelc1b$residuals~x)
summary(modelc1bres)

modelc1bb=lm(y~cos(8*(pi/180)*x))
summary(modelc1bb)

modeld=lm(yy~cos(8*(pi/180)*xx)+yyy)
summary(modeld)
modeldres=lm(modeld$residuals~xx)
summary(modeldres)
plot(xx,modeld$residuals,main="Residuals From Best Linear Fit",ylab="Residuals")
abline(modeld$coefficients,lw=2,lt=2)
lines(lowess(yc2,f=2/3),lw=2,col=2)
plot(acf(modeld$residuals,plot=F), main="Autocorrelation Function-Residuals",lwd=2)

plot(x,data1$total4,ylab="App Usage",xlab='Day of October')
lines(data1$total4,lwd=2)
yymodel=modeld$coefficients[1]+modeld$coefficients[2]*cos(2*(pi/180)*xx)+modeld$coefficients[3]*yyy
lines(yymodel,col='red3',lt=1,lwd=2,type="o")
lines(lowess(data1,f=1/7),lw=2,col='darkolivegreen3',lt=2)
legend("topright", c("Actual Usage","Fitted Model","Smooth"), col = c('black', 'red3', 'darkolivegreen3'),
       text.col = "black", lty = c(1,1, 2), lwd= c(2,2,2),
       merge = F, bg = "gray90")

max(y$freq)
y$freq[y$spec==max(y$spec)]

reslm <- lm(y ~ sin(2*pi*0.3*x)+cos(2*pi*0.3*x))
summary(reslm)

reslm1 <- lm(y ~ sin(2*pi*0.3*x))
summary(reslm1)

reslm2 <- lm(y ~ cos(2*pi*0.3*x))
summary(reslm2)

reslm3 <- lm(y ~ sin(2*pi*0.3*x)+x)
summary(reslm3)

reslm4 <- lm(y ~ sin(2*pi*0.3*x)+cos(2*pi*0.3*x)+x)
summary(reslm4)

reslm5 <- lm(yy ~ sin(2*pi*0.3*xx)+cos(2*pi*0.3*xx)+xx+yyy)
summary(reslm5)

reslm7 <- lm(y ~ sin(2*pi*0.3*x)+cos(2*pi*0.3*x)+poly(x,2))
summary(reslm7)

q <- seq(from=1, to=21)
reslm6=lm(y~poly(q,4))
summary(reslm6)

par(mfrow=c(1,1))
plot(x,data_oct11[,1],ylab="App Usage",xlab='Day of October')
lines(data_oct11[,1],lwd=2)
modeloct=reslm4$coefficients[1]+reslm4$coefficients[2]*sin(2*(pi*0.3)*x)+
        reslm4$coefficients[3]*cos(2*(pi*0.3)*x)+reslm4$coefficients[4]*x
lines(modeloct,col='red3',lt=1,lwd=2,type="o")
lines(lowess(data_oct11[,1],f=0.3),lw=2,col='darkolivegreen3',lt=2)
legend("topright", c("Actual Usage","Fitted Model","Smooth"), col = c('black', 'red3', 'darkolivegreen3'),
       text.col = "black", lty = c(1,1, 2), lwd= c(2,2,2),
       merge = F, bg = "gray90")

##### weekend and weekday ###########

weekend=rbind(data1[4,],data1[5,], data1[11,], data1[12,], data1[18,],data1[19,], data1[25,], data1[26,])

weekday=read.csv("weekdays-significant counties.csv",header=TRUE)
weekday=data.frame(weekday)
 


### Poisson for Weekday  #####
lambda_day=mean(weekday[,1])

plot(dpois(1300:3300,lambda_day),type="l",
     xlab="application usage per day",ylab="probability", main="Weekday")

maxcount=max(weekday[,1])
mincount=min(weekday[,1])
sumcount=sum(weekday[,1])

##### goodness of fit #####
expected <- dpois(weekday[,1],lambda_day, log = FALSE)

observed <- (weekday[,1]/sum(weekday[,1]))
chisq.test(expected, observed)

### Poisson for Weekend  #####
lambda_end=mean(weekend[,1])
par(mfrow=c(1,1))
plot(dpois(1100:3300,lambda_end),type="l",
     xlab="application usage per day",ylab="probability",lwd=2,col='black' )
lines(dpois(1100:3300,lambda_day),type="l",col='red3',
     xlab="application usage per day",ylab="probability",lwd=2)
legend("topright", c("weekend","weekday"), col = c('black', 'red3'),
       text.col = "black", lty = c(1,1), lwd= c(2,2),
       merge = F, bg = "gray90")

maxcount=max(weekend[,1])
mincount=min(weekend[,1])
sumcount=sum(weekend[,1])

##### goodness of fit #####
expected_weekend <- dpois(weekend[,1],lambda_day, log = FALSE)

observed_weekend <- (weekend[,1]/sum(weekend[,1]))
chisq.test(expected_weekend, observed_weekend)

data1=as.matrix(data1)
              ### wavelet #####
library(biwavelet)

x = data_oct11[,1]
Y=c(1:21)

wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")

par(mfrow=c(2,2));par(mar=c(4,5,3,0.5))
plot(Y,x,xlab="day",ylab="App Usage",main="");
lines(lowess(Y,x,f=0.3),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(Y,x));
plot(wt1, type="power.corr.norm", xlab="day", ylab="Period (day)", main="");
plot(wlt$period, wlt$p.avg, main="Global Wavelet Spectrum",xlim=c(2,12),xlab="Period (day)",ylab="Variance", type="b"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red")


############### Wavelet Function ##################
wavelet=function(Y,dj=1){
  
  #Y is time series to be analyzed
  DT=1# is timestep for annual data, 1
  pad=1
  #dj=0.025
  param=6
  #pad data ? 0=F, 1=T
  #dj= spacing between discrete scales (.025)
  #param = wavenumber (6)
  
  s0=2*DT
  
  n1 = length(Y)
  J1=floor((log2(n1*DT/s0))/dj)
  
  
  #....construct time series to analyze, pad if necessary
  x = Y - mean(Y)
  
  
  if (pad == 1){
    base2 = trunc(log(n1)/log(2) + 0.4999)   # power of 2 nearest to N
    x = c(x, rep(0, 2^(base2 + 1) - n1))
  }
  n = length(x)
  
  #....construct wavenumber array used in transform [Eqn(5)]
  k = (1:trunc(n/2))
  k = k*((2*pi)/(n*DT))
  k = c(0, k, -rev(k[1:floor((n-1)/2)]))
  
  #....compute FFT of the (padded) time series
  f = fft(x)    # [Eqn(3)]
  
  #....construct SCALE array & empty PERIOD & WAVE arrays
  scale = s0*2^((0:J1)*dj)
  period = scale;
  wave = matrix(data=0, ncol=n, nrow=J1+1)  # define the wavelet array
  wave = as.complex(wave)  # make it complex
  wave=matrix(data=wave, ncol=n, nrow=J1+1)
  
  # loop through all scales and compute transform
  for(a1 in 1:(J1+1)){
    scl=scale[a1]  	
    
    nn = length(k);
    k0 = param
    expnt = -(scl*k - k0)^2/(2*(k > 0))
    norm = sqrt(scl*k[2])*(pi^(-0.25))*sqrt(nn)    # total energy=N   [Eqn(7)]
    daughter = norm*exp(expnt)
    daughter = daughter*(k > 0)    # Heaviside step function
    fourier_factor = (4*pi)/(k0 + sqrt(2 + k0^2)) # Scale-->Fourier [Sec.3h]
    coi = fourier_factor/sqrt(2)                  # Cone-of-influence [Sec.3g]
    dofmin = 2                                   # Degrees of freedom
    
    out <- list(daughter=daughter, fourier_factor=fourier_factor,coi=coi,dofmin=dofmin)
    
    daughter=out$daughter
    fourier_factor=out$fourier_factor
    coi=out$coi
    dofmin=out$dofmin	
    wave[a1,] = fft((f*daughter), inverse = TRUE)/(length(f*daughter))  # wavelet transform[Eqn(4)]
  }
  
  period = fourier_factor*scale
  
  coi = coi*c(1:(floor(n1 + 1)/2), rev(1:floor(n1/2))) * DT
  
  wave = wave[,1:n1]  # get rid of padding before returning
  power=abs(wave)^2
  ncol=length(power[1,])
  nrow=length(scale)
  avg.power=apply(power,1,mean)
  result=list(wave=wave, period=period, scale=scale, power=power, coi=coi,nc=ncol,nr=nrow,p.avg=avg.power)
  return(result)
}





