DATA<-read.table("E:shampoo.txt")
ts.plot(DATA)

#####First order moving average
DATA<-ts(DATA,start=1,frequency=1)
OUTmov1<-MOV.AVER1(DATA,5,1)
OUTmov1
ts.plot(OUTmov1$forecast,DATA,col=c(1,2)) ##error

DATA<-ts(DATA,start=1,frequency=1)
OUTmov1<-MOV.AVER1(DATA,5,1)
ts.plot(OUTmov1$ma,DATA,col=c(2,1))

### k=20
OUTmov1<-MOV.AVER1(DATA,20,1)
ts.plot(OUTmov1$ma,DATA,col=c(2,1))

#########################################
k<-5; MSE<-0
while (k<=25) { out<-MOV.AVER1(DATA,k,1)
MSE[k]<-sum(out$error[(k+1):36]^2)/(36-k)
k<-k+1
}

ts.plot(MSE[2:36])
MSE

##### Second order moving average
### k=5
OUTmov2<-MOV.AVER2(DATA,5,1)
ts.plot(OUTmov2$forecast,DATA,col=c(2,1))
DATA<-ts(DATA,start=1,frequency=1)
OUTmov2

### k=10
OUTmov2<-MOV.AVER2(DATA,10,1)
ts.plot(OUTmov2$forecast,DATA,col=c(2,1))


#####First order exponential smoothing
### alpha=.05, s0=x1
OUTexp1<-EXP.SMO1(DATA,0.05,DATA[1],1)
ts.plot(OUTexp1$esm,DATA,col=c(2,1))
OUTexp1

### alpha=.1, s0=x1
OUTexp1<-EXP.SMO1(DATA,0.1,DATA[1],1)
ts.plot(OUTexp1$esm,DATA,col=c(2,1))

### alpha=.2, s0=x1
OUTexp1<-EXP.SMO1(DATA,0.2,DATA[1],1)
ts.plot(OUTexp1$esm,DATA,col=c(2,1))

### alpha=.3, s0=x1
OUTexp1<-EXP.SMO1(DATA,0.3,DATA[1],1)
ts.plot(OUTexp1$esm,DATA,col=c(2,1))

#####Second order exponential smoothing
lm(DATA[1:10]~c(1:10))

### alpha=.05, s0=s1=266
OUTexp2<-EXP.SMO2(DATA,0.05,266,266,1)
ts.plot(OUTexp2$forecast,DATA,col=c(1,2))
OUTexp2

### alpha=.1, s0=s1=266
OUTexp2<-EXP.SMO2(DATA,0.1,266,266,1)
ts.plot(OUTexp2$forecast,DATA,col=c(1,2))

ts.plot(OUTexp1$forecast,OUTexp2$forecast,DATA,col=c(1,2,3))




#########seasonal
temp<-read.table("E:temp.txt")
ts.plot(temp)
temp<-ts(temp,start=1,frequency=1)
tempdec=DEC.ADD(temp,4,4)
tempdec

ts.plot(tempdec$forecast,temp,col=c(1,2))

ts.plot(rep(tempdec$seaseffect,6))
ts.plot(OutAdd$betahat[1]+OutAdd$betahat[2]*(1:24))
ts.plot(cbind(OutAdd$forecast,XtAdd),col=c(1,2))

ts.plot(temp,tempdec,col=c(1,2))
tempdecm=DEC.MULT(temp,12,12)
tempdecm
ts.plot(tempdec$forecast,temp,col=c(1,2))

print(sum(tempdec$DECAD$error)**2)
print(sum(tempdecm$DECMU$error)**2)

lm(temp[1:10]~c(1:10))
tempwin=WINTERS(temp,12,0.05,0.05,0.01,1)
tempwin
ts.plot(tempwin$WIN,tempwin,col=c(1,2))
ts.plot(tempwin,tempwin$WIN,col=c(1,2))

