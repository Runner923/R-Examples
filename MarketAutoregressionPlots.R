library(quantmod)
# A program to compute an autoregressive model (n=1) of the logarithm of closing stock
# prices for 30 stocks.  Generates plots of the fitted values vs actuals with correlation
# coefficient (R^2) values and histograms of the residuals.  Plots are sent to .png files.
# Charles Howard 7-July-2014
stock.symbol.vector<-c("SMSI","DWSN","CTRX","COHU","EMC","IBM","FSLR","FORM","BSX","ADSK","AAPL","MSFT","ORCL","SWKS","INTC","VZ",
                       "HD","SPWR","AMZN","PM","NUAN","CE","ROK","AME","WLL","QEP","SM","TRMB","MTD","CSC")
to.date<-Sys.Date()
from.date<-to.date-90
## creating auto-regressive models for log price for each symbol
price.data.ls<-lapply(1:length(stock.symbol.vector),function(n){
  x<-getSymbols(stock.symbol.vector[n],src="yahoo",from='2014-04-01',to=Sys.Date())
  xcl<-Cl(get(x))
  log.xcln<-log(xcl[2:length(xcl)])
  log.xclnm1<-log(xcl[1:(length(xcl)-1)])
  xreg.lm<-lm(log.xcln~log.xclnm1)
  list("symbol"=stock.symbol.vector[n],"Closing.Prices"=xcl,"Model"=xreg.lm)
  })
## plotting the log price against the fitted log price for each model
	png("arplots.png",width=1024,height=768,pointsize=12)
	layout(matrix(c(rep(1,6),seq(2,31),rep(32,6)),7,6,byrow=T))
  plot.new()
  mtext(paste("Autoregressive (n=1) Log Closing Prices\nFrom ",
              from.date," to ",to.date,sep=""),side=1,font=2,cex=1.5)
  for (i in 1:length(stock.symbol.vector)){
  syb<-stock.symbol.vector[i]
  log.prices<-log(price.data.ls[[i]][['Closing.Prices']])
  plot.prices<-as.numeric(log.prices[2:length(log.prices)])
  fitted.prices<-as.numeric(price.data.ls[[i]][['Model']]$fitted.values)
  rsquared<-round(summary(price.data.ls[[i]][['Model']])$adj.r.squared,2)
  plot(plot.prices,fitted.prices,
       main=bquote(.(syb) ~ " "~ R^2 ~ " = " ~ .(rsquared)),xlab='log.price',
       pch=20,ylab="",bty="n",yaxt="n")
  }
	dev.off()
	graphics.off()
## plotting the residuals histograms
	png("arplots-hist.png",width=1024,height=768,pointsize=12)
  layout(matrix(c(rep(1,6),seq(2,31),rep(32,6)),7,6,byrow=T))
  plot.new()
  mtext(" Residual Distributions\nNormal Distributions in Blue ",side=1,font=2,cex=1.5)
set.seed(1234)
  for (i in 1:length(stock.symbol.vector)){
  syb<-stock.symbol.vector[i]
  resids<-as.numeric(price.data.ls[[i]][['Model']]$residuals)
  hist(resids,main=syb,font.main=2,col=grey(0.6),yaxt="n",xlab="",ylab="",bty="n",freq=F)
  norm.resids<-hist(rnorm(10000,mean=0,sd=sd(resids)),plot=F)
  lines(norm.resids$mids,norm.resids$density,lwd=2,col='blue')
  }
	dev.off()
	graphics.off()
## plotting histograms of next day's close
png("arpredict-hist.png",width=1024,height=768,pointsize=12)
layout(matrix(c(rep(1,6),seq(2,31),rep(32,6)),7,6,byrow=T))
plot.new()
mtext(" Next Closing Price Distribution ",side=1,font=2,cex=1.5)
set.seed(1234)
for (i in 1:length(stock.symbol.vector)){
  syb<-stock.symbol.vector[i]
  inter.cept<-as.numeric(price.data.ls[[i]][['Model']]$coefficients[1])
  slope<-as.numeric(price.data.ls[[i]][['Model']]$coefficients[2])
  last.price<-as.numeric(log(price.data.ls[[i]][['Closing.Prices']][length(price.data.ls[[i]][['Closing.Prices']])]))
  resids<-as.numeric(price.data.ls[[i]][['Model']]$residuals)
  norm.resids<-rnorm(1000,mean=0,sd=sd(resids))
  log.predict<-(inter.cept+slope*last.price)+norm.resids
  price.predict<-exp(log.predict)
  hist(price.predict,main=syb,font.main=2,col=grey(0.6),yaxt="n",xlab="$",ylab="",bty="n",freq=F)
  }
dev.off()
graphics.off()
