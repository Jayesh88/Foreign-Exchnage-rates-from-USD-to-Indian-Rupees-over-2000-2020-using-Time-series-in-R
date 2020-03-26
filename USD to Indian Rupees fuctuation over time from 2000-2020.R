library("anytime")
library(bsts)
library("car")
library("caret")
library("forecast")
library("keras")
library("MCMCpack")
library("smooth")
library("tensorflow")
library("tseries")
library("TTR")
library(xts)

time_sr_dta<-read.csv(file.choose())
View(time_sr_dta)

head(time_sr_dta)
str(time_sr_dta)

#converting data for analysis

for (i in 3:ncol(time_sr_dta)) {
  time_sr_dta[[i]]<-round(as.numeric(as.character(time_sr_dta[[i]])),digits=2)
}

time_sr_dta$Time.Serie<-as.Date(anytime(time_sr_dta$Time.Serie))
str(time_sr_dta)

time_sr_dta<-na.omit(time_sr_dta)


ggplot(time_sr_dta, aes(Time.Serie, time_sr_dta[[11]])) + geom_line() + scale_x_date("year") + ylim(0,100) + ylab("Indian Rupees")+ggtitle("Rise of Indian Rupees over years compared to 1 USD")

#convet data to time series model
Train <- xts(time_sr_dta[, -1], order.by = as.POSIXct(time_sr_dta$Time.Serie))
tim_series <- ts(time_sr_dta[,11], frequency = 365.25,start = c(2000,01,03))
plot(tim_series,type='l',lwd = 1.5,ylim=c(0,100),col='red',main="US Dollar rate over time")

#check trends and seasonality
decomp<-decompose(tim_series)
plot(decomp)

#HOLTWinters Smoothing technique
hltwin<-HoltWinters(time_sr_dta[[11]],
                    alpha=0.2,
                    beta=FALSE,
                    gamma=FALSE)

hlt_pred<-predict(hltwin,n.ahead = 10,prediction.interval = TRUE)
plot.ts(time_sr_dta[[11]])
lines(hltwin$fitted[,1],col='green')
lines(hlt_pred[,1],col='blue')
lines(hlt_pred[,2],col='red')
lines(hlt_pred[,3],col='red')

#Adf test
tsdf<-diff(time_sr_dta[,11],lag=2)
adf.test(tim_series)
plot.ts(tsdf)

#acf and pacf
acf(tsdf)
pacf(tsdf)
at_ar<-auto.arima(time_sr_dta[,11])
tsdisplay(arima.errors(at_ar),main = "ARIMA errors")
plot(forecast(at_ar))
