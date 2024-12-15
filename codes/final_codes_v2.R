
##### Dependencies #####

library(tidyverse)
library(fpp2)
library(xts)
library(gridExtra)
library(ggthemes)

########################


########## 1. Basic information of the data ##########

## Load and set data
#path <- "C:/study/R/time_series_analysis(4-2)/final_project"
#setwd(path)

totalQ <- read_csv("TotalQuestions.csv")
selQ <- totalQ %>% select(Month, Python, R, Javascript)

selQ <- selQ[-1,] # remove first observation
ts.py <- selQ$Python %>% ts(start=c(2008,11), end=c(2024,2), frequency=12)
# - - - - - - - - - - - -
ts.R <- selQ$R %>% ts(start=c(2008,11), end=c(2024,2), frequency=12)


## Time series plot (original)
autoplot(ts.py) + ggtitle("Time series plot (Python)") + ylab("Count")
# - - - - - - - - - - - -
autoplot(ts.R) + ggtitle("Time series plot (R)") + ylab("Count")


## Additional information (original)
acf(coredata(ts.py), lag.max=50, main="ACF of Python")  # linear? decreasing
pacf(coredata(ts.py), lag.max=50, main="PACF of Python") # high peak in evert lag 12
# -> Seasonal ARIMA with S=12 

# +)
checkresiduals(ts.py) # Residual ts plot, ACF, histogram
# autocorrelations are not normally distributed (Ljung-Box test p-value < 2.2e-16)

# - - - - - - - - - - - -
acf(coredata(ts.R), lag.max=50, main="ACF of R")  # linear? decreasing
pacf(coredata(ts.R), lag.max=50, main="PACF of R") # high peak in evert lag 12
# -> Seasonal ARIMA with S=12 

# +)
checkresiduals(ts.R)  # Residual ts plot, ACF, histogram
# autocorrelations are not normally distributed (Ljung-Box test p-value < 2.2e-16)



########## 2. Box-Cox transformation & SARIMA model selection ##########

## Box-Cox transformation
lambda.py <- BoxCox.lambda(ts.py)
box.py <- BoxCox(ts.py, lambda=lambda.py)
plot(box.py)

# - - - - - - - - - - - -
lambda.R <- BoxCox.lambda(ts.R)
box.R <- BoxCox(ts.R, lambda=lambda.R)
plot(box.R)


## Automatic SARIMA model using auto.arima()
fit.auto.py <- auto.arima(ts.py, lambda=lambda.py)  # model 1
fit.auto.R <- auto.arima(ts.R, lambda=lambda.R)


## Difference needed?
plot(diff(box.py))
plot(diff(box.R))
# differencing makes the series more like white noise, but not sufficient enough...


## Other SARIMA models
fit.auto.py   # model 1
(model2.py <- Arima(ts.py, order=c(0,2,1), seasonal=c(0,1,2), lambda=lambda.py))  # model 2
(model3.py <- Arima(ts.py, order=c(0,2,1), seasonal=c(0,1,1), lambda=lambda.py))  # model 3
(model4.py <- Arima(ts.py, order=c(0,1,0), seasonal=c(0,1,1), lambda=lambda.py))  # model 4  *
(model5.py <- Arima(ts.py, order=c(0,1,0), seasonal=c(0,1,2), lambda=lambda.py))  # model 5  *
(model6.py <- Arima(ts.py, order=c(0,1,1), seasonal=c(0,1,1), lambda=lambda.py))  # model 6
(model7.py <- Arima(ts.py, order=c(0,1,1), seasonal=c(0,1,2), lambda=lambda.py))  # model 7  *
(model8.py <- Arima(ts.py, order=c(1,1,1), seasonal=c(0,1,2), lambda=lambda.py))  # model 8
(model9.py <- Arima(ts.py, order=c(1,1,0), seasonal=c(0,1,2), lambda=lambda.py))  # model 9
(model10.py <- Arima(ts.py, order=c(0,1,0), seasonal=c(1,1,1), lambda=lambda.py)) # model 10  selected!

arima.py <- Arima(ts.py, order=c(0,1,0), seasonal=c(1,1,1), lambda=lambda.py)


# - - - - - - - - - - - -
fit.auto.R    # model 1
(model2.R <- Arima(ts.R, order=c(0,1,1), seasonal=c(0,1,1), lambda=lambda.R)) # model 2  *
(model3.R <- Arima(ts.R, order=c(0,1,1), seasonal=c(1,2,1), lambda=lambda.R)) # model 3
(model4.R <- Arima(ts.R, order=c(0,1,1), seasonal=c(1,2,2), lambda=lambda.R)) # model 4
(model5.R <- Arima(ts.R, order=c(1,1,1), seasonal=c(1,1,1), lambda=lambda.R)) # model 5
(model6.R <- Arima(ts.R, order=c(1,1,0), seasonal=c(0,1,1), lambda=lambda.R)) # model 6  selected!

arima.R <- Arima(ts.R, order=c(1,1,0), seasonal=c(0,1,1), lambda=lambda.R)



########## 3. Prediction using SARIMA + STL and naive method (stlf, snaive) ##########

# Use test.py, test.R variable in "test_data.R" file.

## Forecast using given models.
H <- 9
fc.arima.py <- forecast(arima.py, h=H)  # SARIMA model
stlf.ets.py <- stlf(ts.py, method="ets", h=H, lambda=lambda.py)  # stlf-ets
spline.py <- splinef(ts.py, h=H, lambda=lambda.py)  # cubic spline
snaive.py <- snaive(ts.py, h=H, lambda=lambda.py)   # Naive forecast
ses.py <- ses(ts.py, h=H, lambda=lambda.py)   # SES

# Advanced methods
tbats.py <- tbats(ts.py, use.box.cox=TRUE)
fc.tbats.py <- forecast(tbats.py, h=H)

nnar.py <- nnetar(ts.py, lambda=lambda.py)
fc.nnar.py <- forecast(nnar.py, h=H)

# - - - - - - - - - - - -
H <- 9
fc.arima.R <- forecast(arima.R, h=H)
stlf.ets.R <- stlf(ts.R, method="ets", h=H, lambda=lambda.R)
spline.R <- splinef(ts.R, h=H, lambda=lambda.R)
snaive.R <- snaive(ts.R, h=H, lambda=lambda.R)
ses.R <- ses(ts.R, h=H, lambda=lambda.R)

# Advanced methods
tbats.R <- tbats(ts.R, use.box.cox=TRUE)
fc.tbats.R <- forecast(tbats.R, h=H)

nnar.R <- nnetar(ts.R, lambda=lambda.R)
fc.nnar.R <- forecast(nnar.R, h=H)


## Plot the SARIMA forecasts and test set
p1.py <- fc.arima.py %>% autoplot(size=1, series="SARIMA") + 
  autolayer(test.py, size=1, series="True values") + 
  ggtitle("Prediction using SARIMA model (Python)")

p2.py <- fc.arima.py %>% autoplot(size=1, series="SARIMA") + 
  autolayer(test.py, size=1, series="True values") + 
  scale_x_continuous(limits=c(2023, 2025)) + 
  scale_y_continuous(limits=c(0, 20000)) + 
  ggtitle("Prediction using SARIMA model (Python)")

p1.py


# - - - - - - - - - - - -
p1.R <- fc.arima.R %>% autoplot(size=1, series="SARIMA") + 
  autolayer(test.R, size=1, series="True values") + 
  ggtitle("Prediction using SARIMA model (R)")

p2.R <- fc.arima.R %>% autoplot(size=1, series="SARIMA") + 
  autolayer(test.R, size=1, series="True values") + 
  scale_x_continuous(limits=c(2023, 2025)) +
  scale_y_continuous(limits=c(0, 5000)) + 
  ggtitle("Prediction using SARIMA model (R)")

p1.R

#grid.arrange(p1.py, p1.R)
grid.arrange(p2.py, p2.R)


## Plots of advanced methods
p3.py <- fc.tbats.py %>% autoplot(size=1, series="TBATS") + 
  autolayer(test.py, size=1, series="True values") + 
  scale_x_continuous(limits=c(2023, 2025)) + 
  scale_y_continuous(limits=c(0, 20000)) + 
  ggtitle("Prediction using TBATS model (Python)")

p4.py <- fc.nnar.py %>% autoplot(size=1, series="NNAR") + 
  autolayer(test.py, size=1, series="True values") + 
  scale_x_continuous(limits=c(2023, 2025)) + 
  scale_y_continuous(limits=c(0, 20000)) + 
  ggtitle("Prediction using NNAR model (Python)")

# - - - - - - - - - - - -
p3.R <- fc.tbats.R %>% autoplot(size=1, series="TBATS") + 
  autolayer(test.R, size=1, series="True values") + 
  scale_x_continuous(limits=c(2023, 2025)) +
  scale_y_continuous(limits=c(0, 5000)) + 
  ggtitle("Prediction using TBATS model (R)")

p4.R <- fc.nnar.R %>% autoplot(size=1, series="NNAR") + 
  autolayer(test.R, size=1, series="True values") + 
  scale_x_continuous(limits=c(2023, 2025)) +
  scale_y_continuous(limits=c(0, 5000)) + 
  ggtitle("Prediction using NNAR model (R)")

grid.arrange(p3.py, p3.R)
grid.arrange(p4.py, p4.R)



## Plot the smoothing methods altogether and compare
ts.py %>% autoplot(size=1, series="Original", col="black") + 
  autolayer(test.py, size=1, series="True values", lty=2) + 
  autolayer(stlf.ets.py, PI=FALSE, size=1, series="STL-ETS") + 
  autolayer(spline.py, PI=FALSE, size=1, series="spline") + 
  autolayer(snaive.py, PI=FALSE, size=1, series="snaive") + 
  autolayer(ses.py, PI=FALSE, size=1, series="SES") + 
  autolayer(fc.arima.py, PI=FALSE, size=1, series="SARIMA") + 
  autolayer(fc.tbats.py, PI=FALSE, size=1, series="TBATS") + 
  autolayer(fc.nnar.py, PI=FALSE, size=1, series="NNAR") + 
  scale_x_continuous(limits=c(2023, 2025)) + 
  scale_y_continuous(limits=c(0, 20000)) + 
  ggtitle("Comparing forecasts with the test data (Python)")

# cubic spline, NNAR: trend with opposite direction (increasing)
# 

# - - - - - - - - - - - -
ts.R %>% autoplot(size=1, series="Original", col="black") + 
  autolayer(test.R, size=1, series="True values", lty=2) + 
  autolayer(stlf.ets.R, PI=FALSE, size=1, series="STL-ETS") + 
  autolayer(spline.R, PI=FALSE, size=1, series="spline") + 
  autolayer(snaive.R, PI=FALSE, size=1, series="snaive") + 
  autolayer(ses.R, PI=FALSE, size=1, series="SES") + 
  autolayer(fc.arima.R, PI=FALSE, size=1, series="SARIMA") + 
  autolayer(fc.tbats.R, PI=FALSE, size=1, series="TBATS") + 
  autolayer(fc.nnar.R, PI=FALSE, size=1, series="NNAR") + 
  scale_x_continuous(limits=c(2023, 2025)) + 
  scale_y_continuous(limits=c(0, 5000)) + 
  ggtitle("Comparing forecasts with the test data (R)")


## Test accuracy
accuracy(fc.arima.py, test.py)
accuracy(fc.tbats.py, test.py)
accuracy(fc.nnar.py, test.py)
accuracy(stlf.ets.py, test.py)
accuracy(spline.py, test.py)
accuracy(snaive.py, test.py)
accuracy(ses.py, test.py)

# - - - - - - - - - - - -
accuracy(fc.arima.R, test.R)
accuracy(fc.tbats.R, test.R)
accuracy(fc.nnar.R, test.R)
accuracy(stlf.ets.R, test.R)
accuracy(spline.R, test.R)
accuracy(snaive.R, test.R)
accuracy(ses.R, test.R)


########## 4. Spectral analysis (Basic stuffs) ##########

## Spectrum
spectrum(box.py, log=c("no"))  # freq=12
#spectrum(ts(box.py, frequency=1), log=c("no"))
# - - - - - - - - - - - -
spectrum(box.R, log=c("no"))
#spectrum(ts(box.R, frequency=1), log=c("no"))
# note) frequency = 12 -> x-axis: 0 ~ 6

# very low frequency (almost no cycles for every n years?) have big importance in both Python and R series.
# small bumps in each frequency = 1/12 ~ 3/12.


## Linking with some events in each languages
pPy <- fc.tbats.py %>% autoplot(size=1) + 
  autolayer(test.py, size=1) + 
  geom_vline(xintercept=2022 + 10/12, col="red2") + # Initial release of ChatGPT
  geom_vline(xintercept=2020 + 2/12, col="green3") + # COVID-19 pandemic
  geom_vline(xintercept=2009:2024, lty=2, col="darkgray") + # Seasonal patterns
  theme_classic() + 
  guides(col="none")
  
# steadly increasing from 2008 to 2020: becoming one of the most popular programming language
# seasonal pattern: related with school semester?
# high: March, May, October
# low: September, December, Feb?

pR <- fc.tbats.R %>% autoplot(size=1) + 
  autolayer(test.R, size=1) + 
  geom_vline(xintercept=2022 + 10/12, col="red2") + # Initial release of ChatGPT
  geom_vline(xintercept=2020 + 2/12, col="green3") + # COVID-19 pandemic
  geom_vline(xintercept=2009:2024, lty=2, col="darkgray") + # Seasonal patterns
  theme_classic() + 
  guides(col="none")

grid.arrange(pPy, pR)

Y = 2020
pPy2 <- fc.tbats.py %>% autoplot(size=1) + 
  scale_x_continuous(limits=c(Y, Y+1)) + 
  scale_y_continuous(limits=c(15000, 20000)) + 
  geom_vline(xintercept=Y+(0:12)/12, lty=2, col="darkgray") + 
  theme_classic() + 
  guides(col="none")
  
pPy2

