rm(list=ls())
setwd("C:/Users/hatha/Desktop/SJSU/Spring 2021/Econ 138/Project Econ")
library(readxl)
us_ev <- read_excel("US_EV_SALES_138.xlsx")
View(us_ev)
library(ggplot2)
library(gplots)
library(fpp)
library(fpp2)
library(fpp3)
library(seasonal)
library(GGally)
library(forecast)
library(readxl)
library(scales)
library(tsibble)
library(dplyr)
library(urca)
library(seastests)

#####################################
#######DATA SUMMARY
summary(us_ev)
sd(us_ev$Sales) #find standard deviation of sales
aggregate(us_ev$Sales,us_ev['Date'],mean)
sum(us_ev$Sales)


#####################################
#######DATA EXAMINATION
#######PLOT DATA
#Change to time series
evs <- ts(us_ev[,2],start=c(2011,1), frequency = 12)
#Plot the data
autoplot(evs) + labs(x="Year", y="Sales", title="EVs sales from 2011 to 2020")
#There's trend, maybe seasonal pattern

########CHECK STATIONARY
#Use acf, pacf to indentify non-stationary
acf(evs)
pacf(evs)
#According to acf and pacf plots: non-stationary data
#Use Dickey Fuller test to ensure the claim
adf.test(evs)
#p-value = 0.1054
#alternative hypothesis: stationary
#P-value shows that we fail to reject null hypothesis
#Non-stationary data
###

nsdiffs(evs)
#0
#We don't need to do seasonal difference
#check how many first difference we need to get stationary data
ndiffs(evs)
#1
#So, we do 1 first difference to get the stationary data
#Differencing
evs_df= diff(evs)
#Check stationary
adf.test(evs_df)
#p-value = 0.01
#We reject null hypothesis
#Stationary data



##########TIME PLOT
cbind("EVs sale" = evs,
      "First difference"= evs_df) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly EVs sales")

#ACF and PACF plots of differenced data
us_ev_tsb = as_tsibble(evs_df)
us_ev_tsb %>% gg_tsdisplay(plot_type = "partial")


##########ARIMA Model
#Fit ARIMA model
evs_tsb = as_tsibble(evs)

#"Auto" ARIMA is with stepwise = TRUE, greedy = TRUE, approximation = NULL, trace = FALSE
#"Search" ARIMA is with stepwise = FALSE, greedy = FALSE, approximation = FALSE, trace = FALSE
fit = evs_tsb %>% model(auto = ARIMA(value),search = ARIMA(value, stepwise = FALSE, approximation = FALSE, trace = FALSE))
glance(fit) %>% arrange(AICc)
fit %>% pivot_longer(everything(), names_to = "Model name",
                           values_to = "Orders")
report(fit)

#Choose ARIMA model with smaller AICc
fit %>% select(search) %>% gg_tsresiduals()

#Check with Ljung-box test
augment(fit) %>% features(.innov, ljung_box, lag=12)



##########FORECASTING
evs_tsb %>%
  model(ARIMA(value ~ 0+ pdq(0,1,4) + PDQ(2,0,0))) %>%
  forecast(h="10 years") %>%
  autoplot(evs_tsb) +
  labs(y=" Units sold", x = "Year",
       title="EVs Sales Forecasting ")

View(fit %>% select(search) %>% forecast( h= "10 years" ))
fc = fit %>% select(search) %>% forecast( h= "10 years" )
#Units sold for year 2015, 2020 and 2030
fc_2030 = fc %>% filter_index("2030 Jan" ~ "2030 Dec")
(sum_2030 = sum(fc_2030$.mean))
year_2020 = evs_tsb %>% filter_index("2020 Jan" ~ "2020 Dec")
(sum_2020 = sum(year_2020$value))
year_2015 = evs_tsb %>% filter_index("2015 Jan" ~ "2015 Dec")
(sum_2015 = sum(year_2015$value))


##########SPLITTING DATA INTO TRAINING and TESTING SETS
#Fit ARIMA
train <- evs_tsb %>% filter_index(. ~ "2019 Dec")
test <- evs_tsb %>% filter_index("2020 Jan" ~ "2020 Dec")
fit_arima <- train %>% model(ARIMA(value))
report(fit_arima)
fit_arima %>% gg_tsresiduals()
augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 10)
train %>%
  model(ARIMA(value ~ 0+ pdq(0,1,5) + PDQ(0,0,1))) %>%
  forecast(h="1 years") %>%
  autoplot(train) + autolayer(test)+
  labs(y=" Units sold", x = "Year",
       title="EVs Sales Forecasting ")
View(fit_arima %>% forecast( h= "1 years" ))
#Fit ETS 
fit_ets <- train %>% model(ETS(value))
report(fit_ets)
fit_ets %>%
  gg_tsresiduals()
augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 10)
