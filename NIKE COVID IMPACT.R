

###################################################   COVID IMPACT ON NIKE  ##################################################

library(readxl)
library(rvest)
library(tidyverse)
library(zoo)
library(forecast)
library(dygraphs)
library(dplyr)

############################################################# DATA PREPARATION #####################################################

# Reading the Nike historical quarterly income statement

Profit = "Nike.xlsx" %>% read_xlsx(range = "B22:DJ22",col_types= "numeric",col_names = FALSE) %>% unlist(use.names=FALSE)
Dates = "Nike.xlsx" %>% read_xlsx(range = "B11:DJ11",col_types= "text",col_names = FALSE) %>% unlist(use.names=FALSE)

# Create a time Series

y.a = ts(na.omit(Profit),start=c(1994,1),frequency = 4)

length(Profit)
Dates

# Extract Year from Time Series

Year = floor(as.vector(time(y.a)))
length(Year1)

Quarter = c(rep(1:4,2022-1994),1)

length(Quarter)


Nike = data.frame(Year,Quarter,Profit=as.vector(y.a))

Nike$Time = 1:dim(Nike)[1]
Nike$Trend = Nike$Time

############################################################# DATA MODELING #####################################################


Nike %>% ggplot(aes(x=Time,y=Profit)) + geom_line() + geom_point() + theme_bw()
Nike$Actual = Nike$Profit

Nike$Profit[Nike$Year>2019]=NA
Nike

Nike$Quarter = factor(Nike$Quarter)

M1 = lm(Profit~Time + Quarter,data=Nike)
summary(M1)

Nike$M1 = NA
Nike$M1[!is.na(Nike$Profit)]=M1$fitted.values

Nike %>% ggplot(aes(x=Time,y=Profit)) + geom_line() + geom_point() + theme_bw() + geom_line(aes(x=Time,y=M1),color="red") + geom_point()


tsdisplay(M1$residuals)

## Looking at the ACF graph it indicates significant lags and the PACF indicates Lag1

Nike$ProfitLag1 = lag(Nike$Profit,1)
head(data)
tail(data)


M2 = lm(Profit ~ Time + Quarter + ProfitLag1, data = Nike)

tsdisplay(M2$residuals)

Nike$M2[!is.na(Nike$Profit) & !is.na(Nike$ProfitLag1)] = M2$fitted.values

Nike %>% ggplot(aes(x=Time,y=Profit)) + geom_line() + geom_point() + theme_bw() +
  geom_line(aes(x=Time,y=M2),color="red") + geom_point()

## Looking at the ACF graph it indicates significant lags and the PACF indicates Lag1, Lag3, Lag4, and Lag5

Nike$ProfitLag3 = lag(Nike$Profit,3)
Nike$ProfitLag4 = lag(Nike$Profit,4)
Nike$ProfitLag5 = lag(Nike$Profit,5)


M3 = lm(Profit ~ Time + Quarter + ProfitLag1 + ProfitLag3 + ProfitLag4  + ProfitLag5, data = Nike)
summary(M3)


tsdisplay(M3$residuals)

# The model pretty accurately captures systematic patterns and there are no significant Lags

Nike$M3[!is.na(Nike$Profit) & !is.na(Nike$ProfitLag1) & !is.na(Nike$ProfitLag3) & 
          !is.na(Nike$ProfitLag4) & !is.na(Nike$ProfitLag5)] = M3$fitted.values

Nike %>% ggplot(aes(x=Time,y=Profit)) + geom_line() + geom_point() + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point()

tail(Nike,10)

# Predict the COVID Period from 2020 onwards

for(i in 105:113){
  Nike$ProfitLag1[i] = ifelse(!is.na(Nike$Profit[i-1]),Nike$Profit[i-1],Nike$M3[i-1])
  Nike$ProfitLag3[i] = ifelse(!is.na(Nike$Profit[i-2]),Nike$Profit[i-2],Nike$M3[i-2])
  Nike$ProfitLag4[i] = ifelse(!is.na(Nike$Profit[i-3]),Nike$Profit[i-3],Nike$M3[i-3])
  Nike$ProfitLag5[i] = ifelse(!is.na(Nike$Profit[i-4]),Nike$Profit[i-4],Nike$M3[i-4])
  Nike$M3[i] = predict(M3,newdata = Nike[i,])
  
}

tail(Nike,10)

# data, predited/fitted values on the training set and forecast on the testing set

Nike %>% ggplot(aes(x=Time,y=Actual)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point(aes(x=Time,y=M3),color="red",size=2)


# just training set

Nike[Nike$Year<2020,] %>% ggplot(aes(x=Time,y=Actual)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point(aes(x=Time,y=M3),color="red",size=2)


# just testing set

Nike[Nike$Year>=2020,] %>% ggplot(aes(x=Time,y=Actual)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point(aes(x=Time,y=M3),color="red",size=2)

head(Nike)

# RMSE calculations: 

# Create a column that represents errors:

Nike$M3errors = NA
Nike$M3errors = Nike$Actual - Nike$M3

# RMSE on training set:

sqrt(mean(Nike[Nike$Year<2020,"M3errors"]^2,na.rm= TRUE))

# CV on training set: less than 10% is considered OK

paste(round(100*sqrt(mean(Nike[Nike$Year<2020,"M3errors"]^2,na.rm= TRUE)) / mean(Nike$Actual[Nike$Year<2020]),2),"%")

# RMSE on testing set:

sqrt(mean(Nike[Nike$Year>=2020,"M3errors"]^2,na.rm= TRUE))

# MAPE calculations: 

# Create a column that represents Absolute Percentage errors:

Nike$M3APerrors = NA
Nike$M3APerrors = abs((Nike$Actual - Nike$M3)/Nike$Actual)*100

# MAPE on the training set:

paste(round(mean(Nike$M3APerrors[Nike$Year<2020],na.rm = TRUE),2),"%")

# MAPE on the testing set:
paste(round(mean(Nike$M3APerrors[Nike$Year>=2020],na.rm = TRUE),2),"%")


# Covid impact in $
sum(Nike$M3errors[Nike$Year>=2020])
# -361332
# There was approximately 361 million dollar loss during COVID-19

# Covid impact in %
paste(round(100 * sum(Nike$M3errors[Nike$Year>=2020]) / sum(Nike$Actual[Nike$Year>=2020]),2),"%")

# There was 0.86% decline in the gross profit for NIKE due to COVID-19 Pandemic

########################################################## END CASE NIKE #############################################################


