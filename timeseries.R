library(dplyr)
library(ggplot2)
library(forecast)

rm(list=ls())
data <- read_csv("life expectancy.csv")
data <- data %>% 
  na.omit()

names(data)[names(data) == "Life Expectancy World Bank"] <- "LifeExpectancy"
names(data)[names(data) == "Country Name"] <- "Country"

bangladesh<- data %>%
  filter(Country=="Bangladesh")


ggplot(bangladesh , aes(x = Year, y = LifeExpectancy)) +
  geom_step() + 
  geom_point() +
  geom_text(aes(label = paste(Year, LifeExpectancy, sep = ": ")), vjust = -1, hjust = 1.5) +
  labs(title = "Life Expectancy in Bangladesh From 2006 to 2019",
       x = "Year",
       y = "Life Expectancy") +
  theme_minimal()  

newBa <- ts(bangladesh$LifeExpectancy, start = c(2006), end = c(2019), frequency=1)
TBa <- window(newBa, end = c(2012))
VBa <- window(newBa, start = c(2013))

Reg1Ba <- tslm(TBa ~ trend)
Reg2Ba <- tslm(TBa ~ trend + I(trend^2))
Reg3Ba <- tslm(TBa ~ poly(trend, degree = 3))



nVBa<- length(VBa)

fReg1Ba <-forecast(Reg1Ba, h=nVBa)
fReg2Ba <-forecast(Reg2Ba, h=nVBa)
fReg3Ba <-forecast(Reg3Ba, h=nVBa)


accuracy(fReg1Ba,VBa)
accuracy(fReg2Ba,VBa)
accuracy(fReg3Ba,VBa)


RegFinBa <- tslm(newBa ~ poly(trend, degree = 3))
forecast(RegFinBa, h=10)

autoplot(forecast(RegFinBa, h=10)) +
  ggtitle("Forecast of Life Expectancy for Bangladesh ") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()

tanzania <- data %>% 
  filter(Country=="Tanzania")

ggplot(tanzania , aes(x = Year, y = LifeExpectancy)) +
  geom_line() + 
  geom_point() +
  geom_text(aes(label = paste(Year, LifeExpectancy, sep = ": ")), vjust = -1, hjust = 1.5) +
  labs(title = "Life Expectancy in Tanzania From 2006 to 2019",
       x = "Year",
       y = "Life Expectancy") +
  theme_minimal() 


newTa <- ts(tanzania$LifeExpectancy, start = c(2006), end = c(2019), frequency=1)
TTa <- window(newTa, end = c(2016))
VTa <- window(newTa, start = c(2017))

Reg1Ta <- tslm(TTa ~ trend)
Reg2Ta <- tslm(TTa ~ trend + I(trend^2))
Reg3Ta <- tslm(TTa ~ poly(trend, degree = 3))


nVTa<- length(VTa)

fReg1Ta <-forecast(Reg1Ta, h=nVTa)
fReg2Ta <-forecast(Reg2Ta, h=nVTa)
fReg3Ta <- forecast(Reg3Ta, h = length(VTa))



accuracy(fReg1Ta,VTa)
accuracy(fReg2Ta,VTa)
accuracy(fReg3Ta, VTa)


RegFinTa <- tslm(TTa ~ poly(trend, degree = 3))
forecast(RegFinTa, h=10)

autoplot(forecast(RegFinTa, h=10)) +
  ggtitle("Forecast of Life Expectancy for Tanzania") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()

