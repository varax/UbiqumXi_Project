library(zoo)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(forecast)
library(ggfortify)
library(tseries)

#Data Preprocess
DataEnergy <- read.csv("household_power_consumption.txt",header = TRUE, sep = ";", dec=".")
DataEnergy[DataEnergy == "?"] <- NA
DataEnergy$Date <- as.Date(DataEnergy$Date, format = "%d/%m/%Y")
DataEnergy$Global_active_power <-as.numeric(as.character(DataEnergy$Global_active_power))
DataEnergy$Global_reactive_power<-as.numeric(as.character(DataEnergy$Global_reactive_power))
DataEnergy$Voltage <-as.numeric(as.character(DataEnergy$Voltage))
DataEnergy$Global_intensity <- as.numeric(as.character(DataEnergy$Global_intensity))
DataEnergy$Sub_metering_1 <- as.numeric(as.character(DataEnergy$Sub_metering_1))
DataEnergy$Sub_metering_2 <- as.numeric(as.character(DataEnergy$Sub_metering_2))
str(DataEnergy)
DataEnergy <- DataEnergy %>%
  unite(DateTime,c(Date, Time))
summary(DataEnergy)
DataEnergy$DateTime <- as.POSIXct(DataEnergy$DateTime, format = "%Y-%m-%d_%H:%M:%S")
DataEnergy<- DataEnergy %>%
  mutate(GAP = Global_active_power * 1000 / 60, others = GAP - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

#deal with NAs by seperating continuosly NAs , indicating not at home) and others (1/2/3)
NaValue <- subset(DataEnergy, is.na(DataEnergy$Voltage))
missingValue_2 <- NaValue %>%
  group_by(Date(DateTime)) %>%
  summarise(count = n())
View(missingValue_2)
missingDay <- missingValue_2 %>%
  filter(count >120)

missingDay$con <-  missingDay$Date - shift(missingDay$Date)
View(missingDay)
missingDay$row <- rownames(missingDay)
missingDay$temp <-ifelse( missingDay$con !=1 & shift(missingDay$con == 1, type = "lead")|
                            missingDay$con == 1,"con", "non-con")

missingDay$temp[is.na(missingDay$temp)] <- "con"
"%notin%" <- Negate("%in%")

restmv_ <- missingValue_2[which (missingValue_2$Date %notin% missingDay$Date), ]
#handle two different types of NAs
DataEnergy[which (date(DataEnergy$DateTime) %in% missingValue_2$Date),][is.na(DataEnergy[which (date(DataEnergy$Date) %in% missingValue_2$Date),])] <- 0
DataEnergy[which (date(DataEnergy$DateTime) %in% restmv_$Date),] <-transform(DataEnergy[which (date(DataEnergy$DateTime) %in% restmv_$Date),], 
                                                                   Global_active_power = na.locf(Global_active_power),
                                                                   Global_reactive_power = na.locf(Global_reactive_power),
                                                                   Voltage = na.locf(Voltage),
                                                                   Global_intensity = na.locf(Global_intensity),
                                                                   Sub_metering_1 = na.locf(Sub_metering_1),
                                                                   Sub_metering_2 = na.locf(Sub_metering_2),
                                                                   Sub_metering_3 = na.locf(Sub_metering_3),
                                                                   GAP = na.locf(GAP),
                                                                   others = na.locf(others))
summary(DataEnergy)

#bin into month
DataEnergyNew <-  DataEnergy %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(All = sum(GAP),
            Kitchen = sum(Sub_metering_1),
            Laundry = sum(Sub_metering_2),
            ACandHS = sum(Sub_metering_3),
            Others = sum(others))

#in order to do ggplot
#autoplot and plot.ts is also good for plotting time series
dfAll <- melt(DataMonth2, id.vars=c("Year","Month"))

ggplot(dfAll[dfAll$Year == "2007",], aes(x=as.numeric(Month),y = value))+
  geom_line(aes(color = variable))+
  geom_point(aes(color = variable))+
  scale_x_continuous(name = "Month", breaks = c(3,6,9,12))
#bin into day
DataDaily <- DataEnergy %>%
  group_by(year(DateTime), month(DateTime), day(DateTime)) %>%
  summarise(All = sum(GAP),
            Kitchen = sum(Sub_metering_1),
            Laundry = sum(Sub_metering_2),
            ACandHS = sum(Sub_metering_3),
            Others = sum(others))
dfDaily <- melt(DataDaily, id.vars = c("year(DateTime)", "month(DateTime)", "day(DateTime)"))



#bin into hour

hourEnergy = DataEnergy %>%
  group_by(DateTime_2=cut(DateTime, "1 hour")) %>%
  summarise(
    Kitchen = sum(Sub_metering_1),
    Laundry = sum(Sub_metering_2),
    ACandHS = sum(Sub_metering_3),
    All = sum(GAP),
    Others =sum(others))

#binning into every 15 mins
DataEnergy_2 = DataEnergy %>%
  group_by(DateTime_2=cut(DateTime, "15 min")) %>%
  summarise(
    Kitchen = sum(Sub_metering_1),
    Laundry = sum(Sub_metering_2),
    ACandHeating = sum(Sub_metering_3),
    GAP_ = sum(GAP),
    others_ =sum(others))

#prediction sucks when the granuity is too small
DataEnergy_2$DateTime_2 <- as.POSIXct(DataEnergy_2$DateTime_2, format = "%Y-%m-%d %H:%M:%S")
View(DataEnergy_2)
DataEnergy_2<- DataEnergy_2[-c(1:27),]
DataEnergy_2010 <- DataEnergy_2 %>%
  filter(year(DateTime_2) == "2010")
DataEnergy_2010ts <- ts(DataEnergy_2010, frequency = 96)
plot.ts(DataEnergy_2010ts)
GAP2010 <- DataEnergy_2010$GAP_
GAP_2010ts <- ts(GAP2010, frequency = 96)
dcGAP2010 <- decompose(GAP_2010ts)
plot(dcGAP2010)
gapForecast <- HoltWinters(GAP_2010ts)
plot(gapForecast)
checkresiduals(gapForecast)

#try day
gpadaily <- DataDaily$All
gpadailyts <- ts(gpadaily,frequency = 7)
plot.ts(gpadailyts)
dcgpadaily <-decompose(gpadailyts)
plot(dcgpadaily)


gpadailyForecast1 <- HoltWinters(gpadailyts)
adjutgpadaily <- HoltWinters(adjustdaily)
checkresiduals(gpadailyForecast1)





dailyets <- ets(gpadailyts)
checkresiduals(dailyets)




 #arima model ???
auto.arima(gpadailyts, approximation = FALSE)
acf(logfaily)
pacf(gpadailytsdf)

gpadailytsdf <- diff(gpadailyts)
adf.test(gpadailytsdf)
auto.arima(gpadailytsdf)

gpadayfit <- arima(gpadailyts,c(3,1,4),seasonal=list(order=c(0,0,2),period= 7 ))
gpadaiyfit2 <- arima(gpadailyts, order = c(2,1,3))
summary(gpadayfit)
checkresiduals(gpadayfit)
gpafordaily <- forecast(gpadayfit, 10)
plot(gpafordaily, include = 100)
summary(gpafordaily)

#try month, the best!!
energymonth <- DataEnergyNew[,3]
energymonthts <- ts(energymonth, frequency = 12)
DataEnergy2df <- DataEnergyNew %>%
  unite(YearMon, c("year(DateTime)", "month(DateTime)"))
DataEnergy2df <- DataEnergy2df[, c(1,2)]
auto.arima(energymonthts)
gpamonthfit <- arima(energymonthts, c(0,0,0), seasonal=list(order=c(1,1,0),period= 12 ))
checkresiduals(gpamonthfit)
forcasarima <- forecast(gpamonthfit, h = 12)
plot(forcasarima)
monthhw <-HoltWinters(energymonthts)
str(energymonthts)
monthets <- ets(energymonthts)
monthlm <- tslm(All~season + trend,energymonthts)
checkresiduals(monthlm)
forcaslm <- forecast(monthlm, h = 12)
plot(forcaslm)
summary(monthlm)
plot(monthlm)

