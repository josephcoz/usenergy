#Written by: Joe Cosby
#Date: Jan 16 2019

# US residential energy consumption
# http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption
data1 <- read.csv("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T02.01")
# subset to TERCBUS Total Energy Consumed by the Residential Sector
data2 <- subset(data1, MSN == "TERCBUS")
# subset to "your lifetime"
data3 <- subset(data2, data2$YYYYMM > 199100)
# remove yearly total (coded "month 13", every 13th obs)
data4 <- subset(data3, data3$YYYYMM %% 100 != 13)
energy <- data4$Value

#check data
head(energy)
tail(energy)

#plot time series
plot.ts(energy, ylab = "Energy Usage (in trillion Btu)")

#it does appear that there is a monthly pattern of energy usage. This is probably
#due to seasonal changes in energy use, such as using heaters in the winter
#or not using climate control during more temperate months

#ANALYSIS

#according to plot, data IS ADDITIVE

#data HAS CONSTANT MEAN CHANGE according to plot

#Model: ARIMA(1,1,1) X (1,1,1)_12
#attach astsa library to fit seasonal ARIMA
library(astsa)

energyARIMA <- sarima(energy, 1, 1, 1, 1, 1, 1, 12)

energy_future <- sarima.for(energy, n.ahead = 27, 1, 1, 1, 1, 1, 1, 12)

energy_future_U <- energy_future$pred + qnorm(.975) * energy_future$se
energy_future_L <- energy_future$pred - qnorm(.975) * energy_future$se

energy_future_tbl <- cbind(energy_future$pred, energy_future_U, energy_future_L)

#check table
energy_future_tbl

#format table
energy_future_tbl <- as.data.frame(energy_future_tbl)

#check formatting
class(energy_future_tbl)

#change column names
colnames(energy_future_tbl) <- c("2 Year Predictions by Indexed Month", "Upper Prediction Interval",
"Lower Prediction Interval")

#check energy index length
length(energy)

#create final plot
plot(energy, type = "b", xlim = c(0, 360))
lines(334:360, energy_future$pred, col = "darkorange", type = "b", pch = 19)
lines(334:360, energy_future_U, col = "gold", lty = 2)
lines(334:360, energy_future_L, col = "gold", lty = 2)

