#initialization
info <- read.table(file="dataSem1.txt", sep=",", header=TRUE, stringsAsFactors = T) 
info$ura <- as.factor(info$ura) 
info$datum <- as.Date(info$datum)

tempInfo <- data.frame(datum = character(), stavba = integer(), avgPorava = double(),
                       maxPoraba = double(), minPoraba = double(),  sumPoraba = double())
for(i in 1:nrow(info)) {
    ID <- info$stavba[i]
    dat <- as.Date(info$datum[i])
    if(as.Date(info$datum[i]) == as.Date(min(info$datum))) {
        avgPoraba <- info$poraba[i]
        maxPoraba <- info$poraba[i]
        minPoraba <- info$poraba[i]
        sumPoraba <- info$poraba[i]
    }
    else{
        minus <- 1
        avgPoraba <- mean(info$poraba[info$datum == as.Date(dat - 1) &
                                          info$stavba == ID])
        if(is.nan(avgPoraba)) {
            minus <- 2
        }
        
        avgPoraba <- mean(info$poraba[info$datum == as.Date(dat - minus) &
                                                    info$stavba == ID])
        maxPoraba <- max(info$poraba[info$datum == as.Date(dat - minus) &
                                                    info$stavba == ID])
        minPoraba <- min(info$poraba[info$datum == as.Date(dat - minus) &
                                                    info$stavba == ID])
        sumPoraba <- sum(info$poraba[info$datum == as.Date(dat - minus) &
                                                    info$stavba == ID])
    }
    tempInfo[i,] <- c(as.character(dat), ID, avgPoraba, maxPoraba, minPoraba, sumPoraba)
}

#Weekend vs Weekday
library(chron)
info$dayOfWeek <- "Weekday" 
info$dayOfWeek[is.weekend(info$datum)] <- "Weekend" 
info$dayOfWeek <- as.factor(info$dayOfWeek) 
boxplot(info$poraba[info$dayOfWeek == "Weekend"] / 2,
    info$poraba[info$dayOfWeek == "Weekday"] / 5,
    main = "Energy consumption (in kWh) based on Day of the Week", 
    xlab = "Day of the Week", ylab = "Energy consumption (in kWh)",
    names = c("Weekend", "Weekday"))

#Seasons
library(lubridate) 
info$month <- month(info$datum)
info$season[info$month == 12 | info$month == 1 | info$month == 2] <- "Winter"
info$season[info$month == 3 | info$month == 4 | info$month == 5] <- "Spring"
info$season[info$month == 6 | info$month == 7 | info$month == 8] <- "Summer" 
info$season[info$month == 9 | info$month == 10 | info$month == 11] <- "Fall" 
info$season <- as.factor(info$season) 
plot(info$season, info$poraba, main = "Energy consumption (in kWh) based on Season", 
     xlab = "Season", ylab = "Energy consumption (in kWh)")

#Temperature intensity
info$weather <- "Moderate"
info$weather[info$temp_zraka <= 15] <- "Cold" 
info$weather[30 <= info$temp_zraka] <- "Hot"
info$weather <- as.factor(info$weather)
boxplot(info$poraba[info$weather == "Cold"] / 2,
        info$poraba[info$weather == "Moderate"] / 2,
        info$poraba[info$weather == "Hot"],
        main = "Energy consumption (in kWh) based on Intensity of Weather", 
        xlab = "Intensity of the weather", ylab = "Energy consumption (in kWh)",
        names = c("Cold", "Moderate", "Hot"))

#Pressure intensity; Low means rainier, High means warmer/clearer, Medium means present conditions continue
install.packages("dplyr")
info$pressure <- "Medium"
info$pressure[info$pritisk <= 1009] <- "Low"
info$pressure[info$pritisk >= 1023] <- "High"
info$pressure <- as.factor(info$pressure)
boxplot(info$poraba[info$pressure == "Low"] / 42618,
        info$poraba[info$pressure == "Medium"] / 134556,
        info$poraba[info$pressure == "High"] / 29611,
        main = "Energy consumption (in kWh) based on Intensity of Pressure", 
        xlab = "Intensity of the weather", ylab = "Energy consumption (in kWh)",
        names = c("Low", "Medium", "High"))

#Wind speed
info$windSpeed <- "Slow"
info$windSpeed[info$hitrost_vetra >= 5] <- "Fast"
info$windSpeed <- as.factor(info$windSpeed)
boxplot(info$poraba[info$windSpeed == "Slow"] / 163170,
        info$poraba[info$windSpeed == "Fast"] / 43615,
        main = "Energy consumption (in kWh) based on Intensity of Wind", 
        xlab = "Intensity of the wind", ylab = "Energy consumption (in kWh)",
        names = c("Slow", "Fast"))

temp <- read.csv("fml.csv")
finInfo <- cbind(info, temp[,c(4, 5, 6, 7)])

###################################################################evaluation########################################################
goodInfo <- read.csv("data.csv", sep=",", stringsAsFactors = T)
goodInfo <- na.omit(goodInfo)
goodInfo$datum <- as.Date(goodInfo$datum)
goodInfo$ura <- as.factor(goodInfo$ura)
goodInfo$X <- NULL
goodInfo$month <- NULL

infoClass <- goodInfo[-c(15, 22:25)]
infoReg <- goodInfo[-c(16)]

library(CORElearn)
sort(attrEval(norm_poraba ~ ., infoClass, "InfGain"), decreasing = TRUE)
sort(attrEval(norm_poraba ~ ., infoClass, "GainRatio"), decreasing = TRUE)
sort(attrEval(norm_poraba ~ ., infoClass, "MDL"), decreasing = TRUE)
sort(attrEval(norm_poraba ~ ., infoClass, "ReliefFequalK"), decreasing = TRUE)

sort(attrEval(poraba ~ ., infoReg, "MSEofMean"), decreasing = TRUE)
sort(attrEval(poraba ~ ., infoReg, "RReliefFexpRank"), decreasing = TRUE)
####################################################################new models#######################################################
