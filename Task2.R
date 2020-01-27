##### Task 2 - Sub-metering ####

#Load libraries

library(ggplot2)  # data visualization
library(scales)   # date/time scales for plots
library(TSA)      # handle seasons in date-time data
library(hydroTSM) # handle seasons

library(dplyr)    # data wrangling
library(tidyr)    # reshaping data
library(stringr)  # tools for strings
library(microbenchmark) 
library(viridis)
library(forecast)

library(RMySQL)
library(DBI)
library(lubridate) # tools for handeling time (year,weeks,days etc)
library(hrbrthemes) #to get more themes in ggplot
library(reshape2)
library(plotly) # för att kunna göra tuffa linjer 
library(imputeTS)
library(stats)


## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')


# List the tables contained in the database 
dbListTables(con)


## Use asterisk to specify all attributes for download
Year2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
Year2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
Year2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
Year2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
Year2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")


### Information about the attributes ###

#date: Date in format dd/mm/yyyy
#time: time in format hh:mm:ss
#sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.



######Create new data frame with data from all COMPLETE years included######
AllYears <- bind_rows(Year2007, Year2008, Year2009)


# Explore AllYears #
str(AllYears)
summary(AllYears)
head(AllYears)
tail(AllYears)

## Combine Date and Time attribute values in a new attribute column
AllYears$DateTime <- paste(AllYears$Date,AllYears$Time,sep = " ")

## Move the DateTime attribute within the dataset
AllYears <- AllYears[,c(ncol(AllYears), 1:(ncol(AllYears)-1))]
head(AllYears)
tail(AllYears)


## Convert DateTime from POSIXlt to POSIXct 
AllYears$DateTime <- as.POSIXct(strptime(AllYears$DateTime,tz= "Europe/Paris", "%Y-%m-%d %H:%M:%S"))

## Create new attributes ##
AllYears$year <- year(AllYears$DateTime) #reads last hou of 2009 as 2010??
AllYears$month <- month(AllYears$DateTime)
AllYears$week <- week(AllYears$DateTime)
AllYears$weekday <- wday(AllYears$DateTime) 
AllYears$day <- day(AllYears$DateTime)
AllYears$hour <- hour(AllYears$DateTime)
AllYears$minute <- minute(AllYears$DateTime)
AllYears$remaining_power <- (AllYears$Global_active_power *(1000/60)) - AllYears$Sub_metering_1-
                                    AllYears$Sub_metering_2 - AllYears$Sub_metering_3

AllYears$Total_Usage <- AllYears$remaining_power + AllYears$Sub_metering_1 +
                                AllYears$Sub_metering_2 + AllYears$Sub_metering_3


########  Function for creating seasons #####

#winter=january, febryary and december of previous year
#spring= mars, april, may
#summer= june, july, august
#fall= september, october, november


seasons <- function(x){
  if(x %in% c(2:4)) {return("Spring")}
  if(x %in% c(5:7)) {return("Summer")}
  if(x %in% c(8:10)) {return("Fall")}
  if(x %in% c(11,12,1)) {return("Winter")}
  
}

### Apply season function ###

#All years
AllYears$season<- sapply(AllYears$month, function(x)seasons(x))
AllYears$season<- factor(AllYears$season,levels = c("Spring","Summer","Fall","Winter"))



########################### HANDLING MISSING ROWS AND NA´s ##############################

# Detect jumps in time -> diff computes differences of intervals
# Then we use which in order to find jumps bigger then 1

#missing<-which(diff(AllYears$DateTime) > 1 )
#missing

#Generate a sequence of datetimes for the same perior of time to avoid having jumps.
my_seq <- seq(as.POSIXct("2006-12-16 17:24:00"),as.POSIXct("2010-11-26 21:02:00"),by="min")

#Merge datasets by the common Datetime and create new rows with NA
allValues <- merge(
  x=data.frame(DateTime=my_seq),
  y=AllYears,
  all.x=TRUE)


#Uppdate columns with new DateTime column
allValues$year <- year(allValues$DateTime)
allValues$Date <- date(allValues$DateTime)
allValues$Time <-data.table::as.ITime(allValues$DateTime)
allValues$month <- month(allValues$DateTime)
allValues$week <- week(allValues$DateTime)
allValues$weekday <- wday(allValues$DateTime)
allValues$day <- day(allValues$DateTime)
allValues$hour <- hour(allValues$DateTime)
allValues$minute <- minute(allValues$DateTime)
allValues$season   <- sapply(allValues$month,function(x) seasons(x))


#Create a variable containing the variables to run in the loop
cols_to_fill <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3","remaining_power")


#Loop fo filling in missing values with imputeTS
for (j in 1:length(cols_to_fill)) {
    print(cols_to_fill[j])
    allValues[,cols_to_fill[j]] <- imputeTS::na_kalman(allValues[,cols_to_fill[j]], model = "StructTS", smooth = TRUE)
}  


#Turn the NA that were to big for na_kalman function to handle into 0
# Equals energy usage with 0 those specific days
allValues[which(allValues$Sub_metering_1 < 0),]$Sub_metering_1 <- 0
allValues[which(allValues$Sub_metering_2 < 0),]$Sub_metering_2 <- 0
allValues[which(allValues$Sub_metering_3 < 0),]$Sub_metering_3 <- 0
allValues[which(allValues$remaining_power < 0),]$remaining_power <- 0


#Create variable for Total usage and Global active power in new data frame
allValues$Total_Usage <- (allValues$Sub_metering_1 + allValues$Sub_metering_2 + allValues$Sub_metering_3 +
                            allValues$remaining_power)

allValues$Global_active_power <- (allValues$Sub_metering_1 + allValues$Sub_metering_2 + allValues$Sub_metering_3 + 
                                    allValues$remaining_power)*(60/1000)


#Reasure there is no remaining missing values
E<-which(is.na(allValues))
E

#Take away 2010 and replace with 2009
allValues <- filter(allValues, year!= "2010")
tail(allValues)



##################################### PLOTTING  ########################################### 
          ## Energy consumption viewed upon with different time settings ##

### Subset the 9th day of January 2008 - All observations
houseDay <- filter(allValues, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')


################# By day 

## Day:Sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  #add_trace(y = ~houseDay$Total_Usage, name = 'Total Usage', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Day: Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(AllYears, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##Plotting the total usage 9th of january - 10 min frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Total_Usage, name = 'Total energy consumption', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


############### By week 

# Week: 6 hour frequency
houseWeek38 <- filter(allValues, year == 2008 & week == 38  & (hour == 0 | hour == 6 | hour == 12 | hour == 18 ))

## Plot sub-meter 1, 2 and 3 with title, legend and labels -  6 hour frequency
plot_ly(houseWeek38, x = ~houseWeek38$DateTime, y = ~houseWeek38$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek38$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek38$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  #add_trace(y = ~houseWeek38$Total_Usage, name = 'Total Usage', mode = 'lines')%>%
  layout(title = "Power Consumption of week 38 year 2008",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)"))

# Week: 3h frequency
houseWeek38new <- filter(allValues, year == 2008 & week == 38  & (hour == 0 | hour == 3 | hour == 6 | hour == 12| hour == 15 | hour == 18 | hour ==21))

## Plot sub-meter 1, 2 and 3 with title, legend and labels -  6 hour frequency
plot_ly(houseWeek38new, x = ~houseWeek38new$DateTime, y = ~houseWeek38new$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek38new$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek38new$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
 # add_trace(y = ~houseWeek38new$Total_Usage, name = 'Total Usage', mode = 'lines') %>%
  layout(title = "Power Consumption of week 38 year 2008",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)"))


################# By month 

## Month: 6h frequency
houseSept <- filter(allValues, year == 2008 & month == 9  & (hour == 0 | hour == 8 | hour == 16))

## Plot sub-meter 1, 2 and 3 with title, legend and labels -  6 hour frequency
plot_ly(houseSept, x = ~houseSept$DateTime, y = ~houseSept$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseSept$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseSept$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseSept$Total_Usage, name = 'Total Usage', mode = 'lines') %>%
  layout(title = "Power Consumption of week 38 year 2008",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)"))


## Month: 12h frequency
houseSept12 <- filter(allValues, year == 2008 & month == 9  & (hour == 0 | hour == 12))

## Plot sub-meter 1, 2 and 3 with title, legend and labels -  12 hour frequency
plot_ly(houseSept12, x = ~houseSept12$DateTime, y = ~houseSept12$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseSept12$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseSept12$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  #add_trace(y = ~houseSept12$Total_Usage, name = 'Total Usage', mode = 'lines')%>%
  layout(title = "Power Consumption of September year 2008",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)"))



################ By year 

#Year: Seasonal frequency - sub-meter 1, 2 & 3
houseYear <- filter(allValues, (month == 1|month == 4|month == 8|month == 12))

plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYear$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2007-2009",
         xaxis = list(title = "months"),
         yaxis = list (title = "Power (watt-hours)"))

#Year: Seasonal frequency - Total Usage 
houseYear <- filter(allValues, (month == 1|month == 4|month == 8|month == 12))

plot_ly(houseYear, x = ~houseYear$DateTime, y = ~houseYear$Total_Usage, name = 'Total Usage', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption 2007-2009",
         xaxis = list(title = "months"),
         yaxis = list (title = "Power (watt-hours)"))





############################### CREATING TIME SERIES AND PLOTTING ####################################

##### Subset to observations group by total energy cosnumption per day/week/month #####

#dayily 
DayData<- allValues %>% select(year,month, week, day, Sub_metering_1,Sub_metering_2, Sub_metering_3, Total_Usage) %>% 
  group_by(year,month, week, day) %>% 
  summarise(total1=sum(Sub_metering_1),total2=sum(Sub_metering_2), total3=sum(Sub_metering_3), total4=sum(Total_Usage))

#weekly
data<- allValues %>% select(year,month, week, Sub_metering_1,Sub_metering_2, Sub_metering_3, Total_Usage) %>% 
  group_by(year,month, week) %>% 
  summarise(total1=sum(Sub_metering_1),total2=sum(Sub_metering_2), total3=sum(Sub_metering_3), total4=sum(Total_Usage))

#monthly
dataMonth<- allValues %>% select(year,month, Sub_metering_1,Sub_metering_2, Sub_metering_3, Total_Usage) %>% 
  group_by(year,month) %>% 
  summarise(total1=sum(Sub_metering_1),total2=sum(Sub_metering_2), total3=sum(Sub_metering_3), total4=sum(Total_Usage))



################################Calculated energy consumption of filtered plots

#Energy consumption of the 9th day of January 2008 
filter(DayData, year==2008, month==9, day==9,sum(DayData)) 
#sub-meter 1: 236 watt-hours = o,236 kw/h
#sub-meter 2: 453 watt-hours = 0,453 kw/h
#sub_meter 3: 7728 watt-hours = 7,728 kw/h
#total usuage: 19328 watt-hours = 19,328 kw/h


#Energy consumption of week 38 year 2008
filter(data, year==2008, week==38, sum(data))
#sub-meter 1: 12705 watt hours = 12,705 kw/h
#sub-meter 2: 10002 watt hours = 10,002 kw/h
#sub-meter 3: 69029 watt hours = 69,029 kw/h
#total usuage: 166024 watt hours = 166,024 kw/h


######## SubMeter1 - Kitchen #########

###### Weekly time serie 
TSsub1<- ts(data$total1, start=c(2007,1),end = c(2009, 52), frequency=52,)

## Plot with autoplot 
autoplot(TSsub1, colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1 - Kitchen")

#plot with plot.ts
plot.ts(TSsub1)


##### Monthlt time serie
TSsub1m<- ts(dataMonth$total1, start=c(2007,1),end = c(2009, 12), frequency=12,)

## Plot with autoplot 
autoplot(TSsub1m, colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1 - Kitchen")


######### SubMeter2 - Laundry room #########

####### Weekly time serie
TSsub2 <- ts(data$total2, start=c(2007,1), end= c(2009, 52), frequency=52)
summary(TSsub2)

## Plot with autoplot 
autoplot(TSsub2, colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2 - Laundry room")

#plot with plot.ts
plot.ts(TSsub2)



####### Monthly time serie
TSsub2m<- ts(dataMonth$total2, start=c(2007,1),end = c(2009, 12), frequency=12,)

## Plot with autoplot 
autoplot(TSsub2m, colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2 - Laundry room")




######### SubMeter3 - Water heater and AC #########

###### Weekly time serie 
TSsub3 <- ts(data$total3, start=c(2007,1), end= c(2009,52), frequency=52)
summary(TSsub3)

## Plot with autoplot 
autoplot(TSsub3, colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3 - Water Heater and AC")

#Plot with plot.ts
plot.ts(TSsub3)


####### Monthly time serie
TSsub3m<- ts(dataMonth$total3, start=c(2007,1),end = c(2009, 12), frequency=12,)

## Plot with autoplot 
autoplot(TSsub3m, colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3 - Water heater and AC")



####### Total consumption ####

##### Weekly time serie
TStotal <- ts(data$total4, start=c(2007,1), end= c(2009, 52), frequency=52)

## Plot with autoplot 
autoplot(TStotal, colour = 'purple', xlab = "Time", ylab = "Watt Hours", main = "Total Usage")

#plot with plot.ts
plot.ts(TStotal)



#### Monthly time serie 
TSmonthTotal<- ts(dataMonth$total4, start=c(2007,1), end=c(2009,12), frequency=12)

## Plot with autoplot 
autoplot(TSmonthTotal, colour = 'purple', xlab = "Time", ylab = "Watt Hours", main = "Total Usage")

#plot with plot.ts
plot.ts(TSmonthTotal)




##### Plot of all sub-meters
autoplot(TSmonthTotal, series = "Total Consumption") +
  autolayer(TSsub1m, series = "Sub-meter 1") +
  autolayer(TSsub2m, series = "Sub-meter 2")+
  autolayer(TSsub3m, series = "Sub-meter 3")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Energy consumption of all sub-meters") +
  guides(colour=guide_legend(title="Series"))

#####Monthly 2008/2009 hence the trend is negative and 2007 the consumption is higher than following years
Total20082009<- ts(dataMonth$total4, start=c(2008,1), end=c(2009,12), frequency=12)

## Plot with autoplot 
autoplot(Total20082009, colour = 'black', xlab = "Time", ylab = "Watt Hours", main = "Total Usage")



######################################## FORECASTING ################################################

## Split data into training and testing
## Applying time series linear regression + obtaining R2 and RMSE
## Applying Holt-Winter model

#############################  TSLM -model ####################################

############################# Sub-meter 1 - Kitchen ####################################

# Split data into training and testing set
trainSub1 <-window(TSsub1, start= c(2007, 1), end = c(2008, 52))
testSub1 <- window(TSsub1, start= c(2009, 1), end = c(2009, 52))

#Apply linear model
fitSM1 <- tslm(trainSub1 ~ trend + season, lambda = "auto")
summary(fitSM1)

#Residuals to model
resSub1 <- residuals(fitSM1)
autoplot(resSub1, main = "Residuals tslm model Sub-meter 1")


#Create the forecast for sub-meter 1
forecastfitSM1 <- forecast(fitSM1, h=80, level=c(80,90))
summary(forecastfitSM1)


## Plot the forecast for sub-meter 1. 
plot(forecastfitSM1$mean, ylab= "Watt-Hours", xlab = "Time")

autoplot(forecastfitSM1) +
  autolayer(testSub1, series = "Test data") +
  autolayer(trainSub1, series = "Training data")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Forecast: Energy consumption Sub-meter1") +
  guides(colour=guide_legend(title="Series"))



  
############################# Sub-meter 2 ######################################

# Split data into training and testing set
trainSub2 <-window(TSsub2, start= c(2007, 1), end = c(2008, 52))
testSub2 <- window(TSsub2, start= c(2009, 1), end = c(2009, 52))

#Apply linear model
fitSM2 <- tslm(trainSub2 ~ trend + season, lambda = "auto") 
summary(fitSM2)

#Residuals to model
resSub2 <- residuals(fitSM2)
autoplot(resSub2, main = "Residuals tslm model Sub-meter 2")


## Create the forecast for sub-meter 2
forecastfitSM2 <- forecast(fitSM2, h=80, level=c(80,90))
summary(forecastfitSM2)


## Plot the forecast for sub-meter 2. 
plot(forecastfitSM2, ylab = "Watt-Hours", xlab = "Time")

autoplot(forecastfitSM2) +
  autolayer(testSub2, series = "Test data") +
  autolayer(trainSub2, series = "Training data")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Forecast: Energy consumption Sub-meter2") +
  guides(colour=guide_legend(title="Series"))




############################### Sub-meter 3 ######################################
#Predicts higher consumtions - which doesn´t make sense, look into that

# Split data into training and testing set
trainSub3 <-window(TSsub3, start= c(2007, 1), end = c(2008, 52))
testSub3 <- window(TSsub3, start= c(2009, 1), end = c(2009, 52))


#Apply linear model
fitSM3 <- tslm(trainSub3 ~ trend + season, lambda = "auto") 
summary(fitSM3)

#Residuals to model
resSub3 <- residuals(fitSM3)
autoplot(resSub3, main = "Residuals tslm model Sub-meter 3")

## Create the forecast for sub-meter 3
forecastfitSM3 <- forecast(fitSM3, h=80, level=c(80,90))
summary(forecastfitSM3)


## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3, ylab = "Watt-Hours", xlab="Time")


autoplot(forecastfitSM3) +
  autolayer(testSub3, series = "Test data") +
  autolayer(trainSub3, series = "Training data") +
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Forecast: Energy consumption Sub-meter3") +
  guides(colour=guide_legend(title="Series"))



############################### Total Usage ###################################

###Weekly

# Split data into training and testing set
trainTotal <-window(TStotal, start= c(2007, 8), end = c(2008, 52))
testTotal <- window(TStotal, start= c(2009, 1), end = c(2009, 52))


#Apply linear model
fitTotal <- tslm(trainTotal ~ trend + season) 
summary(fitTotal)

#Resiudals to model
#residuals = observed - predicted 
#negative residual = predicted value too high and positive meaning too low
resTotal <- residuals(fitTotal)
autoplot(resTotal)

## Create the forecast for Total usage 
forecastfitTotal <- forecast(fitTotal, h=80, level=c(80,90))
summary(forecastfitTotal)


## Plot the forecast for Total Usage 
plot(forecastfitTotal, ylab = "Watt-Hours", xlab= "Time")

autoplot(forecastfitTotal) +
  autolayer(testTotal, series = "Test data") +
  autolayer(trainTotal, series = "Training data")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Weekly forecast: Energy consumption of total usage") +
  guides(colour=guide_legend(title="Series"))



####Monthly
trainMonthTotal <-window(TSmonthTotal, start= c(2007, 1), end = c(2008, 12))
testMonthTotal <- window(TSmonthTotal, start= c(2009, 1), end = c(2009, 12))


#Apply linear model
fitMonthTotal <- tslm(trainMonthTotal ~ trend + season) 
summary(fitMonthTotal)

#Resiudals to model
#residuals = observed - predicted 
#negative residual = predicted value too high and positive meaning too low
resMonthTotal <- residuals(fitMonthTotal)
autoplot(resMonthTotal)

## Create the forecast for Total usage 
forecastMonthTotal <- forecast(fitMonthTotal, h=18, level=c(80,90))
summary(forecastMonthTotal)


## Plot the forecast for Total Usage 
plot(forecastMonthTotal, ylab = "Watt-Hours", xlab= "Time")

autoplot(forecastMonthTotal) +
  autolayer(testMonthTotal, series = "Test data") +
  autolayer(trainMonthTotal, series = "Training data")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Monthly forecast: Energy consumption of total usage") +
  guides(colour=guide_legend(title="Series"))



### Forecasted consumption of predicted period:
#Jan 2010      1121054.1 873082.5 1369025.8 794430.5 1447677.7
#Feb 2010      1161352.9 913381.3 1409324.6 834729.4 1487976.5
#Mar 2010       925049.7 677078.1 1173021.4 598426.2 1251673.3
#Apr 2010       995018.6 747047.0 1242990.3 668395.1 1321642.2
#May 2010       749256.5 501284.9  997228.2 422633.0 1075880.1
#Jun 2010       790876.1 542904.5 1038847.8 464252.6 1117499.7

### 2008 and 2009  eventuellt fråga om hur detta funkish?

#Split data into training and testing
train20082009 <-window(Total20082009, start= c(2008, 1), end = c(2009, 9))
test20082009 <- window(Total20082009, start= c(2009, 10), end = c(2009, 12))

#Apply linear model
fit20082009 <- tslm(train20082009 ~ trend + season) 
summary(fit20082009)

#residuals
res20082009 <- residuals(fit20082009)
autoplot(res20082009)

## Create the forecast for Total usage 
forecast20082009 <- forecast(fit20082009, h=18, level=c(80,90))
summary(forecast20082009)


## Plot the forecast for Tota usuage 2008-2009 
plot(forecast20082009, ylab = "Watt-Hours", xlab= "Time")

autoplot(forecast20082009) +
  autolayer(test20082009, series = "Test data") +
  autolayer(train20082009, series = "Training data")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Monthly forecast due to recent years: Energy consumption of total usage") +
  guides(colour=guide_legend(title="Series"))

############################### HOLT WINTER FORCASTING #####################################


########## Sub-meter 1 ###########

##### Decomposing

##Decomposing Sub-meter 1 into trend, seasonal and remainder
componentsSub1 <- decompose(TSsub1)
summary(componentsSub1)
x1<-stl(TSsub1, "periodic")

## Plot decomposed sub-meter 1 
plot(componentsSub1)
plot(x1)

### Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
Sub1Adjusted <- TSsub1 - componentsSub2$seasonal

#Plot ajusted sub-meter 1 
autoplot(Sub1Adjusted, colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1 - Kitchen")

## Test Seasonal Adjustment by running Decompose again
plot(decompose(Sub1Adjusted))


########### Apply Holt Winter to Sub-meter 1 

# Split data into training and testing set
#AdtrainSub1 <- window( Sub1Adjusted, start= c(2007, 1), end = c(2008, 12))
#AdtestSub1  <- window( Sub1Adjusted, start= c(2009, 1), end = c(2009, 12))


## Holt Winters Exponential Smoothing & Plot
Sub1Holt <- HoltWinters(trainSub1)
plot(Sub1Holt)

## HoltWinters forecast & plot
Sub1HoltFor <- forecast(Sub1Holt, h=25)
plot(Sub1HoltFor, ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

## With diminished confidence levels
Sub1HoltForC <- forecast(Sub1Holt, h=25, level=c(10,25))

## Plot only the forecasted area
plot(Sub1HoltForC, ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

summary(Sub1HoltForC)


########## Sub-meter 2 ###########

###### Decomposing 

#Decompose Sub-meter 2 into trend, seasonal and remainder
componentsSub2 <- decompose(TSsub2)
x2 <-stl(TSsub2, "periodic")

## Plot decomposed sub-meter 2
plot(componentsSub2)
plot (x2)

## Check summary statistics for decomposed sub-meter 2
summary(componentsSub2)

### Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
Sub2Adjusted <- TSsub2 - componentsSub2$seasonal

#Plot adjusted sub-meter 2
autoplot(Sub2Adjusted, colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2 - Laundry room")

## Test Seasonal Adjustment by running Decompose again
plot(decompose(Sub2Adjusted))


##### Applying Holt Winters at Sub-meter 2 

# Split data into training and testing set
#AdtrainSub2 <- window( Sub2Adjusted, start= c(2007, 1), end = c(2008, 12))
#AdtestSub2 <-  window( Sub2Adjusted, start= c(2009, 1), end = c(2009, 12))

## Holt Winters Exponential Smoothing & Plot 
#no trend, gamma=NULL = no seasonality)
Sub2Holt <- HoltWinters(trainSub2)
plot(Sub2Holt)

## HoltWinters forecast & plot
Sub2HoltFor <- forecast(Sub2Holt, h=25)
plot(Sub2HoltFor, ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

## With diminished confidence levels
Sub2HoltForC <- forecast(Sub2Holt, h=25, level=c(10,25))

## Plot only the forecasted area
plot(Sub2HoltForC, ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

#Summary sub-meter 2
summary(Sub2HoltForC)



############### Sub-meter 3 #################

###### Decomposing 

#Decompose sub-meter 3 into trend, seasonal and remainder
componentsSub3 <- decompose(TSsub3)
x3<- stl(TSsub3, "periodic")

## Plot decomposed sub-meter 3 
plot(componentsSub3)
plot(x3)

## Check summary statistics for decomposed sub-meter 3 
summary(componentsSub3)

### Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
Sub3Adjusted <- TSsub3 - componentsSub3$seasonal

#Plot
autoplot(Sub3Adjusted, colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3 - Water Heater and AC")

## Test Seasonal Adjustment by running Decompose again
plot(decompose(Sub3Adjusted))


###### Applying Holt Winters to sub-meter 3 

# Split data into training and testing set
#AdtrainSub3 <- window (Sub3Adjusted, start= c(2007, 1), end = c(2008, 12))
#AdtestSub3  <- window (Sub3Adjusted, start= c(2009, 1), end = c(2009, 12))

## Holt Winters Exponential Smoothing & Plot - SAME ERROR no or less than 2 periods
Sub3Holt <- HoltWinters(trainSub3)
plot(Sub3Holt)


## HoltWinters forecast & plot
Sub3HoltFor <- forecast(Sub3Holt, h=25)
plot(Sub3HoltFor, ylab= "Watt-Hours", xlab= "Time - Sub-meter 3")


## With diminished confidence levels
Sub3HoltForC <- forecast(Sub3Holt, h=25, level=c(10,25))


## Plot only the forecasted area
plot(Sub3HoltForC, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")



####### Total Usage #########

### Weekly

##Decomposing Total Usage into trend, seasonal and remainder
componentsTotal <- decompose(TStotal)

## Plot decomposed sub-meter 1 
plot(componentsTotal)

## Check summary statistics for decomposed sub-meter 
summary(componentsTotal)

### Seasonal adjusting Total Usage by subtracting the seasonal component & plot
TotalAdjusted <- TStotal - componentsTotal$seasonal

#Plot adjusted Total Usage  
autoplot(TotalAdjusted, colour = 'purple', xlab = "Time", ylab = "Watt Hours", main = "Total energy consumption")

## Test Seasonal Adjustment by running Decompose again
plot(decompose(TotalAdjusted))


#Plot only the trend of total consumption
autoplot(componentsTotal)

###### Applying Holt Winters to Total Usage 

# Split data into training and testing set
#AdtrainTotal <- window (TotalAdjusted, start= c(2007, 1), end = c(2008, 12))
#AdtestTotal  <- window (TotalAdjusted, start= c(2009, 1), end = c(2009, 12))


## Holt Winters Exponential Smoothing & Plot
SubTotalHolt <- HoltWinters(trainTotal) #making an error "ts has no or less than 2 periods"
plot(SubTotalHolt)


## HoltWinters forecast & plot
SubTotalHoltFor <- forecast(SubTotalHolt, h=25)
plot(Sub3HoltFor, ylab= "Watt-Hours", xlab= "Weekly total energy consumption")


## With diminished confidence levels
Sub3HoltForC <- forecast(Sub3Holt, h=25, level=c(10,25))


## Plot only the forecasted area
plot(Sub3HoltForC, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")




### Monthly


##Decomposing Total Usage into trend, seasonal and remainder
componentsMonthTotal <- decompose(TSmonthTotal)

## Plot decomposed monthly usage
plot(componentsMonthTotal) #Använd i presentation

## Check summary statistics for decomposed monthly usuage 
summary(componentsMonthTotal)

### Seasonal adjusting Total Monthly Usage by subtracting the seasonal component & plot
TotalMonthAdjusted <- TSmonthTotal - componentsMonthTotal$seasonal

#Plot adjusted Total Usage  
autoplot(TotalMonthAdjusted, colour = 'purple', xlab = "Time", ylab = "Watt Hours", main = "Total energy consumption/monthly")

## Test Seasonal Adjustment by running Decompose again
plot(decompose(TotalMonthAdjusted))

#Apply Holt Winter

trainMonthTotal <-window(TSmonthTotal, start= c(2007, 1), end = c(2008, 12))
testMonthTotal <- window(TSmonthTotal, start= c(2009, 1), end = c(2009, 12))

## Holt Winters Exponential Smoothing & Plot
MonthHolt <- HoltWinters(trainMonthTotal)
plot(MonthHolt)


## HoltWinters forecast & plot
MonthHoltFor <- forecast(MonthHolt, h=18)
plot(MonthHoltFor, ylab= "Watt-Hours", xlab= "Monthly total energy consumption")
summary(MonthHoltFor)

## With diminished confidence levels
MonthHoltForC <- forecast(Sub3Holt, h=25, level=c(10,25))

## Plot With diminished confidence levels
plot(MonthHoltForC, ylab= "Watt-Hours", xlab="Total enegry consumption/ monthly")

autoplot(MonthHoltFor) +
  autolayer(testMonthTotal, series = "Test data") +
  autolayer(trainMonthTotal, series = "Training data")+
  xlab("Year") + ylab("Watt-Hours") +
  ggtitle("Monthly forecast: Energy consumption of total usage") +
  guides(colour=guide_legend(title="Series"))


#####################################  TSstudio  ################################################
#Implement the TSstudio package on the same data

#Libraries
library(TSstudio)

# ARIMA
fit <- auto.arima(TSmonthTotal)

# Simulate 100 possible forecast path, with horizon of 60 months
firfor<-forecast_sim(model = fit, h = 6, n = 100)


#Plot time serie of total usuage
ts_plot(TSmonthTotal)

#Heat map of total usuage 
ts_heatmap(TSmonthTotal)
plot_error(fniss, error = "MAPE", palette = "Set1")

#Plot forecast of total usage/month 
plot_forecast(forecastMonthTotal, title = "Forecasted total usuage per month", Xtitle = "Year",
              Ytitle = "Watt hours", color = "Red", width = 2)


### Method copied from TSstudio #########

#Way to create many models in one 
methods <- list(arima1 = list(method = "arima",
                              method_arg = list(order = c(2,1,0)),
                              notes = "ARIMA(2,1,0)"),
                arima2 = list(method = "arima",
                              method_arg = list(order = c(2,1,2),
                                                seasonal = list(order = c(1,1,1))),
                              notes = "SARIMA(2,1,2)(1,1,1)"),
                hw = list(method = "HoltWinters",
                          method_arg = NULL,
                          notes = "HoltWinters Model"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm model with trend and seasonal components"))


# Training the models with backtesting
md <- train_model(input = TSmonthTotal,
                  methods = methods,
                  train_method = list(partitions = 4,
                                      sample.out = 12,
                                      space = 3),
                  horizon = 12,
                  error = "MAPE")

#Silumate forecast
forecast_sim(md, h, n, sim_color = "blue", opacity = 0.05,
             plot = TRUE)

################################################################################################
