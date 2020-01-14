###SUB-METERING####
#


##Load libraries##

library(ggplot2)  # data visualization
library(scales)   # date/time scales for plots
library(TSA)      # handle seasons in date-time data
library(hydroTSM) # handle seasons

library(dplyr)    # data wrangling
library(tidyr)    # reshaping data
library(stringr)  # tools for strings
library(microbenchmark) #to handle 
library(viridis)

library(RMySQL)
library(DBI)
library(lubridate) # tools for handeling time (year,weeks,days etc)
library(hrbrthemes) #to get more themes in ggplot
library(reshape2)


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


##Explore data##
#Data and sub meterings from every minute

#2006# 
#Starts 2006-12-16 17:24 and ends 2006-12-31 23:59
str(Year2006)
summary(Year2006)
head(Year2006)
tail(Year2006)

#2007#
str(Year2007)
summary(Year2007)
head(Year2007)
tail(Year2007)

#2008#
str(Year2008)
summary(Year2008)
head(Year2008)
tail(Year2008)

#2009#
str(Year2009)
summary(Year2009)
head(Year2009)
tail(Year2009)

#2010#
#Ends 2010-11-26 21:02
str(Year2010)
summary(Year2010)
head(Year2010)
tail(Year2010)


######Create new data frame with data from all COMPLETE years included######

AllYears <- bind_rows(Year2007, Year2008, Year2009)


# Explore AllYears #
str(AllYears)
summary(AllYears)
head(AllYears)
tail(AllYears)

## Combine Date and Time attribute values in a new attribute column
AllYears <-cbind(AllYears,paste(AllYears$Date,AllYears$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 
colnames(AllYears)[7] <-"DateTime"

## Move the DateTime attribute within the dataset
AllYears <- AllYears[,c(ncol(AllYears), 1:(ncol(AllYears)-1))]
head(AllYears)
tail(AllYears)

#Add datetime into each complete year

#2007
Year2007<-cbind(Year2007,paste(Year2007$Date,Year2007$Time), stringsAsFactors=FALSE)
colnames(Year2007)[7] <-"DateTime"
Year2007 <- Year2007[,c(ncol(Year2007), 1:(ncol(Year2007)-1))]
head(Year2007)


#2008
Year2008 <-cbind(Year2008,paste(Year2008$Date,Year2008$Time), stringsAsFactors=FALSE)
colnames(Year2008)[7] <-"DateTime"
Year2008 <- Year2008[,c(ncol(Year2008), 1:(ncol(Year2008)-1))]
head(Year2008)

#2009
Year2009 <-cbind(Year2009,paste(Year2009$Date,Year2009$Time), stringsAsFactors=FALSE)
colnames(Year2009)[7] <-"DateTime"
Year2009 <- Year2009[,c(ncol(Year2009), 1:(ncol(Year2009)-1))]
head(Year2009)

## Convert DateTime from POSIXlt to POSIXct 
AllYears$DateTime <- as.POSIXct(strptime(AllYears$DateTime,tz= "Europe/Paris", "%Y-%m-%d %H:%M:%S"))
Year2007$DateTime <- as.POSIXct(strptime(Year2007$DateTime,tz= "Europe/Paris", "%Y-%m-%d %H:%M:%S"))
Year2008$DateTime <- as.POSIXct(strptime(Year2008$DateTime,tz= "Europe/Paris", "%Y-%m-%d %H:%M:%S"))
Year2009$DateTime <- as.POSIXct(strptime(Year2009$DateTime,tz= "Europe/Paris", "%Y-%m-%d %H:%M:%S"))

## Add the time zone
#attr(AllYears$DateTime, "tzone") <- "Europe/Paris"
#attr(Year2007$DateTime, "tzone") <- "Europe/Paris"
#attr(Year2008$DateTime, "tzone") <- "Europe/Paris"
#attr(Year2009$DateTime, "tzone") <- "Europe/Paris"


## Inspect the data frames
str(AllYears)
str(Year2007)
str(Year2008)
str(Year2009)


## Create new attributes ##


#2007
Year2007$year <- year(Year2007$DateTime)
Year2007$month <- month(Year2007$DateTime)
Year2007$week <- week(Year2007$DateTime)
Year2007$weekday <- weekdays(Year2007$DateTime)
Year2007$day <- day(Year2007$DateTime)
Year2007$hour <- hour(Year2007$DateTime)
Year2007$minute <- minute(Year2007$DateTime)
Year2007$remaining_power <- cbind(Year2007$Global_active_power *1000/60 - Year2007$Sub_metering_1 -
                             Year2007$Sub_metering_2 - Year2007$Sub_metering_3)
Year2007$Total_Usage <- cbind(Year2007$remaining_power + Year2007$Sub_metering_1 +
                               Year2007$Sub_metering_2 + Year2007$Sub_metering_3)

View(Year2007)
summary(Year2007)
glimpse(Year2007)


#2008
Year2008$year <- year(Year2008$DateTime)
Year2008$month <- month(Year2008$DateTime)
Year2008$week <- week(Year2008$DateTime)
Year2008$weekday <- weekdays(Year2008$DateTime)
Year2008$day <- day(Year2008$DateTime)
Year2008$hour <- hour(Year2008$DateTime)
Year2008$minute <- minute(Year2008$DateTime)
Year2008$remaining_power <- cbind(Year2008$Global_active_power *1000/60 - Year2008$Sub_metering_1 -
                                    Year2008$Sub_metering_2 - Year2008$Sub_metering_3)
Year2008$Total_Usage <- cbind(Year2008$remaining_power + Year2008$Sub_metering_1 +
                                Year2008$Sub_metering_2 + Year2008$Sub_metering_3)


View(Year2008)
summary(Year2008)
glimpse(Year2008)

#2009
Year2009$year <- year(Year2009$DateTime)
Year2009$month <- month(Year2009$DateTime)
Year2009$week <- week(Year2009$DateTime)
Year2009$weekday <- weekdays(Year2009$DateTime)
Year2009$day <- day(Year2009$DateTime)
Year2009$hour <- hour(Year2009$DateTime)
Year2009$minute <- minute(Year2009$DateTime)
Year2009$remaining_power <- cbind(Year2009$Global_active_power *1000/60 - Year2009$Sub_metering_1 -
                                    Year2009$Sub_metering_2 - Year2009$Sub_metering_3)
Year2009$Total_Usage <- cbind(Year2009$remaining_power + Year2009$Sub_metering_1 +
                                Year2009$Sub_metering_2 + Year2009$Sub_metering_3)


View(Year2009)
summary(Year2009)
glimpse(Year2009)

#All years
AllYears$year <- year(AllYears$DateTime) #reads last hou of 2009 as 2010??
AllYears$month <- month(AllYears$DateTime)
AllYears$week <- week(AllYears$DateTime)
AllYears$weekday <- weekdays(AllYears$DateTime)
AllYears$day <- day(AllYears$DateTime)
AllYears$hour <- hour(AllYears$DateTime)
AllYears$minute <- minute(AllYears$DateTime)
AllYears$remaining_power <- cbind(AllYears$Global_active_power *1000/60 - AllYears$Sub_metering_1 -
                                    AllYears$Sub_metering_2 - AllYears$Sub_metering_3)

AllYears$Total_Usage <- cbind(AllYears$remaining_power + AllYears$Sub_metering_1 +
                                AllYears$Sub_metering_2 + AllYears$Sub_metering_3)

#AllYears$Mean_Day <- AllYears%>%
 # group_by(day)%>%
#mutate(Mean_day= mean(Total_Usage))
       
#AllYears$Mean_Day

View(AllYears)
summary(AllYears)
glimpse(AllYears)




#############Function for creating seasons#####################

#winter=january, febryary and december of previous year
#spring= mars, april, may
#summer= june, july, august
#fall= september, october, november



seasons <- function(x){
  if(x %in% 2:4) return("Spring")
  if(x %in% 5:7) return("Summer")
  if(x %in% 8:10) return("Fall")
  if(x %in% c(11,12,1)) return("Winter")

}

### Apply season function ###

#All years
AllYears$season<- sapply(AllYears$month, function(x)seasons(x))
AllYears$season<- factor(AllYears$season,levels = c("Spring","Summer","Fall","Winter"))


#2007
Year2007$season<- sapply(Year2007$month, function(x)seasons(x))

# 2008 
Year2008$season<- sapply(Year2008$month, function(x)seasons(x))

# 2009
Year2009$season<- sapply(Year2009$month, function(x)seasons(x))

View(Year2009)



#Difference among submeterings
arrange(Sub_metering_1)



################ Visualization of submetering over time  ################

# Energu usage per sub_meter every season and year
AllYears_grouped <- AllYears %>% group_by(year,season) %>% 
  summarise(New_total=sum(Total_Usage),
            New_sub1=sum(Sub_metering_1),
            New_sub2=sum(Sub_metering_2),
            New_sub3=sum(Sub_metering_3),
            Not_measured=sum(remaining_power))

AllYears_grouped <- as.data.frame(AllYears_grouped)

AllYears_grouped <- AllYears_grouped[c(1:12),]

AllYears_grouped_melted <- melt(AllYears_grouped,
                                id.vars = c("year","season"),
                                measure.vars = c("New_sub1", "New_sub2", "New_sub3", "Not_measured"))

ggplot(AllYears_grouped_melted,aes(x=variable,y=value,fill=variable))+ geom_col() + facet_grid(year~season)+ theme(axis.text.x = element_blank())




# Sub-meter 1 over all years
AllYears %>%
  ggplot( aes(x=month, y=Sub_metering_1)) +
  geom_line(color="blue") +
  geom_point(color="blue", size=4) +
  ggtitle("Sub-metering 1") +
  ylab("Energy used") +
  theme_ipsum() +  facet_wrap(~year) 




###### Mean usage per month all years #####

use_mean <- AllYears %>% 
  group_by(month) %>% 
  summarise(mean_energy_use = mean(Total_Usage))
use_mean<-use_mean[c(1:12),]
use_mean


ggplot(use_mean, aes(fill=month, y=mean_energy_use, x=month)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Mean usage per month")+ scale_x_discrete(breaks=c("1","2","3",
  "4", "5", "6", "7", "8", "9", "10", "11", "12" ),labels=c("January", "February", "Mars",
  "April", "May", "June", "July", "August", "September", "Oktober", "November", "December"))


#### #Mean usage per day #####
use_day <- AllYears %>% 
  group_by(hour) %>% 
  summarise(mean_energy_use_day = mean(Total_Usage))
use_day

        
ggplot(use_day, aes(fill=hour, y=mean_energy_use_day, x=hour)) + geom_bar(position="dodge", stat="identity") + ggtitle("Mean usage per day") 


                                                                                                                                          

################ Summaries ########
summary(AllYears$Sub_metering_1)
summary(AllYears$Sub_metering_2)
summary(AllYears$Sub_metering_3)
summary(AllYears$Total_Usage)
summary(AllYears$remaining_power)

