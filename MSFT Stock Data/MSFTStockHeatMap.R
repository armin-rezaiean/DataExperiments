library(ggplot2)
library(plyr)
library(scales)
library(zoo)
library(lubridate)

#Getting data from csv and removing row of current day unformatted data
all_content = readLines("MSFTHistorical10YearsQuotes.csv")
dat = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)

#Extracting year from date and adding new column
years <- format(as.Date(dat$Date, format="%Y-%m-%d"),"%Y")
dat$year <- years

#Creating yearmonth data column
months <- format(as.Date(dat$Date, format="%Y-%m-%d"),"%m")
string_months <- month.abb[as.numeric(months)]
dat$month_num <- months
dat$month_string <- string_months
dat$yearmonth <- paste(string_months, as.character(years), sep=" ")

#Creating monthweek data column
dayofmonth <- day(dat$Date)
week_of_month <- ceiling(dayofmonth/7)
dat$monthweek <- week_of_month

#Creating dayofweek data column
weekday_string <- (weekdays(as.Date(dat$Date,"%Y-%m-%d")))
weekday <- substr(weekday_string, 0, 3)
dat$weekday <- weekday
dat$weekdaynum <- weekday

#Clean up data and keep relevant columns
dat <- dat[, c("year", "yearmonth", "month_string", "monthweek", "weekday", "Close", "Open", "High", "Low")]

#Reorder months and weekdays so they plot in proper order instead of alphabetically
dat$weekday <- with(dat,factor(weekday,levels = c("Fri","Thu","Wed","Tue","Mon")))
dat$month_string <- with(dat, factor(month_string, levels=c("Jan","Feb","Mar","Apr","May","Jun",
                                                            "Jul","Aug","Sep","Oct","Nov","Dec")))

#Data visualization - creating heat map of closing prices
p <-ggplot(dat, aes(monthweek, weekday, fill = Close)) +   
      geom_tile(colour = "white") + 
      facet_grid(year~month_string) + 
      scale_fill_gradient(low="red", high="green") +
      labs(x="Week of Month",
           y="",
           title = "Microsoft Stock Closing Price Heatmap", 
           fill="Close")

print(p)
