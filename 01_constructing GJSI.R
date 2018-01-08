rm(list=ls()) #clear all
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas")
library(data.table)
library(gtrendsR)
library(dplyr)
library(lubridate)

states = c("AK", "AL","AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL",
"IN", "KS", "KY", "LA", "MA", "MD", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", 
"NY", "OH", "OK", "OR", "PA", "RI", "SC", "TN", "TX", "UT", "VA", "WA", "WI", "WV", "ME", "MT", 
"WY", "ND", "SD", "VT")

# 1. Monthly Google Trend data for 'jobs'(default)
keyword="jobs -apple -steve"
gt <- data.frame()
for (i in 1:length(states)){
  geo_input = paste("US", states[i], sep="-")
  gt<- rbind(gt, gtrends(keyword, geo=geo_input, time="all")$interest_over_time)
  #gt<- rbind(gt, gtrends(keyword, geo=geo_input, res="week", start_date = as.Date("2004-01-01"), end_date = as.Date("2017-01-01") )$interest_over_time)
}

# Separate year and month
gt<- cbind(gt,Year=year(gt$date), Month=month(gt$date))
# Change the variable names
#gt$keyword = substr(gt$keyword,1,4) # Drop -apple -steve
gt$geo = substr(gt$geo,4,5) #Drop US-prefix
colnames(gt)[2] <- "jobs_search"
colnames(gt)[4] <- "state"
# Drop other variables(keyword, gprop, category)
gt_jobs <- select(gt, 2,4,7,8) #Drop unnecessary variables
# Save the index
saveRDS(gt_jobs, "gtrends_jobs.rds")

# 2. Monthly Google Trend data for 'weather'
keyword="weather"
gtw <- data.frame()
for (i in 1:length(states)){
  geo_input = paste("US", states[i], sep="-")
  gtw<- rbind(gtw, gtrends(keyword, geo=geo_input, time="all")$interest_over_time)
}

# Separate year and month
gtw<- cbind(gtw,Year=year(gtw$date), Month=month(gtw$date))
gtw$geo = substr(gtw$geo,4,5) #Drop US-prefix
colnames(gtw)[2] <- "weather"
colnames(gtw)[4] <- "state"
# Drop other variables(keyword, gprop, category)
gt_weather <- select(gtw, 2,4,7,8) #Drop unnecessary variables
# Save the index
saveRDS(gt_weather, "gtrends_weather.rds")

## dat %>% glimpse: function call(pipe). equiv to glimpse(dat)
#mutate(speaker=str_c(firstname , "", lastname))