rm(list=ls()) #clear all
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas")
library(data.table)
library(gtrendsR)
library(dplyr)
library(lubridate)
library(stringr)
states = c("AK", "AL","AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL",
           "IN", "KS", "KY", "LA", "MA", "MD", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", 
           "NY", "OH", "OK", "OR", "PA", "RI", "SC", "TN", "TX", "UT", "VA", "WA", "WI", "WV", "ME", "MT", 
           "WY", "ND", "SD", "VT")

# 1. Monthly Google Trend data for 'part time jobs'
keyword="part time jobs"
gtp <- data.frame()
for (i in 1:length(states)){
  geo_input = paste("US", states[i], sep="-")
  gtp<- rbind(gtp, gtrends(keyword, geo=geo_input, time="all")$interest_over_time)
}

# Separate year and month
gtp<- cbind(gtp,Year=year(gtp$date), Month=month(gtp$date))
# Change the variable names
gtp$geo = substr(gtp$geo,4,5) #Drop US-prefix
colnames(gtp)[2] <- "ptj_search"
colnames(gtp)[4] <- "state"
# Drop other variables(keyword, gprop, category)
gt_ptjobs <- select(gtp, 2,4,7,8) #Drop unnecessary variables
# Save the index
saveRDS(gt_ptjobs, "gtrends_part_time_jobs.rds")

### Below procedurs is needed to correct extreme values(0 and 100).
## It follows Stephens-Davidowitz(2013)
## 3. Read Monthly Google Trend data for 'weather'
#gt_weather <- readRDS("gtrends_weather.rds")
## Eliminates state with weather==0 or 100
#gtall_no_ext <- gtall[weather!=0 | weather!=100]
#gtall_mean <-aggregate(gtall_no_ext$ptj_search, by=list(gtall_no_ext$state), FUN=mean, na.rm=TRUE, na.action=NULL)