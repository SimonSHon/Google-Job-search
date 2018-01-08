## ATUS file
# Data used here are ATUS activity summary files(downloadable from https://www.bls.gov/tus/datafiles_2016.htm, in case of 2016)
#from 2003 to 2016, and ATUS-CPS 2003-2016 file(https://www.bls.gov/tus/datafiles_0316.htm). 

rm(list=ls()) #clear all
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/ATUS")
library(data.table)
library(cdlTools)
library(lfe)

## Stack ATUS summary data
#data <- read.delim("./atussum_2016/atussum_2016.dat", sep=",",header=TRUE)
#readLines("atussum_2015.dat", n=2)
dat03 <- fread("atussum_2003.dat", select = c("TUCASEID","TEAGE", "t050401", "t050402","t050403", "t050404", 
"t050405", "t050499", "t180504"))
dat03$t050401 <- dat03$t050401 + dat03$t050402 #only in 2003, 2004
dat03 <- select(dat03, -4)
colnames(dat03)[1] <- "tucaseid"
emptyvector <- c("t050405", "t180504") # N/A in 2003
dat03[ , emptyvector] <- 0
dat04 <- fread("atussum_2004.dat", select = c("TUCASEID","TEAGE", "t050401", "t050402","t050403", "t050404", 
                                              "t050405", "t050499", "t170504"))
dat04$t050401 <- dat04$t050401 + dat04$t050402 #only in 2003, 2004
dat04 <- select(dat04, -4)
colnames(dat04)[1] <- "tucaseid"
colnames(dat04)[7] <- "t180504" #t170504 up to 2004
emptyvector05 <- c("t050405") # N/A in 2004
dat04[ , emptyvector05] <- 0

# From Here, No change in the code. But small problems persist...
dat05 <- fread("atussum_2005.dat", select = c("TUCASEID","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat05[ , emptyvector05] <- 0 # N/A in 2005
colnames(dat05)[1] <- "tucaseid"
dat06 <- fread("atussum_2006.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
emptyvector0599 <- c("t050405", "t050499") # N/A in 2006
dat06[ , emptyvector0599] <- 0
dat07 <- fread("atussum_2007.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat08 <- fread("atussum_2008.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat08[ , emptyvector0599] <- 0
dat09 <- fread("atussum_2009.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat09[ , emptyvector05] <- 0
dat10 <- fread("atussum_2010.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat10[ , emptyvector05] <- 0
dat11 <- fread("atussum_2011.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat11[ , emptyvector0599] <- 0
dat12 <- fread("atussum_2012.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat12[ , emptyvector0599] <- 0
dat13 <- fread("atussum_2013.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat13[ , emptyvector0599] <- 0
dat14 <- fread("atussum_2014.dat", select = c("tucaseid","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat14[ , emptyvector05] <- 0
dat15 <- fread("atussum_2015.dat", select = c("TUCASEID","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat15[ , emptyvector05] <- 0
colnames(dat15)[1] <- "tucaseid"
dat16 <- fread("atussum_2016.dat", select = c("TUCASEID","TEAGE", "t050401","t050403", "t050404", 
                                              "t050405", "t050499", "t180504"))
dat16[ , emptyvector05] <- 0
colnames(dat16)[1] <- "tucaseid"
atussum<-rbind(dat03,dat04,dat05,dat06,dat07,dat08,dat09,dat10,dat11,dat12,dat13,dat14,dat15,dat16)
atussum <- as.data.table(atussum)
#Connect ATUS-CPS data to get geographic information
atuscps <- fread("atuscps_0316.dat", select = c("TUCASEID","GESTFIPS"))
colnames(atuscps)[1] <- "tucaseid"
atuscps <- as.data.table(atuscps)
atus_final <- merge(atussum, atuscps, by = c("tucaseid"))

# Extract Year and Month
Year <- as.numeric(substr(atus_final$tucaseid, 1, 4))
Month<- as.numeric(substr(atus_final$tucaseid, 5, 6))
atus_final<-cbind(atus_final,Year,Month)

#GESTFIPS to state, using cdlTools package
#states = c("AL","AK", "NA", "AZ", "AR", "CA", "NA", "CO", "CT","DE", "DC", "FL", "GA", "NA", "HI",
#           "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME","MD", "MA", "MI", "MN", "MS", "MO", "MT",
#           "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "NA", "RI", "SC",
#           "SD", "TN", "TX", "UT", "VT", "VA", "NA", "WA", "WV","WI", "WY")
state <-vector()
for (i in 1:length(atus_final$GESTFIPS)){
  state[i]<- fips(atus_final$GESTFIPS[i], to='Abbreviation')
}
atus_final <-cbind(atus_final,state)
atus_final1 <- subset(atus_final, TEAGE >= 20 & TEAGE  <= 65) 
atus_final2 <- subset(atus_final1, atus_final1$tucaseid!=lag(atus_final1$tucaseid,1))

#Construct Job search Indicators
AllJSTimeWTravel <- atus_final2$t050401+ atus_final2$t050403+ atus_final2$t050404 + atus_final2$t050499 + atus_final2$t050405+ atus_final2$t180504
AllJSTime <- atus_final2$t050401+ atus_final2$t050403+ atus_final2$t050404 + atus_final2$t050499 + atus_final2$t050405
JSWTIndicator <- as.numeric(AllJSTimeWTravel > 0)
JSIndicator <- as.numeric(AllJSTime > 0)
JSWTIndicator[is.na(JSWTIndicator)] <- 0
JSIndicator[is.na(JSIndicator)] <- 0
#sum(AllJSTime==0) #count number of zeros
#summary(AllJSTime) #compare with the original
atus_final2 <-cbind(atus_final2,AllJSTimeWTravel,AllJSTime,JSWTIndicator,JSIndicator)

#Aggregate in terms of per capita time use in a state at one period, and then save
atus_final2percap <-aggregate(atus_final2$AllJSTime, by=list(atus_final2$Year,atus_final2$Month,atus_final2$state), FUN=mean, na.rm=TRUE, na.action=NULL)
colnames(atus_final2percap)[1] <- "Year"
colnames(atus_final2percap)[2] <- "Month"
colnames(atus_final2percap)[3] <- "state"
colnames(atus_final2percap)[4] <- "JST_percap"
saveRDS(atus_final2percap, "atus_final2percap.rds")


gtjobs_and_atus <- merge(atus_final2percap, gt, by = c("state", "Year", "Month"))
ljobs <-log(gtjobs_and_atus$jobs_search)
gtjobs_and_atus<-cbind(gtjobs_and_atus,ljobs)
model.a <- lm(JST_percap ~ ljobs + 0, data = gtjobs_and_atus)
summary(model.a, cluster=c("state"))
model.b <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = gtjobs_and_atus,subset=gtjobs_and_atus$JST_percap>0)
summary(model.b, cluster=c("state"))
model.c <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = gtjobs_and_atus)
summary(model.c, cluster = c("state"))