## ATUS search time correlation(TABLE 3)
rm(list=ls()) #clear all
library(lfe)
library(data.table)
library(stargazer)

# Load Google trends data and ATUS 
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas")
gt_jobs <- readRDS("gtrends_jobs.rds")
atus_js <- readRDS("./ATUS/atus_final2percap.rds")
gtjobs_and_atus <- merge(atus_js, gt_jobs, by = c("state", "Year", "Month"))
ljobs <-log(gtjobs_and_atus$jobs_search)
gtjobs_and_atus<-cbind(gtjobs_and_atus,ljobs)

# also merge weather search data for robustness check (optional)
gtw <- readRDS("gtrends_weather.rds")
gtjobs_and_atus <- merge(gtjobs_and_atus, gtw, by = c("state", "Year", "Month"))
lweather <-log(gtjobs_and_atus$weather)
gtjobs_and_atus<-cbind(gtjobs_and_atus,lweather)

# Run regressions (Whole 2004-2016 data. 2003 omitted due to Google Trend, 2017 due to ATUS)
model.a <- lm(JST_percap ~ ljobs + 0, data = gtjobs_and_atus)
summary(model.a, cluster=c("state"))
model.b <- lm(JST_percap ~ ljobs + 0, data = gtjobs_and_atus,subset=gtjobs_and_atus$JST_percap>0)
summary(model.b, cluster=c("state"))
model.c <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = gtjobs_and_atus)
summary(model.c, cluster = c("state"))
model.d <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = gtjobs_and_atus,subset=gtjobs_and_atus$JST_percap>0)
summary(model.d, cluster=c("state"))
model.e <- felm(JST_percap ~ ljobs + lweather+ 0| factor(state) + factor(Month), data = gtjobs_and_atus)
summary(model.e, cluster=c("state"))

#export tables
line2 <- c("State FE", c("No", "No", "Yes", "Yes", "Yes"))
line3 <- c("Year-Month FE", c("No", "No", "Yes", "Yes", "Yes"))
lines <- list(line2, line3)
table_folder <- "/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/Tables/"
stargazer(model.a, model.b, model.c,model.d, model.e, title = "ATUS Search Time Correlation, 2004-2016",column.labels=c("Search Time(ST)","Nonzero ST", "ST", "NZST", "ST"), 
          out = paste(table_folder, "table3_allyears.tex", sep = ''), add.lines = lines,
          dep.var.labels.include = FALSE, covariate.labels=c("log(GJSI)", "log(weather)"), 
          omit.stat=c("ser","f","adj.rsq"), align=TRUE, dep.var.labels = NULL, dep.var.caption = "ATUS Search Time", no.space=TRUE)

# Run regressions (2004-2009 data as in Baker and Fradkin(2017))
bfdata <- subset(gtjobs_and_atus, Year  <= 2009)
model.c1 <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = bfdata)
summary(model.c1, cluster = c("state"))
model.d1 <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = bfdata, subset=bfdata$JST_percap>0)
summary(model.d1, cluster=c("state"))
model.e1 <- felm(JST_percap ~ ljobs + lweather+ 0| factor(state) + factor(Month), data = bfdata)
summary(model.e1, cluster=c("state"))
# Regression with omitted period, after 2009
laterdata <- subset(gtjobs_and_atus, Year > 2009)
model.c2 <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = laterdata)
summary(model.c2, cluster = c("state"))
model.d2 <- felm(JST_percap ~ ljobs + 0| factor(state) + factor(Month), data = laterdata, subset=laterdata$JST_percap>0)
summary(model.d2, cluster=c("state"))
model.e2 <- felm(JST_percap ~ ljobs + lweather+ 0| factor(state) + factor(Month), data = laterdata)
summary(model.e2, cluster=c("state"))
rep("Yes", 6)
line22 <- c("State FE", rep("Yes", 6))
line23 <- c("Year-Month FE", rep("Yes", 6))
lines2 <- list(line22, line23)
stargazer(model.c1, model.d1, model.e1, model.c2,model.d2, model.e2, title = "ATUS Search Time Correlation, replicated", column.labels=c("Search Time(ST)","Nonzero ST", "ST", "ST", "NZST", "ST"), 
          out = paste(table_folder, "table3_bfandlater.tex", sep = ''), add.lines = lines2,
          dep.var.labels.include = FALSE, covariate.labels=c("log(GJSI)", "log(weather)"), 
          omit.stat=c("ser","f","adj.rsq"), align=TRUE, dep.var.labels = NULL, dep.var.caption = "ATUS Search Time", no.space=TRUE)

