rm(list = ls())
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/")
table_folder <- "/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/Tables/"
library(data.table)
library(lfe)
library(stargazer)
library(lubridate)

## Read Main Data
gt_jobs <- readRDS("gtrends_jobs.rds")
merge_fin<- readRDS("merge_st_euc.rds")
final_merged <- merge(merge_fin, gt_jobs, by = c("state", "Year", "Month"))
final_merged <- as.data.table(final_merged)
setorder(final_merged, state, Year, Month)
final_merged[, year_month := paste(Year, Month, sep="-")]
final_merged <- final_merged[Year>=2005 & Year<2015]
final_merged <- final_merged[!(state=='LA' & Year==2005)]
final_merged[, tur_sq := unemp_rate^2]
final_merged[, iur_sq := insured_unemp_rate^2]
final_merged <- select(final_merged, 1:6,9,11:15)
final_merged[, quarter := floor(Month/4)]

# Find the mean search intensity for each state
final_merged[, mean_search := mean(jobs_search), by = state]
# Indicator for the high search period
final_merged[, high_period := as.numeric(jobs_search > mean_search)]

# Construct duration of high search periods
high_duration <- c()
for(i in 1:length(final_merged$high_period)){
  if(final_merged$high_period[i]==0){
    high_duration[i] <- 0
  }
  if(final_merged$high_period[i]==1){
    if(i==1){
      high_duration[i] <-1
    } else if(i!=1){
      high_duration[i] <- final_merged$high_period[i]+high_duration[i-1]
    }
  }
}
final_merged<- cbind(final_merged, high_duration)

# Restrict to TUR [4,10]%
final_merged <- final_merged[unemp_rate>=4 & unemp_rate<=10]

# Regressions
final_merged[, lhigh := log(high_duration+1)]
model.a <-felm(high_duration ~ alt_totwks+ unemp_rate + insured_unemp_rate+ tur_sq + iur_sq | Month | 0 | state, data = final_merged)
summary(model.a)
model.b <-felm(high_duration ~ alt_totwks+ unemp_rate + insured_unemp_rate+ tur_sq + iur_sq | state+year_month | 0 | state, data = final_merged)
summary(model.b)
model.c <-felm(high_duration ~ alt_totwks+ unemp_rate + insured_unemp_rate+ tur_sq + iur_sq+ frac_lf | state+year_month | 0 | state, data = final_merged)
summary(model.c)
model.d <-felm(high_duration ~ alt_totwks+ unemp_rate + insured_unemp_rate+ tur_sq + iur_sq+ frac_lf | state+year_month | 0 | state, data = final_merged, subset=final_merged$Year<=2011)
summary(model.d)
model.e <-felm(high_duration ~ alt_totwks+ unemp_rate + insured_unemp_rate+ tur_sq + iur_sq+ frac_lf | state+year_month | 0 | state, data = final_merged, subset=final_merged$Year>2011)
summary(model.e)

### Regression Table
line2 <- c("State FE", c("No", "Yes", "Yes", "Yes", "Yes"))
line3 <- c("Year FE", c("No", "Yes", "Yes", "Yes", "Yes"))
line4 <- c("Month FE", c("Yes", "Yes", "Yes", "Yes", "Yes"))
lines <- list(line2, line3, line4)
stargazer(model.a, model.b, model.c, model.d,model.e, title="Effect of UI extensions on high job-search duration", align=TRUE,
          dep.var.labels.include = FALSE, covariate.labels=c("Potential Benefit Duration", "Unemployment Rate", "Insured Unemployment Rate", "Unemployment Rate squared", "Insured Unemployment Rate squared", "Fraction population in labor force"), 
          out = paste(table_folder, "ext2_high_duration.tex", sep = ''), add.lines = lines, dep.var.caption = "Duration of high job-search intensity",
          omit.stat=c("ser","f", "adj.rsq"), no.space=TRUE)