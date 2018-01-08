## This file prepares necessary data to replicate Table 7
rm(list=ls()) #clear all
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas")
library(data.table)
library(haven)
library(readxl)
library(cdlTools)
library(lfe)
# Read Rothstein(2011) data
Rothstein <- read_dta("./restat_data/National_Analysis_Data/rothstein_data.dta")
Rothstein <- as.data.table(Rothstein)
# Merge with the census code
colnames(Rothstein)[6] <- "gestcen"
censcode <- read_dta("./restat_data/National_Analysis_Data/gestcenstate.dta")
Rothstein <- merge(Rothstein, censcode, by = "gestcen")

# Clean-up Rothstein data
Rothstein <- subset(Rothstein, hrmis==1 | hrmis==2 | hrmis==5 | hrmis==6)
Rothstein$totwks[Rothstein$totwks==92] <- 93
Rothstein <- subset(Rothstein, uielig==1)
setnames(Rothstein, old = c('hrmonth','hryear4'), new = c('Month','Year'))

national_current_policy_wks <- select(Rothstein, "Month","Year","State","alt_totwks")
#sum(is.na(national_current_policy_wks$alt_totwks)) #count number of NA values
national_current_policy_wks <-aggregate(national_current_policy_wks$alt_totwks, by=list(national_current_policy_wks$Year, national_current_policy_wks$Month, national_current_policy_wks$State), FUN=mean, na.rm=TRUE, na.action=NULL)
setnames(national_current_policy_wks, old = c('Group.1','Group.2', 'Group.3', 'x'), new = c('Year', 'Month', 'state', 'alt_totwks'))

# Merge state statistics(fraction of labor force, unemployment rate, etc)
# Data downloaded from BLS Local Area Unemployment Statistics, https://www.bls.gov/lau/rdscnp16.htm
statestats <- read_excel("ststdsadata.xlsx")
statestats <-statestats[!(statestats$state_long=="Los Angeles County" | statestats$state_long=="New York city"),]
statestats <- subset(statestats, Year>=2004 & Year<2017)
#FIPS to state names
state <-vector()
for (i in 1:length(statestats$FIPS)){
  state[i]<- fips(statestats$FIPS[i], to='Abbreviation')
}
statestats <-cbind(statestats,state)
ststats <- select(statestats, 3:5,9:10)
ststats$Year<-as.numeric(ststats$Year)
ststats$Month<-as.numeric(ststats$Month)
merge_st <- merge(national_current_policy_wks, ststats, by = c("state", "Year", "Month"), all=TRUE)

# Merge Other information(insured UE, PBD)
# In order to construct PBD(total_weeks variable), need to process all weekly "trigger" reports from the dept. of labor
euc_eb <- read_dta("./restat_data/National_Analysis_Data/new_euc_eb_weeks_monthly.dta")
setnames(euc_eb, old = c('month','year'), new = c('Month','Year'))
euc_eb <- select(euc_eb, 1:3,5:7,12,14)
merge_st_euc<- merge(merge_st, euc_eb, by = c("state", "Year", "Month"), all=TRUE)
merge_st_euc <- as.data.table(merge_st_euc)
merge_st_euc <- merge_st_euc[Year>2003]
merge_st_euc[!is.na(total_weeks), alt_totwks := total_weeks]
merge_st_euc[is.na(alt_totwks) & Year>2014, alt_totwks := lag(alt_totwks,1)]
saveRDS(merge_st_euc, "merge_st_euc.rds")

## Regression (Table 7)
# First merge with google trends data
gt_jobs <- readRDS("gtrends_jobs.rds")
merge_fin<- readRDS("merge_st_euc.rds")
final_merged <- merge(merge_fin, gt_jobs, by = c("state", "Year", "Month"))
final_merged[, ljobs := log(jobs_search)]

# Drop observations we don't want to use
final_merged <- final_merged[Year>=2005 & Year<2015]
final_merged <- final_merged[!(state=='LA' & Year==2005)]
# Rescale percentage into numbers
final_merged <- transform(final_merged, frac_lf = frac_lf / 100, unemp_rate = unemp_rate / 100, insured_unemp_rate= insured_unemp_rate/100)
final_merged[, unemp_rate_sq := unemp_rate^2]

# Construct state specific trends
final_merged[, year_month := paste(Year, Month, sep="-")]
setDT(final_merged)[, state_id := .GRP, by = state]
setDT(final_merged)[, ym_id := .GRP, by = year_month]
# state specific trend
for(t in unique(final_merged$state_id)){
  final_merged[,paste("state",t,sep="")] <- ifelse(final_merged$state_id==t,1,0)*final_merged$ym_id}
# trend squared
for(t in unique(final_merged$state_id)){
  final_merged[,paste("state2",t,sep="_")] <- ifelse(final_merged$state_id==t,1,0)*(final_merged$ym_id^2)}

#write_dta(final_merged, final_merged.dta)
# Regressions
final_merged_reg<- final_merged[!is.na(lag(Month,1))]
#final_merged_reg <- final_merged_reg %>% mutate(state = as.factor(state))
#final_merged_reg <- final_merged_reg %>% mutate(year_month = as.factor(year_month))
model.a <-felm(ljobs ~ alt_totwks | factor(state)+factor(year_month) | 0 | state, data = final_merged_reg)
summary(model.a,cluster = c("state") )
model.b <-felm(ljobs ~ alt_totwks+ unemp_rate | factor(state)+factor(year_month) | 0 | state, data = final_merged_reg)
summary(model.b)
model.c <-felm(ljobs ~ alt_totwks+ unemp_rate + unemp_rate_sq+ insured_unemp_rate+ frac_lf | factor(state)+factor(year_month) | 0 | state, data = final_merged_reg)
summary(model.c)
formula1<- paste('ljobs~', paste("alt_totwks+ unemp_rate + unemp_rate_sq+ insured_unemp_rate+ frac_lf+"),
              paste("state", 1:51, sep = "", collapse = "+"), paste("+"),
              paste("state2_", 1:51, sep = "", collapse = "+"), paste("| factor(state)+factor(year_month)| 0 | state"))
#form <- reformulate(alt_totwks, grep("^state", names(final_merged_reg), value = TRUE), response = "ljobs")
model.d<-felm(as.formula(formula1),final_merged_reg)
summary(model.d)
model.e <-felm(ljobs ~ alt_totwks+ unemp_rate + unemp_rate_sq+ insured_unemp_rate+ frac_lf | factor(state)+factor(year_month) | 0 | state, data = final_merged_reg, subset=final_merged_reg$Year<=2011)
summary(model.e)
model.f <-felm(ljobs ~ alt_totwks+ unemp_rate + unemp_rate_sq+ insured_unemp_rate+ frac_lf | factor(state)+factor(year_month) | 0 | state, data = final_merged_reg, subset=final_merged_reg$Year>2011)
summary(model.f)

### Regression Table
line2 <- c("State FE", rep("Yes", 6))
line3 <- c("Year-Month FE", rep("Yes", 6))
line4 <- c("Quadratic state trends", c("No", "No", "No", "Yes", "No", "No"))
lines <- list(line2, line3, line4)
table_folder <- "/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/Tables/"
stargazer(model.a, model.b, model.c, model.d,model.e, model.f, title="Effect of UI extensions and composition by state", align=TRUE, omit = "state",
          dep.var.labels.include = FALSE, covariate.labels=c("Potential Benefit Duration", "Unemployment Rate", "Unemployment Rate squared", "Insured Unemployment Rate", "Fraction population in labor force"), 
          out = paste(table_folder, "table7_aggsearch.tex", sep = ''), add.lines = lines, dep.var.caption = "Log GJSI",
          omit.stat=c("ser","f", "adj.rsq"), no.space=TRUE)