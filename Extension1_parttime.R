rm(list = ls())
setwd("/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/")
table_folder <- "/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/Tables/"
figure_folder <- "/Users/phoebe/Documents/Applied Micro 2017/Roland/rep_texas/Figures/"
library(data.table)
library(lfe)
library(stargazer)
library(lubridate)
library(tidyr)
library(ggplot2)

## Read Main Data
gt_ptjobs <- readRDS("gtrends_part_time_jobs.rds")
merge_fin<- readRDS("merge_st_euc.rds")
final_merged <- merge(merge_fin, gt_ptjobs, by = c("state", "Year", "Month"))
final_merged[, lptjobs := log(ptj_search)]
final_merged <- final_merged[Year>=2005 & Year<2015]
final_merged <- select(final_merged, 1:4,12:13)
final_merged <- as.data.table(final_merged)
setorder(final_merged, state, Year, Month)
final_merged[, year_month := paste(Year, Month, sep="-")]
final_merged[, year_month_date := ymd(paste(Year, Month, 1, sep="-"))]

# For 'blips' in weeks left of 2 months or less, smooth them out:
final_merged[, new_totwks := alt_totwks]
final_merged[, prev_altwks1 := lag(alt_totwks,1), by = list(state)]
final_merged[, prev_altwks2 := lag(alt_totwks,2), by = list(state)]
final_merged[, prev_altwks3 := lag(alt_totwks,3), by = list(state)]
final_merged[, prev_altwks_max := pmax(prev_altwks1, prev_altwks2, prev_altwks3)]
final_merged[year_month %in% c('2010-7', '2010-6') & !(state %in% c("AK", "VT")), new_totwks := prev_altwks_max]
final_merged[Year==2010 & Month==12, new_totwks := prev_altwks1]

## Construct the jump data
# Calculate Difference in PBD v. one lag before:
final_merged[, prev_totwks := lag(new_totwks,1), by = list(state)]
final_merged[, diff_weeks := new_totwks - prev_totwks]
final_merged[is.na(diff_weeks), diff_weeks := 0]

# Pull jump dates
setorder(final_merged, state, Year, Month)
jumps <- final_merged[diff_weeks>0, list(diff_weeks, state, year_month)]
# Exclude "common change" dates
exclude_dates <- c("2008-12", "2009-11", "2008-7", "2012-02-01")
jumps <- jumps[!(year_month %in% exclude_dates)]
jumps[, max_diff_weeks := max(diff_weeks), by = state]
# Keep only "largest" jump dates
jumps <- jumps[max_diff_weeks==diff_weeks]

# Pick First date of largest jumps occur(by state):
jumps[, seq_N := seq(.N), by = list(state)]
jumps <- jumps[seq_N==1]
merge_jumps <- merge(final_merged, jumps[, list(state, year_month, seq_N)], by = c("state", "year_month"), all.x = TRUE)
merge_jumps[, largest_jump := as.numeric(!is.na(seq_N))] #Turn to dummy var
merge_jumps <- select(merge_jumps, 1:7,9,15,17)

# Get the number of months from the largest (and first)jump date for each state
setorder(merge_jumps, state, Year, Month)
merge_jumps[, seq_N_all := seq(.N), by = state]
merge_jumps[, order_jump := max(largest_jump*seq_N_all), by = state]
merge_jumps[, diff_from_jump := seq_N_all - order_jump] #Year-month Difference from jump date

# Keep only [-8,7] months from the highest PBD dates (Marinescu 2017)
selected <- merge_jumps[diff_from_jump<=7 & diff_from_jump>=-8]

# loop through data and exclude observations where pbd changed:
this_state <- merge_jumps[1, state] #AK
this_state_index <- 1
this_impulse <- -8
exclude_rows <- c()
for(i in 1:dim(selected)[1]){
  this_impulse <- selected[i, diff_from_jump]
  this_diff_weeks <- selected[i, diff_weeks]
  
  if(this_impulse==-8){
    this_state <- selected[1, state]
    this_state_index <- i
    this_state_last_obs <- max(selected[i:(i+15), diff_from_jump])
  }
  if(this_impulse!=0){
    if(this_impulse<0 & this_diff_weeks!=0){
      exclude_rows <- c(exclude_rows, this_state_index:i)
    } else if(this_impulse>0 & this_diff_weeks!=0){
      exclude_rows <- c(exclude_rows, i:(i+(this_state_last_obs + 1 - this_impulse)))
    }
  }
}
exclude_rows <- unique(exclude_rows)
selected <- selected[!exclude_rows]

## Get the IRFs
selected[, jump_size := max(diff_weeks), by = list(state)]
selected[, after := as.numeric(diff_from_jump>=0)]
selected[, impulse := diff_from_jump]
# Keep only observations with before the change available
selected[, min_imp := min(impulse), by = state]
selected[, max_imp := max(impulse), by = state]
selected1 <- selected[min_imp < 0 & max_imp>0]
# Set the baseline as two months before the largest PBD
selected1 <- selected1 %>% mutate(impulse = as.factor(impulse))
selected1 <- within(selected1, impulse <- relevel(impulse, ref = "-2"))
before_after_inc <- felm(lptjobs ~ impulse |state+year_month| 0 | state, data = selected1)
summary(before_after_inc)
# Plot and export
coef1 <- as.data.table(tidy(before_after_inc))
coef1 <- coef1[term!='impulse6']
coef1[, impulse := c(-6:-3,-1:5)]
plot1 <- ggplot(coef1, aes(impulse, estimate)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=-2, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=estimate - 1.645*std.error, ymax=estimate + 1.645*std.error), 
                lwd=1, colour="blue", width=0.1) +
  geom_point(size=4, pch=21, fill="red") + scale_x_continuous(breaks=c(-6,-5,-4,-3,-1,0,1,2,3,4,5))+
  theme_bw() + xlab('Month Relative to largest PBD increase') + theme(axis.title=element_text(size=20)) + ylab("Estimate")
ggsave(plot1, file = paste(figure_folder, "inc_before_after.pdf", sep = ''))

# Change dummies to PBD, "dose-response" reaction
ba_pbd<- felm(lptjobs ~ new_totwks |state+year_month| 0 | state, data = selected1)
summary(ba_pbd)

## Difference-in-Difference specification
# Create  and add control states
selected1 <- as.data.table(selected1)
states <- unique(selected1[, state])
for(this_state in states){
  state_obs <- selected1[state==this_state]
  setnames(state_obs, "diff_from_jump", "diff_from_jump_ctr" )
  controls <- merge(merge_jumps, state_obs[, list(year_month, diff_from_jump_ctr, jump_size,after)], by = "year_month")
  # Get states without change
  controls[, num_unique := length(unique(new_totwks)), by = state]
  controls <- controls[num_unique==1 | this_state==state]
  controls[, group_fe := this_state]
  controls[, expansion := as.numeric(this_state==state)]
  # Stack control observations
  if(this_state==states[1]){
    full_controls <- controls
  } else{
    full_controls <- rbind(full_controls, controls)
  }
}

full_controls[, state_exp_fe := paste(group_fe, state, sep = '')]
full_controls <- full_controls[!is.infinite(lptjobs),] # there is one infinite value
dd_increase <- felm(lptjobs ~ new_totwks+after | state_exp_fe + year_month | 0 | state, data = full_controls, exactDOF=TRUE)
summary(dd_increase)

## Drop by EUC lapse
large_drops <- final_merged[as.Date(year_month_date) < '2014-05-01' & as.Date(year_month_date) > '2013-07-01']
large_drops <-select(large_drops, 1:3,5:9,15)
large_drops[, drop_size := min(diff_weeks * as.numeric(year_month=='2014-1')), by = state]
large_drops[, impulse := seq(.N) - 6, by = state]
large_drops[, large_drop := as.numeric(drop_size < -24)]
large_drops[, after := as.numeric(impulse>=0 & large_drop==1)]
large_drops[, impulse_factor := factor(impulse, c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4"))]
reg_drop <- felm(lptjobs ~ new_totwks+after | large_drop+year_month | 0 |state , data = large_drops)
summary(reg_drop)

### Table w/ all combined impulse regressions:
line2 <- c("Year-Month FE", rep("Yes", 4))
line3 <- c("State FE:", rep("Yes", 4))
lines <- list(line2, line3)
stargazer(ba_pbd, dd_increase, reg_drop, title="Impact of largest PBD increase on part-time job search", column.labels=c("Event","DiD", "Drop-DiD"), 
          out = paste(table_folder, "table4_ba_increase.tex", sep = ''), add.lines = lines, dep.var.caption = "Log part-time job search Google Index",
          dep.var.labels.include = FALSE, covariate.labels=c("Potential Benefit Duration", "After"), 
          omit.stat=c("ser","f","adj.rsq"), align=TRUE, dep.var.labels = NULL, no.space=TRUE)