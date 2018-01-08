# Google-Job-search
This repository contains data and R codes necessary to replicate Baker and Fradkin 2017, tables 3 and 7, and additional analyses on part time job search and higb-search duration.

# Replication Repository: "The Impact of Unemployment Insurance on Job Search: Evidence from Google Search Data"
(+ Two original analyses)

*Codes*

The below three are needed to replicate the table 3 of BF 2017. It is better to run them in order.
- 01_constructing GJSI.R : Construct the Google Job Search Index, from the Google Trends 2004-2017
-> generates 'gtrends_jobs'.rds and 'gtrends_weather' files
- 02_constructing ATUS.R: Merge ATUS-CPS and summary files, 2003-2016, from the Bureau of Labor Statistics: https://www.bls.gov/tus
-> requires files in 'ATUS' folder and 'ststdsadata.xlsx' file
- 03_ATUS Correlation.R: Analyzes correlation between ATUS and the GJSI, replicating table 3 of the BF 2017 paper (denoted as table 1 and 2 in "replicated" paper)

The next code replicates the table 7 of BF 2017.
- 04_Aggregate search.R: replicates the table 7 of BF 2017 (denoted as table 3 in "replicated" paper)
-> Needs files in "restat_data" folder
-> generates 'merge_st_euc'.rds file

The final two codes conduct additional analyses
- Extension1_constructing parttime.R: Generates part-time job search index, from the Google Trends 2004-2017
-> generates a 'gtrends_part_time_jobs'.rds file
- Extension1_parttime.R: Runs analysis on the part-time job search intensity (Figure 1 and Table 4)
-> requires 'gtrends_part_time_jobs'.rds, 'merge_st_euc'.rds files
- Extension2_high_search_period.R: Runs analysis on the high job-search duration (Table 5)
-> requires 'gtrends_jobs'.rds, 'merge_st_euc'.rds files

*Data*
- /ATUS: ATUS-CPS and summary files necessary to replicate the table 1 and 2(table 3 of BF 2017), 2003-2016, from the Bureau of Labor Statistics: https://www.bls.gov/tus
- /restat_data: files(from Baker and Fradkin) necessary to replicate the table 3(table 7 of BF 2017) and extensions.
