library(tidyverse)
# CALIFORNIA ----
## Data source ----
# GA/TA/postdoc: https://ucdavis.app.box.com/v/salaryscale2122
# Notes: Reported monthly at 100% FTE; postdoc minimum


## Populating data ----
uni <- "University of California, Davis"
fall_year <- 2021
ta <- 5156.89 # annual pay estimated for a 9 months period
gsr1 <-3667.58
gsr2 <-3952.92
gsr3 <- 4383.67 # entry-level; annual pay estimated for a 12 month period
gsr4 <- 4734.83
gsr5 <- 5049.92 # masters-level; annual pay estimated for a 12 month period
gsr6 <- 5384.33
gsr7 <- 5707.67
gsr8 <- 6161.75 # senior-level; annual pay estimated for a 12 month period
postdoc <- 54540/12
  
payscale_ucd <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta,
  "TA_candidate" = ta,
  "GSR_premaster" = gsr3,
  "GSR_candidate" = gsr5,
  "postdoc" = postdoc
)

# WASHINGTON ----
## Data source ----
# GA/TA: https://grad.uw.edu/wp-content/uploads/2022-23-TA-RA-SA_salary_chart.pdf
# Notes: reported monthly at 50% FTE
# Postdoc: https://hr.uw.edu/files/labor/UAW-4121-Postdoc-2021-2023-CBA-TA.pdf
# Notes: minumim

## Populating data ----
# Notes: Converting to represent 100% monthly pay
uni <- "University of Washington"
fall_year <- 2022
ta_premaster <- 2586*2 
ta_candidate <- 2986*2
gsr_premaster <- 2586*2
gsr_candidate <- 2986*2
postdoc <- 50004/12

payscale_uw <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta_premaster,
  "TA_candidate" = ta_candidate,
  "GSR_premaster" = gsr_premaster,
  "GSR_candidate" = gsr_candidate,
  "postdoc" = postdoc
)

# FLORIDA ----
## Data source ----
# GA/TA: https://hr.ufl.edu/manager-resources/recruitment-staffing/hiring-center/preparing-an-offer/requirements-for-an-appointment/#salaries
# Notes: Reported as annual salary for 12 month; only list a minimum 
# Postdoc: https://postdoc.aa.ufl.edu/human-resources/

## Populating data ----
# Notes: Converting to represent 100% monthly pay
uni <- "University of Florida"
fall_year <- 2022
gsr_premaster <- 45507.70/12 
gsr_candidate <- NA 
ta_premaster <- NA
ta_candidate <- NA
postdoc <- 47476/12

payscale_uf <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta_premaster,
  "TA_candidate" = ta_candidate,
  "GSR_premaster" = gsr_premaster,
  "GSR_candidate" = gsr_candidate,
  "postdoc" = postdoc
)


# MICHIGAN ----
## Data source ----
# https://hr.umich.edu/sites/default/files/updated_7.18.22_preliminary_2022-2023_gsa_salary_memo_new_flint_numbers.pdf
# Notes: Reported as 4-months of 100% FTE 

## Populating data ----
# Notes: Converting to represent 100% monthly pay
uni <- "University of Michigan"
fall_year <- 2022
## All the same, reported in 4 month increments
ta_premaster <- 24055/4
ta_candidate <- 24055/4
gsr_premaster <- 24055/4
gsr_candidate <- 24055/4
postdoc <- NA # can't find

payscale_um <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta_premaster,
  "TA_candidate" = ta_candidate,
  "GSR_premaster" = gsr_premaster,
  "GSR_candidate" = gsr_candidate,
  "postdoc" = postdoc
)

# GEORGIA ----
## Data source ----
# https://grad.uga.edu/wp-content/uploads/2022/07/FY23.RATE_.SPREADSHEET-final.pdf
# Notes: Reported as yearly pay at 100% FTE 

## Populating data ----
# Notes: Converting to represent 100% monthly pay
uni <- "University of Georgia"
fall_year <- 2022
ta_premaster <- 55600/12
ta_candidate <- 60100/12
gsr_premaster <- 55600/12
gsr_candidate <- 60100/12
postdoc <- 47476/12

payscale_uga <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta_premaster,
  "TA_candidate" = ta_candidate,
  "GSR_premaster" = gsr_premaster,
  "GSR_candidate" = gsr_candidate,
  "postdoc" = postdoc
)

# INDIANA ----
## Data source: ----
# https://news.iu.edu/live/news/28008-iu-bloomington-increases-minimum-stipends-waives
# Notes: Reported as annual minimum pay (assuming at 50% based on other sources), assuming 9 months given the details of minimum hiring
# https://vpfaa.indiana.edu/doc/graduate-student-academic-appointees-guide.pdf

## Populating data ----
# Notes: Converting to represent 100% monthly pay
uni <- "University of Indiana"
fall_year <- 2022
ta_premaster <- NA
ta_candidate <- NA
gsr_premaster <- (22000*2)/9
gsr_candidate <- NA
postdoc <- NA # cannot find

payscale_ui <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta_premaster,
  "TA_candidate" = ta_candidate,
  "GSR_premaster" = gsr_premaster,
  "GSR_candidate" = gsr_candidate,
  "postdoc" = postdoc
)

# ARIZONA ----
## Data source: ----
# https://researchadmin.asu.edu/salaries-wages-and-ere#Student%20Salaries%20and%20Wages
# Notes: Grad Student pay Reported as annual minimum pay at 50% FTE based on 9 month salary

## Populating data ----
# Notes: Converting to represent 100% monthly pay
uni <- "Arizona State University"
fall_year <- 2022
ta_premaster <- NA
ta_candidate <- NA
gsr_premaster <- (21879*2)/9
gsr_candidate <- NA
postdoc <- 47476/12

payscale_asu <- data.frame(
  "university" = uni,
  "fall_year" = fall_year,
  "TA_premaster" = ta_premaster,
  "TA_candidate" = ta_candidate,
  "GSR_premaster" = gsr_premaster,
  "GSR_candidate" = gsr_candidate,
  "postdoc" = postdoc
)

# COMBINING ----

payscale <- rbind(payscale_ucd, payscale_uw,
                  payscale_uf, payscale_um,
                  payscale_uga, payscale_ui,
                  payscale_asu)

payscale$min_annual_100FTE <- rowMeans(payscale[,c("TA_premaster",
                                                   "GSR_premaster")], 
                                       na.rm = T)*12
payscale$min_annual_50FTE <- payscale$min_annual_100FTE/2
payscale$avg_annual_100FTE <- rowMeans(payscale[,c("TA_premaster",
                                                 "TA_candidate",
                                                 "GSR_premaster",
                                                 "GSR_candidate")], 
                                     na.rm = T)*12
payscale$avg_annual_50FTE <- payscale$avg_annual_100FTE/2
payscale$hourly_rate <- payscale$avg_annual_100FTE/(52*40) # Should I assume 52?
payscale$postdoc_annual <- payscale$postdoc*12

payscale$gsr_premaster_annual_100FTE = payscale$GSR_premaster*12
payscale$gsr_candidate_annual_100FTE = payscale$GSR_candidate*12
payscale$ta_premaster_annual_100FTE = payscale$TA_premaster*12
payscale$ta_candidate_annual_100FTE = payscale$TA_candidate*12
payscale$gsr_premaster_annual_50FTE = payscale$gsr_premaster_annual_100FTE/2
payscale$gsr_candidate_annual_50FTE = payscale$gsr_candidate_annual_100FTE/2
payscale$ta_premaster_annual_50FTE = payscale$ta_premaster_annual_100FTE/2
payscale$ta_candidate_annual_50FTE = payscale$ta_candidate_annual_100FTE/2

write.csv(payscale, "data/grad_student_pay.csv", row.names = F)