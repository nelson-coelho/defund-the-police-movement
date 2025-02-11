# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code filters the databases for the article.
#
# Funding: This research was supported by funding from the Center for Studies of 
# the Economic Order at the Federal University of Sao Paulo (CEOE/Unifesp). The 
# funding was allocated through Decentralized Execution Agreement (02/2020) in 
# partnership with the Diffuse Rights Fund, managed by the National Consumer 
# Secretariat of the Ministry of Justice and Public Security (Process SEI 
# no. 08012.003253/2018-45).
#
# Author: Nelson Coelho
# Organization: Centro de Estudos da Ordem Economica - CEOE/Unifesp
# Date: 2024-11-18
#
# File Name: database_filter_3rd.R
#
# Description: This file contains R code to filter the databases
#              used in the article.
#
# Modifications:
#
# Changes:
#
# - 2024-11-18: Initial version by Nelson Coelho
#
# ==============================================================================

# Install and load required packages ===========================================
required_packages <- c("dplyr", "plyr", "haven", "R.utils", 
                       "tibble", "tidyr", "xts", "tidyverse", "Hmisc",
                       "readxl", "knitr", "janitor", "jsonlite", "readr",
                       "tidycensus", "skimr", "devtools", 
                       "magrittr", "kableExtra", "data.table")

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load libraries ===============================================================
library(dplyr)       # Used for data manipulation
library(plyr)        # Used for data manipulation
library(haven)       # Import and Export 'SPSS', 'Stata' and 'SAS' Files
library(R.utils)     # Various Programming Utilities
library(tibble)      # Provides a 'tbl_df' class (the 'tibble')
library(tidyr)       # Used for data manipulation
library(xts)         # Used for data manipulation
library(tidyverse)   # Used for data manipulation
library(Hmisc)       # Used for data manipulation
library(readxl)      # Read excel files
library(knitr)       # Used for data manipulation
library(janitor)     # Used for data manipulation
library(jsonlite)    # Converting between R objects and JSON
library(readr)       # Read csv files
library(tidycensus)  # Used to retrieve CENSUS data
library(skimr)       # Used to provide summary statistics
library(devtools)    # Used to install packages
library(magrittr)    # A Forward-Pipe Operator
library(kableExtra)  # Adds to "magrittr" and "knitr"
library(data.table)  # Converts list into dataframe


#===============================================================================
# Load and prepare the data ====================================================
# Loading main db
main_initial_db_w_names <- read.csv('C:/main_initial_db_w_names.csv')


#lower caps
main_initial_db_w_names$state <- tolower(main_initial_db_w_names$state)
main_initial_db_w_names$census_name <- tolower(main_initial_db_w_names$census_name)

#create BREAKlns
#use this to join acs table with shr
main_initial_db_w_names <- main_initial_db_w_names %>% unite('BREAKln_comma', census_name, state , sep = ", ", remove = FALSE) 
main_initial_db_w_names <- main_initial_db_w_names %>% unite('BREAKln_censusname', year, BREAKln_comma, remove = FALSE)
ACS <- ACS %>% unite('BREAKln_censusname', year, NAME, remove = FALSE)


#removing vars
ACS <- subset(ACS, select = -c(BREAKln, year))

#join dbs
main_initial_db_w_ACS <- left_join(main_initial_db_w_names, ACS, by = "BREAKln_censusname")

#save copies
#write.csv(main_initial_db_w_ACS, "C:/main_initial_db_w_ACS.csv", row.names=FALSE)
#write.csv(ACS, "C:/ACS_v2.csv", row.names=FALSE)


#new month var
main_initial_db_w_ACS$month_of_offense <- main_initial_db_w_ACS$month

#transforming month names to numbers
#doing this to make it easier to join and clean later, also to define periods
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="1",'january',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="2",'february',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="3",'march',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="4",'april',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="5",'may',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="6",'june',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="7",'july',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="8",'august',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="9",'september',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="10",'october',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="11",'november',month_of_offense))
main_initial_db_w_ACS <-main_initial_db_w_ACS %>% mutate(month_of_offense=ifelse(month_of_offense=="12",'december',month_of_offense))

#create a new breakln to join with homs and JH dbs below
main_initial_db_w_ACS <- main_initial_db_w_ACS %>% unite('BREAKln_ymo', year, month_of_offense, ori, remove = FALSE)


#===============================================================================
# now, we have joined ACS data (GEOID) with the oris
# next we filter for homicides and for JHs
#===============================================================================






#===============================================================================
# Filtering homicide data ======================================================
#here we filter by making sure that the murder is not a justifiable homicide
murder_db <- shr_db %>%  filter(!shr_db$offender_1_circumstance=='felon killed by private citizen' 
                                & !shr_db$offender_1_circumstance=='felon killed by police')
#below is done to account for additional victims
murder_db_additional <- shr_db %>%  filter(!shr_db$offender_1_circumstance=='felon killed by private citizen' 
                                           & !shr_db$offender_1_circumstance=='felon killed by police' 
                                           & shr_db$additional_victim_count > 0)

#doing the counts of homs
murder_db_ct <- murder_db %>%
  group_by(ori9, BREAKln) %>%
  tally()

murder_db_ct <- as.data.frame(murder_db_ct)

#cleaning and adding breaklns
murder_db_ct <- murder_db_ct %>% unite('BREAKln_ymo', BREAKln, ori9, remove = FALSE)
murder_db_additional <- murder_db_additional %>% unite('BREAKln_ymo', BREAKln, ori9, remove = FALSE)
murder_db_additional <- subset(murder_db_additional, select = c(additional_victim_count, BREAKln_ymo))

#below join tables
murder_db_ct <- left_join(murder_db_ct, murder_db_additional, by = "BREAKln_ymo")

#rename
murder_db_ct <- murder_db_ct %>% rename_with( ~"homs_occurrence", "n")
murder_db_ct <- murder_db_ct %>% rename_with( ~"homs_additional_victim_count", "additional_victim_count")




#===============================================================================
# Filtering for citizen perpetrator justifiable homicide data ==================
JH_db_new_cit <- shr_db %>%  filter(shr_db$offender_1_circumstance=='felon killed by private citizen') #filter by occurrence of CJH
JH_db_new_cit_additional <- JH_db_new_cit %>%  filter(JH_db_new_cit$additional_victim_count > 0) #filter if there were additional victims


#doing the counts of CJH
JH_db_new_cit_ct <- JH_db_new_cit %>%
  group_by(ori9, BREAKln) %>%
  tally()

JH_db_new_cit_ct <- as.data.frame(JH_db_new_cit_ct) #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% unite('BREAKln_ymo', BREAKln, ori9, remove = FALSE)
JH_db_new_cit_additional <- JH_db_new_cit_additional %>% unite('BREAKln_ymo', BREAKln, ori9, remove = FALSE)
JH_db_new_cit_additional <- subset(JH_db_new_cit_additional, select = c(additional_victim_count, BREAKln_ymo))

#below join tables
JH_db_new_cit_ct <- left_join(JH_db_new_cit_ct, JH_db_new_cit_additional, by = "BREAKln_ymo")


#renaming other variables
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% rename_with( ~"cit_JHs_occurrence", "n")
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% rename_with( ~"cit_additional_victim_count", "additional_victim_count")




#===============================================================================
# Filtering for police perpetrator justifiable homicide data ===================
JH_db_new_pol <- shr_db %>%  filter(shr_db$offender_1_circumstance=='felon killed by police') #filter by occurrence of PJH
JH_db_new_pol_additional <- JH_db_new_pol %>%  filter(JH_db_new_pol$additional_victim_count > 0) #filter if there were additional victims


#doing the counts of PJH
JH_db_new_pol_ct <- JH_db_new_pol %>%
  group_by(ori9, BREAKln) %>%
  tally()


JH_db_new_pol_ct <- as.data.frame(JH_db_new_pol_ct) #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% unite('BREAKln_ymo', BREAKln, ori9, remove = FALSE)
JH_db_new_pol_additional <- JH_db_new_pol_additional %>% unite('BREAKln_ymo', BREAKln, ori9, remove = FALSE)
JH_db_new_pol_additional <- subset(JH_db_new_pol_additional, select = c(additional_victim_count, BREAKln_ymo))

#below join tables
JH_db_new_pol_ct <- left_join(JH_db_new_pol_ct, JH_db_new_pol_additional, by = "BREAKln_ymo")


#renaming other variables
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_JHs_occurrence", "n")
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_additional_victim_count", "additional_victim_count")



#===============================================================================
# Cleaning and joining cit and pol JH with homs db  ============================

#remove dups
# JH_db_new_ct <- JH_db_new_ct %>% distinct(BREAKln, .keep_all = TRUE)
JH_db_new_pol_ct1 <- JH_db_new_pol_ct %>% distinct(BREAKln, .keep_all = TRUE)
JH_db_new_cit_ct1 <- JH_db_new_cit_ct %>% distinct(BREAKln, .keep_all = TRUE)

#remove vars for join
# JH_db_new_ct <- subset(JH_db_new_ct, select = -c(BREAKln, ori9))
JH_db_new_pol_ct1 <- subset(JH_db_new_pol_ct1, select = -c(BREAKln, ori9))
JH_db_new_cit_ct1 <- subset(JH_db_new_cit_ct1, select = -c(BREAKln, ori9))


#join homs, CJH, and PJH tables
JH_db_new_ct1 <- left_join(murder_db_ct, JH_db_new_pol_ct1, by = "BREAKln_ymo")

JH_db_new_ct1 <- left_join(JH_db_new_ct1, JH_db_new_cit_ct1, by = "BREAKln_ymo")

JH_db_homs_and_JH <- JH_db_new_ct1

#this is not data imputation (we do this simply so when we add, this NA works as a 0)
JH_db_homs_and_JH <- JH_db_homs_and_JH %>%
  mutate(cit_additional_victim_count = coalesce(cit_additional_victim_count, 0),
         pol_additional_victim_count = coalesce(pol_additional_victim_count, 0),
         homs_additional_victim_count = coalesce(homs_additional_victim_count, 0)
  )


#===============================================================================
# Here we join the December db of homs and JH with the main_initial_w_ACS data 

#remove repeated vars
main_initial_db_w_ACS <- subset(main_initial_db_w_ACS, select = -c(BREAKln))


#join dbs
JH_db_homs_JH_and_ACS <- left_join(main_initial_db_w_ACS, JH_db_homs_and_JH, by = "BREAKln_ymo")


#removing duplicate rows
JH_db_homs_JH_and_ACS <- JH_db_homs_JH_and_ACS %>% distinct(BREAKln_ymo, .keep_all = TRUE)


#===============================================================================
# Here we join the JH_db_homs_JH_and_ACS with the Arrest data ==================
#===============================================================================
# Load and prepare the data ====================================================
# Loading Arrest db and Personnel db
Arrest_db <- read.csv('C:/Arrest_db.csv')


#extract first two chars into a new var for the month
Arrest_db$month_twochars <- substr(Arrest_db$date, 1, 2)

#extract last four chars into a new var for the year
Arrest_db$year_fourchars <- substring(Arrest_db$date, nchar(Arrest_db$date)-3)


#transforming month names to numbers
#doing this to make it easier to join and clean later, also to define periods
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="01",'january',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="02",'february',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="03",'march',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="04",'april',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="05",'may',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="06",'june',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="07",'july',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="08",'august',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="09",'september',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="10",'october',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="11",'november',month_twochars))
Arrest_db <-Arrest_db %>% mutate(month_twochars=ifelse(month_twochars=="12",'december',month_twochars))


#create BREAKln for join
Arrest_db <- Arrest_db %>% unite('BREAKln_ymo', year_fourchars, month_twochars, ori, remove = FALSE)

#joining arrest db with JH_db_homs_JH_and_ACS
JH_db_homs_JH_and_ACS_arrest <- left_join(JH_db_homs_JH_and_ACS, Arrest_db, by = "BREAKln_ymo")

#save copy
#write.csv(JH_db_homs_JH_and_ACS_arrest, "C:/JH_db_homs_JH_and_ACS_arrest.csv", row.names=FALSE)


#===============================================================================
# Here we join the JH_db_homs_JH_and_ACS_arrest_personnel with the LEOKA data ==
#===============================================================================
# Load and prepare the data ====================================================
# Loading LEOKA and filtering it
leoka_2010_2022 <- read.csv('C:/leoka_2010_2022.csv')
#load this to join with personnel from leoka
JH_db_homs_JH_and_ACS_arrest <- read.csv('C:/JH_db_homs_JH_and_ACS_arrest.csv')


#create BREAKln for join
leoka_2010_2022 <- leoka_2010_2022 %>% unite('BREAKln_ymo', year, month, ori9, remove = FALSE)
leoka_2010_2022 <- leoka_2010_2022 %>% unite('BREAKln', year, month, remove = FALSE)


#keep only needed vars
leoka_2010_2022_personnel <- subset(leoka_2010_2022, select = c(BREAKln_ymo, total_employees_total))


#joining leoka personnel data with JH_db_homs_JH_and_ACS
JH_db_homs_JH_and_ACS_arrest_LEOKApersonnel <- left_join(JH_db_homs_JH_and_ACS_arrest, leoka_2010_2022_personnel, by = "BREAKln_ymo")

#save copy
#write.csv(JH_db_homs_JH_and_ACS_arrest_LEOKApersonnel, "C:/JH_db_homs_JH_and_ACS_arrest_LEOKApersonnel.csv", row.names=FALSE)



#===============================================================================
# Here we filter LEOKA data for officers killed and assaulted ==================
#===============================================================================

#create copy
leoka_2010_2022_cp <- leoka_2010_2022

#officers killed
leoka_2010_2022_killed <- leoka_2010_2022_cp %>%  filter(leoka_2010_2022_cp$officers_killed_total >'0') #filter by occurrence of killed

#create copy
leoka_2010_2022_cp <- leoka_2010_2022

#officers assaulted
leoka_2010_2022_assaulted <- leoka_2010_2022_cp %>%  filter(leoka_2010_2022_cp$total_assaults_total >'0') #filter by occurrence of assaults


#doing the counts of officers killed
leoka_2010_2022_killed_ct <- leoka_2010_2022_killed %>%
  group_by(BREAKln_ymo) %>%
  tally()

#doing the counts of officers assaulted total
leoka_2010_2022_assaulted_ct <- leoka_2010_2022_assaulted %>%
  group_by(BREAKln_ymo) %>%
  tally()

#rename vars
leoka_2010_2022_killed_ct <- leoka_2010_2022_killed_ct %>% rename_with( ~"leoka_killed", "n")
leoka_2010_2022_assaulted_ct <- leoka_2010_2022_assaulted_ct %>% rename_with( ~"leoka_assaulted", "n")


#joining with main
dtp_db_v1 <- left_join(JH_db_homs_JH_and_ACS_arrest_LEOKApersonnel, leoka_2010_2022_killed_ct, by = "BREAKln_ymo")
dtp_db_v1 <- left_join(dtp_db_v1, leoka_2010_2022_assaulted_ct, by = "BREAKln_ymo")

#rename vars
dtp_db_v1 <- dtp_db_v1 %>% rename_with( ~"ori", "ori.x")

#save copy v1 not clean
#write.csv(dtp_db_v1, "C:/dtp_db_v1_not_clean.csv", row.names=FALSE)


#create BREAKln for filtering
dtp_db_v1 <- dtp_db_v1 %>% unite('BREAKln_ym', year, month, remove = FALSE)



#keep only needed vars
dtp_db_v1 <- subset(dtp_db_v1, select = -c(BREAKln_comma, NAME, USPS, ori9,
                                           BREAKln, date, ori.y, month_twochars,
                                           year_fourchars, BREAKln_ori, BREAKln_censusname,
                                           crosswalk_agency_name))

#save copy
#write.csv(dtp_db_v1, "C:/dtp_db_v1.csv", row.names=FALSE)

