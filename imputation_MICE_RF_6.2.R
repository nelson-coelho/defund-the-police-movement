# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code performs the imputation of missing data with MICE RF.
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
# Date: 2025-1-14
#
# File Name: imputation_MICE_RF_6.2.R
#
# Description: This file contains R code to impute missing values
#              with MICE RF.
#
# Modifications:
#
# Changes:
#
# - 2025-1-14: Initial version by Nelson Coelho
#
# ==============================================================================

# Install and load required packages ===========================================
required_packages <- c("dplyr", "plyr", "haven", "R.utils", 
                       "tibble", "tidyr", "xts", "tidyverse", "Hmisc",
                       "readxl", "knitr", "janitor", "jsonlite", "readr",
                       "tidycensus", "missForest", "mice", "skimr",
                       "magrittr", "kableExtra", "data.table",
                       "pastecs", "gsynth", "miceRanger", "kit")

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
library(missForest)  # Used for missing data imputation
library(mice)        # Used for missing data imputation
library(miceRanger)  # Used for missing data imputation
library(skimr)       # Used to provide summary statistics
library(magrittr)    # A Forward-Pipe Operator
library(kableExtra)  # Adds to "magrittr" and "knitr"
library(data.table)  # Converts list into dataframe
library(pastecs)     # Gets descriptive statistics
library(gsynth)      # Performs the Generalized Synthetic Control Tests
library(kit)         # Contains function psum to add vars w NA values


#===============================================================================
# Load and prepare the data ====================================================
# Loading v3.1
dtp_db_v3_1 <- read.csv('C:/dtp_db_v3_1.csv')



#===============================================================================
# First we get the amount of missing data for every variable ===================


#descriptive stats for high school graduates population
Hmisc::describe(dtp_db_v3_1$hs_grad_pop_interpolation)
stat.desc(dtp_db_v3_1$hs_grad_pop_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$hs_grad_pop_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for hs_grad_pop_interpolation


#descriptive stats for population
Hmisc::describe(dtp_db_v3_1$population_interpolation)
stat.desc(dtp_db_v3_1$population_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$population_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for population_interpolation


#descriptive stats for black population
Hmisc::describe(dtp_db_v3_1$black_pop_interpolation)
stat.desc(dtp_db_v3_1$black_pop_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$black_pop_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for black_pop_interpolation


#descriptive stats for poverty rate
Hmisc::describe(dtp_db_v3_1$poverty_rate_interpolation)
stat.desc(dtp_db_v3_1$poverty_rate_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$poverty_rate_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#72 missing for poverty_rate_interpolation


#descriptive stats for unemployment rate
Hmisc::describe(dtp_db_v3_1$unemp_rate_interpolation)
stat.desc(dtp_db_v3_1$unemp_rate_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$unemp_rate_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#72 missing for unemp_rate


#descriptive stats for housing occupancy rate
Hmisc::describe(dtp_db_v3_1$housing_occupancy_interpolation)
stat.desc(dtp_db_v3_1$housing_occupancy_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$housing_occupancy_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for housing_occupancy_interpolation


#descriptive stats for land area
Hmisc::describe(dtp_db_v3_1$land_area_interpolation)
stat.desc(dtp_db_v3_1$land_area_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$land_area_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for land_area_interpolation


#descriptive stats for water area
Hmisc::describe(dtp_db_v3_1$water_area_interpolation)
stat.desc(dtp_db_v3_1$water_area_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$water_area_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for water_area_interpolation


#descriptive stats for land area sq mile
Hmisc::describe(dtp_db_v3_1$land_area_sq_mile_interpolation)
stat.desc(dtp_db_v3_1$land_area_sq_mile_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$land_area_sq_mile_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#48 missing for land_area_sq_mile_interpolation


#descriptive stats for homicide occurrence
Hmisc::describe(dtp_db_v3_1$homs_occurrence)
stat.desc(dtp_db_v3_1$homs_occurrence)
#percentage of zeroes
sumCol <- dtp_db_v3_1$homs_occurrence
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#413427 missing for homs_occurrence


#descriptive stats for justified homicides police
Hmisc::describe(dtp_db_v3_1$pol_JHs_occurrence)
stat.desc(dtp_db_v3_1$pol_JHs_occurrence)
#percentage of zeroes
sumCol <- dtp_db_v3_1$pol_JHs_occurrence
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#452079 missing for pol_JHs_occurrence


#descriptive stats for justified homicides citizen
Hmisc::describe(dtp_db_v3_1$cit_JHs_occurrence)
stat.desc(dtp_db_v3_1$cit_JHs_occurrence)
#percentage of zeroes
sumCol <- dtp_db_v3_1$cit_JHs_occurrence
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#452064 missing for cit_JHs_occurrence


#descriptive stats for arrests
Hmisc::describe(dtp_db_v3_1$arrests)
stat.desc(dtp_db_v3_1$arrests)
#percentage of zeroes
sumCol <- dtp_db_v3_1$arrests
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#30735 missing for arrests


#descriptive stats for total police employees
Hmisc::describe(dtp_db_v3_1$total_police_employees_interpolation)
stat.desc(dtp_db_v3_1$total_police_employees_interpolation)
#percentage of zeroes
sumCol <- dtp_db_v3_1$total_police_employees_interpolation
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#492 missing for total_police_employees_interpolation


#descriptive stats for killed officers (LEOKA)
Hmisc::describe(dtp_db_v3_1$leoka_killed)
stat.desc(dtp_db_v3_1$leoka_killed)
#percentage of zeroes
sumCol <- dtp_db_v3_1$leoka_killed
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#451956 missing for leoka_killed


#descriptive stats for assaulted officers (LEOKA)
Hmisc::describe(dtp_db_v3_1$leoka_assaulted)
stat.desc(dtp_db_v3_1$leoka_assaulted)
#percentage of zeroes
sumCol <- dtp_db_v3_1$leoka_assaulted
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#361677 missing for leoka_assaulted


#===============================================================================
# Below we perform the MICE RF imputation for the following variables:
#
# high school graduates population, population, black population
# poverty rate, unemployment rate, housing occupancy rate, land area,
# water area, land area sq mile, homicide occurrence, justified homicides police,
# justified homicides citizen, arrests, total police employees, leoka killed and assaulted

#we do below to add vars of killed and assaulted officers
#if vars are NA + 1 or 1 + NA they become 1 and it is assumed the NA was a zero
#if vars were NA NA they become zero, but we turn them into NA once again
#because we assume they are true NAs and impute that data with MICE RF
dtp_db_v3_1 <- transform(dtp_db_v3_1, k_and_a_leoka=psum(leoka_killed, leoka_assaulted, na.rm=TRUE)) #sum vars
dtp_db_v3_1$k_and_a_leoka[dtp_db_v3_1$k_and_a_leoka == 0] <- NA #transform 0s into NA

# First we separate vars w missing data, do mice, then bring it back
forest_db <- subset(dtp_db_v3_1, select = c(BREAKln_ymo2, hs_grad_pop_interpolation, population_interpolation,
                                            black_pop_interpolation, poverty_rate_interpolation, unemp_rate_interpolation,
                                            housing_occupancy_interpolation, land_area_interpolation, water_area_interpolation,
                                            land_area_sq_mile_interpolation, total_police_employees_interpolation, 
                                            homs_occurrence, pol_JHs_occurrence, cit_JHs_occurrence, arrests,
                                            k_and_a_leoka))


#===============================================================================
# running mice
mice_imp <- complete(mice(forest_db,       # Predictive mean matching imputation
                          m = 25, maxit = 50,
                          method = "rf", num.threads = 6))
head(mice_imp)



#renaming vars
mice_imp <- mice_imp %>% rename_with( ~"arrests_imp", "arrests")
mice_imp <- mice_imp %>% rename_with( ~"hs_grad_pop_interpolation_imp", "hs_grad_pop_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"population_interpolation_imp", "population_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"black_pop_interpolation_imp", "black_pop_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"poverty_rate_interpolation_imp", "poverty_rate_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"unemp_rate_interpolation_imp", "unemp_rate_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"housing_occupancy_interpolation_imp", "housing_occupancy_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"land_area_interpolation_imp", "land_area_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"water_area_interpolation_imp", "water_area_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"land_area_sq_mile_interpolation_imp", "land_area_sq_mile_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"total_police_employees_interpolation_imp", "total_police_employees_interpolation")
mice_imp <- mice_imp %>% rename_with( ~"homs_occurrence_imp", "homs_occurrence")
mice_imp <- mice_imp %>% rename_with( ~"pol_JHs_occurrence_imp", "pol_JHs_occurrence")
mice_imp <- mice_imp %>% rename_with( ~"cit_JHs_occurrence_imp", "cit_JHs_occurrence")
mice_imp <- mice_imp %>% rename_with( ~"k_and_a_leoka_imp", "k_and_a_leoka")


#join back again
dtp_db_v5_1 <- left_join(dtp_db_v3_1, mice_imp, by = "BREAKln_ymo2")


#save copy
write.csv(dtp_db_v5_1, "C:/dtp_db_v5_1.csv", row.names=FALSE)

#===============================================================================
# Once again we get the amount of missing data for variables ===================

#descriptive stats for high school graduates population
#Hmisc::describe(dtp_db_v5_1$hs_grad_pop_interpolation_imp)
stat.desc(dtp_db_v5_1$hs_grad_pop_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$hs_grad_pop_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#48 missing for hs_grad_pop_interpolation_imp
#to
#0

#descriptive stats for population
Hmisc::describe(dtp_db_v5_1$population_interpolation_imp)
stat.desc(dtp_db_v5_1$population_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$population_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#48 missing for population_interpolation_imp
#to
#0

#descriptive stats for black population
Hmisc::describe(dtp_db_v5_1$black_pop_interpolation_imp)
stat.desc(dtp_db_v5_1$black_pop_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$black_pop_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#48 missing for black_pop_interpolation_imp
#to
#0

#descriptive stats for poverty rate
Hmisc::describe(dtp_db_v5_1$poverty_rate_interpolation_imp)
stat.desc(dtp_db_v5_1$poverty_rate_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$poverty_rate_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#72 missing for poverty_rate_interpolation_imp
#to
#0

#descriptive stats for unemployment rate
Hmisc::describe(dtp_db_v5_1$unemp_rate_interpolation_imp)
stat.desc(dtp_db_v5_1$unemp_rate_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$unemp_rate_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#72 missing for unemp_rate_interpolation_imp
#to
#0

#descriptive stats for housing occupancy rate
#Hmisc::describe(dtp_db_v5_1$housing_occupancy_interpolation_imp)
stat.desc(dtp_db_v5_1$housing_occupancy_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$housing_occupancy_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#48 missing for housing_occupancy_interpolation_imp
#to
#0

#descriptive stats for land area
Hmisc::describe(dtp_db_v5_1$land_area_interpolation_imp)
stat.desc(dtp_db_v5_1$land_area_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$land_area_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#48 missing for land_area_interpolation_imp
#to
#0

#descriptive stats for water area
Hmisc::describe(dtp_db_v5_1$water_area_interpolation_imp)
stat.desc(dtp_db_v5_1$water_area_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$water_area_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#48 missing for water_area_interpolation_imp
#to
#0

#descriptive stats for homicide occurrence
Hmisc::describe(dtp_db_v5_1$homs_occurrence_imp)
stat.desc(dtp_db_v5_1$homs_occurrence_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$homs_occurrence_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#413427 missing for homs_occurrence_imp
#to
#30485
#to
#0

#descriptive stats for justified homicides police
Hmisc::describe(dtp_db_v5_1$pol_JHs_occurrence_imp)
stat.desc(dtp_db_v5_1$pol_JHs_occurrence_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$pol_JHs_occurrence_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#452079 missing for pol_JHs_occurrence_imp
#to
#30735
#to
#0

#descriptive stats for justified homicides citizen
Hmisc::describe(dtp_db_v5_1$cit_JHs_occurrence_imp)
stat.desc(dtp_db_v5_1$cit_JHs_occurrence_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$cit_JHs_occurrence_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#452064 missing for cit_JHs_occurrence_imp
#to
#30735
#to
#0

#descriptive stats for arrests
Hmisc::describe(dtp_db_v5_1$arrests_imp)
stat.desc(dtp_db_v5_1$arrests_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$arrests_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#30735 missing for arrests_imp
#to
#0

#descriptive stats for total police employees
Hmisc::describe(dtp_db_v5_1$total_police_employees_interpolation_imp)
stat.desc(dtp_db_v5_1$total_police_employees_interpolation_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$total_police_employees_interpolation_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#492 missing for total_police_employees_interpolation_imp
#to
#0

#descriptive stats for killed officers (LEOKA)
Hmisc::describe(dtp_db_v5_1$leoka_killed_imp)
stat.desc(dtp_db_v5_1$leoka_killed_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$leoka_killed_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#451956 missing for leoka_killed_imp
#to
#30734
#to
#0

#descriptive stats for assaulted officers (LEOKA)
Hmisc::describe(dtp_db_v5_1$leoka_assaulted_imp)
stat.desc(dtp_db_v5_1$leoka_assaulted_imp)
#percentage of zeroes
sumCol <- dtp_db_v5_1$leoka_assaulted_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#from
#451956 missing for leoka_assaulted_imp
#to
#29666
#to
#0


#===============================================================================
#read last save
dtp_db_v5_1 <- read.csv('C:/dtp_db_v5_1.csv')


#this is not data imputation (we do this simply so when we sum, this NA works as a 0)
dtp_db_v5_1 <- dtp_db_v5_1 %>%
  mutate(cit_additional_victim_count = coalesce(cit_additional_victim_count, 0),
         pol_additional_victim_count = coalesce(pol_additional_victim_count, 0),
         homs_additional_victim_count = coalesce(homs_additional_victim_count, 0)
  )

#===============================================================================
#below we add additional victims with the occurrences (this only works if you do above first or MICE RF imputation first)
dtp_db_v5_1$pol_total_victims <- dtp_db_v5_1$pol_JHs_occurrence_imp + dtp_db_v5_1$pol_additional_victim_count
dtp_db_v5_1$cit_total_victims <- dtp_db_v5_1$cit_JHs_occurrence_imp + dtp_db_v5_1$cit_additional_victim_count
dtp_db_v5_1$homs_total_victims <- dtp_db_v5_1$homs_occurrence_imp + dtp_db_v5_1$homs_additional_victim_count
#===============================================================================

#===============================================================================
#We ready the database for testing
#Perform certain calculations and clean the table


#redoing some formulas after interpolation
dtp_db_v5_1$hs_grad_rate <- ((dtp_db_v5_1$hs_grad_pop_interpolation_imp / dtp_db_v5_1$population_interpolation_imp) * 100)
dtp_db_v5_1$pop_density <- ((dtp_db_v5_1$population_interpolation_imp / dtp_db_v5_1$land_area_interpolation_imp) * 100000)


#creating rates per 100,000 population
dtp_db_v5_1$murder_rate <- ifelse (is.na((dtp_db_v5_1$homs_total_victims / dtp_db_v5_1$population_interpolation_imp) * 100000), NA, (dtp_db_v5_1$homs_total_victims / dtp_db_v5_1$population_interpolation_imp) * 100000)
dtp_db_v5_1$arrest_rate <- ifelse (is.na((dtp_db_v5_1$arrests_imp / dtp_db_v5_1$population_interpolation_imp) * 100000), NA, (dtp_db_v5_1$arrests_imp / dtp_db_v5_1$population_interpolation_imp) * 100000)
dtp_db_v5_1$police_rate <- ifelse (is.na((dtp_db_v5_1$total_police_employees_interpolation_imp / dtp_db_v5_1$population_interpolation_imp) * 100000), NA, (dtp_db_v5_1$total_police_employees_interpolation_imp / dtp_db_v5_1$population_interpolation_imp) * 100000)

#do logs
dtp_db_v5_1$logarrests <- log(dtp_db_v5_1$arrests_imp + 1)
dtp_db_v5_1$logmurder_rate <- log(dtp_db_v5_1$murder_rate + 1)
dtp_db_v5_1$logarrest_rate <- log(dtp_db_v5_1$arrest_rate + 1)
dtp_db_v5_1$logpolice_rate <- log(dtp_db_v5_1$police_rate + 1)
dtp_db_v5_1$log_population <- log(dtp_db_v5_1$population_interpolation_imp)
dtp_db_v5_1$logpop_density <- log(dtp_db_v5_1$pop_density + 1)

#remove dups
dtp_db_v5_1 <- dtp_db_v5_1 %>% distinct(BREAKln_ymo2, .keep_all = TRUE)



#===============================================================================
#below is performed to create a fiscal year variable
#fiscal year begins in July and ends on June of the following year

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
FY_instance <- data.frame() 
FY_db <- data.frame()

#make copy
FY_counter <- dtp_db_v5_1
ori_db <- FY_counter
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
k = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- FY_counter %>%  filter(FY_counter$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i) #filter for current year
    #here we iterate through the months
    
    if (i == 2012) { #if this is 2012, it will be FY2013 (only July, Aug, Sep, Oct, Nov, Dec)
      FY_instance <- subset(year_current, select = c(month, year, ori, BREAKln_ymo2)) #begin database
      
      for (m in 1:6) {
        FY_instance$FY[m] <- "FY2013" #assign 
        count = count + 1
      }
    }
    else if (i == 2022) { #if it is the last year, it will be FY2022 (only Jan, Feb, Mar, Apr, May, Jun)
      FY_instance <- subset(year_current, select = c(month, year, ori, BREAKln_ymo2)) #begin database
      
      for (n in 1:6) {
        FY_instance$FY[n] <- "FY2022" #assign
      }
    } 
    else { #if not the first nor last year
      FY_instance <- subset(year_current, select = c(month, year, ori, BREAKln_ymo2)) #begin database
      
      for (o in 1:12) { #count the months
        
        if (o < 7) { #if month is smaller than July we assign the same year
          FY_instance$FY[o] <- paste("FY", i, sep = "", collapse=NULL) #assign
        }
        else { #if month is equal or larger than July we assign the following year
          k = i + 1
          FY_instance$FY[o] <- paste("FY", k, sep = "", collapse=NULL) #assign
        }
        count = count + 1
      }
    }
    print(count) # validation check
    FY_db <- rbind(FY_db, FY_instance) #bind to database
  }
  count = 0
  print(ori_current$ori) # validation check
}


#remove dups
FY_db <- FY_db %>% distinct(BREAKln_ymo2, .keep_all = TRUE)

#keep only needed vars before joining
FY_db <- subset(FY_db, select = c(BREAKln_ymo2, FY))

#joining with main
dtp_db_v5_2 <- left_join(dtp_db_v5_1, FY_db, by = "BREAKln_ymo2")

#move column forward
dtp_db_v5_2 <- dtp_db_v5_2 %>% relocate(FY, .before = year)

#save copy
write.csv(dtp_db_v5_2, "C:/dtp_db_v5_2.csv", row.names=FALSE)

