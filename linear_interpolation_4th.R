# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code performs the interpolation.
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
# Date: 2024-11-14
#
# File Name: linear_interpolation_4th.R
#
# Description: This file contains R code to filter the databases
# used in the article.
#
# Modifications:
#
# Changes:
#
# - 2024-11-21: Initial version by Nelson Coelho
#
# ==============================================================================

# Install and load required packages ===========================================
required_packages <- c("ggplot2","dplyr", "plyr", "haven", "R.utils", 
                       "tibble", "tidyr", "xts", "tidyverse", "Hmisc",
                       "readxl", "knitr", "janitor", "jsonlite", "readr",
                       "tidycensus", "psych", "missForest", "mice", "skimr",
                       "devtools", "magrittr", "kableExtra", "data.table")

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load libraries ===============================================================
library(ggplot2)     # Used for creating graphics
library(dplyr)       # Used for data manipulation
library(plyr)        # Used for data manipulation
library(haven)       # Import and Export 'SPSS', 'Stata' and 'SAS' Files
library(R.utils)     # Various Programming Utilities
library(tibble)      # Provides a 'tbl_df' class (the 'tibble') with stricter checking and better formatting than the traditional data frame.
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
library(psych)       # Used for correlation test
library(missForest)  # Used for missing data imputation
library(mice)        # Used for missing data imputation
library(skimr)       # Used to provide summary statistics
library(devtools)    # Used to install packages
library(magrittr)    # A Forward-Pipe Operator
library(kableExtra)  # Adds to "magrittr" and "knitr"
library(data.table)  # Converts list into dataframe


#===============================================================================
# Load and prepare the data ====================================================
# Loading v1
dtp_db_v1 <- read.csv('C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/dtp_db_v1.csv')

#remove dups
dtp_db_v1 <- dtp_db_v1 %>% distinct(BREAKln_ymo, .keep_all = TRUE)


#filter only necessary periods
dtp_db_interpolation <- dtp_db_v1

#here we are keeping last period of 2011
dtp_db_interpolation <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$year > 2011
                                       | dtp_db_interpolation$BREAKln_ym == "2011_12" & !dtp_db_interpolation$BREAKln_ym == "2011_1" 
                                       & !dtp_db_interpolation$BREAKln_ym == "2011_2" & !dtp_db_interpolation$BREAKln_ym == "2011_3"
                                       & !dtp_db_interpolation$BREAKln_ym == "2011_4" & !dtp_db_interpolation$BREAKln_ym == "2011_5"
                                       & !dtp_db_interpolation$BREAKln_ym == "2011_6" & !dtp_db_interpolation$BREAKln_ym == "2011_7"
                                       & !dtp_db_interpolation$BREAKln_ym == "2011_8" & !dtp_db_interpolation$BREAKln_ym == "2011_9"
                                       & !dtp_db_interpolation$BREAKln_ym == "2011_10" & !dtp_db_interpolation$BREAKln_ym == "2011_11")


#remove dups
dtp_db_interpolation <- dtp_db_interpolation %>% distinct(BREAKln_ymo, .keep_all = TRUE)


#subset test db even further and keep only the vars needed for interpolation w the breaklns and ori
dtp_db_interpolation <- subset(dtp_db_interpolation, select = c(BREAKln_ym, year, hs_grad_pop, population, month,
                                              black_pop, poverty_rate, unemp_rate, housing_occupancy,
                                              ALAND, AWATER, ALAND_SQMI,
                                              total_employees_total, ori))

#move column
dtp_db_interpolation <- dtp_db_interpolation %>%
  select(month, everything())




#===============================================================================
# high school graduation population ============================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_hs_grad_pop = first_month$hs_grad_pop[1] #assign value of first month
      right_side = ((last_month$hs_grad_pop - first_month$hs_grad_pop) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, hs_grad_pop)) #begin database
        interpolation_instance$hs_grad_pop_interpolation[k] <- fm_hs_grad_pop #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        hs_grad_pop_interpolation <- fm_hs_grad_pop + (right_side * (k-1)) #interpolation formula
        interpolation_instance$hs_grad_pop_interpolation[k] <- hs_grad_pop_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

hs_grad_pop_interp_db <- interpolation_db


#===============================================================================
# population ===================================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_population = first_month$population[1] #assign value of first month
      right_side = ((last_month$population - first_month$population) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, population)) #begin database
        interpolation_instance$population_interpolation[k] <- fm_population #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        population_interpolation <- fm_population + (right_side * (k-1)) #interpolation formula
        interpolation_instance$population_interpolation[k] <- population_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

population_interp_db <- interpolation_db


#===============================================================================
# black population =============================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_black_pop = first_month$black_pop[1] #assign value of first month
      right_side = ((last_month$black_pop - first_month$black_pop) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, black_pop)) #begin database
        interpolation_instance$black_pop_interpolation[k] <- fm_black_pop #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        black_pop_interpolation <- fm_black_pop + (right_side * (k-1)) #interpolation formula
        interpolation_instance$black_pop_interpolation[k] <- black_pop_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

black_pop_interp_db <- interpolation_db


#===============================================================================
# poverty rate =================================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_poverty_rate = first_month$poverty_rate[1] #assign value of first month
      right_side = ((last_month$poverty_rate - first_month$poverty_rate) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, poverty_rate)) #begin database
        interpolation_instance$poverty_rate_interpolation[k] <- fm_poverty_rate #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        poverty_rate_interpolation <- fm_poverty_rate + (right_side * (k-1)) #interpolation formula
        interpolation_instance$poverty_rate_interpolation[k] <- poverty_rate_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

poverty_rate_interp_db <- interpolation_db


#===============================================================================
# unemployment rate ============================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_unemp_rate = first_month$unemp_rate[1] #assign value of first month
      right_side = ((last_month$unemp_rate - first_month$unemp_rate) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, unemp_rate)) #begin database
        interpolation_instance$unemp_rate_interpolation[k] <- fm_unemp_rate #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        unemp_rate_interpolation <- fm_unemp_rate + (right_side * (k-1)) #interpolation formula
        interpolation_instance$unemp_rate_interpolation[k] <- unemp_rate_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

unemp_rate_interp_db <- interpolation_db


#===============================================================================
# housing occupancy rate =======================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_housing_occupancy = first_month$housing_occupancy[1] #assign value of first month
      right_side = ((last_month$housing_occupancy - first_month$housing_occupancy) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, housing_occupancy)) #begin database
        interpolation_instance$housing_occupancy_interpolation[k] <- fm_housing_occupancy #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        housing_occupancy_interpolation <- fm_housing_occupancy + (right_side * (k-1)) #interpolation formula
        interpolation_instance$housing_occupancy_interpolation[k] <- housing_occupancy_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

housing_occupancy_interp_db <- interpolation_db


#===============================================================================
# land area ====================================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_land_area = first_month$ALAND[1] #assign value of first month
      right_side = ((last_month$ALAND - first_month$ALAND) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, ALAND)) #begin database
        interpolation_instance$land_area_interpolation[k] <- fm_land_area #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        land_area_interpolation <- fm_land_area + (right_side * (k-1)) #interpolation formula
        interpolation_instance$land_area_interpolation[k] <- land_area_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

land_area_interp_db <- interpolation_db


#===============================================================================
# water area ===================================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_water_area = first_month$AWATER[1] #assign value of first month
      right_side = ((last_month$AWATER - first_month$AWATER) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, AWATER)) #begin database
        interpolation_instance$water_area_interpolation[k] <- fm_water_area #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        water_area_interpolation <- fm_water_area + (right_side * (k-1)) #interpolation formula
        interpolation_instance$water_area_interpolation[k] <- water_area_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

water_area_interp_db <- interpolation_db


#===============================================================================
# land area sq mile ============================================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_land_area_sq_mile = first_month$ALAND_SQMI[1] #assign value of first month
      right_side = ((last_month$ALAND_SQMI - first_month$ALAND_SQMI) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, ALAND_SQMI)) #begin database
        interpolation_instance$land_area_sq_mile_interpolation[k] <- fm_land_area_sq_mile #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        land_area_sq_mile_interpolation <- fm_land_area_sq_mile + (right_side * (k-1)) #interpolation formula
        interpolation_instance$land_area_sq_mile_interpolation[k] <- land_area_sq_mile_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

land_area_sq_mile_interp_db <- interpolation_db


#===============================================================================
# total police employees (civilian and officers) ===============================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
interpolation_db <- data.frame()
interpolation_instance <- data.frame() 
interpolation_year <- data.frame()
ori_db <- dtp_db_interpolation
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- dtp_db_interpolation %>%  filter(dtp_db_interpolation$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i | (ori_current$year == i-1 & ori_current$month == 12)) #filter for current year
    
    #here we iterate through the months
    for (k in 1:13) { 
      last_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i)    # filter the last month of current year
      first_month <- year_current %>%  filter(year_current$month == 12 & year_current$year == i-1) # filter last month of previous year
      
      fm_total_police_employees = first_month$total_employees_total[1] #assign value of first month
      right_side = ((last_month$total_employees_total - first_month$total_employees_total) / 12) #right side of formula
      
      if (k == 1) { #if this is the first month it remains the same
        interpolation_instance <- subset(year_current, select = c(month, year, ori, total_employees_total)) #begin database
        interpolation_instance$total_police_employees_interpolation[k] <- fm_total_police_employees #assign 
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      else { #if not the first month, we perform the formula
        total_police_employees_interpolation <- fm_total_police_employees + (right_side * (k-1)) #interpolation formula
        interpolation_instance$total_police_employees_interpolation[k] <- total_police_employees_interpolation #assign
        
        interpolation_instance$right_side[k] <- right_side #assign right side of formula just to double check
      }
      print(k) # validation check
    }
    #following ifelse is done to remove repeated last month rows for every year other than the first
    if (i == 2012) {
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
      
    } else {
      interpolation_instance = interpolation_instance[-1,] #removing first row
      interpolation_db <- rbind(interpolation_db, interpolation_instance) #bind to database
    }
  }
  print(ori_current$ori) # validation check
}

total_police_employees_interp_db <- interpolation_db










#create BREAKln for joins
dtp_db_v1 <- dtp_db_v1 %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
hs_grad_pop_interp_db <- hs_grad_pop_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
population_interp_db <- population_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
black_pop_interp_db <- black_pop_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
poverty_rate_interp_db <- poverty_rate_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
unemp_rate_interp_db <- unemp_rate_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
housing_occupancy_interp_db <- housing_occupancy_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
land_area_interp_db <- land_area_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
water_area_interp_db <- water_area_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
land_area_sq_mile_interp_db <- land_area_sq_mile_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)
total_police_employees_interp_db <- total_police_employees_interp_db %>% unite('BREAKln_ymo2', year, month, ori, remove = FALSE)


#keep only needed vars before joining
hs_grad_pop_interp_db <- subset(hs_grad_pop_interp_db, select = c(BREAKln_ymo2, hs_grad_pop_interpolation))
population_interp_db <- subset(population_interp_db, select = c(BREAKln_ymo2, population_interpolation))
black_pop_interp_db <- subset(black_pop_interp_db, select = c(BREAKln_ymo2, black_pop_interpolation))
poverty_rate_interp_db <- subset(poverty_rate_interp_db, select = c(BREAKln_ymo2, poverty_rate_interpolation))
unemp_rate_interp_db <- subset(unemp_rate_interp_db, select = c(BREAKln_ymo2, unemp_rate_interpolation))
housing_occupancy_interp_db <- subset(housing_occupancy_interp_db, select = c(BREAKln_ymo2, housing_occupancy_interpolation))
land_area_interp_db <- subset(land_area_interp_db, select = c(BREAKln_ymo2, land_area_interpolation))
water_area_interp_db <- subset(water_area_interp_db, select = c(BREAKln_ymo2, water_area_interpolation))
land_area_sq_mile_interp_db <- subset(land_area_sq_mile_interp_db, select = c(BREAKln_ymo2, land_area_sq_mile_interpolation))
total_police_employees_interp_db <- subset(total_police_employees_interp_db, select = c(BREAKln_ymo2, total_police_employees_interpolation))


#remove 2010 and 2011 from main db
#filter only necessary periods
dtp_db_v1 <- dtp_db_v1 %>%  filter(dtp_db_v1$year >= 2012 & !dtp_db_v1$BREAKln_ym == "2012_1" 
                                   & !dtp_db_v1$BREAKln_ym == "2012_2" & !dtp_db_v1$BREAKln_ym == "2012_3"
                                   & !dtp_db_v1$BREAKln_ym == "2012_4" & !dtp_db_v1$BREAKln_ym == "2012_5"
                                   & !dtp_db_v1$BREAKln_ym == "2012_6" & !dtp_db_v1$BREAKln_ym == "2022_7"
                                   & !dtp_db_v1$BREAKln_ym == "2022_8" & !dtp_db_v1$BREAKln_ym == "2022_9"
                                   & !dtp_db_v1$BREAKln_ym == "2022_10" & !dtp_db_v1$BREAKln_ym == "2022_11"
                                   & !dtp_db_v1$BREAKln_ym == "2022_12")

#joining with main
dtp_db_v2 <- left_join(dtp_db_v1, hs_grad_pop_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, population_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, black_pop_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, poverty_rate_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, unemp_rate_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, housing_occupancy_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, land_area_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, water_area_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, land_area_sq_mile_interp_db, by = "BREAKln_ymo2")
dtp_db_v2 <- left_join(dtp_db_v2, total_police_employees_interp_db, by = "BREAKln_ymo2")


#save copy
#write.csv(dtp_db_v2, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/dtp_db_v2.csv", row.names=FALSE)

