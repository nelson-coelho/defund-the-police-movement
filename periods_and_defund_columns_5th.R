# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code performs the creation of two columns.
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
# Date: 2024-11-27
#
# File Name: periods_and_defund_columns.R
#
# Description: This file contains R code to create the periods and defund
#              columns.
#
# Modifications:
#
# Changes:
#
# - 2024-11-27: Initial version by Nelson Coelho
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
# Loading v2
dtp_db_v2 <- read.csv('C:/dbs/dtp_db_v2.csv')


#remove dups
dtp_db_v2 <- dtp_db_v2 %>% distinct(BREAKln_ymo2, .keep_all = TRUE)

#make copy
periods_counter <- dtp_db_v2


#===============================================================================
# below we create the periods column ===========================================
#===============================================================================
#===============================================================================

# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
year_current <- data.frame()
periods_instance <- data.frame() 
periods_db <- data.frame()
ori_db <- periods_counter
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
count = 0
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  ori_current <- periods_counter %>%  filter(periods_counter$ori == as.character(ori_db[j])) #filter for the current ori
  
  #here we iterate through the years
  for (i in 2012:2022) {
    year_current <- ori_current %>%  filter(ori_current$year == i) #filter for current year
    #here we iterate through the months

      if (i == 2012) { #if this is the first month it remains the same
        periods_instance <- subset(year_current, select = c(month, year, ori, BREAKln_ymo2)) #begin database
        
        for (m in 1:6) {
          periods_instance$periods[m] <- m #assign 
          count = count + 1
        }
      }
      else if (i == 2022) {
        periods_instance <- subset(year_current, select = c(month, year, ori, BREAKln_ymo2)) #begin database
        
        for (n in 1:6) {
          periods_instance$periods[n] <- count + n #assign 
        }
      } 
      else { #if not the first nor last year
        periods_instance <- subset(year_current, select = c(month, year, ori, BREAKln_ymo2)) #begin database
        
        for (o in 1:12) {
          count = count + 1
          periods_instance$periods[o] <- count #assign
        }
      }
      print(count) # validation check
      periods_db <- rbind(periods_db, periods_instance) #bind to database
    }
  count = 0
  print(ori_current$ori) # validation check
}


#remove dups
periods_db <- periods_db %>% distinct(BREAKln_ymo2, .keep_all = TRUE)


#keep only needed vars before joining
periods_db <- subset(periods_db, select = c(BREAKln_ymo2, periods))


#joining with main
dtp_db_v3 <- left_join(dtp_db_v2, periods_db, by = "BREAKln_ymo2")


#save copy
#write.csv(dtp_db_v3, "C:/dtp_db_v3.csv", row.names=FALSE)




#===============================================================================
# below is a list fo all cities that defunded pd and the years they defunded ===
#===============================================================================
#===============================================================================


# Albuquerque, NM = ori NM0010100 (2021)
# Asheville, NC = ori NC0110100 (2021)
# Austin, TX = ori TX2270100 (2021)
    # Boston, MA = ori MA0130100 (2021 and 2022)
    # Burlington, VT = ori VT0040100 (2021 and 2022)
  # Columbus, OH ori = OHCOP0000 (2022)
# Denver, CO ori = CODPD0000 (2021)
# Eureka, CA ori = CA0120300 (2021)
  # Los Angeles, CA ori = CA0194200 (2022)
# Madison, WI ori = WI0130100 (2021)
    # Milwaukee, WI ori = WIMPD0000 (2021 and 2022)
# Minneapolis, MN ori = MN0271100 (2021)
# New York, NY ori = NY0303000 (2021)
# Norman, OK ori = OK0140200 (2021)
    # Oakland, CA ori = CA0010900 (2021 and 2022)
# Oklahoma City, OK ori = OK0550600 (2021)
# Philadelphia, PA ori = PAPEP0000 (2021)
# Portland, OR ori = OR0260200 (2021)
# Salt Lake City, UT ori = UT0180300 (2021)
    # San Francisco, CA ori = CA0380100 (2021 and 2022)
# Seattle, WA ori = WASPD0000 (2021)
# Steamboat Springs, CO ori = CO0540100 (2021) **** not on SHR db
  # Washington, DC ori = DCMPD0000 (2022)


#load db
dtp_db_v3 <- read.csv('C:/dtp_db_v3.csv')

#make copy
defund_counter <- dtp_db_v3


#===============================================================================
# below we create the defund column ============================================
#===============================================================================
#===============================================================================


# Getting all oris and creating dbs
ori_db <- data.frame()
ori_current <- data.frame()
defund_instance <- data.frame() 
defund_db <- data.frame()
ori_db <- defund_counter
ori_db <- subset(ori_db, select = c(ori))
ori_db <- as.vector(unique(ori_db$ori)) #only works as vector

#Loop 
#first we iterate through the oris
for (j in 1:length(ori_db)) {
  
  ori_current <- defund_counter %>%  filter(defund_counter$ori == as.character(ori_db[j])) #filter for the current ori
  
  defund_instance <- subset(ori_current, select = c(month, year, ori, BREAKln_ymo2, BREAKln_ym, periods)) #begin database
  
  defund_instance <- defund_instance %>% add_column(defund = 0) #add defund column with all zeroes
  
  i = 0 #reset i
  
  #here we iterate through the years
  for (i in 1:120) {

    # here we insert one for every period of FY2022 for cities that defunded in the second year
    if (defund_instance$periods[i] > 108) {
      
      if (ori_current$ori[i] == "MA0130100") {
        defund_instance$defund[i] <- 1
      }
      else if (ori_current$ori[i] == "VT0040100") {
        defund_instance$defund[i] <- 1
      }
      else if (ori_current$ori[i] == "OHCOP0000") {
        defund_instance$defund[i] <- 1
      }
      else if (ori_current$ori[i] == "CA0194200") {
        defund_instance$defund[i] <- 1
        
      }
      else if (ori_current$ori[i] == "WIMPD0000") {
        defund_instance$defund[i] <- 1
      }
      else if (ori_current$ori[i] == "CA0010900") {
        defund_instance$defund[i] <- 1
      }
      else if (ori_current$ori[i] == "CA0380100") {
        defund_instance$defund[i] <- 1
      }
      else if (ori_current$ori[i] == "DCMPD0000") {
        defund_instance$defund[i] <- 1
      }
      
    # here we insert one for every period of FY2021 for cities that defunded in the first year
    } else if (defund_instance$periods[i] > 96) {

        if (ori_current$ori[i] == "NM0010100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "NC0110100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "TX2270100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "MA0130100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "VT0040100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "CODPD0000") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "CA0120300") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "WI0130100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "WIMPD0000") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "MN0271100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "NY0303000") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "OK0140200") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "CA0010900") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "OK0550600") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "PAPEP0000") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "OR0260200") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "UT0180300") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "CA0380100") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "WASPD0000") {
          defund_instance$defund[i] <- 1
        }
        else if (ori_current$ori[i] == "CO0540100") {
          defund_instance$defund[i] <- 1
        }
        
      } else { #all other cities and periods
      
          print("howdy")
      }
  }
  defund_db <- rbind(defund_db, defund_instance) #bind to database
}


#remove dups
defund_db <- defund_db %>% distinct(BREAKln_ymo2, .keep_all = TRUE)


#keep only needed vars before joining
defund_db <- subset(defund_db, select = c(BREAKln_ymo2, defund))


#joining with main
dtp_db_v3_1 <- left_join(dtp_db_v3, defund_db, by = "BREAKln_ymo2")


#save copy
#write.csv(dtp_db_v3_1, "C:/dtp_db_v3_1.csv", row.names=FALSE)

