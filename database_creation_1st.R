# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code prepares the database for the article.
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
# Date: 2024-11-02
#
# File Name: database_creation_1st.R
#
# Description: This file contains R code to create the database 
# used in the article. We begin by pulling loading files for the land area of cities,
# the CENSUS ACS, followed by retrieving data for arrests
# and personnel from the FBI API Crime Explorer and LEOKA data.
#
# Modifications:
#
# Changes:
#
# - 2024-10-24: Initial version by Nelson Coelho
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

# CENSUS API key to retrieve covariates from the CENSUS ACS
#census_api_key("1536c1312955b80cf3be556bea63f3656d4303d1", install = TRUE)

#===============================================================================
# Load and prepare the data ====================================================
# Loading Kaplan's db
# Following is performed to get the ORI values to get data from FBI Crime Explorer for arrests and police Personnel
shr_db <- read_dta('C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/shr_1976_2022.dta') # Check for the correct file and path

# Keeping years from 2010-2022
shr_db <- shr_db %>%  filter(shr_db$year>='2010')

# Doing below to fix the oris - it worked
shr_db$ori <- paste0(shr_db$ori, "00")

# Fixing certain instances of incorrect oris
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="CA0191R00",'CA0191R0X',ori))
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="CA0333200",'CA033320X',ori))
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="CA0199600",'CA019960X',ori))
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="IN0494900",'INIPD0000',ori))

# Create a new variable called "BREAKln" used for cleaning, joining, and filtering
shr_db <- shr_db %>% unite('BREAKln_ori', year, month_of_offense, ori9, remove = FALSE)
shr_db <- shr_db %>% unite('BREAKln', year, month_of_offense, remove = FALSE)
shr_db <- shr_db %>% unite('BREAKln_censusname', year, census_name, state_abb, remove = FALSE) #use this to join acs table with shr

# Uncomment code after fixing issue with FBI API pulls and pulling from CENSUS ACS
# ACS <- ACS %>% unite('BREAKln_censusname', year, NAME, USPS, remove = FALSE) #use this to join acs table with shr
# ACS <- subset(ACS, select = -c(year, BREAKln))

# Below is performed to remove pds that are not local
shr_db <- shr_db %>%
  filter(grepl('city', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('_undetermined', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('parish', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('consolidated', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('town', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('village', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('borough', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('airport', crosswalk_agency_name))
shr_db <- shr_db %>% 
  filter(!grepl('sheriff', crosswalk_agency_name))
shr_db <- shr_db %>% 
  filter(!grepl('park', crosswalk_agency_name))

# Clean certain cities with incorrect names
shr_db$census_name <- ifelse(shr_db$census_name=="san francisco city and county","san francisco city",shr_db$census_name)
shr_db$census_name <- ifelse(shr_db$census_name=="denver city and county","denver city",shr_db$census_name)
shr_db$census_name <- ifelse(shr_db$census_name=="honolulu city and county","honolulu city", shr_db$census_name)
shr_db$census_name <- ifelse(shr_db$census_name==" city and county"," city", shr_db$census_name)

# Creating a copy
shr_db1 <- shr_db

# Create new breakln
shr_db1 <- shr_db1 %>% unite('BREAKln_main', BREAKln_ori, census_name, remove = FALSE) #use this to join acs table with shr
shr_db1 <- shr_db1 %>% unite('BREAKln_year_month', BREAKln, census_name, state_abb, remove = FALSE) #use this to join acs table with shr

#shr_db1 <- shr_db1 %>%  filter(shr_db1$population>=500)

# Getting all oris - doing this to retrieve arrests data from FBI api
ori_db <- data.frame()
ori_db <- shr_db1

ori_db <- subset(ori_db, select = c(ori))

#only works as vector
ori_db <- as.vector(unique(ori_db$ori))

#===============================================================================
# Below we pull the actuals of arrests per month from the FBI database =========
# If you need an example visit the following link: 
# https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/docApi

count = 0
# pulling arrests monthly
Arrest_db <- data.frame()
for (i in 1:3768) {
  randname <- fromJSON(paste0("https://api.usa.gov/crime/fbi/cde/arrest/agency/"
                              , as.character(ori_db[i])
                              , "/all?type=counts&ori="
                              , as.character(ori_db[i])
                              ,"&from=01-2010&to=12-2022&API_KEY=6kJ4gIRT29jihAED612EtwkhVreM3OREHRLDwQFJ"))
  
  Arrest_db1 <- randname[["actuals"]] #store in list
  
  if (length(Arrest_db1) != 0) {

    Arrest_db1 <- rbindlist(Arrest_db1) #turn into dataframe
    Arrest_db1 <- as.data.frame(t(Arrest_db1)) #transpose
    Arrest_db1 <- Arrest_db1 %>% rename_with( ~"arrests", "V1") #rename var
  
    Arrest_db1$ori = ori_db[i] #store the ori to identify agency
    
    #only runs if statement in the first iteration of for loop, then runs alternative for every other iteration
    if (i==1) {
      Arrest_db <- tibble::rownames_to_column(Arrest_db1, "date") #turn rownames into column for the date
      print("hi") #check
    }
    else {
      Arrest_db1 <- tibble::rownames_to_column(Arrest_db1, "date") #turn rownames into column for the date
      Arrest_db <- rbind(Arrest_db, Arrest_db1) #join iteration into database
    }
  }
  else { # this else statement is in case the agency has no arrests data on FBI API
    print("donothing")
  }
  
  count = count + 1 #validation check 
  print(count) #validation check
  print(ori_db[i]) #validation check
}

#saving just in case
write.csv(Arrest_db, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/Arrest_db.csv", row.names=FALSE)


#===============================================================================
#  Here we get data for covariates from the CENSUS ACS 5years ==================
#
# B02009_001E	Estimate!!Total:	Black or African American Alone or in 
# Combination With One or More Other Races
#get acs poverty and unemp, housing occupancy, hs grad pop, and population
CENSUS2010_2022PULL <- data.frame()
for (i in 2010:2022) {
  randname <- get_acs(
    geography = "place",
    variables = c(poverty_rate = "DP03_0119PE",unemp_rate ="DP03_0005PE",
                  housing_occupancy = "DP04_0002PE", hs_grad_pop = "B06009_003E",
                  population = "B01003_001E", black_pop = "B02009_001E"),
    year = i,
    output = "wide",
    survey = "acs5")
  randname$year = i
  CENSUS2010_2022PULL <- rbind(CENSUS2010_2022PULL, randname)
}

#saving just in case
write.csv(CENSUS2010_2022PULL, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/CENSUS2010_2022PULL.csv", row.names=FALSE)


#===============================================================================
# Retrieving land area for later creating pop density variable =================
# unfortunately we do not have 2011 data

count = 0
for (i in 2010:2022) {
  
  print(count) # validation check
  
  if (count != 1) {
    land <- read.delim(paste0("C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/land area for pop density/"
                                  , as.character(i)
                                  ,"_Gaz_place_national.txt"))
    
    land$year = i #assign year column
    land <- subset(land, select = c(USPS, GEOID, NAME, ALAND, AWATER, ALAND_SQMI, year)) #keep only certain variables
    
    #below renames the db depending on the value of i
    #assign(paste0("land", i), land)

    #in the first iteration we simply assign 
    if  (count == 0) {
      land_2010_2022 <- land
    }
    #on other iterations we bind
    else {
      land_2010_2022 <- rbind(land_2010_2022, land)
    }
    
    count = count + 1 # validation check
  }
  #we do below because we dont have data for 2011, so we are using 2010 data for 2011
  else {
    i = i - 1 #we do this so we retrieve data for 2010 instead of 2011
    land <- read.delim(paste0("C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/land area for pop density/"
                              , as.character(i)
                              ,"_Gaz_place_national.txt"))

    land$year = i + 1 #assign year column (2011)
    land <- subset(land, select = c(USPS, GEOID, NAME, ALAND, AWATER, ALAND_SQMI, year)) #keep only certain variables
    land_2010_2022 <- rbind(land_2010_2022, land) #bind data
    
    count = count + 1 # validation check
    i = i + 1 #get i back to its correct value
  }
}

#saving just in case
write.csv(land_2010_2022, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/land_2010_2022.csv", row.names=FALSE)


#===============================================================================
# Retrieving leoka for later creating suicide_by_cop and =======================
# officers_assaulted_and_killed variables

count = 0
for (i in 2010:2022) {
  
  print(count) # validation check
  
    leoka <- read_dta(paste0("C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/leoka/ucr_leoka_monthly_1960_2022_dta/leoka_monthly_"
                              , as.character(i)
                              ,".dta"))
    
    #in the first iteration we simply assign 
    if  (count == 0) {
      leoka_2010_2022 <- leoka
    }
    #on other iterations we bind
    else {
      leoka_2010_2022 <- rbind(leoka_2010_2022, leoka)
    }
    
    count = count + 1 # validation check
}

#saving just in case
write.csv(leoka_2010_2022, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/leoka_2010_2022.csv", row.names=FALSE)


#===============================================================================
# creating the main db =========================================================

# Getting all oris and year
ori_db <- data.frame()
ori_db <- shr_db1

ori_db <- subset(ori_db, select = c(ori, year))

ori_db <- ori_db %>% unite('BREAKln_year_ori', year, ori, remove = FALSE) #we use this to check for duplicates

#check for dups
ori_db <- ori_db %>% distinct(BREAKln_year_ori, .keep_all = TRUE)


unique_ori_db <- unique(ori_db$ori)
unique_ori_db <- as.data.frame(unique_ori_db)

unique_ori_db <- unique_ori_db %>% rename_with( ~"ori", "unique_ori_db")

#creating an initial row to bind our loop iterations to, it later gets deleted
oriyear <- data.frame(ori= "x", year = 1, month = 1)


#Loop is done to create a database that has all periods for all oris
count = 0
#first we iterate through the years
for (j in 2010:2022) {
  
  print(j) # validation check
  
  #here we iterate through the oris (3769)
  for (i in 1:length(unique_ori_db$ori)) {
    
    #this loop is for the imputation of columns of ori, year, month
    for (k in 1:12) {
      oriyear[nrow(oriyear) + 1,] <- list(unique_ori_db$ori[i], j, k)
      
      print(k) # validation check
    }
    print(i) # validation check
  }
}

#removing first row
oriyear = oriyear[-1,]

#saving just in case
write.csv(oriyear, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/main_initial_db.csv", row.names=FALSE)

#SAVING NEW SHR
#write.csv(shr_db, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/shr_db_filtered.csv", row.names=FALSE)


#===============================================================================
# Load and prepare the data ====================================================
# Loading CENSUS db
CENSUS2010_2022 <- read.csv('C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/CENSUS2010_2022PULL.csv')
# Loading land area data
land_2010_2022 <- read.csv('C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/land_2010_2022.csv')


#change name for honolulu
CENSUS2010_2022$NAME <- str_replace(CENSUS2010_2022$NAME, "Urban Honolulu CDP, Hawaii", "honolulu city, hawaii")
land_2010_2022$NAME <- str_replace(land_2010_2022$NAME, "Urban Honolulu CDP, Hawaii", "honolulu city, hawaii")

#cleaning
#remove all CDPs, towns, villages, etc...
CENSUS2010_2022 <- CENSUS2010_2022 %>% 
  filter(!grepl(' CDP', NAME))
CENSUS2010_2022 <- CENSUS2010_2022 %>% 
  filter(!grepl(' Puerto Rico', NAME))

#clean
CENSUS2010_2022 <- subset(CENSUS2010_2022, select = -c(DP03_0119PM, DP03_0005PM, DP04_0002PM, B06009_003M, B01003_001M, B02009_001M))

#breaklns for join (makes joining tables easier)
CENSUS2010_2022 <- CENSUS2010_2022 %>% unite('BREAKln', year, GEOID, remove = FALSE)
land_2010_2022 <- land_2010_2022 %>% unite('BREAKln', year, GEOID, remove = FALSE)

#clean
land_2010_2022 <- subset(land_2010_2022, select = -c(year, NAME, GEOID))

#join land area with census vars
ACS <- left_join(CENSUS2010_2022, land_2010_2022, by = "BREAKln")

#saving
#write.csv(ACS, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/ACS.csv", row.names=FALSE)

