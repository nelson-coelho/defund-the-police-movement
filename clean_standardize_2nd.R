# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code cleans and standardizes the database for the article.
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
# File Name: clean_standardize_2nd.R
#
# Description: This file contains R code to clean and standardize the database 
# used in the article.
#
# Modifications:
#
# Changes:
#
# - 2024-11-14: Initial version by Nelson Coelho
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
# Loading CENSUS db
ACS <- read.csv('C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/ACS.csv')


#make city names lowercase
ACS$NAME <- tolower(ACS$NAME)

#get pop density  (pop /  land area)
#ACS$pop_densitySQMI <- (ACS$population / ACS$ALAND_SQMI)
#ACS$pop_density <- (ACS$population / ACS$ALAND)

#ACS$NAME <- gsub('[^[:alnum:] ]','', ACS$NAME)


#cleaning
#some city names were changed in excel
ACS$NAME <- str_replace(ACS$NAME, "st. ", "st ")
ACS$NAME <- str_replace(ACS$NAME, "washington city, district of columbia", "washington dc city, district of columbia")
ACS$NAME <- str_replace(ACS$NAME, " city city", " city")
ACS$NAME <- str_replace(ACS$NAME, "boise city city", "boise city")
ACS$NAME <- str_replace(ACS$NAME, "phenix city city", "phenix city")
ACS$NAME <- str_replace(ACS$NAME, "temple city city", "temple city")
ACS$NAME <- str_replace(ACS$NAME, "king city city", "king city")
ACS$NAME <- str_replace(ACS$NAME, "cathedral city city", "cathedral city")
ACS$NAME <- str_replace(ACS$NAME, "vist city", "vista city")
ACS$NAME <- str_replace(ACS$NAME, "vista city, california", "vista  city, california")
ACS$NAME <- str_replace(ACS$NAME, "coeur d'alene city", "coeur d alene city")
ACS$NAME <- str_replace(ACS$NAME, "de kalb city, texas", "dekalb city, texas")
ACS$NAME <- str_replace(ACS$NAME, "o fallon", "o'fallon")
ACS$NAME <- str_replace(ACS$NAME, "o'fallon city, illinois", "o fallon city, illinois")
ACS$NAME <- str_replace(ACS$NAME, "dekalb city, illinois", "de kalb city, illinois")
ACS$NAME <- str_replace(ACS$NAME, "bedford town, virginia", "bedford city, virginia")
ACS$NAME <- str_replace(ACS$NAME, "macon-bibb county, georgia", "macon city, georgia")
ACS$NAME <- str_replace(ACS$NAME, "la porte city, indiana", "laporte city, indiana")
ACS$NAME <- str_replace(ACS$NAME, "deridder city, louisiana", "de ridder city, louisiana")
ACS$NAME <- str_replace(ACS$NAME, "barnstable town city, massachusetts", "barnstable city, massachusetts")
ACS$NAME <- str_replace(ACS$NAME, "georgiana town, alabama", "georgiana city, alabama")
ACS$NAME <- str_replace(ACS$NAME, "august city", "augusta city")
ACS$NAME <- str_replace(ACS$NAME, "minnetrist city, minnesota", "minnetrista city, minnesota")
ACS$NAME <- str_replace(ACS$NAME, "lee's summit city, missouri", "lees summit city, missouri")
ACS$NAME <- str_replace(ACS$NAME, "groveport village, ohio", "groveport city, ohio")
ACS$NAME <- str_replace(ACS$NAME, "sebring village, ohio", "sebring city, ohio")
ACS$NAME <- str_replace(ACS$NAME, "vian town, oklahoma", "vian city, oklahoma")
ACS$NAME <- str_replace(ACS$NAME, "dubois city, pennsylvania", "du bois city, pennsylvania")
ACS$NAME <- str_replace(ACS$NAME, "wilkes-barre city, pennsylvania", "wilkes barre city, pennsylvania")
ACS$NAME <- str_replace(ACS$NAME, "north august city, south carolina", "north augusta city, south carolina")
ACS$NAME <- str_replace(ACS$NAME, "soddy-daisy city, tennessee", "soddy daisy city, tennessee")
ACS$NAME <- str_replace(ACS$NAME, "lacy-lakeview city, texas", "lacy lakeview city, texas")
ACS$NAME <- str_replace(ACS$NAME, "corpus christ city, texas", "corpus christi city, texas")
ACS$NAME <- str_replace(ACS$NAME, "bullard town, texas", "bullard city, texas")
ACS$NAME <- str_replace(ACS$NAME, "merkel town, texas", "merkel city, texas")
ACS$NAME <- str_replace(ACS$NAME, "sault ste. marie city, michigan", "sault ste marie city, michigan")
ACS$NAME <- str_replace(ACS$NAME, "mount morris city", "mt morris city")
ACS$NAME <- str_replace(ACS$NAME, "mt pleasant city", "mount pleasant city")
ACS$NAME <- str_replace(ACS$NAME, "morehead city town", "morehead city")
ACS$NAME <- str_replace(ACS$NAME, "absecanon city", "absecon city")
ACS$NAME <- str_replace(ACS$NAME, "pocola town, oklahoma", "pocola city, oklahoma")
ACS$NAME <- str_replace(ACS$NAME, "el paso de robles (paso robles) city, california", "el paso de robles city")
ACS$NAME <- str_replace(ACS$NAME, "caro village, michigan", "caro city, michigan")
ACS$NAME <- str_replace(ACS$NAME, "st john city, missouri", "st johns city, missouri")
ACS$NAME <- str_replace(ACS$NAME, "mentor-on-the-lake city", "mentor on the lake city")
ACS$NAME <- str_replace(ACS$NAME, "milton-freewater city", "milton freewater city")
ACS$NAME <- str_replace(ACS$NAME, "flower mound town", "flower mound city")
ACS$NAME <- str_replace(ACS$NAME, "corrigan town, texas", "corrigan city, texas")
ACS$NAME <- str_replace(ACS$NAME, "san saba town", "san saba city")
ACS$NAME <- str_replace(ACS$NAME, "sedro-woolley city", "sedro woolley city")
ACS$NAME <- str_replace(ACS$NAME, "mount shast city", "mount shasta city")
ACS$NAME <- str_replace(ACS$NAME, "lasalle city, illinois", "la salle city, illinois")
ACS$NAME <- str_replace(ACS$NAME, "gallipolis village", "gallipolis city")
ACS$NAME <- str_replace(ACS$NAME, "erwin town, tennessee", "erwin city, tennessee")
ACS$NAME <- str_replace(ACS$NAME, "mount juliet city", "mt juliet city")
ACS$NAME <- str_replace(ACS$NAME, "ponder town, texas", "ponder city, texas")
ACS$NAME <- str_replace(ACS$NAME, "poth town, texas", "poth city, texas")
ACS$NAME <- str_replace(ACS$NAME, "bellaire village", "bellaire city")
ACS$NAME <- str_replace(ACS$NAME, "loudon town", "loudon city")
ACS$NAME <- str_replace(ACS$NAME, "morgan's point resort city", "morgans point resort city")
ACS$NAME <- str_replace(ACS$NAME, "lake st louis city", "lake saint louis city")
ACS$NAME <- str_replace(ACS$NAME, "o'neill city", "oneill city")
ACS$NAME <- str_replace(ACS$NAME, "winfield town, tennessee", "winfield city, tennessee")
ACS$NAME <- str_replace(ACS$NAME, "dequincy city, louisiana", "de quincy city, louisiana")
ACS$NAME <- str_replace(ACS$NAME, "wardell town", "wardell city")
ACS$NAME <- str_replace(ACS$NAME, "unalaska city", "unalaska  city")
ACS$NAME <- str_replace(ACS$NAME, "holly hill town, south carolina", "holly hill city, south carolina")
ACS$NAME <- str_replace(ACS$NAME, "truckee town", "truckee city")
ACS$NAME <- str_replace(ACS$NAME, "lincoln heights village", "lincoln heights city")
ACS$NAME <- str_replace(ACS$NAME, "denver city town, texas", "denver city, texas")
ACS$NAME <- str_replace(ACS$NAME, "ste. genevieve city", "ste genevieve city")
ACS$NAME <- str_replace(ACS$NAME, "bruceville-eddy city", "bruceville eddy city")
ACS$NAME <- str_replace(ACS$NAME, "southwest city town", "southwest city")
ACS$NAME <- str_replace(ACS$NAME, "tieton town", "tieton city")
ACS$NAME <- str_replace(ACS$NAME, "modest city, california", "modesto city, california")
ACS$NAME <- str_replace(ACS$NAME, "cost mesa city, california", "costa mesa city, california")
ACS$NAME <- str_replace(ACS$NAME, "yucca valley town, california", "yucca valley city, california")
ACS$NAME <- str_replace(ACS$NAME, "reno town, texas", "reno city, texas")
ACS$NAME <- str_replace(ACS$NAME, "mount pleasant city, michigan", "mt pleasant city, michigan")
ACS$NAME <- str_replace(ACS$NAME, "arkansas city, kansas", "xx, kansas")
ACS$NAME <- str_replace(ACS$NAME, "kansas city, kansas", "wyandotte county and kansas city unified governmen, kansas")
ACS$NAME <- str_replace(ACS$NAME, "xx, kansas", "arkansas city, kansas")
ACS$NAME <- str_replace(ACS$NAME, "chula vista  city, california", "chula vista city, california")
ACS$NAME <- str_replace(ACS$NAME, "valdost city, georgia", "valdosta city, georgia")



#ACS$NAME <- str_replace(ACS$NAME, "city", "city,")




#===============================================================================
# Load and prepare the data ====================================================
# Loading Kaplan db SHR
shr_db <- read_dta('C:/Users/sarah/Desktop/lookinto/CEOE/shr_1976_2022.dta')


#taking out all agencies that are not local
shr_db <- shr_db %>%  filter(shr_db$agency_type=='local police department')


#keeping years from 2010-2022
shr_db <- shr_db %>%  filter(shr_db$year>='2010')

#below 5 lines may have to be deleted doing a test to fix the oris - it worked
shr_db$ori <- paste0(shr_db$ori, "00")

#fix oris
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="CA0191R00",'CA0191R0X',ori))
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="CA0333200",'CA033320X',ori))
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="CA0199600",'CA019960X',ori))
shr_db <-shr_db %>% mutate(ori=ifelse(ori=="IN0494900",'INIPD0000',ori))


shr_db <- shr_db %>% unite('BREAKln', year, month_of_offense, remove = FALSE)
shr_db <- shr_db %>% unite('BREAKln_censusname', year, census_name, state_abb, remove = FALSE) #use this to join acs table with shr
#ACS <- ACS %>% unite('BREAKln_censusname', year, NAME, USPS, remove = FALSE) #use this to join acs table with shr

#below are done to remove pds that are not local, also some cities with lots of missing data
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
  filter(!grepl('airport', census_name))
shr_db <- shr_db %>% 
  filter(!grepl('sheriff', crosswalk_agency_name))
shr_db <- shr_db %>% 
  filter(!grepl('park', crosswalk_agency_name))





# #clean
# shr_db$census_name <- ifelse(shr_db$census_name=="san francisco city and county","san francisco city", shr_db$census_name)
# shr_db$census_name <- ifelse(shr_db$census_name=="denver city and county","denver city", shr_db$census_name)
# shr_db$census_name <- ifelse(shr_db$census_name=="honolulu city and county","honolulu city", shr_db$census_name)
# shr_db$census_name <- ifelse(shr_db$census_name==" city and county"," city", shr_db$census_name)


shr_db$census_name <- str_replace(shr_db$census_name, " city city", " city")
shr_db$census_name <- str_replace(shr_db$census_name, " city and county", " city")

#remove county pd
shr_db <- shr_db %>% 
  filter(!grepl('county', crosswalk_agency_name))

#create breakln
#shr_db <- shr_db %>% unite('BREAKln_main', BREAKln_ori, census_name, remove = FALSE) #use this to join acs table with shr
shr_db <- shr_db %>% unite('BREAKln_year_month', BREAKln, census_name, state_abb, remove = FALSE) #use this to join acs table with shr



#===============================================================================
# Load and prepare the data ====================================================
# Loading main db with all the oris
main_initial_db <- read.csv('C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/main_initial_db.csv')


#remove county pd
main_initial_db <- main_initial_db %>%
  filter(!grepl('VA0470100', ori))


#retrieving city names to bind to main db
shr_db_names <- subset(shr_db, select = c(ori, ori9, census_name, state, state_abb,
                                                   crosswalk_agency_name, year))





#create breakln for filtering
#shr_db_names <- shr_db_names %>% unite('BREAKln_ori', year, ori9, remove = FALSE)
main_initial_db <- main_initial_db %>% unite('BREAKln_ori', year, ori, remove = FALSE)



#removing equal names for join
shr_db_names <- subset(shr_db_names, select = -c(ori9, year))

#join dbs
main_initial_db <- left_join(main_initial_db, shr_db_names, by = "ori")


# create new breakln to remove duplicates
main_initial_db <- main_initial_db %>% unite('BREAKln', BREAKln_ori, month, remove = FALSE)


#removing duplicate rows
main_initial_db <- main_initial_db %>% distinct(BREAKln, .keep_all = TRUE)

#save it
#write.csv(main_initial_db, "C:/Users/sarah/Desktop/lookinto/_DTP article Nov new db/dbs/main_initial_db_w_names.csv", row.names=FALSE)

