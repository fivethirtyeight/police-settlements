# This script takes the raw pdf on settlements received from Indianapolis, and outputs a csv
# with variables cleaned and standardized, and filtered

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 12/4/2020
# Updated by Damini Sharma 

# Note: The spreadsheet has a sheet called "Notes" with the following note:
# The litigation settlement information in this spreadsheet was compiled from records kept by the Office of Corporation Counsel. 
# We cannot guarantee the accuracy or completeness of records kept before March 2016 when the current management team joined the office. 
# Cases are placed in the appropriate year's tab based on the date the case was moved to closed status for Office of Corporation Counsel 
# case tracking purposes, not necessarily the date a settlement agreement was signed.

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())


# Read in data
# It's in separate sheets for each year and has superfluous columns
indy2019 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2019",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2019,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"))

indy2018 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2018",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2018,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"))

indy2017 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2017",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2017,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"))
# Marvin Coffey: 1976??

indy2016 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2016",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2016)

indy2015 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2015",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2015,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"),
         `Date closed` = as.Date(as.numeric(`Date closed`), origin = "1899-12-30"))

indy2014 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2014",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2014,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"))

indy2013 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2013",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2013,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"),
         `Date opened` = as.Date(as.numeric(`Date opened`), origin = "1899-12-30"))

indy2012 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2012",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2012,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"))

indy2011 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2011",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2011)

indy2010 <- read_excel(paste0(raw_data_path,"Indianapolis.xlsx"), sheet = "2010",
                       col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                     "guess", "guess", "guess", "guess", "guess", "guess", "guess", "numeric", "guess", "guess")) %>%
  select(`Case Name`, `Cause Number`, `Incident date`, `Date opened`, `Date closed`, `Type of claim`, `Settlement amount`, `Plaintiff's Attorney`) %>%
  mutate(year = 2010,
         `Incident date` = as.Date(as.numeric(`Incident date`), origin = "1899-12-30"))



indy <- bind_rows(indy2019, indy2018, indy2017, indy2016, indy2015, indy2014, indy2013, indy2012, indy2011, indy2010)

## Rename

indy <- indy %>%
  rename(matter_name = `Case Name`,
         docket_number = `Cause Number`,
         incident_date = `Incident date`,
         filed_date = `Date opened`,
         closed_date = `Date closed`,
         summary_allegations = `Type of claim`,
         amount_awarded = `Settlement amount`,
         plaintiff_attorney = `Plaintiff's Attorney`) %>%
  mutate(plaintiff_name = ifelse(year < 2014, matter_name, NA), # Before 2014, it seems to be plaintiff name... not case name. Will add that but keep it under case name just in case.
         calendar_year = year(closed_date),
         incident_year = if_else(matter_name == "Lincoln Plowman, Rebecca Lake, Tim Motsinger, Pat Commisky", 2006, year(incident_date)),
         filed_year = year(filed_date),
         city = "Indianapolis",
         state = "IN",
         other_expenses = NA,
         total_incurred = NA,
         claim_number = NA,
         collection = NA,
         case_outcome = NA,
         court = NA,
         location = NA,
         summary_allegations = tolower(gsub(",", ";", summary_allegations))
         )

### Filter

table(indy$summary_allegations)

### Questionable things
# breach of contract
# apra; attorney's fees
# civil rights; ada
# civil rights; due process
# civil rights; negligence
# contract
# declaratory judgment
# defamation
# employment law; discrimination
# employment; wrongful termination
# motor vehicle accident
# negligence
# negligence; motor veh accident
# negligence; negligent pursuit
# negligence; property damage
# return of property
# subrogration;respondeat superior


indy <- indy %>%
  filter(!summary_allegations %in% c("motor vehicle accident", # Taking out this one but NOT "negligence: motor veh accident"
                                     "employment; wrongful termination", "employment law; discrimination", 
                                     "breach of contract", "contract",
                                     "apra; attorney's fees" # This seems like a FOIA settlement (I think APRA is IN's Access to Public Records Act)
                                     ))


### 

# CHECKS
# Time period of closed date? NA
summary(indy$closed_date) # Min 2010, max 2019, 1 NA
# time period of calendar year or incident year or filed year if closed date missing
summary(indy$calendar_year) # Min 2010, max 2019
summary(indy$incident_year) # 1976 - 2018, 39 NAs
summary(indy$incident_date) # 1976 - 2018, 39 NAs

# Perfect duplicates? 0 duplicates
nrow(indy %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(indy %>% group_by(docket_number) %>% filter(n()>1))
dups <- indy %>% group_by(docket_number) %>% filter(n()>1)
# 2 cases with 2 different payments, filed / closed dates and even matter name in one case
# seem like separate payments for the same docket number - leaving as is


# What's filtered out? what's left that ambiguous?
table(indy$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(indy %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(indy %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(indy %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(indy %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(indy %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(indy))

print("Total amount awarded")
sum(indy$amount_awarded)

# Check with WSJ - they have $6.6m from 2015-2020. We have ~$7m from 2015-2019, but $6.2m if we exclude "negligence: motor vehicle accidents"
indy_2015_2020 <- indy %>% filter(closed_date>="2015-01-01")
sum(indy_2015_2020$amount_awarded)

write.csv(indy,paste0(out_data_path,"indianapolis_edited.csv"), na = "",row.names = FALSE)


