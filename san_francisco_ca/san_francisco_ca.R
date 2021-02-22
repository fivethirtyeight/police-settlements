# This script takes the raw data on settlements received from San Francisco and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Damini Sharma at The Marshall Project
# Original date 8/13/20

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

sf <- read.csv(paste0(raw_data_path,"Rq20200701a (1).csv"),stringsAsFactors = FALSE)

# Steps taken:
# 1. standardize variable names to match agreed upon format, generate empty vars for unavailable vars
# 2. Clean up date vars, amount awarded and do some sanity checks
# 3. Filter to cases we care about

# calendar_year/fiscal_year -> calendar year, pulled from payment date
# city	-> generated 
# incident_date	-> don't have, generate empty var
# date_filed	-> filed date
# closed_date	 -> payment date
# amount_awarded -> complete payment	
# other_expenses	-> don't have generate NA var
# collection		-> don't have generate NA var
# total_incurred		-> don't have generate NA var
# case_outcome		-> don't have generate NA var
# docket_number		-> "Court" var
# claim_number		-> don't have generate NA var
# court		-> don't have generate NA var
# plaintiff_name		-> don't have generate NA var
# matter_name	-> "File Name"
# plaintiff_attorney		-> don't have generate NA var
# location		-> don't have generate NA var
# summary_allegations (separated by ;) 	-> "alleged cause"

# Step 1
# generate NA vars for incident_date, other_expenses, collection, total_incurred, case_outcome, claim_number, court, plaintiff_name, 
# plaintiff_attorney, location
# rename Court -> docket number
# rename summary_allegations -> alleged cause
# rename matter_name -> file name
# generate city var
sf <- sf %>% 
  rename(docket_number = Court.,
         summary_allegations = Alleged.Cause,
         matter_name = File.Name) %>% 
  mutate(city = "San Francisco", state="CA") %>% 
  mutate(incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         collection= NA,
         total_incurred = NA,
         case_outcome = NA,
         claim_number = NA,
         court = NA,
         plaintiff_name = NA,
         plaintiff_attorney = NA,
         location= NA)

# Step 2
# Convert Filed Date and Payment Date in to date
sf <- sf %>% 
  mutate(filed_date = mdy(Filed.Date, quiet = TRUE),
         filed_year = year(filed_date),
         closed_date = mdy(Payment.Date, quiet = TRUE))

# Check - did we create any NAs? # Two NAs in Filed Date, but none missing for closed date
sf %>% filter(!is.na(as.integer(Filed.Date)) & is.na(filed_date))
sf %>% filter(!is.na(Filed.Date) & is.na(filed_date))
sf %>% filter(!is.na(as.integer(Payment.Date)) & is.na(closed_date))
sf %>% filter(is.na(closed_date))
sf %>% filter(!is.na(Payment.Date) & is.na(closed_date))

# Calendar year
sf <- sf %>% 
  mutate(calendar_year = year(closed_date))

# amount awarded
sf <- sf %>% 
  mutate(settlement_city_attorney = as.numeric(gsub("[\\$,]", "", Settlement..Judgment.Paid.Through.City.Attorney.s.Office)),
         total_payment = as.numeric(gsub("[\\$,]", "", Complete.Settlement..Judgment))) %>% 
  mutate(flag = ifelse(settlement_city_attorney == total_payment,0,1)) 

# one flag where these are different but this isnt a case we care about for this project (racial discrimination lawsuit)
# so going to keep total_payment as amount awarded
sf %>% filter(flag==1)

sf <- sf %>% rename(amount_awarded = total_payment)

# Step 3
sf %>% count(summary_allegations)

# Based on this, the allegations all have codes and those marked with "Police" have 2000s code. This is likely the subset
# we care about, but there are some others that might be related - e.g. Injury on board, door kicks (property damage)... 
# In this file will conservatively only keep those marked as Police 
print("Total number of cases")
print(nrow(sf))
sf <- sf %>% filter(grepl("Police",summary_allegations ))
print("Total number of cases labeled 'Police'")
print(nrow(sf))

# Restrict to vars we care about and make sure final data looks fine
sf <- sf %>% select(calendar_year,
                    city,
                    state,
                    incident_date,
                    incident_year,
                    filed_date,
                    filed_year,
                    closed_date,
                    amount_awarded,
                    other_expenses,
                    collection,
                    total_incurred,
                    case_outcome,
                    docket_number,
                    claim_number,
                    court,
                    plaintiff_name,
                    plaintiff_attorney,
                    matter_name,
                    location,
                    summary_allegations)



# CHECKS
# Time period of closed date? 
summary(sf$closed_date) # Min 2010, max 2019
# time period of calendar year or incident year or filed year if closed date missing
summary(sf$calendar_year) # Min 2010, max 2019

# Perfect duplicates? 0 duplicates
nrow(sf %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(sf %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
table(sf$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(sf %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(sf %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(sf %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(sf %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(sf %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(sf))

print("Total amount awarded")
sum(sf$amount_awarded)

# Check WSJ (they have $22m for 2015-2020, we have ~$20m)
sf_2015_2020 <- sf %>% filter(closed_date>="2015-01-01")
sum(sf_2015_2020$amount_awarded)

write.csv(sf,paste0(out_data_path,"san_francisco_edited.csv"), na = "",row.names = FALSE)



