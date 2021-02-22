# This script takes the raw data on settlements received from Milwaukee and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# The column names in the original spreadsheet had an \n between words that made reading in the data
# harder. Spreadsheet_colnames_clean.xlsx is an intermediate sheet created to fix that problem

# Original code written by Damini Sharma at The Marshall Project
# Original date 8/13/20

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

milwaukee <- read_excel(paste0(raw_data_path,"Spreadsheet_colnames_clean.xlsx"), skip = 1)

# Steps taken:
# 1. standardize variable names to match agreed upon format, generate empty vars for unavailable vars
# 2. Clean up date vars, amount awarded 
# 3. Filter to cases we care about
# 4. Check duplicates on primary key(s)

# calendar_year/fiscal_year -> calendar year, pulled from date paid
# city	-> generated 
# incident_date	-> don't have, generate empty var
# date_filed	-> opened date
# closed_date	 -> date paid
# amount_awarded -> create: judgement + settlement
# other_expenses	-> don't have generate NA var
# collection		-> don't have generate NA var
# total_incurred		-> don't have generate NA var
# case_outcome		-> construct from settlement / judgement vars
# docket_number		-> case no
# claim_number		-> don't have generate NA var
# court		-> court
# plaintiff_name		-> don't have generate NA var
# matter_name	-> matter description
# plaintiff_attorney		-> pltf's atty
# location		-> don't have generate NA var
# summary_allegations (separated by ;) 	-> subject
# status <- status


# Step 1
# generate NA vars for incident_date, other_expenses, collection, total_incurred, claim_number, plaintiff_name, 
# location
milwaukee <- milwaukee %>% 
  rename(docket_number = `Case No.`,
         summary_allegations = Subject,
         matter_name = `Matter Description`,
         status = Status,
         court = Court,
         filed_date = `Opened Date`,
         plaintiff_attorney = `Pltf's Atty`,
         closed_date = `Date Paid`) %>% 
  mutate(city = "Milwaukee", state= "WI",
         filed_date = as.Date(filed_date),
         filed_year = year(filed_date),
         closed_date = as.Date(closed_date),
         calendar_year = year(closed_date)) %>% 
  mutate(incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         collection= NA,
         total_incurred = NA,
         claim_number = NA,
         plaintiff_name = NA,
         location= NA) %>% 
  mutate(amount_awarded = `Judgment Amount` + `Settlement Amount`,
         case_outcome = ifelse(`Settlement Against City`=="Y","Settled","Judgement"))

# Status
milwaukee %>% count(status)

# 6 cases marked as open. According to PACER, these are in fact closed - they may not have been 
# closed out by attorney. Updating status and their closed dates from PACER here
milwaukee %>% filter(status == "Open") %>% select(docket_number, closed_date)
milwaukee <- milwaukee %>% 
  mutate(status = ifelse(status=="Open","Closed",status)) %>% 
  mutate(closed_date = case_when(
    docket_number == "15C0264" ~ as.Date("2017-02-10"),
    docket_number == "15C0480" ~ as.Date("2017-11-01"),
    docket_number == "18C1763" ~ as.Date("2019-05-07"),
    TRUE ~ closed_date
  ))

# Step 2 - date vars already taken care of 

# Step 3 - filtering cases
print("Total number of cases")
print(nrow(milwaukee))

allegations <- milwaukee %>% count(summary_allegations)

# All of these allegations look fine to me - the rough groups are: Body Cavity Search Police,
# Body Cavity Search Search & Seizure/Unlawful, Unlawful Stop & Frisk Police,
# Civil Rights related, Excessive Force, False Arrest, Miranda Rights violation, unlawful detainment,
# shooting - wrongful death, sexual assault police -civil rights.


milwaukee <- milwaukee %>% select(calendar_year,
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
                              summary_allegations,
                              status)


# CHECKS
# Time period of closed date? 
summary(milwaukee$closed_date)
# time period of calendar year or incident year or filed year if closed date missing
summary(milwaukee$calendar_year) # Min 2010, max 2019

# Perfect duplicates? 0 duplicates
nrow(milwaukee %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(milwaukee %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(milwaukee$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(milwaukee %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(milwaukee %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(milwaukee %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(milwaukee %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(milwaukee %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(milwaukee))

print("Total amount awarded")
sum(milwaukee$amount_awarded)

write.csv(milwaukee,paste0(out_data_path,"milwaukee_edited.csv"), na = "",row.names = FALSE)






