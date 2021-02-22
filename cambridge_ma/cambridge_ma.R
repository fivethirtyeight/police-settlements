# This script takes and separates the raw and cleaned data from Cambridge
# Likhita originally cleaned this manually - this script just imports the cleaned
# sheet and outputs it with the rest of the cleaned city data

# Original cleaning done by Likhitha Butchireddygari at FiveThirtyEight
# Original code written by Damini Sharma at The Marshall Project
# Original date 8/22/20
# Code updated by Laura Bronner at FiveThirtyEight on 12/16/2020: 

# From Laura: Amelia Thomson-DeVeaux received an updated spreadsheet with a fourth case, 
# so I replaced Likhitha's manual cleaning & Damini's tidying with programmatic cleaning.

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())


cambridge <- read_excel(paste0(raw_data_path,"CPD Citizen Lawsuits 2010-Present.xlsx")) 

# Cleaning & Tidying
cambridge <- cambridge %>%
  #filter(`Plaintiff(s)` == "Samuel Allen Counter") %>%
  separate(`Court and Docket #`, into = c("court", "docket_number"), sep = ",") %>%
  rename(plaintiff_name = `Plaintiff(s)`,
         plaintiff_attorney = `Plaintiff's Attorney`,
         location = `Location of Incident`,
         summary_allegations = `Summary of Allegations`,
         amount_awarded = `Settlement Amount`) %>%
  mutate(city = "Cambridge",
         state = "MA",
         docket_number = trimws(docket_number),
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         calendar_year = NA,
         filed_date = NA,
         closed_date = NA,
         matter_name = NA,
         case_outcome = NA,
         claim_number = NA,
         incident_date_clean = as.Date(as.numeric(`Date of Incident`),origin = '1899-12-30'),
         incident_date_full = ifelse(is.na(incident_date_clean),`Date of Incident`,as.character(incident_date_clean))) %>% 
  mutate(incident_date = incident_date_clean,
         incident_year = year(incident_date)) 

# Likhitha looked up the docket numbers for the three original cases and added them in manually, so I will add them in here.
# I couldn't find the fourth case in a cursory search; leaving that as NA
cambridge <- cambridge %>%
  mutate(filed_year = case_when(plaintiff_name == "Kaveh L. Afrasiabi" ~ 2014,
                                plaintiff_name == "Jason Freedman" ~ 2016,
                                plaintiff_name == "Richard Strahan" ~ 2015))

# Police Officers data
cambridge_police_officers <- cambridge %>%
  select(city, state, docket_number, police_officer = `Officers Involved`) 

# Does merging on docket numebrs work? # Yes 
merged <- left_join(cambridge, cambridge_police_officers)

# clean up vars
cambridge <- cambridge %>% select(calendar_year,
                      city,
                      state,
                      incident_date,
                      incident_year,
                      incident_date_full,
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
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(cambridge$filed_year) # Min 2014, max 2016

# Perfect duplicates? 0 duplicates
nrow(cambridge %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(cambridge %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# no filtering done, all 3 cases look relevant
table(cambridge$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(cambridge %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(cambridge %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(cambridge %>% filter(is.na(filed_year))),"rows missing filed year"))
print(paste("There are",nrow(cambridge %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(cambridge %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))

# count cases
print("Total number of cases")
print(nrow(cambridge))

print("Total amount awarded")
sum(cambridge$amount_awarded)

write.csv(cambridge,paste0(out_data_path,"cambridge_edited.csv"), na = "",row.names = FALSE)
write.csv(cambridge_police_officers,paste0(out_data_path,"cambridge_police_edited.csv"), na = "",row.names = FALSE)




