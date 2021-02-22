# This script takes the raw data on settlements received from St Louis and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Damini Sharma at The Marshall Project
# Original date 8/17/20

# Read in CSV
# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

stl <- read_excel(paste0(raw_data_path,"reconcile_pdf_excel.xlsx"))
in_excel <- stl$`In excel`
in_pdf <- stl$`In PDF`

in_excel[!in_excel %in% in_pdf]
in_pdf[!in_pdf %in% in_excel]

# THere is one case in the PDF that isn't in the excel file: 4:18-cv-01963
# The pdf description says 
# "THIS WAS A STATE CASE (1822-CC11392) THAT WAS REMOVED TO FEDERAL COURT. PLAINTIFFS AUTO WAS STOLEN, POLICE RECOVERED IT, HAD IT SENT TO"
# We are excluding since it was auto-related

stl <- read_excel(paste0(raw_data_path,"Police Cases Settled.xlsx"))

## Steps taken:
# 1. standardize variable names to match agreed upon format, generate empty vars for unavailable vars
# 2. Clean up date vars, amount awarded and do some sanity checks
# 3. Filter to cases we care about
# 4. Check duplicates on primary key(s)

# calendar_year/fiscal_year -> calendar year, pulled from settle date
# city	-> generated 
# incident_date	-> injury date
# date_filed	-> date_received
# closed_date	 -> settle date
# amount_awarded -> value amount	
# other_expenses	-> don't have generate NA var
# collection		-> don't have generate NA var
# total_incurred		-> don't have generate NA var
# case_outcome		-> verdict
# docket_number		-> cause number
# claim_number		-> don't have generate NA var
# court		-> court
# plaintiff_name		-> don't have generate NA var
# matter_name	-> Style
# plaintiff_attorney		-> Plaintiff Attornye
# location		-> don't have generate NA var
# summary_allegations (separated by ;) 	-> COA

## Other Vars
# - Defendant attorney (include)
# - Department (exclude - all "Police")
# - COA_description (exclude - Etiher Auto or Other - NA for Civil rights cases)
# - Injury (exclude - longer description of case - useful to ensure we have cases we need but dont need var)
# - status (all closed - exclude)
# - PLFF lawfirm (include)
# - Voucher date (unclear what this is... usually before before closed date - include?)


# Step 1
stl <- stl %>% 
  rename(plaintiff_attorney = `Plantiff Attorney`,
         #incident_date = `Injury Date`,
         #filed_date = `Date Received`,
         amount_awarded = `Value Amount`,
         case_outcome = Verdict,
         docket_number = `Cause Number`,
         court = Court,
         matter_name = Style,
         summary_allegations = COA,
         defendant_attorney = `Defendent Attorney`,
         plaintiff_lawfirm = PLFF_LAWFIRM) %>% 
  mutate(city = "St. Louis", state= "MO") %>% 
  mutate(incident_date = mdy(`Injury Date`),
         incident_year = year(incident_date),
         filed_date = mdy(`Date Received`),
         filed_year = year(filed_date),
         voucher_date = as.Date(Voucher_Date),
        other_expenses = NA,
         collection= NA,
         total_incurred = NA,
         claim_number = NA,
         plaintiff_name = NA,
         location = NA)

## Step 2
# Convert Settle Date to Closed date and generate calendar year
stl <- stl %>% 
  mutate(closed_date = mdy(`Settle Date`, quiet = TRUE)) %>% 
  mutate(calendar_year = year(closed_date))

summary(stl$closed_date) # 2015 to 2019

# Check - did we create any NAs? # None missing for closed date
stl %>% filter(!is.na(as.integer(closed_date)) & is.na(`Settle Date`))
stl %>% filter(is.na(as.integer(closed_date)))


## Step 3 - filter to cases we care about
print("Total number of cases")
nrow(stl)

stl %>% count(summary_allegations)

# All cases with "COA_Description" = "Auto" seem like MVA cases based on the injury description (10) - filter these out
# Cases with summary_allegations related to "Employment" (3) should be filtered out
# Case with summary_allegations "Other" has injury description "CLAIMS POLICE DEPT VIOLATED SUNSHINE LAW AND WAS OVERCHARGING FOR MATERIALS" - filter this out

# keep everything else
# Three ambiguous cases: two labeled "Personal injury & property damage" where the 
# injury description is "MVA W/ POLICE VEHICLE". One labeled "Persnal Injury - Other" 
# where description is "PEDESTRIAN STRUCK BY POLICE VAN. 4/13/18-REASSIGNED FROM DUNCAN TO BRUYNS"
# It's not clear if these are comparable to cases with COA Description "Auto" and if so, why they're labeled
# differently. But since they are, we're leaving them in. 

stl <- stl %>% 
  filter(!grepl("auto",tolower(COA_Description))) %>% 
  filter(!grepl("Employment",summary_allegations)) %>% 
  filter(!grepl("other",tolower(summary_allegations)))

table(stl$summary_allegations)

stl %>% count(Injury)
# Under those labeled as Civil Rights:
# inadequate warning from police before tear gassing, excessive force, incarcerated under mistaken identity
# battery by police, wrong conviction, wrongful entry. All of these seem fine (6)
# One of them is "JOURNALIST CLAIMING POLICE INTERFERED IN COVERAGE OF FERGUSON RELATED PROTESTS" - is this misconduct? 
# leaving in for now

print("Total number of cases after filtering")
nrow(stl)

# Restrict to vars we care about and make sure final data looks fine
stl <- stl %>% select(calendar_year,
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
                      defendant_attorney,
                      voucher_date,
                      plaintiff_lawfirm)


# CHECKS
# Time period of closed date? 
summary(stl$closed_date) # Min is 5/2015, max is 6/2019

# time period of calendar year or incident year or filed year if closed date missing
summary(stl$calendar_year) # Min 2016, max 2019

# Perfect duplicates? 0
nrow(stl %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variables?
nrow(stl %>% group_by(docket_number) %>% filter(n()>1)) # 0

# What's filtered out? what's left that ambiguous?
# See above / see notes
table(stl$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(stl %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(stl %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(stl %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(stl %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))

# count cases
print("Total number of cases")
print(nrow(stl))

print("Total amount awarded")
summary(stl$amount_awarded)

write.csv(stl,paste0(out_data_path,"stlouis_edited.csv"), na = "",row.names = FALSE)


