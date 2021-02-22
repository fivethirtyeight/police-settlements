# This script takes the raw csv on settlements received from Atlanta and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Code updated by Damini Sharma at The Marshall Project on 11/23/2020

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())


atl_claims <- read_excel(paste0(raw_data_path,"Atlanta_intermediate_revised.xlsx"), sheet = "claims")
atl_lawsuits <- read_excel(paste0(raw_data_path,"Atlanta_intermediate_revised.xlsx"), sheet = "lawsuits")
colnames(atl_lawsuits)

atl_claims1 <- atl_claims %>%
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(claim_or_lawsuit = "claim") %>% 
  rename(claim_number = `Claim Number`,
         plaintiff_name = Claimant,
         amount_awarded = `Amount of Settlement`,
         incident_date = `Date of Loss`,
         closed_date = `Date Settled`,
         summary_allegations = `Type of Claim`) %>%
  mutate(city = "Atlanta",
         state = "GA",
         denied_date = as.Date(as.numeric(`Date Denied`), origin = "1899-12-30"),
         calendar_year = year(closed_date),
         incident_year = year(incident_date),
         matter_name = NA,
         court = NA,
         docket_number = NA,
         filed_date = NA,
         filed_year = NA,
         location = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         plaintiff_attorney = NA)

atl_lawsuits <- atl_lawsuits %>%
  mutate(claim_or_lawsuit = "lawsuit") %>% 
  # Just basic renaming
  rename(matter_name = `Style of Case`,
         docket_number = `Case No.`,
         court = Courts) %>%
  # Remove empty row
  filter(!is.na(matter_name))

atl_lawsuits <- atl_lawsuits %>%   
  rename(case_outcome = `Case Disposition (Settlement, Dismissal, Judgement)`,
         summary_allegations = `Description of Case`,
         amount_awarded = `Settlement Amount`,
         amount_demanded = `Demand Amount`,
         Department = `Department - Client (APD, DWM, DPW, etc.)`) %>% # Making this line up with the other dataset
  dplyr::mutate(closed_date = as.Date(as.numeric(`Date of Settlement or Judgement`), origin = "1899-12-30"),
                # Fix one row that for some reason had "settled" in it as well as a date
                closed_date = if_else(is.na(closed_date), lubridate::mdy("10/17/14"), closed_date)) %>%
  mutate(city = "Atlanta",
         state = "GA",
         calendar_year = year(closed_date),
         plaintiff_name = NA,
         incident_date = NA,
         incident_year = NA,
         claim_number = NA,
         filed_date = NA,
         filed_year = NA,
         location = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         plaintiff_attorney = NA)



atlanta <- atl_claims1 %>% bind_rows(atl_lawsuits)

  
# Filter
table(atlanta$summary_allegations, atlanta$Department)

# Filter out departments that shouldn't have been included - so that we don't include "Other" from these departments
depts <- atlanta %>% group_by(Department) %>% count() %>% arrange(-n)

# Filter out corrections
atlanta <- atlanta %>% 
  filter(Department %in% c("Police","APD","Municipal Court","Courts",NA))

# Now let's look at summary allegations
allegations <- atlanta %>% group_by(Department, summary_allegations) %>% count() %>% arrange(-n)

# Filtering to the following categories; the other ones seem to just be city expenses
atlanta <- atlanta %>% 
  filter(summary_allegations %in% c("Civil Rights Violation", "Civil Rights Violations", 
                                    "Other", "Other - Miscellaneous", 
                                    "Other - Motor Vehicle Incident", # Included this last one because it said "incident" not "accident"...
                                    "Other - Illegal Towing (Negligence)", 
                                    "Other - Wrongful Demolition (Negligence)", 
                                    "Sidewalk Defect (Negligence)", # Are we including Negligence??
                                    "Police Shooting", 
                                    "Wrongful arrest", "Wrongful Arrest", 
                                    "Wrongful Demolition",
                                    "Regulatory Licensing/ Due Process",NA))

# This is now quite a short list...
allegations <- atlanta %>% group_by(Department, summary_allegations) %>% count() %>% arrange(-n)

# Combine dept name into summary allegations
atlanta <- atlanta %>% 
  mutate(summary_allegations = paste0("Department: ",Department,"; Summary Allegations: ",summary_allegations))

# Fix names
colnames(atlanta)
atlanta <- atlanta %>%
  select(-Department, -`Date Denied`,  -Bureau, -No., -`...7`, -`Inv`, -`Days to Decision`, -`CC District`, -`Judge`, -`Opposing Counsel`,-`Staff`, -`Amount of Demand`, -`Fund Acct`, -Qtr, -`Date of Settlement or Judgement`, -`Case Description`, -`Fund Account`, -Quarter)


# CHECKS
# Time period of closed date? 
summary(atlanta$closed_date) # 7/2014 - 5/2020
# time period of calendar year or incident year or filed year if closed date missing
summary(atlanta$calendar_year) # Min 2014, max 2020

# Perfect duplicates? 
# 6 perfect dups - these are all claims that are repeated in the different source files
# might be because some of the source files have overlapping time frames 
# Theres FY19 Q2-Q4 Settled Claims.xlsx and FY2019 Claims Settlement Report (7-1-18 thru 3-31-19).xlsx
# And FY19 Q2-Q4 Litigation Cases Settled.xlsx and FY2019 Litigation Settlement Report (7-1-18 thru 3-31-19).xlsx
# There's overlap in the lawsuits as well, but not in our final dataset after filtering etc
# We'll use distinct to only keep one copy of the duplicated claims
dups <- atlanta %>% group_by_all() %>% filter(n()>1)

# Check all claims - any dups by claim number? only the same 3 cases repeated 3 times as above
claims <-  atlanta %>% filter(claim_or_lawsuit =="claim")
dups_claim <- claims %>% group_by(claim_number) %>% filter(n()>1)

# Check all lawsuits - any dups by lawsuits? None
lawsuits <- atlanta %>% filter(claim_or_lawsuit == "lawsuit")
dups_matter <- lawsuits %>% group_by(matter_name) %>% filter(n()>1)

atlanta <- atlanta %>% distinct()

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(atlanta$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(atlanta %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(atlanta %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(atlanta %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(atlanta %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))

# count cases
print("Total number of cases")
print(nrow(atlanta))

print("Total amount awarded")
sum(atlanta$amount_awarded)

write.csv(atlanta,paste0(out_data_path,"atlanta_edited.csv"), na = "",row.names = FALSE)
