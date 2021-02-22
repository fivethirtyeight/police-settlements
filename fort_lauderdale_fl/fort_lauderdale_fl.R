# This script takes the raw data on settlements received from Fort Lauderdale and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Likhitha Butchireddygari at FiveThirtyEight
# Code was updated by Damini Sharma at The Marshall Project on 8/19

# Read in CSV
# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

fort_lauderdale_cases <- read_excel(paste0(raw_data_path,"fort_lauderdale_cases.xlsx"))

# Change up names for easy reference
names(fort_lauderdale_cases) <- gsub(names(fort_lauderdale_cases), pattern = " ", replacement = "_")

# Turn date vars into date type
# Closed date is tricky - R reads it in from excel as days from XXX date.
# referencing this, use different function to convert that variable 
# # https://stackoverflow.com/questions/51794461/r-numeric-to-date-gives-wrong-value
# spot checked against original excel to make sure this conversion is right
fort_lauderdale_cases <- fort_lauderdale_cases %>% 
  mutate_at(.funs =  ~as.Date(.,format = "%m/%d/%Y"), .vars = c("Event_Date",
                                                                "Claim__Date")) %>% 
  mutate(closed_date = as_date(as.numeric(`Closed_Date`),origin = '1899-12-30'))

# NAs introduced in closed date - these are marked as 0/0/0000 for closed date - missing
fort_lauderdale_cases %>% filter(is.na(closed_date))

# Change back to match rest of Likhitha's code up names for easy reference
names(fort_lauderdale_cases) <- gsub(names(fort_lauderdale_cases), pattern = "_", replacement = " ")

fort_lauderdale_cases <- fort_lauderdale_cases %>% mutate(`Closed Date` = `closed date`) %>% select(-`closed date`)

table(fort_lauderdale_cases$`Claim Type`)

fort_lauderdale_edited <- fort_lauderdale_cases

names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Closed Date"] <- "closed_date"

#adding and renaming columns!
fort_lauderdale_edited$calendar_year <- year(fort_lauderdale_edited$closed_date)
fort_lauderdale_edited$city <- 'Fort Lauderdale'
fort_lauderdale_edited$state <- 'FL'
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Event Date"] <- "incident_date"
fort_lauderdale_edited$incident_year <- year(fort_lauderdale_edited$incident_date)
fort_lauderdale_edited$filed_date <- NA
fort_lauderdale_edited$filed_year <- NA
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Indemnity Paid"] <- "amount_awarded"
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Expense Paid"] <- "other_expenses"
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Collection"] <- "collection"
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Total Incurred"] <- "total_incurred"
fort_lauderdale_edited$case_outcome <- NA
fort_lauderdale_edited$docket_number <- NA
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Claim Number"] <- "claim_number"
fort_lauderdale_edited$court <- NA
fort_lauderdale_edited$plaintiff_name <- NA
fort_lauderdale_edited$matter_name <- NA
fort_lauderdale_edited$plaintiff_attorney <- NA
fort_lauderdale_edited$location <- NA
names(fort_lauderdale_edited)[names(fort_lauderdale_edited) == "Claim Type"] <- "summary_allegations"

fort_lauderdale_edited <- fort_lauderdale_edited[, c("calendar_year", "city", "state","incident_date",
                                                     "incident_year", "filed_date", "filed_year",
                                                "closed_date", "amount_awarded",
                                               "other_expenses", "collection", "total_incurred",
                                               "case_outcome", "docket_number", "claim_number",
                                               "court", "plaintiff_name", "matter_name",
                                               "plaintiff_attorney", "location", "summary_allegations")]


fort_lauderdale_edited$summary_allegations[fort_lauderdale_edited$summary_allegations=="Excessive Force/False Arrest/Civil Rights"] <- "Excessive Force; False Arrest; Civil Rights"


# CHECKS
# Time period of closed date? NA
summary(fort_lauderdale_edited$closed_date) # 12/2011 - 9/2019, 4 NA's
# there are four cases where the closed date is NA
# Per our contact, these cases made settlement payments, but are open pending payment of legal fees. So
# we should leave them in.

# time period of calendar year or incident year or filed year if closed date missing
summary(fort_lauderdale_edited$calendar_year) # Min 2011, max 2019, 4 NA's

# Perfect duplicates? 0 duplicates
nrow(fort_lauderdale_edited %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on claim number
nrow(fort_lauderdale_edited %>% group_by(claim_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
table(fort_lauderdale_edited$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(fort_lauderdale_edited %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(fort_lauderdale_edited %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(fort_lauderdale_edited %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(fort_lauderdale_edited %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(fort_lauderdale_edited %>% filter(is.na(claim_number))),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(fort_lauderdale_edited))

print("Total amount awarded")
sum(fort_lauderdale_edited$amount_awarded)


write.csv(fort_lauderdale_edited,paste0(out_data_path,"fort_lauderdale_edited.csv"), na = "",row.names = FALSE)



