# This script takes the raw data on settlements received from Springfield, MA and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Likhitha Butchireddygari at FiveThirtyEight
# Code was updated by Damini Sharma at The Marshall Project on 8/19

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

# Note - Likhitha did some manual cleaning across the three sheets of data we received, so what is being imported
# is the "edited" sheet
springfield_civil_rights_cases <- read_excel(paste0(raw_data_path,"springfield_civil_rights_cases.xlsx"),
                                             sheet = "edited")

springfield_civil_rights_cases <- springfield_civil_rights_cases %>% 
  mutate(closed_int = as.numeric(closed))

# There is one case where the city is appealing the court's 2019 verdict of $27m. Replacing closed year
# with 2019 and noting this in our notes
springfield_civil_rights_cases %>% filter(is.na(closed_int))
springfield_civil_rights_cases <- springfield_civil_rights_cases %>% mutate(closed_int = ifelse(closed=="Appeal",2019,closed_int))

#filter out cases that didn't result in monetary settlement
springfield_cleaned <- springfield_civil_rights_cases %>%
  filter(Paid > 0)

#adding and formatting columns
names(springfield_cleaned)[names(springfield_cleaned) == "closed_int"] <- "calendar_year"
names(springfield_cleaned)[names(springfield_cleaned) == "Paid"] <- "amount_awarded"
names(springfield_cleaned)[names(springfield_cleaned) == "Disposal of  case"] <- "case_outcome"
names(springfield_cleaned)[names(springfield_cleaned) == "CASE #"] <- "docket_number"
names(springfield_cleaned)[names(springfield_cleaned) == "CASE #"] <- "docket_number"
names(springfield_cleaned)[names(springfield_cleaned) == "MATTER #"] <- "claim_number"
names(springfield_cleaned)[names(springfield_cleaned) == "COURT"] <- "court"
names(springfield_cleaned)[names(springfield_cleaned) == "NAME"] <- "plaintiff_name"


springfield_cleaned$city <- 'Springfield'
springfield_cleaned$state <- 'MA'
springfield_cleaned$incident_date <- NA
springfield_cleaned$incident_year <- NA
springfield_cleaned$filed_date <- NA
springfield_cleaned$filed_year <- NA
springfield_cleaned$closed_date <- NA
springfield_cleaned$other_expenses <- NA
springfield_cleaned$collection <- NA
springfield_cleaned$total_incurred <- NA
springfield_cleaned$matter_name <- NA
springfield_cleaned$plaintiff_attorney <- NA
springfield_cleaned$location <- NA
springfield_cleaned$summary_allegations <- NA

springfield_cleaned <- springfield_cleaned[, c("calendar_year", "city", "state", "incident_date", "incident_year",
                               "filed_date", "filed_year", "closed_date", "amount_awarded",
                               "other_expenses", "collection", "total_incurred",
                               "case_outcome", "docket_number", "claim_number",
                               "court", "plaintiff_name", "matter_name",
                               "plaintiff_attorney", "location", "summary_allegations")]


# CHECKS
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(springfield_cleaned$calendar_year) # Min 206, max 2020

# Perfect duplicates? 0 duplicates
nrow(springfield_cleaned %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number or claim number
nrow(springfield_cleaned %>% group_by(docket_number) %>% filter(n()>1))
nrow(springfield_cleaned %>% group_by(claim_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# no info on allegations
table(springfield_cleaned$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(springfield_cleaned %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(springfield_cleaned %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(springfield_cleaned %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(springfield_cleaned %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))

# count cases
print("Total number of cases")
print(nrow(springfield_cleaned))

print("Total amount awarded")
sum(springfield_cleaned$amount_awarded)


write.csv(springfield_cleaned,paste0(out_data_path,"springfield_edited.csv"), na = "",row.names = FALSE)


