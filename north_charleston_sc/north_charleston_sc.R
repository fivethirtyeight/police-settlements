# This script takes the raw csv on settlements received from North Charleston and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Code updated by Damini Sharma at The Marshall Project

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

## Read in files
north_charleston <- read_excel(paste0(raw_data_path,"north_charleston_intermediate.xlsx"))
colnames(north_charleston)

# Rename
north_charleston <- north_charleston %>%
  rename(claim_number = `CLAIM IDENTIFICATION`,
         summary_allegations = `CAUSE OF CLAIM`,
         incident_date = `Date of incident`,
         filed_date = `Date reported`,
         closed_date = `Date Closed`,
         plaintiff_name = Claimant,
         plaintiff_attorney = `Claimant attorney`,
         docket_number = `Civil action number`, # Is this right? # i think so
         defendant_name = `Defendant`,
         defendant_attorney = `Defendant Attorney`,
         amount_awarded = `Paid losses`,
         other_expenses = `Paid expenses`
  ) %>%
  mutate(total_incurred = amount_awarded + other_expenses,
         city = "North Charleston",
         state = "SC",
         calendar_year = year(closed_date),
         filed_year = year(filed_date),
         incident_year = year(incident_date),
         location = NA,
         collection = NA,
         court = NA,
         matter_name = NA,
         case_outcome = NA) 



north_charleston <- north_charleston %>% select(calendar_year,
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
                                    defendant_name,
                                    defendant_attorney)



# CHECKS
# Time period of closed date? NA
summary(north_charleston$closed_date) # 1/2010 - 12/2019
# time period of calendar year or incident year or filed year if closed date missing
summary(north_charleston$calendar_year) # Min 2010, max 2019

# Perfect dups? None
nrow(north_charleston %>% group_by_all() %>% filter(n()>1))

# Duplicates by claim number? 66
nrow(north_charleston %>% group_by(claim_number) %>% filter(n()>1))

# Duplicates by claim number and plaintiff name? 20
nrow(north_charleston %>% group_by(claim_number,plaintiff_name) %>% filter(n()>1))
dups <- north_charleston %>% group_by(claim_number,plaintiff_name) %>% filter(n()>1)

# In all of these 20 duplicates, there's something that is different across the rows - such as
# the docket number or the defendant name. It's not clear why for the same plaintiff/case these would be different
# but in all cases the amount awarded / expenses are 0 or NA for them. so let's filter out and see if we still have duplicates

# Just filtering out those with missing or 0 payment amounts.
north_charleston <- north_charleston %>%
  tidylog::filter(!is.na(total_incurred), total_incurred != 0) 

# Duplicates by claim number? 33
nrow(north_charleston %>% group_by(claim_number) %>% filter(n()>1))

# Duplicates by claim number and plaintiff name? 12 (some of these have different docket numbers / different defendants)
nrow(north_charleston %>% group_by(claim_number,plaintiff_name) %>% filter(n()>1))

# Duplicates by claim number, plaintiff name, and docket number? 6 (different defendants, attorneys or amounts)
nrow(north_charleston %>% group_by(claim_number,plaintiff_name,docket_number) %>% filter(n()>1))
dups <- north_charleston %>% group_by(claim_number,plaintiff_name,docket_number) %>% filter(n()>1)

# What's filtered out? what's left that ambiguous?
# Filter
table(north_charleston$summary_allegations)
# This has already been manually filtered when it was copied/pasted from the pdf by Damini, and indeed all the allegations seem relevant

# Missing ness of variables
print(paste("There are",nrow(north_charleston %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(north_charleston %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(north_charleston %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(north_charleston %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(north_charleston %>% filter(total_incurred==0)),"rows with total incurred = 0"))
print(paste("There are",nrow(north_charleston %>% filter(is.na(docket_number))),"rows missing docket number"))

north_charleston %>% filter(is.na(docket_number))

# count cases
print("Total number of cases")
print(nrow(north_charleston))

print("Total amount awarded")
sum(north_charleston$amount_awarded)


write.csv(north_charleston,paste0(out_data_path,"north_charleston_edited.csv"), na = "",row.names = FALSE)


