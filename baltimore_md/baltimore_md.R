# This script takes the raw data on settlements received from Baltimore and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 1/27/2021


# This is data from sometime in 2015 (probably not complete for that year) to 2020-06-30


# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())
baltimore <- read_excel(paste0(raw_data_path,"20200715  Police Misconduct Report - Sort by Resolution Date.xlsx"))


## Renaming etc.

baltimore <- baltimore %>%
  filter(!grepl("TOTAL RESOLVED", Case)) %>%
  rename(matter_name = Case, 
         plaintiff_attorney = `Plaintiff's Attorney`,
         filed_date = `Date Filed`,
         closed_date = `Date Resolved`,
         summary_allegations = Claim,
         case_outcome = Outcome,
         amount_awarded = `Amount Paid`) %>%
  separate(Court, into = c("court", "docket_number"), sep = "[[:space:]]{2,}") %>%
  mutate(plaintiff_attorney = gsub("[[:space:]]{2,}", "; ", plaintiff_attorney), # Remove extra spaces
         # Fix filed date
         filed_date = as_date(as.numeric(filed_date),origin = '1899-12-30'),
         # Three of the filed_dates originally had a 4-digit year and so therefore weren't converted correctly
         filed_date = if_else(matter_name %in% c("Ruth Grant v. City of Baltimore", "Drew Hinton v. City of Baltimore"), ymd("2017-11-22"), filed_date),
         filed_date = if_else(matter_name == "Lauren Holmes v. BPD", ymd("2019-11-25"), filed_date),
         amount_awarded = as.numeric(amount_awarded),
         calendar_year = year(closed_date),
         filed_year = year(filed_date),
         city = "Baltimore",
         state = "MD",
         incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         collection= NA,
         total_incurred = NA,
         claim_number = NA,
         location = NA)

## Filter 
baltimore <- baltimore %>% filter(!is.na(amount_awarded), amount_awarded != 0)

# Note: some of these are judgments... 
# 71 settlements, 6 judgments for plaintiff, 1 jury verdict, 2 settlement & adverse ruling by court of appeals, 1 split decision?
table(baltimore$case_outcome)

baltimore %>% group_by(case_outcome) %>% summarise(n = n(), mean = mean(amount_awarded), median = median(amount_awarded))

# Will leave these in for now

# CHECKS
# Perfect dups? 0
nrow(baltimore %>% group_by_all() %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
table(baltimore$summary_allegations)

# time period of calendar year 
summary(baltimore$calendar_year) # Min 2015, max 2020, no missing

# Missing ness of variables
print(paste("There are",nrow(baltimore %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(baltimore %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(baltimore %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(baltimore %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(baltimore %>% filter(is.na(docket_number))),"rows missing docket number"))
print(paste("There are",nrow(baltimore %>% filter(is.na(claim_number))),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(baltimore))

print("Total amount awarded")
sum(baltimore$amount_awarded)

# Sanity checking with WSJ: they have $18.4M for 2015-2020, we do too!


write.csv(baltimore,paste0(out_data_path,"baltimore_edited.csv"), na = "",row.names = FALSE)


