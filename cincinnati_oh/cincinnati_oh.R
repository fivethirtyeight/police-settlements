# This script takes the raw csv on settlements received from Cincinnati and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 9/2/2020
# Code updated by Damini Sharma at The Marshall Project on 11/16/20
# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

cincinnati <- read_excel(paste0(raw_data_path,"00318638.XLSX"),
                 #col_types = "text", 
                 skip = 2)

# Checking on the case which has a date with a typo, Case No 1:19cv366:
# https://www.pacermonitor.com/public/case/28221507/Tunstill_et_al_v_City_Of_Cincinnati
# Correct Date Closed is 04/15/2020

cincinnati <- cincinnati %>%
  filter(!is.na(`Case No`)) %>% # drop last row which is empty
  mutate(`Judgment Paid` = ifelse(is.na(`Judgment Paid`), `...7`, `Judgment Paid`), # Shifting the content of columns over to the right column
         `Case Type` = ifelse(is.na(`Case Type`), `...10`, `Case Type`),
         `Date Closed` = as.Date(as.numeric(`Date Closed`), origin = "1899-12-30"),
         `Date Closed` = if_else(`Case No` == "1:19cv366", mdy("04/15/2020"), `Date Closed`)) %>%
  select(-(starts_with("..."))) # Drop incorrect empty columns

# Rename columns to match naming scheme,
# Generate year, city and state variables
# Generate NAs for other columns

cincinnati <- cincinnati %>%
  rename(amount_awarded = `Judgment Paid`,
         docket_number = `Case No`,
         closed_date = `Date Closed`,
         filed_date = `Date Filed`,
         matter_name = `Case Caption`,
         summary_allegations = `Case Type`,
         case_outcome = `Outcome`) %>%
  mutate(calendar_year = year(closed_date),
         filed_year = year(filed_date),
         city = "Cincinnati",
         state = "OH",
         incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         claim_number = NA,
         court = NA,
         plaintiff_name = NA,
         plaintiff_attorney = NA,
         location = NA)

# Filter
# From Amelia's correspondence with Cincinnati: 'litigation" is a catch-all category they use when there isn't a specific sub-category;
# so i think we should use it; we'll get a few things we don't want, but mostly it's things we do want'
table(cincinnati$summary_allegations)

# Based on filtering rules, filtering out employment, leaving in everything else
cincinnati <- cincinnati %>% 
  filter(summary_allegations!="Employment")


# CHECKS
# Time period of closed date? 6/2010 - 7/2020
summary(cincinnati$closed_date)
# time period of calendar year or incident year or filed year if closed date missing
summary(cincinnati$calendar_year) # Min 2010, max 2020

# Perfect duplicates? 0 duplicates
nrow(cincinnati %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(cincinnati %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
table(cincinnati$summary_allegations)
# Declaratory Judgment, Employment, Negligence, Personal Injury, and Presuit ambiguous - leaving them in

# Missing ness of variables
print(paste("There are",nrow(cincinnati %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(cincinnati %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(cincinnati %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(cincinnati %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(cincinnati %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(cincinnati))

print("Total amount awarded")
sum(cincinnati$amount_awarded)

write.csv(cincinnati,paste0(out_data_path,"cincinnati_edited.csv"), na = "",row.names = FALSE)


