# This script takes the raw csv on settlements received from Orlando and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 9/3/2020

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())
# Note: before reading in, data contained the note "Police professional 01/01/2010 - 7/23/2020" in the top left

orlando <- read_excel(paste0(raw_data_path,"Orlando data updated.xlsx"), skip = 1)

# Fix date filed, rename variables, and generate the rest of the variables.

orlando <- orlando %>%
  mutate(`Date Lawsuit filed` = as.Date(as.numeric(`Date Lawsuit filed`), origin = "1899-12-30")) %>%
  rename(plaintiff_name = `Name of Plaintiff`,
         docket_number = `Docket Number`,
         incident_date = `Date of Incident`,
         location = `Location of Incident`, # This is just "Orlando, FL" for all of them
         filed_date = `Date Lawsuit filed`,
         closed_date = `Date Lawsuit Resolved`,
         amount_awarded = `Settlement Amount`) %>%
  mutate(summary_allegations = paste0(`Type of misconduct`, ": ", `Summary of allegations`), # Do we want one or the other?
         city = "Orlando",
         state = "FL",
         calendar_year = year(closed_date),
         filed_year = year(filed_date),
         incident_year = year(incident_date),
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         claim_number = NA,
         court = NA,
         plaintiff_attorney = NA,
         matter_name = NA,
         case_outcome = NA)

orlando <- orlando %>% select(calendar_year,
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
summary(orlando$closed_date) # 2 cases that closed after 2020. Could be more because closed date missing for 14 cases
orlando %>% filter(closed_date>"2019-12-31")
summary(orlando$filed_date) # 2011 to 2019
summary(orlando$incident_date) #2010 to 2018. Its possible that they filtered on incident date, not closed date?

# Perfect duplicates? 0 duplicates
nrow(orlando %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on plaintiff name
nrow(orlando %>% group_by(plaintiff_name) %>% filter(n()>1))

nrow(orlando %>% group_by(docket_number) %>% filter(n()>1))
dups <- orlando %>% group_by(docket_number) %>% filter(n()>1)
# 5 duplicates because docket number missing
# 3 duplicates because docket number is "suit not filed"
# 2 duplicates - same docket number, different amounts and plaintiffs

# What's filtered out? what's left that ambiguous?
table(orlando$summary_allegations)
# Doesn't look like it needs to be filtered at all -- type of misconduct is just "Excessive force" and "False arrest"

# Missing ness of variables
print(paste("There are",nrow(orlando %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(orlando %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(orlando %>% filter(is.na(filed_year))),"rows missing filed year"))
print(paste("There are",nrow(orlando %>% filter(is.na(incident_year))),"rows missing incident year"))
print(paste("There are",nrow(orlando %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(orlando %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(orlando %>% filter(is.na(docket_number))),"rows missing docket number"))
print(paste("There are",nrow(orlando %>% filter(is.na(plaintiff_name))),"rows missing plaintiff name"))

# count cases
print("Total number of cases")
print(nrow(orlando))

print("Total amount awarded")
sum(orlando$amount_awarded)


write.csv(orlando,paste0(out_data_path,"orlando_edited.csv"), na = "",row.names = FALSE)

