# This script takes the raw csv on settlements received from Boston and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Updated by Damini Sharma at The Marshall Project

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

library(tabulizer)

# Note: before reading in, data contained the header: 
# "From: City of Boston's Office of the Corporation Counsel"
# "Subject: Allegations of Civil Rights Violations, including Excessive Force, false arrest, wrongful conviction by the Boston Police Department Ending in Settlement"


# Extract tables from the pdf using tabulizer
out <- extract_tables(paste0(raw_data_path,"Police_Misconduct_Settlements_2010-2019_-_Sheet1.pdf"))

# Set column names
colnames_boston <- out[[1]][6,]
colnames(out[[1]]) <- colnames_boston
colnames(out[[2]]) <- colnames_boston
colnames(out[[3]]) <- colnames_boston

# Remove empty space at the top of first page
out[[1]] <- out[[1]][-(1:6),]

# Combine 
boston <- bind_rows(data.frame(out[[1]]), data.frame(out[[2]]), data.frame(out[[3]]))
colnames(boston)

# Tidy
boston <- boston %>%
  rename(matter_name = Plaintiff.s..v..Defendant.s.,
         docket_number = Docket.Number,
         court = Venue.of.Filing,
         plaintiff_attorney = Plaintiff.s.Attorney,
         closed_date_old = Date.of.Settlement,
         amount_awarded_old = Amount.of.Settlement) %>%
  mutate(closed_date = lubridate::mdy(as.character(closed_date_old)),
         amount_awarded2 = gsub(")", "", amount_awarded_old),
         amount_awarded3 = gsub("\\$", "", amount_awarded2),
         amount_awarded4 = gsub(",", "", amount_awarded3),
         amount_awarded = as.numeric(amount_awarded4)) %>%
  select(-amount_awarded_old, -amount_awarded2, -amount_awarded3, -amount_awarded4) %>%
  mutate(city = "Boston",
         state = "MA",
         # A couple of the dates didn't parse correctly, so for those I take the year from the poorly formatted date I called "_old"
         calendar_year = ifelse(!is.na(closed_date), year(closed_date), str_sub(closed_date_old,-4,-1)),
         calendar_year = as.numeric(calendar_year),
         summary_allegations = NA,
         plaintiff_name = NA,
         case_outcome = NA,
         filed_year = NA,
         filed_date = NA,
         incident_date = NA,
         location = NA,
         incident_year = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         claim_number = NA) %>% 
  select(-closed_date_old)
  

# CHECKS
# Time period of closed date? 
summary(boston$closed_date) # Min 1/2010, max 5/2019
# time period of calendar year or incident year or filed year if closed date missing
summary(boston$calendar_year) # Min 2010, max 2019

# Perfect duplicates? 0 duplicates
nrow(boston %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(boston %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# Filteration check - no filteration because of note provided on data received

# Missing ness of variables
print(paste("There are",nrow(boston %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(boston %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(boston %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(boston %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(boston %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(boston))

print("Total amount awarded")
sum(boston$amount_awarded)

# Check WSJ (they have $4m for 2015-2020, we have >$1m for 2015-2019)
# possible what we received was much more filtered than what they received
boston_2015_2020 <- boston %>% filter(closed_date>="2015-01-01")
sum(boston_2015_2020$amount_awarded)
sum(boston$amount_awarded)

write.csv(boston,paste0(out_data_path,"boston_edited.csv"), na = "",row.names = FALSE)


