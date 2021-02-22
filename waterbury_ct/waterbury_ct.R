# This script takes the raw csv on settlements received from Waterbury and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Code updated by Damini Sharma at The Marshall Project

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

waterbury <- read_excel(paste0(raw_data_path,"7-31-20 FOI Responsive Docs.xls"))

waterbury <- waterbury %>%
  # Fix amount
  dplyr::mutate(amount = gsub(",", "", DISPOSITION),
                amount = gsub("(.*\\$)(.*)", "\\2", amount),
                amount = str_extract(amount, "\\d+\\.*\\d*"),
                amount = gsub("\\.\\.", "", amount),
                amount = as.numeric(amount),
                amount = ifelse(PLAINTIFF == "Ruiz, Manolo; Ruiz, Elniriz", 33750, amount), # This gives two plaintiffs and says they received diff amounts, this is the sum
                amount = ifelse(PLAINTIFF == "Jones, Ann & Mark", 150000, amount), # This gives two plaintiffs and says they received diff amounts, this is the sum
                filed_date = as.Date(as.numeric(`Date Filed with Court`), origin = "1899-12-30")) %>% 
  separate(`Judge/Dkt #`, into = c("Judge", "docket_number"), sep = "\\s{2,}") %>%
  rename(plaintiff_name = PLAINTIFF,
         defendant = `NAMED DEFENDANT`, # separate defendant column
         case_outcome = DISPOSITION,
         closed_date = `Disposition Date`,
         summary_allegations = `NATURE OF CASE`,
         amount_awarded = amount) %>%
  select(-Judge, -`Date Filed with Court`) %>%
  mutate(city = "Waterbury",
         state = "CT",
         calendar_year = year(closed_date),
         filed_year = year(filed_date),
         matter_name = NA,
         court = NA,
         incident_date = NA,
         location = NA,
         incident_year = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         claim_number = NA)


## Filter
table(waterbury$summary_allegations)

# Filter out just the ones labeled "MVA" only and "MVA - UM/UIM" (uninsured/underinsured motorist). Leaving in MVA-Personal injury, MVA/Pursuit, and other things.
waterbury <- waterbury %>%
  tidylog::filter(!summary_allegations %in% c("MVA", "MVA- UM/UIM"))
  

# CHECKS
# Time period of closed date? 
summary(waterbury$closed_date) # 2011 to 2019 

# time period of calendar year or incident year or filed year if closed date missing
summary(waterbury$calendar_year) # Min 2011, max 2019

# Perfect duplicates? 0 duplicates
nrow(waterbury %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(waterbury %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(waterbury$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(waterbury %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(waterbury %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(waterbury %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(waterbury %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(waterbury %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(waterbury))

print("Total amount awarded")
sum(waterbury$amount_awarded)

write.csv(waterbury,paste0(out_data_path,"waterbury_edited.csv"), na = "",row.names = FALSE)


