# This script takes and separates the raw and cleaned data from Philadelphia
# Likhita originally cleaned this manually - this script just imports the cleaned
# sheet and outputs it with the rest of the cleaned city data

# Original cleaning done by Likhitha Butchireddygari at FiveThirtyEight
# Original code written by Damini Sharma at The Marshall Project
# Original date 8/23/20

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

philly <- read_excel(paste0(raw_data_path,"philadelphia_police_settlements_records.xlsx"),
                        sheet = "edited",
                        col_types = "text")


# Fix names, city name
philly <- philly %>% 
  rename(calendar_year = calender_year) %>% 
  mutate(city = "Philadelphia", state= "PA") %>% 
  mutate(amount_awarded = as.numeric(amount_awarded),
         filed_date = NA,
         filed_year = NA,
         calendar_year = as.numeric(calendar_year),
         incident_date_clean =  as_date(as.numeric(incident_date),origin = '1899-12-30'),
         closed_date_clean =  as_date(as.numeric(closed_date),origin = '1899-12-30'))

# 22 observations missing incident date, 1 observation missing closed date

# clean up vars
philly <- philly %>% 
  mutate(closed_date = closed_date_clean,
         incident_date = incident_date_clean,
         incident_year = year(incident_date)) %>% 
  select(calendar_year,
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
# Time period of closed date? 
summary(philly$closed_date) # min is 2009, max is 2019, 1 NA
# time period of calendar year or incident year or filed year if closed date missing
summary(philly$calendar_year) # Min 2009, max 2019

# Perfect duplicates? 0 duplicates
nrow(philly %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - 18 duplicates
nrow(philly %>% group_by(docket_number) %>% filter(n()>1))
docket_no_dups <- philly %>% group_by(docket_number) %>% filter(n()>1)

# 10 of these are duplicated because docket_no is NA. Plaintiff names are different, but this might not be enough to 
# distinguish cases

# for the remaining 8, 2 duplicates have the same plaintiff name and incident date, so likely its a case where money
# is paid out on separate dates. for the other 6, the plaintiff names/incident dates arent the same. 

# filtering - none, no info received

# Missing ness of variables
print(paste("There are",nrow(philly %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(philly %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(philly %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(philly %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(philly %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(philly))

print("Total amount awarded")
sum(philly$amount_awarded)


# Check WSJ (they have $64m for 2015-2020, we have ~$60m for 2015-2019)
philly_2015_2020 <- philly %>% filter(closed_date>="2015-01-01")
sum(philly_2015_2020$amount_awarded)


write.csv(philly,paste0(out_data_path,"philly_edited.csv"), na = "",row.names = FALSE)


