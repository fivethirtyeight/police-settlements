# This script takes and separates the raw and cleaned data from New Orleans
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

NOLA <- read_excel(paste0(raw_data_path,"new_orleans_police_settlements_records.xlsx"),
                     sheet = "amounts",
                     col_types = "text")


NOLA_PO <- read_excel(paste0(raw_data_path,"new_orleans_police_settlements_records.xlsx"),
                   sheet = "police_officers",
                   col_types = "text")

# Fix names, city name
NOLA <- NOLA %>% 
  mutate(city = "New Orleans", state="LA") %>% 
  mutate(amount_awarded = as.numeric(amount_awarded),
         calendar_year = as.numeric(calendar_year),
         filed_year  = as.numeric(date_filed),
         filed_date = NA,
         incident_date_clean =  as_date(as.numeric(incident_date),origin = '1899-12-30'),
         closed_date_clean =  as_date(as.numeric(closed_date),origin = '1899-12-30'))


# Check dates conversion
NOLA %>% filter(is.na(incident_date_clean))
NOLA %>% filter(is.na(closed_date_clean))
# Incident date missing for 15 observations, Closed date missing for 1 case

# one date gets messed up to 1905-07-04 because its entered as just 2012.
# turning to NA, already have it as calendary year
NOLA <- NOLA %>% 
  mutate(closed_date_clean = ifelse(closed_date=="2012.0",NA,as.character(closed_date_clean))) %>% 
  mutate(closed_date_clean = as.Date(closed_date_clean))

NOLA <- NOLA %>% 
  mutate(closed_date = closed_date_clean,
         incident_date = incident_date_clean,
         incident_year = year(incident_date))

# Fix city name in police officers
NOLA_PO <- NOLA_PO %>% 
  rename(calendar_year = Year,
         docket_number = `Suit No.`) %>% 
  mutate(city = "New Orleans", state="LA",
         calendar_year = as.numeric(calendar_year),
         closed_date = as_date(as.numeric(Closed_Date),origin = '1899-12-30'),
         closed_date = ifelse(Closed_Date=="2012.0",NA,as.character(closed_date)),
         closed_date = as.Date(closed_date)) %>% 
  select(-City,-Closed_Date)

# Does merging on docket numebrs work? # Yes 
merged <- left_join(NOLA,NOLA_PO,by="docket_number")

# clean up vars
NOLA <- NOLA %>% select(calendar_year,
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
summary(NOLA$closed_date) # min is 2010, max is 2017, 2 NAs (for one we just have "2012" for closed date)
summary(NOLA$calendar_year) # Min 2010, max 2017, 1 NA

# Perfect duplicates? 0 duplicates
nrow(NOLA %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(NOLA %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(NOLA$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(NOLA %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(NOLA %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(NOLA %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(NOLA %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(NOLA %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(NOLA))

print("Total amount awarded")
sum(NOLA$amount_awarded)

write.csv(NOLA,paste0(out_data_path,"new_orleans_edited.csv"), na = "",row.names = FALSE)
write.csv(NOLA_PO,paste0(out_data_path,"new_orleans_police_edited.csv"), na = "",row.names = FALSE)











