# This script takes and separates the cleaned data from Little Rock
# Likhita originally cleaned this manually - this script just imports the cleaned
# sheet and outputs it with the rest of the cleaned city data

# Original cleaning done by Likhitha Butchireddygari at FiveThirtyEight
# Original code written by Damini Sharma at The Marshall Project
# Original date 8/22/20

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

little_rock <- read_excel(paste0(raw_data_path,"little_rock_police_settlements_records.xlsx"),
                        sheet = "amounts",
                        col_types = "text")

little_rock_po <- read_excel(paste0(raw_data_path,"little_rock_police_settlements_records.xlsx"),
                                        sheet = "police_officers")

# Fix names, city name
little_rock <- little_rock %>% 
  mutate(city = "Little Rock", state="AR") %>% 
  mutate(amount_awarded = as.numeric(amount_awarded),
         calendar_year = as.numeric(calendar_year),
         incident_date =  as_date(as.numeric(incident_date),origin = '1899-12-30'),
         incident_year = year(incident_date),
         filed_date =  as_date(as.numeric(date_filed),origin = '1899-12-30'),
         filed_year = year(filed_date),
         closed_date = as.Date(closed_date),
         summary_allegations = NA)


# Fix city name in police officers
little_rock_po <- little_rock_po %>% 
  mutate(city = "Little Rock", state= "AR")


# Does merging on docket numebrs work? # Yes 
merged <- full_join(little_rock,little_rock_po)


# clean up vars
little_rock <- little_rock %>% select(calendar_year,
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

# Remove NA vars
little_rock_po <- little_rock_po %>%  select(-year,-closed_date)


# CHECKS
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(little_rock$incident_year) # Min 2008, max 2015
summary(little_rock$filed_year) # Min 2009, max 2017

# Perfect duplicates? 0 duplicates
nrow(little_rock %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(little_rock %>% group_by(docket_number) %>% filter(n()>1))



# Missing ness of variables
print(paste("There are",nrow(little_rock %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(little_rock %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(little_rock %>% filter(is.na(incident_year))),"rows missing incident year"))
print(paste("There are",nrow(little_rock %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(little_rock %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(little_rock %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(little_rock))

print("Total amount awarded")
sum(little_rock$amount_awarded)

write.csv(little_rock,paste0(out_data_path,"little_rock_edited.csv"), na = "",row.names = FALSE)
write.csv(little_rock_po,paste0(out_data_path,"little_rock_police_edited.csv"), na = "", row.names = FALSE)






