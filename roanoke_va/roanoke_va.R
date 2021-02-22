# This script takes and separates the cleaned data from Roanoke
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

roanoke <- read_excel(paste0(raw_data_path,"roanoke_police_settlements_records.xlsx"),
                       sheet = "amounts",
                       col_types = "text")

roanoke_po <- read_excel(paste0(raw_data_path,"roanoke_police_settlements_records.xlsx"),
                          sheet = "police_officers")

# Fix names, city name
# Only unique identifier is the plaintiff name or year
roanoke <- roanoke %>% 
  rename(summary_allegations = `summary_allegations (;)`) %>% 
  mutate(city = "Roanoke", state= "VA") %>% 
  mutate(amount_awarded = as.numeric(amount_awarded),
         filed_date = NA,
         filed_year = NA,
         closed_date = NA,
         incident_date =  as_date(as.numeric(incident_date),origin = '1899-12-30'),
         incident_year = year(incident_date),
         calendar_year = as.numeric(calendar_year))


# Fix city name in police officers - one of the cases seems to have closed date which 
# matches incident name in the main file?
roanoke_po <- roanoke_po %>% 
  mutate(city = "Roanoke", state="VA") %>% 
  rename(calendar_year = year) %>% 
  mutate(closed_date = NA)


# Does merging on docket numebrs work? # Yes 
merged <- full_join(roanoke_po,roanoke)

# clean up vars
roanoke <- roanoke %>% select(calendar_year,
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
roanoke_po <- roanoke_po %>%  select(-closed_date)

# CHECKS
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(roanoke$calendar_year) # 2010 - 2011 

# Perfect duplicates? 0 duplicates
nrow(roanoke %>% group_by_all() %>% filter(n()>1))

# 2 cases, no clear identifying variable

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(roanoke$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(roanoke %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(roanoke %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(roanoke %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(roanoke %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(roanoke %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(roanoke))

print("Total amount awarded")
sum(roanoke$amount_awarded)

write.csv(roanoke,paste0(out_data_path,"roanoke_edited.csv"), na = "", row.names = FALSE)
write.csv(roanoke_po,paste0(out_data_path,"roanoke_police_edited.csv"), na = "",row.names = FALSE)



