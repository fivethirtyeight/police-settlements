# This script takes and separates the raw and cleaned data from Baton Rouge
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

br <- read_excel(paste0(raw_data_path,"baton_rouge_police_settlements_records.xlsx"),
                   sheet = "amounts",
                   col_types = "text")


br_po <- read_excel(paste0(raw_data_path,"baton_rouge_police_settlements_records.xlsx"),
                      sheet = "police_officers",
                      col_types = "text")

# Fix names, city name
br <- br %>% 
  mutate(city = "Baton Rouge", state="LA") %>% 
  mutate(amount_awarded = as.numeric(amount_awarded),
         calendar_year = as.numeric(calendar_year),
         filed_date  = NA,
         filed_year = NA,
         incident_date_clean =  as_date(as.numeric(incident_date),origin = '1899-12-30'),
         closed_date_clean =  as_date(as.numeric(closed_date),origin = '1899-12-30'))

# Check dates conversion
nrow(br %>% filter(is.na(incident_date_clean)))
nrow(br %>% filter(is.na(closed_date_clean)))
nrow(br %>% filter(is.na(calendar_year)))
# Incident date missing for 3 observations, Closed date missing for 13 cases, calendar year not missing

br <- br %>% 
  mutate(closed_date = closed_date_clean,
         incident_date = incident_date_clean,
         incident_year = year(incident_date))


br <- br %>% 
  mutate(summary_allegations = NA,
         court = NA)


# Fix city name in police officers
br_po <- br_po %>% 
  mutate(city = "Baton Rouge", state= "LA") %>% 
  rename(calendar_year = year) %>% 
  mutate(calendar_year = as.numeric(calendar_year),
         closed_date_clean =  as_date(as.numeric(closed_date),origin = '1899-12-30'),
         closed_date = closed_date_clean) %>% 
  select(-closed_date_clean)

# Does merging on docket numebrs work? # Yes 
merged <- full_join(br,br_po)

# clean up vars
br <- br %>% select(calendar_year,
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
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(br$calendar_year) # Min 2010, max 2019

# Perfect duplicates? 0 duplicates
nrow(br %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(br %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(br$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(br %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(br %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(br %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(br %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(br %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(br))

print("Total amount awarded")
sum(br$amount_awarded)


write.csv(br,paste0(out_data_path,"baton_rouge_edited.csv"), na = "",row.names = FALSE)
write.csv(br_po,paste0(out_data_path,"baton_rouge_police_edited.csv"), na = "", row.names = FALSE)










