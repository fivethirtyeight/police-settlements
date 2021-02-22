# This script takes in the uncleaned, newly updated (2/16/21) data from Richmond
# Likhita originally cleaned this manually - but because of the new data, I just wrote this code to clean the data straight from the source.

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 02/16/2021

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

richmond <- read_excel(paste0(raw_data_path,"Police Litigation Report for 538_Final_2-16-20.xlsx"))


## Rename/create variables
richmond <- richmond %>%
  filter(Court != "Grand Total") %>%
  rename(court = Court,
         matter_name = `File Name`,
         docket_number = `Court Case Number`,
         amount_awarded = `Sum of Settlement Amount`,
         incident_date = `Incident Date`,
         closed_date = `Final Order Date`) %>%
  mutate(summary_allegations = paste0(`Subject Matter:`, ": ", `Brief Description`), # Put the descriptive things together
         incident_date = as_date(as.numeric(incident_date), origin = '1899-12-30'), # Fix dates
         closed_date = as_date(as.numeric(closed_date), origin = '1899-12-30'),
         # It says in the Brief Description field that the "settlement agreement dated April 4, 2012":
         closed_date = if_else(matter_name == "Thomas, Shanelle LaFon v City of Richmond", lubridate::ymd("2012-04-04"), closed_date),
         court = ifelse(court == "N/A", NA, court), # Some NAs have a name
         docket_number = ifelse(docket_number %in% c("N/A", "(blank)"), NA, docket_number), # Some NAs have a name
         calendar_year = lubridate::year(closed_date),
         incident_year = lubridate::year(incident_date)) %>% 
  select(-`Subject Matter:`, -`Brief Description`, -`Count of File Name`, -`Sum of Ad Damnum`) %>% # Remove extraneous variables
  mutate(city = "Richmond", 
         state ="VA",
         case_number = NA,
         location = NA,
         total_incurred = NA,
         other_expenses = NA,
         filed_date = NA,
         filed_year = NA,
         case_outcome = NA,
         collection = NA,
         plaintiff = NA,
         plaintiff_attorney = NA)

## Filter

# Likhitha originally manually selected four cases
# Richmond later sent us 7 additional cases. 6 of those are relevant, so I included them via docket number 
# (one didn't have a docket number so I used matter name)

richmond <- richmond %>%
  filter(docket_number %in% c("CL13-823", "CL13-3007", "CL15-159-6", "CL10-5385") | # These four are selected by Likhitha
           docket_number %in% c("CL-14-3194-2", "3:18-CV-000-30", "3:17-CV-024-MHL", "3:17-CV-005-53", "CL15-4001") |
           matter_name == "Thomas, Shanelle LaFon v City of Richmond")


# CHECKS
# Time period of closed date? NA
summary(richmond$closed_date) # Missing for 4/10 cases
summary(richmond$incident_date) # 2009 - 2014

# Perfect duplicates? 0 duplicates
nrow(richmond %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(richmond %>% group_by(docket_number) %>% filter(n()>1))


# Missing ness of variables
print(paste("There are",nrow(richmond %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(richmond %>% filter(is.na(filed_date))),"rows missing filed date"))
print(paste("There are",nrow(richmond %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(richmond %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(richmond %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(richmond %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(richmond))

print("Total amount awarded")
sum(richmond$amount_awarded)


write.csv(richmond,paste0(out_data_path,"richmond_edited.csv"), na = "",row.names = FALSE)

