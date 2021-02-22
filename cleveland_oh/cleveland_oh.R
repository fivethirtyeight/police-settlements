# This script takes the raw csv on settlements received from Cleveland and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Damini Sharma at The Marshall Project
# Code updated by Damini Sharma at The Marshall Project on 11/25/20

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

cleveland <- read_excel(paste0(raw_data_path,"clevelandstart.xlsx"))

cleveland <- cleveland %>%
  rename(calendar_year = Year,
         amount_awarded = Amount,
         matter_name = `Case Name`,
         plaintiff_name = Payee,
         docket_number = `Case #`) %>%
  mutate(city = "Cleveland",
         state = "OH",
         incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         claim_number = NA,
         court = NA,
         plaintiff_attorney = NA,
         location = NA,
         summary_allegations = NA,
         closed_date = NA,
         filed_date = NA,
         filed_year = NA,
         case_outcome = ...6) # Extraneous info about the case


# Tamir Rice case - in the dataset given to us, there are 5 payees in 2016 and 5 in 2017 that are all assigned
# amount of $6m. However we know that $6m was the total amount, split as $3m and $3m in each year. They gave us
# 1st installment payments in 2016 - we're going to replace the $6m in 2016 with the first installment figures.
# in 2017, we're going to collapse all the payees into 1 row for $3m - we don't want to impute 2nd installment 
# numbers for the second year
cleveland <- cleveland %>% 
  mutate(amount_awarded = case_when(
    ...6 == "1st installment 125000" & calendar_year == 2016 ~ 125000,
    ...6 == "1st installment 43673.18" & calendar_year == 2016 ~ 43673.18,
    ...6 == "1st installment 81326.82" & calendar_year == 2016 ~ 81326.82,
    ...6 == "1st installment 2481326.82" & calendar_year == 2016 ~ 2481326.82,
    ...6 == "1st installment 268673.18" & calendar_year == 2016 ~ 268673.18,
    TRUE ~ amount_awarded
  )) %>% 
  mutate(case_outcome = ...6) 

# filter out 2017 Tamir Rice payments so we can collapse and bring them back on
tamir <- cleveland %>% filter(docket_number=="1:14-CV-319" & calendar_year == 2017)
cleveland_no_tamir <- cleveland %>% filter(docket_number!="1:14-CV-319" | calendar_year !=2017)

tamir <- tamir %>% 
  mutate(case_outcome = ...6) %>% 
  select(-...6,-...7,-...8) %>% 
  mutate(amount_awarded = 3000000) 

plaintiff_concat = paste(tamir$plaintiff_name, collapse = ",")
tamir <- tamir %>% mutate(plaintiff_name = plaintiff_concat) %>% distinct()

cleveland <- bind_rows(cleveland_no_tamir,tamir) 

# One other case where payment info was written as "87500.00/82090.61"
# unclear if they should be added together, so we're gonna save the second
# number as "other expenses" to be conservative
cleveland <- cleveland %>% 
  mutate(other_expenses = 
           ifelse(...6 == 82090.61, 82090.61, other_expenses),
                case_outcome = ifelse(...6==82090.61,"Amount paid listed as 87500.00/82090.61 originally",case_outcome)) %>% 
  select(-...6,-...7,-...8)  
  
cleveland <- cleveland %>% select(calendar_year,
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

# There's no variable to identify type of misconduct, so we're going to keep everything, and note that we are likely
# over including

# CHECKS
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(cleveland$calendar_year) # Min 2010, max 2020

# Perfect duplicates? 0 duplicates
nrow(cleveland %>% group_by_all() %>% filter(n()>1))

# 6 duplicates on year, docket number, payee and matter name - 0 dups if we account for amount awarded so it doesnt seem like 
# a data entry issue, but multiple payments
dups <- cleveland %>% group_by(docket_number,plaintiff_name,matter_name,calendar_year) %>% filter(n()>1)
dups <- cleveland %>% group_by(docket_number,plaintiff_name,matter_name,calendar_year,amount_awarded) %>% filter(n()>1)

# What's filtered out? what's left that ambiguous?
# no summary of allegations var 

# Missing ness of variables
print(paste("There are",nrow(cleveland %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(cleveland %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(cleveland %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(cleveland %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(cleveland %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(cleveland))

print("Total amount awarded")
sum(cleveland$amount_awarded)


write.csv(cleveland,paste0(out_data_path,"cleveland_edited.csv"), na = "",row.names = FALSE)



