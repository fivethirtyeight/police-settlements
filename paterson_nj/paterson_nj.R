# This script takes the raw data on settlements received from Paterson and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Likhitha Butchireddygari at FiveThirtyEight
# Code was updated by Damini Sharma at The Marshall Project on 8/28

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

paterson_litigation <- read_excel(paste0(raw_data_path,"paterson_litigation.xlsx"),
                                  sheet = "edited",
                                  col_types = "text")

paterson_police_dept_claims <- read_excel(paste0(raw_data_path,"paterson_police_dept_claims.xlsx"),
                                  sheet = "edited",
                                  col_types = "text")


# Change up names for easy reference
names(paterson_litigation) <- gsub(names(paterson_litigation), pattern = " ", replacement = "_")
names(paterson_police_dept_claims) <- gsub(names(paterson_police_dept_claims), pattern = " ", replacement = "_")

# Clean up dates in both datasets
paterson_litigation <- paterson_litigation %>% 
  mutate_at(.funs = ~as.numeric(.), .vars = c("Total_Reserve","Total_Incurred", "Total_Paid_to_Date")) %>% 
  mutate_at(.funs = list(date = ~as_date(as.numeric(.),origin='1899-12-30')), 
            .vars = c("Date_of_Loss",
                      "Reported_Date",
                      "Trial_Date"))

paterson_litigation <- paterson_litigation %>% 
  mutate(Date_of_Loss = Date_of_Loss_date,
         Reported_Date = Reported_Date_date,
         Trial_Date = Trial_Date_date)


paterson_police_dept_claims <- paterson_police_dept_claims %>% 
  mutate_at(.funs = ~as.numeric(.), .vars = c("Total_Reserve","Total_Incurred", "Total_Paid_to_Date")) %>% 
  mutate_at(.funs = list(date = ~as_date(as.numeric(.),origin='1899-12-30')), 
            .vars = c("DOL",
                      "date_received",
                      "date_closed",
                      "Date_Reported"))

paterson_police_dept_claims <- paterson_police_dept_claims %>% 
  mutate(DOL = DOL_date,
         Date_Reported = Date_Reported_date,
         date_closed = date_closed_date,
         date_received = date_received_date)


# Change back to match rest code
names(paterson_litigation) <- gsub(names(paterson_litigation), pattern = "_", replacement = " ")
names(paterson_police_dept_claims) <- gsub(names(paterson_police_dept_claims), pattern = "_", replacement = " ")

paterson_police_dept_claims <- paterson_police_dept_claims %>%
  rename(date_closed = `date closed`,
         date_received = `date received`,
         Location_of_accident = `Location of accident`)


# For both dataset (all and lawsuits only), filter out auto liability cases and rename variables
colnames(paterson_litigation)
paterson_litigation <- paterson_litigation %>% 
  rename(claim_number = `Claim Number`,
         claimant = `Claimant Name`,
         incident_date = `Date of Loss`,
         defendant = Client, # Is this right?
         liability_type = `line of business`,
         reported_date = `Reported Date`,
         status = `Clm Status`,
         loss_description = `Description of Loss`,
         defendant_attorney = `Defendant Att. Name`,
         trial_date = `Trial Date`,
         plaintiff_attorney = `Plaintiff Att. Name`,
         total_paid = `Total Paid to Date`,
         total_reserve = `Total Reserve`,
         total_incurred = `Total Incurred`) %>% 
  select(-`Date of Loss date`,-`Reported Date date`,-`Trial Date date`) %>% 
  filter(liability_type!="Auto Liability")

paterson_litigation %>% filter(liability_type=="General Liability") %>% select(loss_description)

paterson_litigation <- paterson_litigation %>%   
    filter(!(liability_type=="General Liability" & !is.na(loss_description))) # 4 cases where loss description is detailed enough to confirm that these aren't misconduct cases

colnames(paterson_police_dept_claims)
paterson_police_dept_claims <- paterson_police_dept_claims %>% 
  rename(claim_number = `Claim #`,
         claimant = `Party Name`,
         incident_date = `DOL`,
         liability_type = LOB,
         department = Department,
         location = Location_of_accident,
         reported_date = `Date Reported`,
         status = `Clm Status`,
         loss_description = `Description of Loss`,
         loss_cause = `Loss Causation`,
         total_paid = `Total Paid to Date`,
         total_reserve = `Total Reserve`,
         total_reimbursed = `Total Reimbursed`,
         total_incurred = `Total Incurred`) %>% 
  select(-`DOL date`,-`Date Reported date`,-`date received date`,-`date closed date`) %>% 
  filter(liability_type!="Auto Liability") %>% 
  filter(!grepl("TRIP AND FALL",loss_description)) %>% 
  filter(!grepl("CLMT INVOLVED IN MVA ALLEGES MISSING STOP SIGN",loss_description)) %>%
  filter(!grepl("CLMT ALLEGES MVA WAS RESULT OF KNOCKED DOWN STOP",loss_description)) %>%
  filter(!grepl("Claimant is looking to have legal bills paid",loss_description)) %>%
  filter(!grepl("Xavier Cuevas-Soto",loss_description)) %>%
  filter(!grepl("Claimant vehicle broke down in City of Paterson",loss_description)) %>%
  filter(!grepl("CLMT ALLEGES IMPROPER TOWED",loss_description))

# not filtering out all General liability cases here. 
# many of them certainly seem relevant. Filtering out some that don't seem related
paterson_police_dept_claims %>% filter(liability_type=="General Liability") %>% select(loss_description)

merged <- full_join(paterson_litigation,paterson_police_dept_claims,by="claim_number")

# does total incurred match?
merged %>% filter(!is.na(claimant.x)) %>% mutate(match = ifelse(total_incurred.x==total_incurred.y,1,0)) %>% count(match)

# 4 don't match - why?
no_match <- merged %>% 
  filter(!is.na(claimant.x)) %>% 
  mutate(match = ifelse(total_incurred.x==total_incurred.y,1,0)) %>% 
  filter(match==0) %>% 
  select(claim_number,claimant.x,claimant.y,total_incurred.x,total_paid.x,total_reserve.x,total_paid.y,total_reserve.y, total_incurred.y)

# What about incident date? # All match
merged %>% filter(!is.na(claimant.x)) %>% 
  mutate(match_incident_date = ifelse(incident_date.x==incident_date.x,1,0)) %>% 
  count(match_incident_date)

# What about reported date? # All match
merged %>% filter(!is.na(claimant.x)) %>% 
  mutate(match_reported_date = ifelse(reported_date.x==reported_date.y,1,0)) %>% 
  count(match_reported_date)

# What about liability type? # All match
merged %>% filter(!is.na(claimant.x)) %>% 
  mutate(match = ifelse(liability_type.x==liability_type.y,1,0)) %>% 
  count(match)

# What about loss description? # Only half match - looking at these, loss description y seems more detailed, so ok to use that
merged %>% filter(!is.na(claimant.x)) %>% 
  mutate(match = ifelse(loss_description.x==loss_description.y,1,0)) %>% 
  count(match)

merged %>% filter(!is.na(claimant.x)) %>% 
  mutate(match = ifelse(loss_description.x==loss_description.y,1,0)) %>% 
  filter(match!=1) %>% 
  select(loss_description.x,loss_description.y)


# The differences here are quite large in some cases
# CRC-00305-0020860 - 329271.6 vs 328346.6 -> will use smaller number to be conservative
# CRC-00305-0024851 - 163481.3 vs 152356.3. Additionally, we have the following loss description: 
  # Federal complaint for complaint previously handled under file # 305-24117 - Plaintiff alleges 
  # he was unlawfully arrested and detained by Paterson PD based on falsified police report/allegations, 
  # stating that clmt refused to unlock certain gates/fences around his property to allow access for the 
  # Fire Inspector, and became combative with arresting police officers - Damages include False arrest 
  # and imprisonment; offensive physical contact & malicious abuse resulting in bodily injury including 
  # facial abrasions, wrist cuts, leg and ankle bruising and abrasions, possible heart attack; police 
  # indifference to prisoner welfare; defamation of character/reputation; property at 48 Pennington 
  # Street taken without authorization; unfair government action; wrongful execution/enforcement of law

  # There's a separate claim number CRC-00305-0024117 which appears in both datasets and has total incurred 9772.63 in both
  # Loss description for this claim says "This is a duplicate claim of 305-24851 (active) other file was opened for the Federal complaint"
  # Don't understand this discrepancy

# CRC-00305-0024882 - 69225.0 vs 44225.0. Not sure which one to use
# CRC-00305-0025073 - 274654.8 vs 35015.0 - this is so off it alsmost feels like a typo. Not sure what to do here.

# In all cases, the claims data figure is smaller. Going to keep the litigation data figures, and leave in the "duplicate" claim
# merged <- merged %>% filter(claim_number!="CRC-00305-0024117")

# Create flags for claims vs lawsuits
merged <- merged %>% mutate(claim_or_lawsuit = ifelse(is.na(claimant.x),"claim","lawsuit"))
merged %>% count(claim_or_lawsuit)

colnames(merged)

# Rename vars 
merged2 <- merged %>% 
  rename(incident_date = incident_date.y,
         closed_date = date_closed,
         plaintiff_name = claimant.y,
         lawsuit_status = status.x,
         claim_status = status.y,
         filed_date = reported_date.y) %>% 
  mutate(summary_allegations = paste(liability_type.y,loss_description.y,loss_cause)) %>% 
  mutate(city = "Paterson", state = "NJ") %>% 
  mutate(incident_year = year(incident_date),
         calendar_year = year(closed_date),
         matter_name = NA,
         docket_number = NA,
         filed_year = year(filed_date),
         court = NA,
         case_outcome = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA) %>% # need to sanity check this 
  mutate(amount_awarded = ifelse(claim_or_lawsuit=="lawsuit",total_incurred.x,total_incurred.y), # should it be this, or total paid?
         total_paid = ifelse(claim_or_lawsuit=="lawsuit",total_paid.x,total_paid.y),
         total_reserve = ifelse(claim_or_lawsuit=="lawsuit",total_reserve.x,total_reserve.y))

colnames(merged2)
# select relevant vars
paterson_merged <- merged2 %>% select(
  -claimant.x,
  -liability_type.x,-liability_type.y,
  -reported_date.x,
  -loss_description.x,-loss_description.y,
  -total_paid.x,-total_paid.y,
  -total_reserve.x,-total_reserve.y,
  -total_incurred.x,-total_incurred.y,
  -incident_date.x,-department
)
colnames(paterson_merged)

# Combine loss cause and summary allegations into one column to standardize with other cities
paterson_merged <- paterson_merged %>%
  mutate(summary_allegations = paste0(loss_cause, ": ", summary_allegations)) %>%
  select(-loss_cause)

# CHECKS
# Time period of closed date? 
summary(paterson_merged$closed_date) # 2011 - 2020, missing for 13
summary(paterson_merged$filed_date) # 2010 - 2019, missing for 13
summary(paterson_merged$incident_date) # 2010 - 2018

# Perfect duplicates? 0 duplicates
nrow(paterson_merged %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on claim number (except the one discussed above)
nrow(paterson_merged %>% group_by(claim_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
# Hard to read table in this form, but have already filtered above to liability type "police professional" and "general liability" for some claims
table(paterson_merged$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(paterson_merged %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(paterson_merged %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(paterson_merged %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(paterson_merged %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(paterson_merged %>% filter(total_paid==0)),"rows with total paid = 0")) # but since amount awarded is non zero, this seems like payments have yet to be made. keeping this in
print(paste("There are",nrow(paterson_merged %>% filter(is.na(claim_number))),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(paterson_merged))

print("Total amount awarded")
sum(paterson_merged$amount_awarded)

summary(paterson_merged$amount_awarded)
summary(paterson_merged$total_paid)

write.csv(paterson_merged,paste0(out_data_path,"paterson_edited.csv"), na = "",row.names = FALSE)


