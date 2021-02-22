# This script takes the raw csv on settlements received from Washington DC and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Laura Bronner at FiveThirtyEight
# Code updated by Damini Sharma at The Marshall Project on 11/16/20


# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

# Note: Before reading in, data contained 
# "Query: [[ (Client Sort contains 'MPD', 'M.P.D.', 'Police Dep', 'Metropolitan Police')] and 
# [(Disposition Date between 1/01/2010 and 3/22/2019) and (Disposition Value >= 1000)]]"
# at the top

DC <- read_excel(paste0(raw_data_path,"MPD Cases Final List 08 06 20.xlsx"),
                   skip = 1)

# Remove one row which is all NA
DC <- DC %>% filter_all(any_vars(!is.na(.)))

# There is one row where all the fields are shifted over; this fixes it
DC[139,]
DC[139,]$`Matter Description`
DC[397,]
DC[397,]$`Matter Description`
DC <- DC %>%
  mutate(`Disposition Value` = ifelse(`Matter ID` == 250899 & !is.na(`Matter ID`), `Disposition Outcome`, `Disposition Value`),
         `Disposition Date` = ifelse(`Matter ID` == 250899 & !is.na(`Matter ID`), Status, `Disposition Date`),
         `Disposition Outcome` = ifelse(`Matter ID` == 250899 & !is.na(`Matter ID`), `Status Date`, `Disposition Outcome`),
         Status = ifelse(`Matter ID` == 250899, `Opened Date`, Status),
         `Status Date` = ifelse(`Matter ID` == 250899, `Matter Category`, `Status Date`),
         `Opened Date` = ifelse(`Matter ID` == 250899, NA, `Opened Date`),
         `Matter Category` = ifelse(`Matter ID` == 250899, "Excessive force - shooting", `Matter Category`),
         `Matter Description` = ifelse(`Matter ID` == 250899, "Rice, Larry D., Jr. v. DC, 09-310", `Matter Description`),
         Status = ifelse(`Matter ID` == 435884, `Status Date`, Status),
         `Status Date` = ifelse(`Matter ID` == 435884, `Opened Date`, `Status Date`),
         `Opened Date` = ifelse(`Matter ID` == 435884, `Matter Category`, `Opened Date`),
         `Matter Category` = ifelse(`Matter ID` == 435884, "False arrest and civil rights violation arising from arrest without a warrant.", `Matter Category`))
         
# Based on discussions with Runako, we've concluded that in rows where everything is missing except for 
# Disposition outcome, disposition date, and disposition value, it's because it's part of the same case as the 
# last non-empty row. Based on this, we're going to fill NAs from top to bottom IF everything but those three variables are missing


# generate flag for if everything is missing except above and filter only on those rows
# we will fill na's there, and then append to rest of data
DC <- DC %>% 
  mutate(flag_fill = ifelse(
                        is.na(`Settlement/Complaint`) &
                            is.na(`Matter ID`) &
                            is.na(`Matter Description`) &
                            is.na(`Matter Category`) &
                            is.na(`Opened Date`) &
                            is.na(`Status Date`) &
                            is.na(Status), 1, 0)) %>%
  mutate(flag_fill2 = ifelse(lead(flag_fill)==1,1,0)) %>% 
  mutate(flag_final = ifelse(flag_fill==1 | flag_fill2 ==1 | is.na(flag_fill2),1,0))

flagged <- DC %>% filter(flag_final==1) %>% 
  fill(`Settlement/Complaint`,`Matter ID`,`Matter Description`,`Matter Category`,`Opened Date`,`Status Date`,Status)

unflagged <- DC %>% filter(flag_final==0)

# join it all back
DC2 <- rbind(flagged,unflagged)

DC <- DC2

# Cleaning dates and rounding numbers;
# Matter Description contains the case name and also sometimes a longer description. 
# Description is usually separated from the case name with a hyphen, though not always; the fix doesn't always capture the description.
# Imputing the longer description into Matter Category if Matter Category is missing.
DC <- DC %>% 
  mutate(`Disposition Value` = round(as.numeric(`Disposition Value`), 2),
         `Opened Date` = as.Date(as.numeric(`Opened Date`), origin = "1899-12-30"),
         `Status Date` = as.Date(as.numeric(`Status Date`), origin = "1899-12-30"),
         `Disposition Date` = as.Date(as.numeric(`Disposition Date`), origin = "1899-12-30")) %>%
  separate(`Matter Description`, into = c("matter_name", "longer_description"), sep = "- ") %>%
  mutate(longer_description = ifelse(is.na(longer_description) & grepl("Sexual Harrasment and National Origin Discrimination", matter_name), "Sexual Harrasment and National Origin Discrimination", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("arrested after inquiry about police activity", matter_name), "arrested after inquiry about police activity", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("sexual harassment claim brought by MPD officer against supervisor", matter_name), "sexual harassment claim brought by MPD officer against supervisor", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("alleged false arrest re: shoplifting", matter_name), "alleged false arrest re: shoplifting", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("Car accident involving MPD", matter_name), "Car accident involving MPD", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("Alleged assault, negligent training and hiring", matter_name), "Alleged assault, negligent training and hiring", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("auto accident in Maryland", matter_name), "auto accident in Maryland", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("Car accident involving police cruiser", matter_name), "Car accident involving police cruiser", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("auto accident with  police cruiser", matter_name), "auto accident with  police cruiser", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("auto accident", matter_name), "auto accident", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("DCHRA and Title VII gay bias and retaliation", matter_name), "DCHRA and Title VII gay bias and retaliation", longer_description),
         longer_description = ifelse(is.na(longer_description) & grepl("sexuall assault in officer forcing teenage girl to strip and have picture taken of her private parts", longer_description), 
                                      "sexuall assault in officer forcing teenage girl to strip and have picture taken of her private parts", longer_description),
         `Matter Category` = ifelse(is.na(`Matter Category`) & grepl("FOIA", matter_name), "FOIA", `Matter Category`),
         `Matter Category` = ifelse(is.na(`Matter Category`) & grepl("VIN", matter_name), "VIN mentioned", `Matter Category`),
         `Matter Category` = ifelse(is.na(`Matter Category`), longer_description, `Matter Category`))

# Renaming into the categories we want, and generating those we don't have
# Do we want to keep Matter ID somehow? 
DC <- DC %>%
  rename(amount_awarded = `Disposition Value`,
         closed_date = `Status Date`,
         filed_date = `Opened Date`,
         disposition_date = `Disposition Date`,
         #matter_name = `Matter Description`,
         case_outcome = `Disposition Outcome`,
         status = Status
         # docket_number = `Settlement/Complaint`, # IS THIS RIGHT????? What about Matter ID?
         ) %>% 
  mutate(city = "Washington", 
         docket_number = paste(`Settlement/Complaint`,`Matter ID`), # Decided to concatenate both
         state = "DC",
         summary_allegations = ifelse((!is.na(`Matter Category`) & !is.na(longer_description) & `Matter Category`!=longer_description),
                                      paste0(`Matter Category`, ": ", longer_description),
                                      ifelse(((!is.na(`Matter Category`) & is.na(longer_description)) | 
                                                (!is.na(`Matter Category`) & `Matter Category`==longer_description)), `Matter Category`,
                                             ifelse(is.na(`Matter Category`) & is.na(longer_description), NA, "something went wrong"))),
         calendar_year = year(closed_date),
         filed_year = year(filed_date),
         incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         claim_number = NA,
         court = NA,
         plaintiff_name = NA,
         plaintiff_attorney = NA,
         location = NA)

# Check
table(DC$summary_allegations == "something went wrong")
# Cool, nothing wrong here

# Can we do anything about the ones missing descriptions?
# About 69 rows still missing any allegations - we will have to leave these in because we don't want to filter out
# relevant cases
missing_desc <- DC %>% filter(is.na(summary_allegations))


# Filter
table(DC$`Matter Category`)
table(is.na(DC$`Matter Category`))

# Remove FOIAs
DC1 <- DC %>% filter(!grepl("FOIA", `Matter Category`))
DC1 <- DC1 %>% filter(!grepl("Freedom of Information Act", `Matter Category`))

# Remove employment issues
DC1 <- DC1 %>% filter(!`Matter Category` %in% c("Personnel", 
                                                "Discrimination", 
                                                "Employment",
                                                "sexual harassment claim brought by MPD officer against supervisor"))
#DC1 <- DC1 %>% filter(!grepl("employment", ignore.case = TRUE, `Matter Category`))

# Remove pure car things
DC1 <- DC1 %>% filter(!`Matter Category` %in% c("Personal Injury: Car accident with MPD vehicle", 
                                                "Personal Injury: Car Accident",
                                                "Personal Injury: Car accident.",
                                                "Personal Injury; Auto Negligence",
                                                "Auto Accident", 
                                                "Auto Negligence",
                                                "Negligence Automobile PI", 
                                                "Negligence Automobile Accident PD",
                                                "Car accident.",
                                                "VIN mentioned"))
DC1 <- DC1 %>% filter(!grepl("auto accident", ignore.case = TRUE, `Matter Category`) | grepl("Negligence Miscellaneous PI: Negligence-MPD-minor auto accident",`Matter Category`))
DC1 <- DC1 %>% filter(!grepl("car accident", ignore.case = TRUE, `Matter Category`) | grepl("Personal Injury: Car accident and alleged cover-up by indiv. officers.",`Matter Category`))


# Slip and fall
DC1 <- DC1 %>% filter(!grepl("Slip and Fall", ignore.case = TRUE, `Matter Category`))

# Breach of contact 
DC1 <- DC1 %>% filter(!grepl("Breach of contract", ignore.case = TRUE, `Matter Category`))
DC1 <- DC1 %>% filter(!`Matter Category` %in% c("Contract"
                                                ))
table(DC1$`Matter Category`)

# # Breathalyzer cases?
# DC1 <- DC1 %>% filter(grepl("breath", ignore.case = TRUE, `Matter Category`))

# Weirdnesses:
# Parker, Shelly v. DC, 03-0213 (EGS) (a.k.a. Heller I) -- no Description or Matter Category but $1.5m payout.
# "Craig, Joanne T. v. DC, 11-1200 (D.D.C.) false arrest" -- listed as "Employment" under Matter Category, but says false arrest in the Matter Description...
# Keeping both

# Clean up columns
colnames(DC1)
DC1 <- DC1 %>% select(-`Settlement/Complaint`,-`Matter ID`,-`Matter Category`,-flag_fill,-flag_fill2,-flag_final)

# Filter out 11 rows where amount awarded = 0 
DC1 <- DC1 %>% filter(amount_awarded!=0)

# CHECKS
# Time period of closed date? NA
summary(DC1$closed_date) # coming from status date - ~8 cases from before 2010. 7 of these seem to be the same matter. leave these in, make a note
summary(DC1$disposition_date) 

# time period of calendar year or incident year or filed year if closed date missing
summary(DC1$calendar_year) # Min 2005, max 2019, 1 NA

# Perfect duplicates? 6 duplicates - drop these?
nrow(DC1 %>% group_by_all() %>% filter(n()>1))
dups <- DC1 %>% group_by_all() %>% filter(n()>1)
# Dropping the 3 perfect duplicates
DC1 <- DC1 %>% distinct()

# Duplicates on identifying variable - now that the 3 perfect duplicates are removed, no others
nrow(DC1 %>% group_by(docket_number,disposition_date,amount_awarded) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
summary <- DC1 %>% group_by(summary_allegations) %>% tally() %>% arrange(-n)
# There are definitely still cases in here that are not misconduct, but that are ambiguous to some extent
# In keeping with the principle of being more lenient rather than less, we're going to leave these in and make a note of it

# Missing ness of variables
print(paste("There are",nrow(DC1 %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(DC1 %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(DC1 %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(DC1 %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(DC1 %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(DC1))

print("Total amount awarded")
sum(DC1$amount_awarded)



# Check WSJ (they have $69.7m for 2015-2020, we have $78m for 2015-2019 (till march 2019))
# perhaps ours should be lower - still working with our contact to get the "right" subset
summary(DC1$closed_date)
dc_2015_2020 <- DC1 %>% filter(closed_date>="2015-01-01")
sum(dc_2015_2020$amount_awarded)
sum(dc_2015_2020$amount_awarded)


# That takes care of the first dataset we received from DC - which had disposition date until March 2019

write.csv(DC1,paste0(out_data_path,"DC_edited.csv"), na = "",row.names = FALSE)


