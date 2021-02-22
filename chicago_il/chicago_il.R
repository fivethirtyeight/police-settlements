# This script takes the raw data on settlements received from Chicago and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Damini Sharma at The Marshall Project
# Original date 8/13/20

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

# Sheet split up into cases by "police board" and by "police dept" so need to import twice, separately
chicago_police_board <- read_excel(paste0(raw_data_path,"Police Matter Dispositions with Money.xlsx"), 
                                                    skip = 2, n_max = 4)
chicago_police_dept <- read_excel(paste0(raw_data_path,"Police Matter Dispositions with Money.xlsx"), 
                                                    skip = 9)

chicago_police_board <- chicago_police_board %>%  mutate(flag_dept = "Chicago Police Board")
chicago_police_dept <- chicago_police_dept %>% mutate(flag_dept = "Chicago Police Dept")

chicago <- rbind(chicago_police_board,chicago_police_dept)

# Several merged rows, show up as NAs. Can filter out
nrow(chicago %>% filter(is.na(`Docket Number`)))
chicago <- chicago %>% filter(!is.na(`Docket Number`))


# Steps taken:
# 1. standardize variable names to match agreed upon format, generate empty vars for unavailable vars
# 2. Clean up date vars, amount awarded and do some sanity checks
# 3. Filter to cases we care about
# 4. Check duplicates on primary key(s)

# calendar_year/fiscal_year -> calendar year, pulled from disposition date
# city	-> generated 
# incident_date	-> don't have, generate empty var
# date_filed	-> don't have, generate empty var
# closed_date	 -> disposition date
# amount_awarded -> Total Amount	
# other_expenses	-> don't have generate NA var
# collection		-> don't have generate NA var
# total_incurred		-> don't have generate NA var
# case_outcome		-> "Description". there is also "Disposition", but this variable seems to have some issues
# docket_number		-> "Docket Number"
# claim_number		-> don't have generate NA var
# court		-> don't have generate NA var
# plaintiff_name		-> don't have generate NA var
# matter_name	-> "Caption"
# plaintiff_attorney		-> don't have generate NA var
# location		-> don't have generate NA var
# summary_allegations (separated by ;) 	-> "Category"
# status <- "Phase"


# Step 1
# generate NA vars for incident_date, other_expenses, collection, total_incurred, claim_number, court, plaintiff_name, 
# plaintiff_attorney, location
# rename docket number -> docket number
# rename summary_allegations -> category
# rename matter_name -> caption
# rename status -> phase
# rename amount awarded -> total amount
# generate city var
# rename case_outcome -> Description

# There is a "Disposition" variable that is similar to "Description" but appears to have some data entry
# errors. When we exclude "Disposition", there are 26 perfect duplicates; including "Disposition" removes
# perfect duplicates. See case 2008 C 2275 for example - I think what is happening is that Disposition
# is breaking out the same payment (usually something like court fees) into multiple "actions" and duplicating that
# data. For 2008 C 2275 case and a handful of others, I confirmed the total amounts here https://projects.chicagoreporter.com/settlements/case/08-cv-2275/
# which means the duplicated row should be removed. Based on that, we're removing the "Disposition" variable,
# keeping "Description", and removing perfect duplicates (i.e. only keeping one copy of the duplicated row)

chicago <- chicago %>% 
  rename(docket_number = `Docket Number`,
         summary_allegations = Category,
         matter_name = Caption,
         status = Phase,
         case_outcome = Description,
         amount_awarded = `Total Amount`,
         role = Role) %>% 
  mutate(city = "Chicago", state = "IL") %>% 
  mutate(incident_date = NA,
         incident_year = NA,
         filed_date = NA,
         filed_year = NA,
         other_expenses = NA,
         collection= NA,
         total_incurred = NA,
         claim_number = NA,
         court = NA,
         plaintiff_name = NA,
         plaintiff_attorney = NA,
         location= NA)

# Step 2
# Convert Disposition Date to closed_date
chicago <- chicago %>% 
  mutate(closed_date = date(`Disposition Date`),
         calendar_year = year(closed_date))


# Step 3 - filtering cases
print("Total number of cases")
print(nrow(chicago))

#chicago %>% filter(`Docket Number`=="00513720411") %>% select(Caption)
category <- chicago %>% count(summary_allegations)
# allegations with "Dispute:General:Police Matters" seem to be the ones we care about. Filtering to those
# this filters out some cases labeled as "Torts", including some that might be ambiguous e.g. "Pursuit".
chicago <- chicago %>% filter(grepl("Dispute:General:Police Matters",summary_allegations))

print("Total number of cases labeled Dispute:General:Police Matters")
print(nrow(chicago))

# Exclude: Division (Federal civil rights, Torts, etc); 
# Main Asignee (who the case is assigned to?); 
# Keep either Disposition - see above
# Keep status (4 open, 1524 closed) and role (1526 defendant, 2 plaintiff)
chicago %>% count(status)
chicago %>% count(role)

# View(chicago %>% filter(status=="Open")) # all have amount awarded (4)
# View(chicago %>% filter(role=="Plaintiff")) # have amount awarded (2)

chicago <- chicago %>% select(calendar_year,
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
                    summary_allegations,
                    status,
                    role,
                    flag_dept)



# CHECKS
# Time period of closed date? 
summary(chicago$closed_date) # Min is 1/2020, max is 12/2019. This looks good.

# time period of calendar year or incident year or filed year if closed date missing
summary(chicago$calendar_year) # Min 2010, max 2019

# Perfect duplicates? 26 duplicates (see above). De-duping to only keep one version
nrow(chicago %>% group_by_all() %>% filter(n()>1))
dups <- chicago %>% group_by_all() %>% filter(n()>1)
chicago <- chicago %>% distinct()

# Duplicates on identifying variables?
nrow(chicago %>% group_by(docket_number) %>% filter(n()>1)) # 146
nrow(chicago %>% group_by(docket_number, case_outcome) %>% filter(n()>1)) # 58
nrow(chicago %>% group_by(docket_number, case_outcome,closed_date) %>% filter(n()>1)) # 40 
nrow(chicago %>% group_by(docket_number, case_outcome,closed_date,amount_awarded) %>% filter(n()>1)) # 0

dups <- chicago %>% group_by(docket_number) %>% filter(n()>1)
dups <- chicago %>% group_by(docket_number,case_outcome) %>% filter(n()>1)
dups <- chicago %>% group_by(docket_number,case_outcome,closed_date) %>% filter(n()>1)

# The above suggests that docket number can be duplicated because:
# "Description" differs (e.g. settlement paid vs court costs)
# Closed date differs (amount paid out over multiple payments)
# Everything is the same about amount awarded differs (multiple payments made for some other reason)
# We're leaving these in and making a note of them

# What's filtered out? what's left that ambiguous?
# See above / see notes
table(chicago$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(chicago %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(chicago %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(chicago %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(chicago %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))

# count cases
print("Total number of cases")
print(nrow(chicago))

print("Total amount awarded")
sum(chicago$amount_awarded)


# Check WSJ (they have $252m for 2015-2020, we have $237 for 2015-2019)
chicago_2015_2020 <- chicago %>% filter(closed_date>="2015-01-01")
sum(chicago_2015_2020$amount_awarded)
sum(chicago$amount_awarded)

write.csv(chicago,paste0(out_data_path,"chicago_edited.csv"), na = "",row.names = FALSE)


