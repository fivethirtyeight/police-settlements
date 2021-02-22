# This script takes the raw data on settlements received from Los Angeles and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Likhitha Butchireddygari at FiveThirtyEight
# Code was updated by Damini Sharma at The Marshall Project on 8/19
# Code was updated by Laura Bronner at FiveThirtyEight on 1/12/2021

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())


LAPD_MASTER <- read_excel(paste0(raw_data_path,"LAPD Master.xlsx"),
                          col_types = "text")

# Change up names for easy reference
names(LAPD_MASTER) <- gsub(names(LAPD_MASTER), pattern = " ", replacement = "_")

LAPD_MASTER <- LAPD_MASTER %>% 
  mutate_at(.funs = ~as.numeric(.), .vars = c("AMOUNT")) %>% 
  mutate_at(.funs = list(date = ~as_date(as.numeric(.),origin='1899-12-30')), 
            .vars = c("DISPO_DT",
                      "INCIDENT",
                      "FILED"))

LAPD_MASTER <- LAPD_MASTER %>% 
  mutate(INCIDENT = INCIDENT_date,
         DISPO_DT = DISPO_DT_date,
         FILED = FILED_date) %>% 
  select(-INCIDENT_date, -DISPO_DT_date, -FILED_date)

# Change back to match rest of Likhitha's code up names for easy reference
names(LAPD_MASTER) <- gsub(names(LAPD_MASTER), pattern = "_", replacement = " ")

type <- LAPD_MASTER %>% group_by(`CASE TYPE`) %>% count()

#removing all the categories that we wouldn't deem as police misconduct'

LAPD_edited <- LAPD_MASTER[!grepl("Traffic Accident", LAPD_MASTER$`CASE TYPE`),]
LAPD_edited <- LAPD_edited[!grepl("Trip and Fall", LAPD_edited$`CASE TYPE`),]
LAPD_edited <- LAPD_edited[!grepl("Employment", LAPD_edited$`CASE TYPE`),]
LAPD_edited <- LAPD_edited[!grepl("FLSA", LAPD_edited$`CASE TYPE`),]
LAPD_edited <- LAPD_edited[!grepl("Veteran", LAPD_edited$`CASE TYPE`),]
LAPD_edited <- LAPD_edited[!grepl("Whistleblower", LAPD_edited$`CASE TYPE`),]

names(LAPD_edited)[names(LAPD_edited) == "FISCAL YEAR"] <- "fiscal_year"
names(LAPD_edited)[names(LAPD_edited) == "INCIDENT"] <- "incident_date"
LAPD_edited$incident_year <- year(LAPD_edited$incident_date)
names(LAPD_edited)[names(LAPD_edited) == "FILED"] <- "filed_date"
LAPD_edited$filed_year <- year(LAPD_edited$filed_date)
names(LAPD_edited)[names(LAPD_edited) == "CASE NAME"] <- "matter_name"
names(LAPD_edited)[names(LAPD_edited) == "CLAIM # / FILE #"] <- "claim_number"
names(LAPD_edited)[names(LAPD_edited) == "CASE #"] <- "docket_number"
names(LAPD_edited)[names(LAPD_edited) == "AMOUNT"] <- "amount_awarded"
names(LAPD_edited)[names(LAPD_edited) == "CASE TYPE"] <- "summary_allegations"
names(LAPD_edited)[names(LAPD_edited) == "DISPO DT"] <- "closed_date"
names(LAPD_edited)[names(LAPD_edited) == "Location"] <- "location"

#extra location field
LAPD_edited <- select(LAPD_edited,-c(Location1))

LAPD_edited$city <- 'Los Angeles'
LAPD_edited$state <- 'CA'
LAPD_edited$other_expenses <- NA
LAPD_edited$collection <- NA
LAPD_edited$total_incurred <- NA
LAPD_edited$case_outcome <- NA
LAPD_edited$court <- NA
LAPD_edited$plaintiff_name <- NA
LAPD_edited$plaintiff_attorney <- NA

# Noting whether it was a pre-litigation claim, or litigation:
# As per email correspondence with Frank Mateljan, LA City Hall
# Anything starting in C or L or no letters is a pre-litigation claim; 
# anything starting in CV, BC, LC, PC, SACV, SC, TA, TC, B, BS, EC, NC is a case in litigation.
LAPD_edited <- LAPD_edited %>%
  mutate(claim_letters = gsub("(^[A-Z]{1,4})(.*)", "\\1", claim_number),
         claim_or_lawsuit = ifelse(claim_letters %in% c("CV", "BC", "LC", "PC", "SACV", "SC", "TA", "TC", "B", "BS", "EC", "NC"), "lawsuit", "claim")) %>%
  select(-claim_letters)
table(LAPD_edited$claim_or_lawsuit)

# CHECKS
# Perfect dups? 22
nrow(LAPD_edited %>% group_by_all() %>% filter(n()>1))
dups <- LAPD_edited %>% group_by_all() %>% filter(n()>1)

# Based on looking at these duplicates, it seems like these perfect duplicates should be added together, rather than removed
# see for example Navas, Rosa v City - a number of payments which all add up to $2.5m (number confirmed here https://verdictvictory.com/blog/los-angeles-to-pay-out-more-than-8-million-in-wrongful-death-settlements/)

LAPD_edit <- ddply(LAPD_edited,.(fiscal_year,city,state,incident_date,incident_year,filed_date,filed_year,closed_date,other_expenses,collection,total_incurred,case_outcome,docket_number,claim_number,court,plaintiff_name,matter_name,plaintiff_attorney,location,summary_allegations,claim_or_lawsuit),summarise, amount_awarded = sum(amount_awarded))

LAPD_edit <- LAPD_edit[, c("fiscal_year", "city", "state", "incident_date", "incident_year",
                           "filed_date", "filed_year", "closed_date", "amount_awarded",
                           "other_expenses", "collection", "total_incurred",
                           "case_outcome", "docket_number", "claim_number",
                           "court", "plaintiff_name", "matter_name",
                           "plaintiff_attorney", "location", "summary_allegations", "claim_or_lawsuit")]

# Filter out 7 rows with amount awarded  = 0 
LAPD_edit <- LAPD_edit %>% filter(amount_awarded!=0)

nrow(LAPD_edit %>% group_by_all() %>% filter(n()>1)) # 0 perfect duplicates

# Duplicates by claim number? 62
nrow(LAPD_edit %>% group_by(claim_number) %>% filter(n()>1))

# Duplicates by claim number and plaintiff name? 50
nrow(LAPD_edit %>% group_by(claim_number,matter_name) %>% filter(n()>1))

# Duplicates by claim number and fiscal year? 2
nrow(LAPD_edit %>% group_by(claim_number,fiscal_year) %>% filter(n()>1))
dups <- LAPD_edit %>% group_by(claim_number,fiscal_year) %>% filter(n()>1)

# Same case/fiscal year, two different closed-dates. Keeping these

# What's filtered out? what's left that ambiguous?
table(LAPD_edit$summary_allegations)

# Closed date missing for most of data before 2015
summary(LAPD_edit$closed_date) # missing for most before 2015
table(LAPD_edit$fiscal_year)  # FY2009/10 to FY2019/20

# Create calendar year as second half of fiscal year
LAPD_edit$calendar_year_temp <- str_split_fixed(LAPD_edit$fiscal_year,"/",2)[,2]

LAPD_edit <-LAPD_edit %>% 
  mutate(calendar_year = paste0("20",calendar_year_temp)) %>% 
  mutate(calendar_year = as.numeric(calendar_year)) %>% 
  select(-calendar_year_temp)

# time period of calendar year or incident year or filed year if closed date missing
summary(LAPD_edit$calendar_year) # Min 2010, max 2020

# Missing ness of variables
print(paste("There are",nrow(LAPD_edit %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(LAPD_edit %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(LAPD_edit %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(LAPD_edit %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(LAPD_edit %>% filter(is.na(docket_number))),"rows missing docket number"))
print(paste("There are",nrow(LAPD_edit %>% filter(is.na(claim_number))),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(LAPD_edit))

print("Total amount awarded")
sum(LAPD_edit$amount_awarded)


# Check with WSJ numbers
LAPD_edit_2015_2020 <- LAPD_edit %>% filter(fiscal_year %in% c("FY2015/16",
                                                               "FY2016/17",
                                                               "FY2017/18",
                                                               "FY2018/19",
                                                               "FY2019/20"))

sum(LAPD_edit_2015_2020$amount_awarded)

write.csv(LAPD_edit,paste0(out_data_path,"los_angeles_edited.csv"), na = "",row.names = FALSE)

