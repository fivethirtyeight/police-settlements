# This script takes the raw csv on settlements received from the NYC Comptroller's office 
# and outputs a csv with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

ny1 <- read_excel(paste0(raw_data_path,"The Marshall Project - NYPD Claims Settled From 1.1.2010 to 12.31.2019.xlsx"))


ny1 <- ny1 %>%
  rename(plaintiff_name = `Claimant Name`,
         claim_number = `Claim Number`,
         summary_allegations = `Claim Type`,
         incident_date = `Date of Occurrence`,
         filed_date = `Date Claim Filed`,
         plaintiff_attorney = Attorney,
         closed_date = `Settlement Date`,
         amount_awarded = `Settlement Amount`) %>%
  mutate(location = ifelse(!is.na(`Occurrence Location`), `Occurrence Location`, "Precise location missing")) %>%
  mutate(location = ifelse(!is.na(`Occurrence Borough`), paste0(location, ", ", `Occurrence Borough`), location)) %>%
  mutate(location = ifelse(!is.na(`Occurrence City`), paste0(location, ", ", `Occurrence City`), location)) %>%
  mutate(location = ifelse(!is.na(`Occurrence State`), paste0(location, ", ", `Occurrence State`), location)) %>%
  mutate(calendar_year = year(closed_date),
         incident_year = year(incident_date),
         filed_year = year(filed_date),
         city = "New York City",
         state = "NY",
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         court = NA,
         docket_number = NA,
         matter_name = NA,
         case_outcome = NA) %>%
  # Removing Agency, which is 100% "Police Department", and the two Defendant columns, which are 100$ missing
  select(-Agency, -`Defendant - Individual`, -`Defendant - Individual with Badge #`, 
         -`Occurrence Location`, -`Occurrence Borough`, -`Occurrence City`, -`Occurrence State`) # Also removing location which we've consolidated

table(ny1$summary_allegations)
# Filter
# Decisions were made based on the descriptions of each category from the PDF entitled "Claims-Report-FY-2019.pdf"
ny2 <- ny1 %>%
  filter(!(summary_allegations %in% c("ADMIRALTY", "ADMIRALTY (PI)", "AUTOMOBILE ACCIDENT", "AUTOMOBILE ACCIDENT (PI)", 
                                      "BUILDING AND PROPERTY (PI)", "BUILDINGS AND PROPERTY", "CONTRACT", "CORRECTION FACILITY(PI)", 
                                      "DAMAGE BY CITY PERSONNEL", "DEFECTIVE SIDEWALK (PI)", "DEFECTIVE ROADWAY (PI)",
                                      "DEFECTIVE TRAFF/LIGHT/SIGN (PI)", "DISPUTES", "EMPLOYEE UNIFORMED SERVICE", "EMPLOYEE UNIFORMED SERVICE (PI)",
                                      "EQUITABLE", "HEALTH FACILITY (PI)", "RECREATION (PI)", "SALARY", "SEWER OVERFLOW")))


# CHECKS
# Perfect dups? 0
nrow(ny2 %>% group_by_all() %>% filter(n()>1))


# No rows where amount awarded = 0
ny2 <- ny2 %>% filter(amount_awarded!=0)


# What's filtered out? what's left that ambiguous?
table(ny2$summary_allegations)

# time period of calendar year 
summary(ny2$calendar_year) # Min 2010, max 2019, no missing

# Missing ness of variables
print(paste("There are",nrow(ny2 %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(ny2 %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(ny2 %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(ny2 %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(ny2 %>% filter(is.na(docket_number))),"rows missing docket number"))
print(paste("There are",nrow(ny2 %>% filter(is.na(claim_number))),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(ny2))

print("Total amount awarded")
sum(ny2$amount_awarded)

# Sanity checking with WSJ: they have $1.1bn for 2015-2020. We have $1.057bn for 2015-2019
sum(ny2$amount_awarded[ny2$calendar_year>=2015])


write.csv(ny2,paste0(out_data_path,"new_york_edited.csv"), na = "",row.names = FALSE)

