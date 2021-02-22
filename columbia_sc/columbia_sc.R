# This script takes the raw pdf on settlements received from Columbia, SC, and outputs a csv
# with variables cleaned and standardized, and filters out car accidents

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 12/4/2020
# Updated by Damini Sharma

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

# Read in data that I copied to Excel
columbia <- read_excel(paste0(raw_data_path,"columbia.xlsx"))
columbia <- columbia  %>% filter_all(any_vars(!is.na(.)))

### Fixing things

# First, fix misalignment of two of the columns

# Separate out date columns and drop NAs

datecol1 <- columbia %>% 
  select(DOL) %>% 
  drop_na()
datecol2 <- columbia %>% 
  select(`Date of Lawsuit`) %>% 
  drop_na()

# Remove them from main df and then drop the NAs from that one

columbia <- columbia %>% 
  select(-DOL, -`Date of Lawsuit`) %>%
  filter(!is.na(Plaintiff))

# Yay everything is 39 rows long! Now we can join 'em back up

columbia <- columbia %>% bind_cols(datecol1, datecol2)

# Second, dropping the NAs in the main df cut off two of the Officer descriptions, let's add those back in

columbia <- columbia %>%
  mutate(`CPD / Officer` = ifelse(Plaintiff == "Wells, Mack", "CPD / Ofc. Steven Susler & Chief Holbrook", `CPD / Officer`),
         `CPD / Officer` = ifelse(Plaintiff == "Strother, Koneka and Bradford, parents & legal guardians of a minor", "CPD / Ofc. Leon Sealey", `CPD / Officer`))

# Third, dates.
# Two of the date columns have strings, which messed up their formatting, so let's fix that
# And then one of those is because of a mistake in the date format. The other is because the date field says "Ongoing" which can't be helped, we'll leave that missing.

columbia <- columbia %>% 
  mutate(`Date of Resolution` = as.Date(as.numeric(`Date of Resolution`), origin = "1899-12-30"),
         `Date of Resolution` = if_else(Plaintiff == "Collins, Bradford", ymd(NA), `Date of Resolution`), # This date field just said "2018" and got turned into a wrong 1905 date
         DOL = as.Date(as.numeric(DOL), origin = "1899-12-30"),
         DOL = if_else(Plaintiff == "Taylor, Quintin", ymd("2010-06-01"), DOL)) # Fix the mistake in the date formatting in one row


# Fourth, turn the Court & CA No. column into docket number and court columns!

columbia <- columbia %>%
  separate(`Court & CA No.`, into = c("docket_number", "court"), sep = " ", extra = "merge", remove = FALSE) %>%
  # This creates two problems where there are missing docket numbers, so I will fix those
  mutate(court = if_else(docket_number == "Richland", "Richland County Court of Common Pleas", court),
         docket_number = if_else(docket_number == "Richland", NA_character_, docket_number))
           
  
### Renaming things

columbia <- columbia %>%
  rename(plaintiff_name = Plaintiff,
         incident_date = DOL, # Likely "date of loss" - could follow up with source, but note that that's the original variable name
         location = Location,
         filed_date = `Date of Lawsuit`,
         closed_date = `Date of Resolution`,
         summary_allegations = `Summary of Allegations`,
         amount_awarded = `Settlement Amount`,
         plaintiff_attorney = `Plaintiff’s Attorney`,
         defendant = `CPD / Officer`
         ) %>%
  mutate(calendar_year = if_else(plaintiff_name == "Collins, Bradford", 2018, year(closed_date)),
         incident_year = year(incident_date),
         filed_year = year(filed_date),
         amount_awarded = as.numeric(amount_awarded),
         city = "Columbia",
         state = "SC",
         other_expenses = NA,
         total_incurred = NA,
         claim_number = NA,
         collection = NA,
         matter_name = NA,
         case_outcome = NA) %>% 
  select(-`Type of Misconduct`)


### Filtering
table(columbia$summary_allegations)

columbia <- columbia %>%
  filter(!is.na(amount_awarded)) %>% # One case missing amount - closed date was "ongoing"
  filter(!(summary_allegations == "MVA" & !is.na(summary_allegations))) %>% # Filter out MVA cases
  select(- `Court & CA No.`)

# CHECKS
# Time period of closed date? NA
summary(columbia$closed_date) # 9/2010 to 3/2019, 4 NAs
# time period of calendar year or incident year or filed year if closed date missing
summary(columbia$calendar_year) # Min 2010, max 2019, 3 NAs

# Perfect duplicates? 0 duplicates
nrow(columbia %>% group_by_all() %>% filter(n()>1))

# Duplicates on identifying variable - no duplicates on docket number
nrow(columbia %>% group_by(docket_number) %>% filter(n()>1))

# What's filtered out? what's left that ambiguous?
table(columbia$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(columbia %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(columbia %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(columbia %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(columbia %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(columbia %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(columbia))

print("Total amount awarded")
sum(columbia$amount_awarded)

write.csv(columbia,paste0(out_data_path,"columbia_edited.csv"), na = "",row.names = FALSE)

