# This script takes the raw pdf on settlements received from Detroit and outputs a csv
# with variables cleaned, standardized, and cases filtered to only those relevant to police misconduct

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 9/2/2020
# Updated by Damini Sharma from The Marshall Project
# Updaed on 11/16/2020

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

# Extract tables from the pdf using tabulizer
out <- extract_tables(paste0(raw_data_path,"Spreadsheet (redacted).pdf"))

# We now have a list in which each page of the pdf is a matrix.
# The first column of every matrix is the rownumber of the original spreadsheet, that we want to match the rows on.

# Create a function to turn the list of matrices into a dataframe
processpage <- function(x){
  x <- x[x[,1] != "1", ] # Drop the row labelled "1" (not present in every matrix) since that's the column names and they got mangled
  df <- as_tibble(x[-1, ]) # Drop the first row (column letters) and turn the matrix into a df
  names(df) = x[1,] # Turn the column letters into the column names
  names(df)[1] = "idnum" # The first column doesn't have a letter, so we call it idnum -- this is what we want to match rows on
  names(df)[names(df) == ""] = "empty" # There is one column that doesn't seem to have a name but shows up sometimes and messes things up, call it "empty" and delete later
  tidyr::pivot_longer(df, -idnum) # Make the whole thing long, so that we can then make it wide later!
}

# Apply the function to the list
test = lapply(out, processpage) 

# Bind all the rows together and make it wide by idnum, the id column!
widedf = do.call("bind_rows", test) %>% 
  tidyr::pivot_wider(id_cols=idnum)

# Remove the "empty" column (first check to make sure it is in fact empty)
table(widedf$empty)
table(is.na(widedf$empty))
widedf <- widedf %>% select(-empty)

# Add original column names 
colnames(widedf) <- c("idnum", "PMT DATE", "AMOUNT", "TITLE", "DEPT DESCRIP", "PAYEE/PAYOR",
                      "PAID DATE", "DESCRIP", "OBJ CODE", "OBJ DESC", "ORGANIZ", "PAID?", "ADDR",
                      "CITY", "STATE", "ZIP", "INV#", "STATUS DESC", "DISPO CD", "DISPO DESC",
                      "CAUSE DESC", "SHORT_DESC", "COUNCIL_APPROVAL_DT")



# Steps taken:
# 1. Clean up date and amount variables; standardize variable names to match agreed upon format, generate empty vars for unavailable vars
# 2. Filter to cases we care about
# 3. Checks

# calendar_year/fiscal_year -> calendar year, pulled from PMT DATE
# city	-> generated 
# state	-> generated 
# incident_date	-> don't have, generate NA var
# date_filed	-> don't have, generate NA var
# closed_date	 -> PMT DATE
# amount_awarded -> AMOUNT
# other_expenses	-> don't have generate NA var
# collection		-> don't have generate NA var
# total_incurred		-> don't have generate NA var
# case_outcome		-> DISPO DESC
# docket_number		-> don't have generate NA var
# claim_number		-> don't have generate NA var
# court		-> don't have generate NA var
# plaintiff_name		-> don't have generate NA var
# matter_name	-> TITLE
# plaintiff_attorney		-> don't have generate NA var
# location		-> don't have generate NA var
# summary_allegations (separated by ;) 	-> CAUSE DESC; if there is also info in SHORT DESC, I appended that as well.


detroit <- widedf %>%
  mutate(closed_date = mdy(`PMT DATE`),
         calendar_year = year(closed_date),
         amount_awarded = as.numeric(AMOUNT),
         matter_name = TITLE,
         case_outcome = `DISPO DESC`,
         location = ADDR,
         status = `STATUS DESC`,
         city = "Detroit",
         state = "MI",
         summary_allegations = ifelse(SHORT_DESC != "", paste0(`CAUSE DESC`, ": ", SHORT_DESC), `CAUSE DESC`),
         incident_date = NA,
         incident_year = NA,
         filed_date = NA,
         filed_year = NA,
         other_expenses = NA,
         collection = NA,
         total_incurred = NA,
         docket_number = NA,
         claim_number = `INV#`,
         court = NA,
         plaintiff_name = NA,
         plaintiff_attorney = NA)
  


# Filter?
allegations <- detroit %>% group_by(summary_allegations) %>% count() %>% arrange(-n)

# Filtering out the following: FOIA related, vehicle related
detroit_filter <- allegations %>% filter(grepl("foia",ignore.case = TRUE,summary_allegations) | 
                                       grepl("vehicle",ignore.case = TRUE, summary_allegations) |
                                         grepl("auto",ignore.case = TRUE, summary_allegations) |
                                         grepl("breach of contract",ignore.case = TRUE, summary_allegations) |
                                         grepl("other collision accidents",ignore.case = TRUE, summary_allegations)|
                                         grepl("non-city veh",ignore.case = TRUE, summary_allegations) |
                                         grepl("pension",ignore.case = TRUE, summary_allegations))


# all look good to remove from data
detroit <- detroit %>% filter(!grepl("foia",ignore.case = TRUE,summary_allegations) & 
                              !grepl("vehicle",ignore.case = TRUE, summary_allegations) &
                                !grepl("Auto accident with P.O. in Detroit",ignore.case = TRUE, summary_allegations) &
                                !grepl("breach of contract",ignore.case = TRUE, summary_allegations) &
                                !grepl("other collision accidents",ignore.case = TRUE, summary_allegations) &
                                !grepl("non-city veh",ignore.case = TRUE, summary_allegations) &
                                !grepl("pension",ignore.case = TRUE, summary_allegations)
                                )

# repeat
allegations <- detroit %>% group_by(summary_allegations) %>% count() %>% arrange(-n)

# some ambiguous ones remain 1 case for each, so about 20 cases :
# General Office: 999 - General Office
# Missing or damaged prop incidental to
# 1983 case
# : ADR-Claim 684, Bankruptcy Court Order, Chapter 9,
# : County of Macomb sues for payment
# : Former DPD employee seeks PIP benefit payment
# : Laneece Cobb, P. R. of the Est. Of Jefferey Theriot
# : Plaintiff claims 4th Amendment Violations
# : Stacy Ortiiz Bankr Claim - alleged delay in trans to hosp
# : Violation of 42 USC Sec. 1983/1985
# Conversion of plaintiff's prop (theft): Return of Plantiff's Property
# General Office
# Injunctive relief: Complaint for Injunctive Relief
# LegalEdge Incident Codes
# LegalEdge Incident Codes: Agreement Resolving Claim !752, Sec.1983 (convenience claim)
# LegalEdge Incident Codes: Ist Amendment civil rights action
# Quiet title action: PZ03 - Quiet title action
# request for injunctive relief
# Slander, libel, defamation of character: P05 - Slander, libel, defamation of character
# Uncoded: Stipulation for Entry of an Order -Unsecured Convenience Claim #1854

detroit <- detroit %>% select(calendar_year,
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
# Time period of closed date? 
summary(detroit$closed_date) # 1/2010 - 12/2019

# time period of calendar year or incident year or filed year if closed date missing
summary(detroit$calendar_year) # Min 2010, max 2019

# Perfect duplicates? 0 duplicates
nrow(detroit %>%  group_by_all() %>% filter(n()>1))

# Duplicates on identifying variables
# No docket/  claim number, but the titles contain case names / docket numbers (sometimes)
# Looks like there are multiple entries for some TITLEs 
detroit %>% 
  group_by(matter_name) %>%
  tally() %>%
  filter(n>1) %>% 
  arrange(-n) # 21 duplicated titles

dups <- detroit %>% 
  group_by(matter_name) %>%
  filter(n()>1)

nrow(detroit %>% group_by(matter_name) %>% filter(n()>1)) # 45 duplicates on TITLE
nrow(detroit %>% group_by(matter_name,closed_date) %>% filter(n()>1)) # 16 duplicates on TITLE and closed date
nrow(detroit %>% group_by(matter_name,closed_date, amount_awarded) %>% filter(n()>1)) # 10 duplicates on TITLE and closed date and amount awarded
nrow(detroit %>% group_by(matter_name,closed_date,claim_number) %>% filter(n()>1)) # 0 duplicates on TITLE and closed date and invoice #

dups <- detroit %>% group_by(matter_name,closed_date, amount_awarded) %>% filter(n()>1)
  
# On spot checking some of these, duplicates, they definitely look like multiple payments over different
# times / invoices. E.g. the Aiyana Jones settlement for $8.25m is split into two payments paid out a few 
# months apart

# What's filtered out? what's left that ambiguous?
# see above

# Missing ness of variables
print(paste("There are",nrow(detroit %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(detroit %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(detroit %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(detroit %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(detroit %>% filter(matter_name=="")),"rows missing matter name"))
print(paste("There are",nrow(detroit %>% filter(claim_number=="")),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(detroit))

print("Total amount awarded")
sum(detroit$amount_awarded)

# Check WSJ (they have $28.5m for 2015-2020, we have $28m for 2015-2019)
detroit_2015_2020 <- detroit %>% filter(closed_date>="2015-01-01")
sum(detroit_2015_2020$amount_awarded)

write.csv(detroit,paste0(out_data_path,"detroit_edited.csv"), na = "",row.names = FALSE)


