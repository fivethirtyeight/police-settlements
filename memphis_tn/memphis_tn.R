# This script takes the raw csv on settlements received from Memphis and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Code updated by Damini Sharma at The Marshall Project on 11/23/2020

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

# Read in files and clean up the multiple tables within each, and standardize column names
memphis13 <- read_excel(paste0(raw_data_path,"Police 2013  -  Settlement Issued.xlsx"), skip= 2)
memphis13 <- memphis13[-c(14:16, 31:33, 39:43),] %>%
  mutate(year = 2013)

memphis14 <- read_excel(paste0(raw_data_path,"Police 2014 - Settlement Issued.xlsx"), skip= 2)
memphis14 <- memphis14[-c(13:15, 26:28, 39:43),] %>%
  mutate(year = 2014)
colnames(memphis14) <- colnames(memphis13)

memphis15 <- read_excel(paste0(raw_data_path,"Police 2015 - Settlement Issued.xlsx"), skip= 2)
memphis15 <- memphis15[-c(11:13, 24:26, 35:37, 41:43),] %>%
  mutate(year = 2015)
colnames(memphis15) <- colnames(memphis13)

memphis16 <- read_excel(paste0(raw_data_path,"Police 2016 - Settlement Issued.xlsx"))
colnames(memphis16) <- colnames(memphis13)
memphis16 <- memphis16[-c(1, 3:7, 17:19, 30:32),] %>%
  mutate(year = 2016)

memphis17 <- read_excel(paste0(raw_data_path,"Police 2017 -  Settlement Issued.xlsx"), skip= 2)
memphis17 <- memphis17[-c(12:14, 26:28, 42:44, 56:58, 63:64),] %>%
  mutate(year = 2017)
colnames(memphis17) <- colnames(memphis13)

memphis18 <- read_excel(paste0(raw_data_path,"Police 2018 -  Settlement Issued.xlsx"), skip= 2)
memphis18 <- memphis18[-c(10:12, 23:25, 37:39, 51:53, 60:62),] %>%
  mutate(year = 2018)
colnames(memphis18) <- colnames(memphis13)

memphis19 <- read_excel(paste0(raw_data_path,"Police 2019  -  Settlement Issued.xlsx"), skip= 2)
memphis19 <- memphis19[-c(11:13, 25:27, 39:41, 48:63),] %>%
  mutate(year = 2019)
colnames(memphis19) <- colnames(memphis13)


# Bind together
memphis <- bind_rows(memphis13, memphis14, memphis15, memphis16, memphis17, memphis18, memphis19)


# Rename
memphis <- memphis %>%
  # Turn this variable into plaintiff and matter name... it actually also has plaintiff attorney in it
  separate(`Case/Matter Style`, into = c("plaintiff_name", "matter_name"), sep = " [R|r]e: ") %>%
  rename(claim_number = `City File #`, # I think? Unclear - make a note of it
         calendar_year = year,
         case_outcome = `Type of Payment`
         ) %>%
  dplyr::mutate(plaintiff_attorney = "See plaintiff_name",
         city = "Memphis",
         state = "TN",
         amount_awarded = as.numeric(gsub(",", "", `Amount Paid`)),
         closed_date = NA,
         incident_date = NA,
         incident_year = NA,
         filed_date = NA,
         filed_year = NA,
         summary_allegations = NA,
         other_expenses = NA,
         court = NA,
         total_incurred = NA,
         collection = NA,
         location = NA,
         docket_number = NA
         ) %>%
  select(-Division, -`Amount Paid`)


# CHECKS
# Time period of closed date? NA
# time period of calendar year or incident year or filed year if closed date missing
summary(memphis$calendar_year) # Min 2013, max 2019

# Perfect duplicates? 0 duplicates
nrow(memphis %>% group_by_all() %>% filter(n()>1))

# Check for duplicates
checks <- memphis %>% group_by(claim_number) %>% mutate(n = n()) %>% filter(n>1) %>% arrange(claim_number)
nrow(memphis %>% group_by(claim_number) %>% filter(n()>1)) # 35 duplicates
nrow(memphis %>% group_by(claim_number,plaintiff_name) %>% filter(n()>1)) # 2 duplicates
nrow(memphis %>% group_by(claim_number,case_outcome) %>% filter(n()>1)) # 22 duplicates
nrow(memphis %>% group_by(claim_number,plaintiff_name,calendar_year) %>% filter(n()>1)) # 0 duplicates
# Sometimes the payment seems to be split into settlement and judgement (attorney fees & expenses). 
# Sometimes there are multiple payments.

# What's filtered out? what's left that ambiguous?
# In the 2019 file, there is one row with a payment amount of $0, and a few missing values -- maybe not decided yet? 
# Anyway this is all I'll filter out. We don't know the allegations so can't filter on those.
memphis <- memphis %>%
  tidylog::filter(!is.na(amount_awarded), amount_awarded != 0)

# Missing ness of variables
print(paste("There are",nrow(memphis %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(memphis %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(memphis %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(memphis %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(memphis %>% filter(is.na(docket_number))),"rows missing docket number"))
print(paste("There are",nrow(memphis %>% filter(is.na(claim_number))),"rows missing claim number"))

# count cases
print("Total number of cases")
print(nrow(memphis))

print("Total amount awarded")
sum(memphis$amount_awarded)


write.csv(memphis,paste0(out_data_path,"memphis_edited.csv"), na = "",row.names = FALSE)

