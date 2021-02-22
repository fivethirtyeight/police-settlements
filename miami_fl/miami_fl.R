# This script takes the raw csv on settlements received from Miami and outputs a csv
# with variables cleaned and standardized

# Original code written by Laura Bronner at FiveThirtyEight
# Original date 9/3/2020

# setup
setwd(dirname(this.dir()))
source("setup.R")
raw_data_path <- get_inpath(this.dir())
out_data_path <- get_outpath(this.dir())

miami <- read_csv(paste0(raw_data_path,"tabula-Police Settled Matters 1-1-10 to7-1-20.csv"))
miami <- miami %>%
  mutate(new = ifelse(grepl("Matter Type", `Matter ID Matter Description`), 1, 0),
         new = ifelse(grepl("MatterType", `Matter ID Matter Description`), 1, new),
         id = cumsum(new),
         `Matter ID Matter Description` = ifelse(is.na(`Matter ID Matter Description`), "", `Matter ID Matter Description`))

miami <- miami %>% 
  filter(`Matter ID Matter Description` != "Matter ID Matter Description",
         `Matter ID Matter Description` != "Matter IDMatter DescriptionAttorney",
         `Matter ID Matter Description` != "Matter ID",
         `Matter ID Matter Description` != "Total Matters:")

miami <- miami %>% 
  group_by(id) %>%
  mutate(nrows = n())

table(miami$nrows)


out <- split( miami , f = miami$id )

processtbl <- function(x){
  test <- as.matrix(x)
  newdf <- tibble(
    matter_id = test[2,1],
    matter_type = test[1,1],
    attorney = test[1,3],
    status = ifelse(is.na(test[1,5]), test[1,4], test[1,5]),
    closed_date = test[1,7],
    case_number = test[3,3],
    opened_date = ifelse(is.na(test[2,5]), test[2,4], test[2,5]),
    city_paid_out = test[2,7],
    case_outcome = test[3,7],
    city_collected = test[4,7],
    served_date = test[4,3],
    matter_name = ifelse(nrow(x) == 5, paste0(test[3,1], " ", test[4,1], " ", test[5,1]),
                         ifelse(nrow(x) == 6, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1]),
                                ifelse(nrow(x) == 7, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1], " ", test[7,1]),
                                       ifelse(nrow(x)==8, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1], " ", test[7,1], " ", test[8,1]),
                                              ifelse(nrow(x)==9, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1], " ", test[7,1], " ", test[8,1], " ", test[9,1]),
                                                     ifelse(nrow(x)==10, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1], " ", test[7,1], " ", test[8,1], " ", test[9,1], " ", test[10,1]),
                                                            ifelse(nrow(x)==11, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1], " ", test[7,1], " ", test[8,1], " ", 
                                                                                       test[9,1], " ", test[10,1], " ", test[11,1]),
                                                                   ifelse(nrow(x)==12, paste0(test[3,1], " ", test[4,1], " ", test[5,1], " ", test[6,1], " ", test[7,1], " ", test[8,1], " ", 
                                                                                              test[9,1], " ", test[10,1], " ", test[11,1], " ", test[12,1])))))))))
  )
}
miami1 = lapply(out, processtbl) 
miami1 = do.call("bind_rows", miami1)

miami1 <- miami1 %>%
  mutate(attorney = gsub(" Status:", "", attorney))

# Some pages are missing, will fill in manually:
# Page 15
# Check
h <- miami1 %>% filter(matter_id == "11-1708") # Doesn't exist
page15 <- tibble(
  matter_id = c("11-1708", "11-1711", "11-1769"),
  matter_type = c("Matter Type: MVA - other", "Matter Type: MVA - other", "Matter Type: MVA - other"),
  attorney = c("Richard S. Otruba", "Christopher A. Green", "Henry J. Hunnefeld"),
  status = rep("Closed", 3),
  closed_date = c("1/29/2013", "1/7/2014", "8/8/2018"),
  case_number = c("Court Case No.: 11-26124 CA 11", "Court Case No.: 11-25294 CA 11", "Court Case No.: 11-28117 CA 11"),
  opened_date = c("8/29/2011", "8/29/2011", "9/8/2011"),
  city_paid_out = c("42,500.00", "10,000.00", "49,900.00"),
  case_outcome = rep("Settled", 3),
  city_collected = rep("0.00", 3),
  served_date = c("Date Served: 8/24/2011", "Date Served: 8/22/2011", "Date Served: 9/6/2011"),
  matter_name = c("BOGDANOWITZ, Jason vs. CITY OF MIAMI, WESLYNE DEBUISSON and PROGRESSIVE SELECT INSURANCE CO.",
                  "ROBERT, Steve and GORDON, Shorraine vs. CITY OF MIAMI POLICE DEPARTMENT aka MIAMI POLICE DEPARTMENT",
                  "MARTINEZ, Alicia and Jorge vs. CITY OF MIAMI")
)

# Page 28
h <- miami1 %>% filter(matter_id == "14-1260") # Doesn't exist
page28 <- tibble(
  matter_id = c("14-1260", "14-1433", "14-1494"),
  matter_type = c("Matter Type: False Arrest w/ Excessive Force", "Matter Type: ", "Matter Type: MVA - other"),
  attorney = c("Christopher A. Green", "Henry J. Hunnefeld", "Christopher A. Green"),
  status = rep("Closed", 3),
  closed_date = c("3/17/2015", "3/15/2017", "6/11/2019"),
  case_number = c("Court Case No.: 14-CV-21663-Altonaga/O'Sullivan", "Court Case No.: 14-14187 CA 34", "Court Case No.: 14-12146 CA 27"),
  opened_date = c("5/19/2014", "6/9/2014", "6/17/2014"),
  city_paid_out = c("80,000.00", "19,750.00", "24,900.00"),
  case_outcome = rep("Settled", 3),
  city_collected = rep("0.00", 3),
  served_date = c("Date Served: 5/15/2014", "Date Served: 6/5/2014", "Date Served: 6/10/2014"),
  matter_name = c("SANON, Polini vs. CITY OF MIAMI, a Municipal Corporation in Dade County, State of Florida; Officer ADRIAN RODRIGUEZ, an individual, Officer GEORGE DIAZ, an individual 
                  and Officer ALEXIS PEREZ, an individual",
                  "ZAPATA, Ufredo vs. CITY OF MIAMI POLICE DEPARTMENT, ET AL.",
                  "COLEMAN, Marvin vs. CITY OF MIAMI and HERZEN BAIN"))

# Page 40
page40 <- tibble(
  matter_id = c("17-1680.002Appeal", "17-1690", "17-1828"),
  matter_type = c("Matter Type: ", "Matter Type: False Arrest w/ Excessive Force", "Matter Type: False Arrest w/ Excessive Force"),
  attorney = c("Eric J. Eves", "Douglas A. Harrison", "Eric J. Eves"),
  status = rep("Closed", 3),
  closed_date = c("4/7/2020", "8/3/2018", "10/30/2018"),
  case_number = c("Court Case No.: 19-13023-B", "Court Case No.: 1:17-cv-22044-MGC", "Court Case No.: 16-CV-24904-KMW"),
  opened_date = c("8/15/2019", "6/15/2017", "6/27/2017"),
  city_paid_out = c("300,000.00", "50,000.00", "17,500.00"),
  case_outcome = rep("Settled", 3),
  city_collected = rep("0.00", 3),
  served_date = c("Date Served: ", "Date Served: 6/14/2017", "Date Served: "),
  matter_name = c("SEVERE, Francois as personal representative of the Estate of Fritz Severe vs. CITY OF MIAMI",
                  "GELLINEAU, Ryan vs. STANLEY MIKE, individually and in an official capacity as a police officer for the City of Miami, and the CITY OF MIAMI",
                  "ALLEN, Richard Dean vs. OFFICER JAVIER GONZALES, IN HIS OFFICIAL AND INDIVIDUAL CAPACITIES, CITY OF MIAMI POLICE DEPARTMENT, and CITY OF MIAMI"))

# Page 47
page47 <- tibble(
  matter_id = c("LW-0300170", "LW-0300402", "LW-9100038"),
  matter_type = c("Matter Type: ", "Matter Type: ", "Matter Type: "),
  attorney = rep("Iliana Forte", 3),
  status = rep("Closed", 3),
  closed_date = c("2/2/2010", "8/23/2010", "5/1/2013"),
  case_number = c("Court Case No.: 03-016081CMH", "Court Case No.: 02-041341GCC", "Court Case No.: 94-009700GCC"),
  opened_date = c("5/1/2003", "11/7/2003", "1/1/1991"),
  city_paid_out = c("39,300.65", "3,683.36", "23,000.00"),
  case_outcome = rep("Settled", 3),
  city_collected = rep("0.00", 3),
  served_date = c("Date Served: 5/1/2003", "Date Served: 9/9/1999", "Date Served: 1/1/1991"),
  matter_name = c("GAINES, Jimmie vs. CITY OF MIAMI",
                  "HERNANDEZ, Madelyn vs. CITY OF MIAMI",
                  "JAFFIE, Raymond vs. CITY OF MIAMI"))


# Remove messy ones:
miami1 <- miami1 %>% 
  tidylog::filter(grepl("-", matter_id)) %>% # This removes the 9 entries without a matter ID number, with attorney names there instead
  tidylog::filter(!matter_id %in% c("LW-0300170", "LW-0300402", "LW-9100038")) # This removes the 3 entries from the last page which we manually override

miami2 <- miami1 %>% 
  bind_rows(page15, page28, page40, page47)
# And we're back to 178 rows

head(miami2)
miami3 <- miami2 %>%
  mutate(matter_type = gsub("Matter Type:", "", matter_type),
         case_number = gsub("Court Case No.: ", "", case_number),
         served_date = gsub("Date Served:", "", served_date),
         matter_type = str_trim(matter_type),
         served_date = str_trim(served_date),
         closed_date = mdy(closed_date),
         opened_date = mdy(opened_date),
         served_date = mdy(served_date),
         amount_awarded = as.numeric(gsub(",", "", city_paid_out)))

# Filter
# Exclusions based on details in the other pdf
exclude_list <- c(
  "06-1296", # Employment case
  "06-2426", # Labeled "Workers' Compensation"
  "07-2681", # Unlabeled, but it's a MVA case
  "08-2101", # Labeled "Workers' Compensation"
  "08-2722", # Unlabeled, but sounds like a car accident
  "09-4083", # Labeled "Workers' Compensation"
  "09-4161", # Labeled "Workers' Compensation"
  "10-811", # Labeled Labor/Employment
  "11-66", # Labeled Worker's Compensation
  "11-1074", # Labeled Worker's Compensation
  "12-996", # Labeled worker's compensation
  "12-1509", # Labeled subrogation, description is of an MVA case
  "12-1564", # Labeled labor/employment
  "13-1958", # Labor/employment
  "14-528", # worker's compensation
  "14-772", # worker's compensation
  "16-3366", # labeled subrogation, description is of an MVA case
  "17-2889", # worker's compensation
  "18-305", # Public Records-Sunshine-Election -- seems unrelated?
  "18-943", # worker's compensation
  "18-2187", # public records - sunshine - election
  "LL-0400246", # labor/employment
  "LT-9900295", # labeled "special assessments - fees" - These two consolidated cases were a class action constitutional challenge to the City's Vehicle Impoundment ("VIP") Ordinance.
  "LW-0100484", # worker's compensation,
  "LW-0200531", # worker's compensation
  "LW-0300170", # worker's compensation
  "LW-0300402", # worker's compensation
  "LW-9100038" # worker's compensation
)



types <- miami3 %>% group_by(matter_type) %>% count() %>% arrange(-n)

# going to filter out MVA - Other (the ones that have summaries in the longer document definitely seem like cases we want to exclude)
# Public Records
# MVA - Bicycle/Pedestrian

# 91 rows removed
miami3 <- miami3 %>% 
  filter(!matter_id %in% exclude_list) %>% 
  filter(!matter_type %in% c("MVA - other", "MVA - Bicycle/Pedestrian","Public Records"))

# replace matter types for ones we know from longer document
miami3 <- miami3 %>% 
  mutate(summary_allegations = ifelse(matter_type=="","Civil Rights",matter_type),
         summary_allegations = case_when(
           matter_id == "08-1719" ~ "Torts",
           matter_id == "08-2082" ~ "Torts",
           matter_id == "14-2855" ~ "Torts - Excessive Force",
           matter_id == "12-1390" ~ "Torts",
           matter_id == "12-1804" ~ "Torts",
           matter_id == "13-1221" ~ "Torts",
           matter_id == "07-2196" ~ "Torts",
           matter_id == "15-194" ~ "Torts",
           matter_id == "15-2155" ~ "Torts",
           matter_id == "17-3003" ~ "Torts",
           matter_id == "LT-0400336" ~ "Subrogation",
           matter_id == "19-1546" ~ "Subrogation",
           matter_id == "14-1433" ~ "Torts",
           TRUE ~ summary_allegations
         ))

types2 <- miami3 %>% group_by(summary_allegations) %>% count() %>% arrange(-n)

# Keeping these because ambiguous
# Settlement Authority
# Request for Investigation
# MVA - Police Chase
# Demolition of Property
# Discrimination Charges
# First Amendment w/out Demonstration
# First Amendment w/ Demonstration

# Keep the below - unlabeled in dataset, but have descriptions in the longer document that 
# are ambiguous or suggest they should be kept
# 08-1719 - torts, other. payer is contender fishing team? SOunds like this was a police boat collision https://law.justia.com/cases/federal/appellate-courts/ca11/10-10454/201010454-2011-03-27.html
# 08-2082 Torts - other
# 10-1112 Labeled "other" but also contains the label "civil rights" 
# 10-1715 Labeled "other" but also contains the label "civil rights" but sounds like maybe this was someone who also worked at the police?
# 12-1565 Labeled "Civil Rights"
# 13-825 Labeled "Civil Rights"
# 13-1433 labeled Civil Rights
# 13-1978 labeled civil rights
# 13-2588 labeled civil rights
# 13-2619 labeled civil rights
# 14-78 labeled civil rights
# 14-211 civil rights
# 14-2003 civil rights
# 14-2855 "torts" but description describes excessive force. Going to keep all torts because of this
# "12-1390", # Labeled Torts
# "12-1804", # Labeled torts
# "13-1221", # Labeled Torts
# "07-2196", # labeled "Torts"
# 14-2866 Civil Rights
# 15-194 Torts - description mentions doing a worker's comp conflict but doesnt explicitly describe the case
# 15-2155 Torts - claims bill
# 16-1816 - civil rights
# 17-994 - civil rights
# 17-3003 - torts
# 18-380 - Civil rights
# 19-717 - "commercial" ?
# LT-0400336 - subrogation, but the client was Ohio Casualty Insurance?
# "19-1546", # subrogation, client is united automobile insurance so seems like MVA but not sure
# 14-1433 - torts
# 17-1680.002Appeal - civil right appeal

miami <- miami3 %>%
  rename(docket_number = case_number,
         filed_date = served_date,
         plaintiff_attorney = attorney,
         claim_number = matter_id,
         collection = city_collected) %>% # not sure if this is quite claim number, but is a unique ID
  mutate(city = "Miami",
         state = "FL",
         calendar_year = year(closed_date),
         filed_year = year(filed_date),
         plaintiff_name = NA,
         incident_date = NA,
         incident_year = NA,
         other_expenses = NA,
         total_incurred = NA,
         court = NA,
         location = NA)

miami <- miami %>% select(calendar_year,
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
                                  opened_date)


# CHECKS
# Time period of closed date? 
summary(miami$closed_date) # 8/2010 - 5/2020

# Perfect dups? None
nrow(miami %>% group_by_all() %>% filter(n()>1))

# Duplicates by claim number (matter ID)? none
nrow(miami %>% group_by(claim_number) %>% filter(n()>1))

# Duplicates by docket number? 2, but have different opened date, matter ID, etc - seem like related
# cases but different payments, so leaving as is
nrow(miami %>% group_by(docket_number) %>% filter(n()>1))
dups <- miami %>% group_by(docket_number) %>% filter(n()>1)

# What's filtered out? what's left that ambiguous?
# Filtering done from PDF, here's what remains:
table(miami$summary_allegations)

# Missing ness of variables
print(paste("There are",nrow(miami %>% filter(is.na(closed_date))),"rows missing closed date"))
print(paste("There are",nrow(miami %>% filter(is.na(calendar_year))),"rows missing calendar year"))
print(paste("There are",nrow(miami %>% filter(is.na(amount_awarded))),"rows missing amount awarded"))
print(paste("There are",nrow(miami %>% filter(amount_awarded==0)),"rows with amount awarded = 0"))
print(paste("There are",nrow(miami %>% filter(is.na(docket_number))),"rows missing docket number"))

# count cases
print("Total number of cases")
print(nrow(miami))

print("Total amount awarded")
sum(miami$amount_awarded)


write.csv(miami,paste0(out_data_path,"miami_edited.csv"), na = "",row.names = FALSE)


