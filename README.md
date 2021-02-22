# Police misconduct settlements

This repo contains the data behind the story [Police Misconduct Costs Cities Millions Every Year, But That's Where The Accountability Ends](https://fivethirtyeight.com/features/police-misconduct-costs-cities-millions-every-year-but-thats-where-the-accountability-ends).

## Words of caution

As we describe in [an accompanying article](https://fivethirtyeight.com/features/police-misconduct-costs-cities-millions-every-year-but-thats-where-the-accountability-ends), this data has major issues. It constitutes our best guess at the amount of money that was paid out as settlements for police misconduct from 2010-19 (or for the range otherwise provided), but different cities have different ways of collecting, storing and categorizing such settlements. As a result, this data **should not be compared across cities**. We have no way of knowing (or checking) whether the kinds of misconduct this covers are comparable across cities. (For example, city officials in Boston likely interpreted our request more narrowly than those in New York City.) Descriptions of the types of misconduct are included where provided, but are also not comparable across cities. For this reason, while we are making all the data we obtained available, we are not providing it in an easy-to-use, collated data set. This is because **we don’t want you to use it this way**!

Moreover, while records within the same city might be more comparable across time, this is also not encouraged. Data storage may have changed in this time period, as have the humans who entered the information into it, and much of the information entered is subject to human categorization based on judgement calls that cannot be assumed to have stayed constant over time. And even if the data collection had stayed the same for the entire period, other factors (such as whether cities are likely to settle in response to police misconduct and if so, for how much) may have changed such that the settlements themselves are a poor measure of police misconduct. For these reasons, we advise extreme caution when comparing this data across time and within a city.

## Folder structure

Each city folder contains:
* an R script that cleans the data
* an `original/` folder that contains original data as provided by the city. 
* an `intermediate/` folder - if we did any manual data cleaning. This folder contains data after the manual data cleaning step (e.g. an `xlsx` sheet into which data from a pdf was copied).
* a `final/` folder that contains data output of the cleaning script, with columns described by the data dictionary below.

## Data dictionary

Variable name | Definition
------------- | ----------
incident_date | Date on which incident took place
incident_year | Pulled from filed_year
filed_date | Date claim or lawsuit was filed
filed_year | Pulled from filed_date
closed_date | Date at which settlement was reached OR paid (depending on what was provided)
calendar_year | Pulled from settlement date or closed_date
city | City name
state | State abbreviation
amount_awarded | Amount awarded to claimant in the settlement
other_expenses | Additional expenses, such as legal fees (e.g. in Charleston, North Charleston), when available
total_incurred | Total expenses: amount_awarded + other_expenses
collection | Whether money was collected?
case_outcome | Case status as of the date the data was collected, e.g. whether a case was settled, went to jury, or is still pending
docket_number | Case docket number, when available
claim_number | Claim number, when available
court | Court in which the settlement was reached, when available
plaintiff_name | Name of plaintiff/claimant
matter_name | Case name (generally of the form "Plaintiff v Defendant")
plaintiff_attorney | Legal representation of plaintiff
location | Location at which the incident happened, when available
summary_allegations | Description of allegations -- sometimes aggregated into categories, sometimes very detailed. We retained as much detail as was available.	Separated by ";"
claim_or_lawsuit | Indicator of whether the entry was a claim or a lawsuit, when available
defendant | Name of defendant(s), when available. Sometimes a list of police officers was provided separately.

## FOIA text

This is the text of the FOIA request we sent to each city:

> Dear Records Officer,
>
> Pursuant to all laws and traditions governing the release of public records in your jurisdiction, I am requesting records related to any and all civil lawsuits brought forth against the [CITY] Police Department or [CITY] PD law enforcement officials that resulted in a monetary legal settlement between the period of January 1, 2010 and December 31, 2019.
>
> Specifically, I am requesting any and all records concerning each legal settlement, including:
>
> * Name(s) of plaintiff(s)
> * Name(s) of officer(s) involved
> * Name of court and docket number
> * Date of incident at issue
> * Location of incident at issue
> * Date lawsuit filed
> * Date lawsuit resolved
> * Type of misconduct
> * Summary of allegations
> * Settlement amount
> * Name of plaintiff’s attorney (or pro se status if plaintiff represented him/herself)
>
> Please provide the records in an electronic spreadsheet. In addition, I request you send along any data dictionaries that accompany these records.
>
> Please waive any applicable fees. I am a representative of the news media through FiveThirtyEight and The Marshall Project. Release of this information is in the public interest because it will help the public understand how taxpayer dollars are used to compensate victims of police misconduct.
>
> If my request is denied in whole or part, I ask that you justify all deletions and denials by reference to specific exemptions of the laws and traditions in your state. I also request that you release all segregable portions of the otherwise exempt material. I reserve the right to appeal your decision to withhold any information or to deny any waiver of fees.
>
> As I am making this request as a journalist and this information is of timely value, I would appreciate your communication through telephone or email if you have any questions regarding this request. I can be reached by email at [email] and by phone at [phone].
>
> Thank you for your assistance.


## List of cities FOIA'd and what happened with each

City | Outcome | Time period
---- | ------- | -----------
Atlanta, GA | Received data | 2015-2020
Baltimore, MD | Received data that was not responsive to FOIA | 2015-2020
Baton Rouge, LA | Received data | 2010-2019
Birmingham, AL | Did not receive data
Boston, MA | Received data | 2010-2019
Bridgeport, CT | Did not receive data
Buffalo, NY | Did not receive data
Cambridge, MA | Received data | 2010-2019
Charleston, SC | Received data | 2010-2019
Chattanooga, TN | Did not receive data
Chicago, IL | Received data | 2010-2019
Cincinnati, OH | Received data | 2010-2020
Cleveland, OH | Received data | 2010-2020
Columbia, SC | Received data | 2010-2019
Dayton, OH | Did not receive data
Detroit, MI | Received data | 2010-2019
Elizabeth, NJ | Did not receive data
Fort Lauderdale, FL | Received data | 2011-2019
Hartford, CT | Received data, unusable
Indianapolis, IN | Received data | 2010-2019
Jersey City, NJ | Received data, unusable
Kansas City, MO | Received data, unusable
Little Rock, AR | Received data | 2010-2019
Los Angeles, CA | Received data | 2010-2019
Memphis, TN | Received data | 2013-2019
Miami, FL | Received data | 2010-2020
Milwaukee, WI | Received data | 2010-2019
New Haven, CT | Did not receive data
New Orleans, LA | Received data | 2010-2019
New York City, NY | Received data | 2010-2019
Newark, NJ | Did not receive data
Norfolk, VA | Did not receive data
North Charleston, SC | Received data | 2010-2019
Orlando, FL | Received data | 2010-2018
Paterson, NJ | Received data | 2010-2019
Philadelphia, PA | Received data | 2009-2019
Pittsburgh, PA | Did not receive data
Richmond, VA | Received data | 2010-2019
Roanoke, VA | Received data | 2010-2019
Rochester, NY | Did not receive data
San Francisco, CA | Received data | 2010-2019
Shreveport, LA | Received data, unusable
Springfield, MA | Received data | 2006-2020
St Louis, MO | Received data | 2015-2019
Syracuse, NY | Did not receive data
Tuscaloosa, AL | Did not receive data
Washington, DC | Received data | 2010-2019
Waterbury, CT | Received data | 2011-2019
West Palm Beach, FL | Did not receive data
Yonkers, NY | Did not receive data
