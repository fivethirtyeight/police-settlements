# Chicago, IL

*This data is a part of a FiveThirtyEight/The Marshall Project effort to collect comprehensive data on police misconduct settlements from 2010-19. See the [data dictionary here](../) and [the story here](https://fivethirtyeight.com/features/police-misconduct-costs-cities-millions-every-year-but-thats-where-the-accountability-ends).*

We received data on settlements reached by the Chicago Police Department, Chicago Police Board and the City of Chicago from 2010 through 2019. We cleaned the data to remove perfect duplicates. We kept in duplicates that were not identical to other records and met these criteria:

* "Description" differs (e.g. settlement paid vs court costs)
* Closed date differs (amount paid out over multiple payments)
* Everything is the same about amount awarded differs (multiple payments made for some other reason)

We also renamed some original fields: 

* “Description” became “case_outcome”
* “Category” became “summary_allegations”
* “Caption” became “matter_name”
* “Description” became “case_outcome”
* “Disposition date” became “closed_date”
* “Phase” became “status”

We filtered out all allegations that did not have “Dispute:General:Police Matters” in the category columns. This does filter out some cases labeled as "Torts," including some that might be ambiguous. We included a handful of cases that were labeled “open” or where the police were noted as the “plaintiff” but had settlement amounts noted. 
