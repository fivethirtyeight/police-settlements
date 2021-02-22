# Paterson, NJ

*This data is a part of a FiveThirtyEight/The Marshall Project effort to collect comprehensive data on police misconduct settlements from 2010-19. See the [data dictionary here](../) and [the story here](https://fivethirtyeight.com/features/police-misconduct-costs-cities-millions-every-year-but-thats-where-the-accountability-ends).*

We received two separate files, one of which was a subset of the other, containing incidents that took place between 2010 and 2019. The data was queried on date of loss, rather than closed date; from our understanding of the data,  the date closed refers to the date on which the final payment was made, not the day the settlement was reached (so it is missing for settlements which have been partially paid out). The data provides information on payments still outstanding, which we disregard, using the total incurred in each case as the settlement amount. In four cases, the total amount incurred did not match across the two files. In those cases, we used the smaller amount to be conservative.
 
The field "summary_allegations" combines information originally found in the columns “Loss Causation,” "Liability type," and "Loss description." We filtered out car accidents and trip-and-falls. The data includes claims as well as settlements, which we indicated. 
