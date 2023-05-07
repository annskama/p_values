# p_value_distribution

Distribution of p-values reported in eight major psychology journals in 1985-2013.

The original file with the data was imported from [here](https://osf.io/gdr4q).

## Codebook

**my_data** content - 19 originally downloaded variables:

-   N - order number of a record

-   Source - order number of a paper, from which data was extracted

-   Statistic, Value, df1, df2 - type and value of statistics reported, relevant degrees of freedom

-   Test_Comparison - the way how statistics value was reported ("=" means as an exact number, "\>" or "\<" means a range above or below Value)

-   Reported_Comparison - the way how p-value was reported ("=" means as an exact number, "\>" or "\<" means a range above or below Reported_P\_Value)

-   Reported_P\_Value - p-value reported (or a threshold for it)

-   Computed - p-value recalculated based on the reported statistics value and degrees of freedom (recalculation is performed by [Nuijten et al](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5101263/) using the automated procedure [statcheck](https://CRAN.R-project.org/package=statcheck).

-   Raw - raw data extracted from a paper

-   Error, DecisionError, OneTail, OneTailedInTxt and CopyPaste - tests not used in this projects, if you are interested 

-   APAfactor, journal (original name journals_jour_) and year (original name years_y_) - the relevant publication's characteristics

In addition Reported_P\_Value and Computed were transformed into numeric class which resulted in two additional variables: p_rep_num and p_calc_num.
