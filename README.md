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

-   Error, DecisionError, OneTail, OneTailedInTxt and CopyPaste - tests not used in this projects

-   APAfactor, journals_jour\_ and years_y\_ - the relevant publication's characteristics

In the analysis only five variables are used with new names (original names are given in brackets):

-    n (=N)

-   comparison (=Reported_Comparison)

-   p_report (=Value)

-   p_compute (=Computed)

-   year (=years_y\_)
