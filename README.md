# p_values

Distribution of p-values reported in eight major psychology journals in 1985-2013.

The original file with the data was imported from [here](https://osf.io/gdr4q).

## Codebook

**my_data** content - 19 originally downloaded variables [**new names used in this analysis are given in bold**]:

-   N [n]- order number of a record

-   Source - order number of a paper, from which data was extracted

-   Statistic, Value, df1, df2 - type and value of statistics reported, relevant degrees of freedom

-   Test_Comparison - the way how statistics value was reported ("=" means as an exact number, "\>" or "\<" means a range above or below Value)

-   Reported_Comparison [**comparison**] - the way how p-value was reported ("=" means as an exact number, "\>" or "\<" means a range above or below Reported_P\_Value)

-   Reported_P\_Value [**p_report**] - p-value reported (or a threshold for it)

-   Computed [**p_compute**] - p-value recalculated based on the reported statistics value and degrees of freedom (recalculation is performed by [Nuijten et al. (2016)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5101263/) using the automated procedure [statcheck](https://CRAN.R-project.org/package=statcheck).

-   Raw - raw data extracted from a paper

-   Error, DecisionError, OneTail, OneTailedInTxt and CopyPaste - tests generated by [statcheck](https://CRAN.R-project.org/package=statcheck){style="font-size: 11pt;"} but not used in this project

-   APAfactor, journals_jour\_ and years_y\_ [**year**] - the relevant publication's characteristics

In the analysis only five variables are used with their new names as stated above.
