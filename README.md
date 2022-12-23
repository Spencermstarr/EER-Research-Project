# EER-Research-Project
A research project investigating a new machine learning procedure for optimal variable and model selection.
This proposed method is called Estimated Exhaustive Regression, it was originally proposed in a working paper by noted econometrician
Dr. Antony Davies, most well known for his invention of the first known framework for analyzing multi-dimensional panel data in his doctoral dissertation.
The aforementioned working paper is included in this repository in the pdf called "(draft version) An Exploration of Regression-Based Data Mining Techniques by Antony Davies (2008)."

This novel feature selection algorithm involves an adjustment on the All Subsets Regression aka Best Subset Selection procedure whereby only a random subset of j of all of the possible 2^k - 1 regression specifications evaluated by the traditional version of ASR, and evaluating which of them is best by a Relative Cross-Model Chi-Square Criterion as an alternative to any of the typical regression model performance metrics. This is used in order to better identify which optimal variables selected for inclusion in the final regression specification out of all candidate variables is only selected by the algorithm spuriously.

I am collaborating with Antony Davies on this updating of his original research (the results of which we intend to re-submit for publication in 2023) which
mostly involves the addition of tw0 extra Benchmark Methods to compare EER to, namely: LASSO Regression and Forward Selection Stepwise (the original
WP only compared EER to Backward Elimination Stepwise regression), an improvement on the randomization of the randomly generated synthetic datasets that
EER and the 3 Benchmark Algorithms are ran on for performance comparison, and my additional inputs and insights from the perspective of a graduate student
in Data Analytics Engineering in terms of how this procedure fits in with the host of modern machine learning algorithms for optimal variable selection.

Dr. Davies is running his EER Procedure on 260,000 randomly generated synthetic datasets using Stata, while I am running LASSO Regression, and BE & FS Stepwise Regression on the same set of 260k datasets using R and comparing the results of EER with them subsequently.


