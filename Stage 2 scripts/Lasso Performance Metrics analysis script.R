# Lasso Performance Metrics analysis script
library(tidyverse)

# load the truncated version of the dataset with the performance metrics for 
# the first Benchmark - Lasso
BM1_PMs_without_EVs <- read_csv("C:/Users/spenc/Downloads/Overall cv.glmnet LASSO's Performance Metrics.csv")
attach(BM1_PMs_without_EVs)


## tidyverse
# ANOVA test of the effect that the level of Collinearity in the model has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_Collinearity_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity), 
                               data = BM1_PMs_with_EVs)
summary(anova_Collinearity_TNR)

# ANOVA test of the effect of the number of true, i.e. structural factors in the model has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_true_factors_TNR <- aov(`Mean TNR` ~ as.factor(`Number of Structural Factors`), 
                              data = BM1_PMs_with_EVs)
summary(anova_true_factors_TNR)


# A factorial ANOVA that examines the main effects of each factor (Collinearity 
# and Number of True Factors) on the dependent variable (True Negative Rate). 
# However, it does not include an interaction term to examine the interaction 
# effect between the two factors.
anova_both_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                      data = BM1_PMs_with_EVs)
summary(anova_both_TNR)

# interaction effect between Collinearity level and # of true explanatory variables
anova_interaction_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                             data = BM1_PMs_with_EVs)
summary(anova_interaction_TNR)

# Generate interaction plot for Collinearity and Number of Structural Factors
interaction.plot(x.factor = BM1_PMs_with_EVs$Collinearity, 
                 trace.factor = BM1_PMs_with_EVs$`Number of Structural Factors`, 
                 response = BM1_PMs_with_EVs$`Mean TNR`, 
                 xlab = "Collinearity", 
                 ylab = "Specificity",
                 trace.label = "True Factors",
                 legend = TRUE)





## the number of overspecified models
# ANOVA for the effect of Collinearity on the number of overspecified models
anova_collinearity_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity), 
                                        data = BM1_PMs_with_EVs)
summary(anova_collinearity_overspecified)

# ANOVA for the effect of the number of true factors on the number of overspecified models
anova_true_factors_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                                        data = BM1_PMs_with_EVs)
summary(anova_true_factors_overspecified)

# Factorial ANOVA for the main effects of both Collinearity and the number of true factors
anova_both_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                                data = BM1_PMs_with_EVs)
summary(anova_both_overspecified)

# ANOVA for the interaction effect between Collinearity and the number of true factors
anova_interaction_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                        data = BM1_PMs_with_EVs)
summary(anova_interaction_overspecified)

## try it again by creating a linear model, then calling an ANOVA table
# Fit the linear model
lm_over_Col_num_of_vars_interaction <- lm(`Overspecified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                           data = BM1_PMs_with_EVs)
# Get the ANOVA table
anova_table <- anova(lm_over_Col_num_of_vars_interaction)
# Print the ANOVA table
print(anova_table)

# Generate interaction plot for Collinearity and Number of Structural Factors
interaction.plot(x.factor = BM1_PMs_with_EVs$Collinearity, 
                 trace.factor = BM1_PMs_with_EVs$`Number of Structural Factors`, 
                 response = BM1_PMs_with_EVs$`Overspecified Models Selected`, 
                 xlab = "Collinearity", 
                 ylab = "Overspecified Models Selected",
                 trace.label = "True Factors",
                 legend = TRUE)











## the number of correctly specified models
# ANOVA for the effect of Collinearity on the number of correctly specified models
anova_collinearity_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity), 
                                        data = BM1_PMs_with_EVs)
summary(anova_collinearity_correct)

# ANOVA for the effect of the number of true factors on the number of correctly specified models
anova_true_factors_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                                        data = BM1_PMs_with_EVs)
summary(anova_true_factors_correct)

# ANOVA test of the effect of Error Variance has 
# on the number of Overspecified Regressions Selected
anova_EV_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(`Error Variance`), 
                              data = BM1_PMs_with_EVs)
summary(anova_EV_correct)

# Factorial ANOVA for the main effects of both Collinearity and the number of true factors
anova_both_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`) + as.factor(`Error Variance`), 
                                data = BM1_PMs_with_EVs)
summary(anova_both_correct)

# ANOVA for the interaction effect between Collinearity and the number of true factors
anova_interaction_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                       data = BM1_PMs_with_EVs)
summary(anova_interaction_correct)

# Generate interaction plot for Collinearity and Number of Structural Factors
interaction.plot(x.factor = BM1_PMs_with_EVs$Collinearity, 
                 trace.factor = BM1_PMs_with_EVs$`Number of Structural Factors`, 
                 response = BM1_PMs_with_EVs$`Correctly Specified Models Selected`, 
                 xlab = "Collinearity", 
                 ylab = "Correct Specifications Selected",
                 trace.label = "True Factors",
                 legend = TRUE)











## PPVs
# ANOVA for the effect of Collinearity on PPV
anova_collinearity_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(Collinearity), 
                              data = BM1_PMs_without_EVs)
summary(anova_collinearity_PPV)

# ANOVA for the effect of the Number of Structural Factors on PPV
anova_true_factors_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(`Number of Structural Factors`), 
                              data = BM1_PMs_without_EVs)
summary(anova_true_factors_PPV)

# Factorial ANOVA for the main effects of both Collinearity and the Number of Structural Factors on PPV
anova_both_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                      data = BM1_PMs_without_EVs)
summary(anova_both_PPV)



## F1-Scores, which are a harmonic mean between the PPVs and the TPRs
# ANOVA for the effect of Collinearity on F1-Score
anova_collinearity_F1 <- aov(`Mean F1 Score` ~ as.factor(Collinearity), data = BM1_PMs_without_EVs)
summary(anova_collinearity_F1)

# ANOVA for the effect of the Number of Structural Factors on F1-Score
anova_true_factors_F1 <- aov(`Mean F1 Score` ~ as.factor(`Number of Structural Factors`), data = BM1_PMs_without_EVs)
summary(anova_true_factors_F1)

# Factorial ANOVA for the main effects of both Collinearity and the Number of Structural Factors on F1-Score
anova_both_F1 <- aov(`Mean F1 Score` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), data = BM1_PMs_without_EVs)
summary(anova_both_F1)








# import the full version (including error variance levels) of the dataset with 
# the performance metrics for the first Benchmark - Lasso
BM1_PMs_with_EVs <- read_csv("C:/Users/spenc/Downloads/LASSO Performance Metrics grouped by error variance (without subtotals).csv")


# ANOVA test of the effect of Error Variance has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_error_variance_TNR <- aov(`Mean TNR` ~ as.factor(`Error Variance`), 
                                 data = BM1_PMs_with_EVs)
summary(anova_error_variance_TNR)



# ANOVA test of the effect of Error Variance has 
# on the number of Overspecified Regressions Selected
anova_EV_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(`Error Variance`), 
                               data = BM1_PMs_with_EVs)
summary(anova_EV_overspecified)


