# Backward Elimination Stepwise Regression Performance Metrics analysis script
library(tidyverse)
library(RColorBrewer)
library(stats)
library(interactions)


# import the full version (including error variance levels) of the dataset with 
# the performance metrics for the second Benchmark - BE Stepwise
BM2_PMs <- read_csv("C:/Users/spenc/Downloads/2nd Benchmark's PMs, grouped by error variance (without subtotals) - csv version.csv")
attach(BM2_PMs)



## the True Negative Rate
# ANOVA test of the effect that the level of Collinearity in the model has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_Collinearity_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity), 
                               data = BM2_PMs)
summary(anova_Collinearity_TNR)

# ANOVA test of the effect of the number of true, i.e. structural factors in the model has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_true_factors_TNR <- aov(`Mean TNR` ~ as.factor(`Number of Structural Factors`), 
                              data = BM2_PMs)
summary(anova_true_factors_TNR)


# ANOVA test of the effect of Error Variance has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_error_variance_TNR <- aov(`Mean TNR` ~ as.factor(`Error Variance`), 
                                data = BM2_PMs)
summary(anova_error_variance_TNR)


# A factorial ANOVA that examines the main effects of each factor (Collinearity 
# and Number of True Factors) on the dependent variable (True Negative Rate). 
# However, it does not include an interaction term to examine the interaction 
# effect between the two factors.
anova_both_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                      data = BM2_PMs)
summary(anova_both_TNR)

# interaction effect between Collinearity level and # of true explanatory variables
anova_interaction_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                             data = BM2_PMs)
summary(anova_interaction_TNR)

# Generate interaction plot for the effect of Collinearity level 
# and Number of Structural Factors on the TNR
# Define the custom color palette
my_colors <- brewer.pal(12, "Set3")  # Change "Set3" to any other palette name to get different colors
interaction.plot(x.factor = BM2_PMs$Collinearity, 
                 trace.factor = BM2_PMs$`Number of Structural Factors`, 
                 response = BM2_PMs$`Mean TNR`, 
                 xlab = "Collinearity", 
                 ylab = "Specificity",
                 trace.label = "True Factors",
                 legend = TRUE,
                 col = my_colors)


## run a linear regression instead
TNR_fit_BM2 <- lm(`Mean TNR` ~ Collinearity + `Number of Structural Factors` +  Collinearity * `Number of Structural Factors` + `Error Variance` + Collinearity * `Error Variance` + `Number of Structural Factors` * `Error Variance`, 
                  data = BM2_PMs)
summary(TNR_fit_BM2)

# create a heatmap
ggplot(BM2_PMs, aes(x = Collinearity, y = `Number of Structural Factors`, fill = `Mean TNR`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap Showing How Collinearity & Num of True Factors Affect the TNR for BM2",
       x = "Collinearity",
       y = "Number of Structural Factors",
       fill = "Mean TNR")

# create an interaction plot
interact_plot(TNR_fit, pred = Collinearity, modx = `Number of Structural Factors`)













## the number of overspecified models
# ANOVA for the effect of Collinearity on the number of overspecified models
anova_collinearity_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity), 
                                        data = BM2_PMs)
summary(anova_collinearity_overspecified)

# ANOVA for the effect of the number of true factors on the number of overspecified models
anova_TFs_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                                        data = BM2_PMs)
summary(anova_TFs_overspecified)

# ANOVA test of the effect of Error Variance has 
# on the number of Overspecified Regressions Selected
anova_EV_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(`Error Variance`), 
                              data = BM2_PMs)
summary(anova_EV_overspecified)

# Factorial ANOVA for the main effects of both Collinearity and the number of true factors
anova_both_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                                data = BM2_PMs)
summary(anova_both_overspecified)

# ANOVA for the interaction effect between Collinearity and the number of true factors
anova_interaction_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                        data = BM2_PMs)
summary(anova_interaction_overspecified)

## try it again by creating a linear model, then calling an ANOVA table
# Fit the linear model
lm_over_Col_num_of_vars_interaction <- lm(`Overspecified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                           data = BM2_PMs)
summary(lm_over_Col_num_of_vars_interaction)

# Get the ANOVA table
anova_table <- anova(lm_over_Col_num_of_vars_interaction)
# Print the ANOVA table
print(anova_table)

# Generate interaction plot for Collinearity and Number of Structural Factors
interaction.plot(x.factor = BM2_PMs$Collinearity, 
                 trace.factor = BM2_PMs$`Number of Structural Factors`, 
                 response = BM2_PMs$`Overspecified Models Selected`, 
                 xlab = "Collinearity", 
                 ylab = "Overspecified Models Selected",
                 trace.label = "True Factors",
                 legend = TRUE,
                 col = my_colors)


## run a linear regression instead
Over_fit_BM2 <- lm(`Overspecified Models Selected` ~ Collinearity + `Number of Structural Factors` +  Collinearity * `Number of Structural Factors` + `Error Variance` + Collinearity * `Error Variance` + `Number of Structural Factors` * `Error Variance`, 
                   data = BM2_PMs)
summary(Over_fit_BM2)

# create a heatmap
ggplot(BM2_PMs, aes(x = Collinearity, y = `Number of Structural Factors`, fill = `Overspecified Models Selected`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "How Collinearity & Num of True Factors Affect the Num of Overspecified Selections for BM2",
       x = "Collinearity",
       y = "Number of Structural Factors",
       fill = "Overspecified Models Selected")

# create an interaction plot
interact_plot(Over_fit_BM2, pred = Collinearity, modx = `Number of Structural Factors`)










## the number of correctly specified models
# ANOVA for the effect of Collinearity on the number of correctly specified models
anova_collinearity_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity), 
                                        data = BM2_PMs)
summary(anova_collinearity_correct)

# ANOVA for the effect of the number of true factors on the number of correctly specified models
anova_true_factors_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                                        data = BM2_PMs)
summary(anova_true_factors_correct)

# ANOVA test of the effect of Error Variance has 
# on the number of Overspecified Regressions Selected
anova_EV_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(`Error Variance`), 
                              data = BM2_PMs)
summary(anova_EV_correct)


# Factorial ANOVA for the main effects of both Collinearity, 
# the # of true factors, and the degree of error variance.
anova_Col_TFs_EV_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`) + as.factor(`Error Variance`), 
                                data = BM2_PMs)
summary(anova_Col_TFs_EV_correct)

# ANOVA for the interaction effect between Collinearity and the number of true factors
anova_interaction_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                       data = BM2_PMs)
summary(anova_interaction_correct)

# Generate interaction plot for Collinearity and Number of Structural Factors
interaction.plot(x.factor = BM2_PMs$Collinearity, 
                 trace.factor = BM2_PMs$`Number of Structural Factors`, 
                 response = BM2_PMs$`Correctly Specified Models Selected`, 
                 xlab = "Collinearity", 
                 ylab = "Correct Specifications Selected",
                 trace.label = "True Factors",
                 legend = TRUE)

# sanity check that the direction of the effect of collinearity on 
# the of correct specifications selected is in fact negative
lm_correct_Collinearity <- lm(`Correctly Specified Models Selected` ~ as.factor(Collinearity), 
                               data = BM2_PMs)
summary(lm_correct_Collinearity)

# sanity check that the direction of the effect from the # of True Factors on 
# the of correct specifications selected is in fact negative
lm_correct_num_TFs <- lm(`Correctly Specified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                              data = BM2_PMs)
summary(lm_correct_num_TFs)


## run a linear regression instead
Correct_fit <- lm(`Correctly Specified Models Selected` ~ Collinearity + `Number of Structural Factors` +  Collinearity * `Number of Structural Factors` + `Error Variance` + Collinearity * `Error Variance` + `Number of Structural Factors` * `Error Variance`, 
                  data = BM2_PMs)
summary(Correct_fit)

# create a heatmap
ggplot(BM2_PMs, aes(x = Collinearity, y = `Number of Structural Factors`, 
                    fill = `Correctly Specified Models Selected`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "How Collinearity & Num of True Factors Affect the Num of Correctly Specified Selections",
       x = "Collinearity", y = "Number of Structural Factors",
       fill = "Correctly Specified Models Selected")

















## PPVs
# ANOVA for the effect of Collinearity on PPV
anova_collinearity_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(Collinearity), 
                              data = BM2_PMs_without_EVs)
summary(anova_collinearity_PPV)

# ANOVA for the effect of the Number of Structural Factors on PPV
anova_true_factors_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(`Number of Structural Factors`), 
                              data = BM2_PMs_without_EVs)
summary(anova_true_factors_PPV)

# Factorial ANOVA for the main effects of both Collinearity and the Number of Structural Factors on PPV
anova_both_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                      data = BM2_PMs_without_EVs)
summary(anova_both_PPV)






## F1-Scores, which are a harmonic mean between the PPVs and the TPRs
# ANOVA for the effect of Collinearity on F1-Score
anova_collinearity_F1 <- aov(`Mean F1 Score` ~ as.factor(Collinearity), data = BM2_PMs_without_EVs)
summary(anova_collinearity_F1)

# ANOVA for the effect of the Number of Structural Factors on F1-Score
anova_true_factors_F1 <- aov(`Mean F1 Score` ~ as.factor(`Number of Structural Factors`), data = BM2_PMs_without_EVs)
summary(anova_true_factors_F1)

# Factorial ANOVA for the main effects of both Collinearity and the Number of Structural Factors on F1-Score
anova_both_F1 <- aov(`Mean F1 Score` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), data = BM2_PMs_without_EVs)
summary(anova_both_F1)




