# Lasso Performance Metrics analysis script
library(tidyverse)
library(RColorBrewer)
library(stats)
library(interactions)


# load the truncated version of the dataset with the performance metrics for 
# the first Benchmark - Lasso
BM1_PMs <- read_csv("C:/Users/spenc/Downloads/LASSO Performance Metrics grouped by error variance (without subtotals).csv")
attach(BM1_PMs)



### Lasso's True Negative Rate
## analyze them using ANOVA tests
# ANOVA test of the effect that the level of Collinearity in the model has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_Collinearity_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity), 
                               data = BM1_PMs)
summary(anova_Collinearity_TNR)

# ANOVA test of the effect of the number of true, i.e. structural factors in the model has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_true_factors_TNR <- aov(`Mean TNR` ~ as.factor(`Number of Structural Factors`), 
                              data = BM1_PMs)
summary(anova_true_factors_TNR)

# ANOVA test of the effect of Error Variance has 
# on the True Negative Rate (or equivalently, on the False Positive Rate)
anova_error_variance_TNR <- aov(`Mean TNR` ~ as.factor(`Error Variance`), 
                                data = BM1_PMs)
summary(anova_error_variance_TNR)

# A factorial ANOVA that examines the main effects of each factor (Collinearity 
# and Number of True Factors) on the dependent variable (True Negative Rate). 
# However, it does not include an interaction term to examine the interaction 
# effect between the two factors.
anova_both_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                      data = BM1_PMs)
summary(anova_both_TNR)

# interaction effect between Collinearity level and # of true explanatory variables
anova_interaction_TNR <- aov(`Mean TNR` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                             data = BM1_PMs)
summary(anova_interaction_TNR)

# Generate interaction plot for Collinearity and Number of Structural Factors
# Define the custom color palette
my_colors <- brewer.pal(12, "Set3")  # Change "Set3" to any other palette name to get different colors
interaction.plot(x.factor = BM1_PMs$Collinearity, 
                 trace.factor = BM1_PMs$`Number of Structural Factors`, 
                 response = BM1_PMs$`Mean TNR`, 
                 xlab = "Collinearity", 
                 ylab = "Specificity",
                 trace.label = "True Factors",
                 legend = TRUE,
                 col = my_colors)


## run a linear regression instead
TNR_fit <- lm(`Mean TNR` ~ Collinearity + `Number of Structural Factors` +  Collinearity * `Number of Structural Factors` + `Error Variance` + Collinearity * `Error Variance` + `Number of Structural Factors` * `Error Variance`, 
              data = BM1_PMs)
summary(TNR_fit)

# create a heatmap
ggplot(BM1_PMs, aes(x = Collinearity, y = `Number of Structural Factors`, fill = `Mean TNR`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap Showing How Collinearity & Num of True Factors Affect the TNR for BM1",
       x = "Collinearity",
       y = "Number of Structural Factors",
       fill = "Mean TNR")

# create an interaction plot
interact_plot(TNR_fit, pred = Collinearity, modx = `Number of Structural Factors`)






### Number of Overspecified Models Selected by Lasso
# ANOVA for the effect of Collinearity on the number of overspecified models
anova_collinearity_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity), 
                                        data = BM1_PMs)
summary(anova_collinearity_overspecified)

# ANOVA for the effect of the number of true factors on the number of overspecified models
anova_true_factors_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                                        data = BM1_PMs)
summary(anova_true_factors_overspecified)

# ANOVA test of the effect of Error Variance has 
# on the number of Overspecified Regressions Selected
anova_EV_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(`Error Variance`), 
                              data = BM1_PMs)
summary(anova_EV_overspecified)

# Factorial ANOVA for the main effects of both Collinearity and the number of true factors
anova_both_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                                data = BM1_PMs)
summary(anova_both_overspecified)

# ANOVA for the interaction effect between Collinearity and the number of true factors
anova_interaction_overspecified <- aov(`Overspecified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                        data = BM1_PMs)
summary(anova_interaction_overspecified)

## try it again by creating a linear model, then calling an ANOVA table
# Fit the linear model
lm_over_Col_num_of_vars_interaction <- lm(`Overspecified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                           data = BM1_PMs)
summary(lm_over_Col_num_of_vars_interaction)

# Get the ANOVA table
anova_table <- anova(lm_over_Col_num_of_vars_interaction)
# Print the ANOVA table
print(anova_table)

# Generate interaction plot for Collinearity and Number of Structural Factors
# Define the custom color palette
my_colors <- brewer.pal(12, "Set3")  # Change "Set3" to any other palette name to get different colors
interaction.plot(x.factor = BM1_PMs$Collinearity, 
                 trace.factor = BM1_PMs$`Number of Structural Factors`, 
                 response = BM1_PMs$`Overspecified Models Selected`, 
                 xlab = "Collinearity", 
                 ylab = "Overspecified Models Selected",
                 trace.label = "True Factors",
                 legend = TRUE,
                 col = my_colors)


## run a linear regression instead
Over_fit <- lm(`Overspecified Models Selected` ~ Collinearity + `Number of Structural Factors` +  Collinearity * `Number of Structural Factors` + `Error Variance` + Collinearity * `Error Variance` + `Number of Structural Factors` * `Error Variance`, 
               data = BM1_PMs)
summary(Over_fit)

# create a heatmap
ggplot(BM1_PMs, aes(x = Collinearity, y = `Number of Structural Factors`, fill = `Overspecified Models Selected`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "How Collinearity & Num of True Factors Affect the Num of Overspecified Selections",
       x = "Collinearity",
       y = "Number of Structural Factors",
       fill = "Overspecified Models Selected")

# create an interaction plot
interact_plot(Over_fit, pred = Collinearity, modx = `Number of Structural Factors`)















### Number of Correctly Specified Models Selected
## analysis using ANOVA tests
# ANOVA for the effect of Collinearity on the number of correctly specified models
anova_collinearity_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity), 
                                        data = BM1_PMs)
summary(anova_collinearity_correct)

# ANOVA for the effect of the number of true factors on the number of correctly specified models
anova_true_factors_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(`Number of Structural Factors`), 
                                        data = BM1_PMs)
summary(anova_true_factors_correct)

# ANOVA test of the effect of Error Variance has 
# on the number of Overspecified Regressions Selected
anova_EV_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(`Error Variance`), 
                              data = BM1_PMs)
summary(anova_EV_correct)

# Factorial ANOVA for the main effects of both Collinearity, 
# the # of true factors, and the degree of error variance.
anova_Col_TFs_EV_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`) + as.factor(`Error Variance`), 
                                data = BM1_PMs)
summary(anova_Col_TFs_EV_correct)

# ANOVA for the interaction effect between Collinearity and the number of true factors
anova_interaction_correct <- aov(`Correctly Specified Models Selected` ~ as.factor(Collinearity) * as.factor(`Number of Structural Factors`), 
                                       data = BM1_PMs)
summary(anova_interaction_correct)

# Generate interaction plot for Collinearity and Number of Structural Factors
# Define the custom color palette
my_colors <- brewer.pal(12, "Set3")  # Change "Set3" to any other palette name to get different colors
interaction.plot(x.factor = BM1_PMs$Collinearity, 
                 trace.factor = BM1_PMs$`Number of Structural Factors`, 
                 response = BM1_PMs$`Correctly Specified Models Selected`, 
                 xlab = "Collinearity", 
                 ylab = "Correct Specifications Selected",
                 trace.label = "True Factors",
                 legend = TRUE,
                 col = my_colors)


## run a linear regression instead
Correct_fit <- lm(`Correctly Specified Models Selected` ~ Collinearity + `Number of Structural Factors` +  Collinearity * `Number of Structural Factors` + `Error Variance` + Collinearity * `Error Variance` + `Number of Structural Factors` * `Error Variance`, 
                  data = BM1_PMs)
summary(Correct_fit)

# create a heatmap
ggplot(BM1_PMs, aes(x = Collinearity, y = `Number of Structural Factors`, 
                    fill = `Correctly Specified Models Selected`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "How Collinearity & Num of True Factors Affect the Num of Correctly Specified Selections",
       x = "Collinearity", y = "Number of Structural Factors",
       fill = "Correctly Specified Models Selected")

# create an interaction plot
interact_plot(Correct_fit, pred = Collinearity, modx = `Number of Structural Factors`)



## PPVs
# ANOVA for the effect of Collinearity on PPV
anova_collinearity_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(Collinearity), 
                              data = BM1_PMs)
summary(anova_collinearity_PPV)

# ANOVA for the effect of the Number of Structural Factors on PPV
anova_true_factors_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(`Number of Structural Factors`), 
                              data = BM1_PMs)
summary(anova_true_factors_PPV)

# Factorial ANOVA for the main effects of both Collinearity and the Number of Structural Factors on PPV
anova_both_PPV <- aov(`Mean Positive Predictive Value` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), 
                      data = BM1_PMs)
summary(anova_both_PPV)



## F1-Scores, which are a harmonic mean between the PPVs and the TPRs
# ANOVA for the effect of Collinearity on F1-Score
anova_collinearity_F1 <- aov(`Mean F1 Score` ~ as.factor(Collinearity), data = BM1_PMs)
summary(anova_collinearity_F1)

# ANOVA for the effect of the Number of Structural Factors on F1-Score
anova_true_factors_F1 <- aov(`Mean F1 Score` ~ as.factor(`Number of Structural Factors`), data = BM1_PMs)
summary(anova_true_factors_F1)

# Factorial ANOVA for the main effects of both Collinearity and the Number of Structural Factors on F1-Score
anova_both_F1 <- aov(`Mean F1 Score` ~ as.factor(Collinearity) + as.factor(`Number of Structural Factors`), data = BM1_PMs)
summary(anova_both_F1)


















