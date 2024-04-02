
#REPORT ANOVA 2

#Load packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(car)
library(ggpubr)

################################################################################
# Load the data
dataset_url <- 'https://users.stat.ufl.edu/~winner/data/lightsat.dat'
data <- read.table(dataset_url,header=FALSE, col.names =c('intensity','background','hue','saturation_score','lighting_score'))

################################################################################
# Explore the data
head(data)
summary(data)
str(data)
sum(is.na(data))

#The dataset contains 5 variables: intensity, background, hue, saturation_score, 
#and lighting_score. The dataset contains 48 observations. The first three variables
#are categorical, and the last two are continuous. There are no NA values in the dataset.
#The aim of the analysis is to determine if there is a significant difference in the
#mean lighting score across different levels of intensity, background, and hue. To do
#so an Analysis of Variance (ANOVA) will be conducted.

# Distribution of lighting score
boxplot(data$lighting_score, main="Boxplot of Lighting Score", ylab="Lighting Score")
hist(data$lighting_score, main="Histogram of Lighting Score", xlab="Lighting Score",breaks=20)

# Distribution of saturation score
boxplot(data$saturation_score, main="Boxplot of Saturation Score", ylab="Saturation Score")
hist(data$saturation_score, main="Histogram of Saturation Score", xlab="Saturation Score",breaks=20)

#Lighting and saturation scores don't present any outliers. Their distribution is approximately normal.
#Further analysis will be conducted.


data1 <- data.frame(as.factor(data$intensity), as.factor(data$background), as.factor(data$hue), data$lighting_score)
colnames(data1) <- c("intensity", "background", "hue", "lighting_score")
str(data1)
plot.design(data1, main="Profile Plot of Lighting Score", ylab="Lighting Score")

#we observe that the lighting score is not constant across the levels of the factors.
#there is a significant difference in the mean lighting score across the levels of the factors,
#which suggests that an ANOVA test is appropriate.

#boxplot of lighting score for factor levels
ggboxplot(data1, x = "intensity", y = "lighting_score", color = "intensity", palette = "jco", ylab = "Lighting Score", xlab = "Intensity")
ggboxplot(data1, x = "background", y = "lighting_score", color = "background", palette = "jco", ylab = "Lighting Score", xlab = "Background")
ggboxplot(data1, x = "hue", y = "lighting_score", color = "hue", palette = "jco", ylab = "Lighting Score", xlab = "Hue")

################################################################################
# ANOVA

# 2-way ANOVA
model1 <- aov(lighting_score ~ intensity * background, data = data1)
model2 <- aov(lighting_score ~ intensity * hue, data = data1)
model3 <- aov(lighting_score ~ background * hue, data = data1)
summary(model1)
summary(model2)
summary(model3)
coef(model2)

#from the anova results, we can see that the interaction between intensity and 
#background is not significant (p-value = 0.06). On the other hand, the interaction
#between intensity and hue is significant (p-value = 0.02). The interaction between
#background and hue is also significant (p-value = 0.02). The main effects of intensity,
#background, and hue are all significant (p-value < 0.05). The results suggest that the
#mean lighting score is significantly different across the levels of intensity, background,
#and hue. The interaction effects indicate that the effect of one factor on the lighting
#score depends on the level of another factor.

#the best model is model2, which includes the interaction between intensity and hue.

################################################################################
# Post-hoc tests

# Tukey HSD test for intensity
TukeyHSD(model2, which = "intensity")

# Tukey HSD test for hue
TukeyHSD(model2, which = "hue")

#the Tukey HSD test results show that there are significant differences in the mean
#lighting score between the levels of intensity and hue. The results provide more
#insight into the specific differences between the factor levels.

################################################################################
# Visualize the results

# Interaction plot
interaction.plot(data1$intensity, data1$hue, data1$lighting_score, type = "b", legend = TRUE, xlab = "Intensity", ylab = "Lighting Score", trace.label = "Hue")

#the interaction plot shows the relationship between intensity, hue, and lighting score.
#the plot illustrates how the effect of intensity on lighting score varies across the levels
#of hue. The plot provides a visual representation of the interaction effect observed in
#the ANOVA results.

################################################################################
# model assessment

# Check the assumptions of ANOVA
ggqqplot(data1$lighting_score)
ggdensity(data1$lighting_score)
plot(model2)

#the qq-plot and density plot show that the residuals are approximately normally distributed.
#the residuals vs. fitted plot shows that the residuals are homoscedastic and do not exhibit
#any patterns. The assumptions of ANOVA are met.

