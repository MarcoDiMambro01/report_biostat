
#REPORT ANOVA 2

#Load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(car)
library(ggpubr)
################################################################################
# Load the data, factorizes it for later use in ANOVA model fitting, 
#renames hues and background to undesrtand better.
dataset_url <- 'https://users.stat.ufl.edu/~winner/data/lightsat.dat'
data <- read.table(dataset_url,header=FALSE, col.names =c('intensity','background','hue','saturation_score','lighting_score'))

intensity = as.factor(data$intensity)
data$intensity = intensity

background = as.factor(data$background)
data$background = background

hue = as.factor(data$hue)
data$hue = hue
levels(data$intensity) = c('135cd','72cd','4.5cd','1.4cd')
levels(data$hue) = c('Red','Yellow','Green','Blue')
levels(data$background)= c('Red','Yellow','Green')
################################################################################
# Groups and Explores the data
head(data)
summary(data)
str(data)
sum(is.na(data))

DataGrouped1 = data %>%group_by( intensity, hue)
DataGrouped2 = data %>%group_by( intensity, background)
DataGrouped3 = data %>%group_by( background, hue)
DataGrouped1 %>% get_summary_stats( lighting_score, type = "mean_sd")
DataGrouped2 %>% get_summary_stats( lighting_score, type = "mean_sd")
DataGrouped3 %>% get_summary_stats( lighting_score, type = "mean_sd")


#The dataset contains 5 variables: intensity, background, hue, saturation_score, 
#and lighting_score. The dataset contains 48 observations. The first three variables
#are categorical, and the last two are continuous. There are no NA values in the dataset.
#The aim of the analysis is to determine if there is a significant difference in the
#mean lighting score across different levels of intensity, background, and hue. To do
#so an Analysis of Variance (ANOVA) will be conducted.

# Distribution of lighting score
boxplot(data$lighting_score, main="Boxplot of Lighting Score", ylab="Lighting Score")
hist(data$lighting_score, main="Histogram of Lighting Score", xlab="Lighting Score",breaks=20)

cbPalette <- c("#D55E00", "#F0E442", "#009E73",  "#0072B2" )

bxp = ggboxplot(data = data, x = "intensity", y = "lighting_score" , ylab= 'Lighting Score', xlab='Intensity(cd)')
bxp = bxp + facet_grid(hue~.)
bxp

ggboxplot(data, x = "intensity", y = "lighting_score", color = "intensity", palette = cbPalette, ylab = "Lighting Score", xlab = "Intensity")
ggboxplot(data, x = "background", y = "lighting_score", color = "background", palette = cbPalette, ylab = "Lighting Score", xlab = "Background")
ggboxplot(data, x = "hue", y = "lighting_score", color = "hue", palette = cbPalette, ylab = "Lighting Score", xlab = "Hue")

#Lighting and saturation scores don't present any outliers. Their distribution is approximately normal.
Outliers1 = identify_outliers(data=DataGrouped1, lighting_score)
Outliers2 = identify_outliers(data=DataGrouped2, lighting_score)
Outliers3 = identify_outliers(data=DataGrouped3, lighting_score)
Outliers1
Outliers2 #Some outliers are present when grouping by Intensity and Background
Outliers3


#we observe that the lighting score is not constant across the levels of the factors.
#there is a significant difference in the mean lighting score across the levels of the factors,
#which suggests that an ANOVA test is appropriate.
data1 <- data.frame(data$intensity, data$background, data$hue, data$lighting_score)
colnames(data1) <- c("intensity", "background", "hue", "lighting_score")
str(data1)
plot.design(data, main="Profile Plot of Lighting Score", ylab="Lighting Score")

################################################################################
# ANOVA
#Significance of factors: Three way ANOVA
lightning.anova = aov(lighting_score ~ (intensity+background+hue+
                      intensity:background+background:hue+intensity:hue),
                      data=data1)

summary(lightning.anova)


#Let's check the Anova assumptions:
#1:The responses for each factor level have a normal population distribution.
#2:These distributions have the same variance.
#3:The data are independent.

#1:Let's perform a Shapirobtest on the grouped data to confirm the first assumption
data1 %>%
  group_by(intensity, hue) %>%
  shapiro_test(lighting_score)


#the p-values are all >0.05 so normality seems assured, let's make a qqPlot to visualize it

ggqqplot(data = data1, "lighting_score", ggtheme = theme_bw()) +
  facet_grid(intensity ~ hue)

#2:Let's perform a leven test on the grouped data to confirm the second assumption

data1 %>%levene_test(lighting_score ~ intensity*hue)

#The p-value of the Levene test is 0.987: more than enough to not refute the H0: distributions have the same variance

#3: Obviously correct, given the experimental design

# 2-way ANOVA

model1 <- aov(lighting_score ~ intensity * background, data = data1)
model2 <- aov(lighting_score ~ intensity * hue, data = data1)
model3 <- aov(lighting_score ~ background * hue, data = data1)
summary(model1)
summary(model2) #Two is best, but there is still no significance in intensity- Hue interactions
summary(model3)
modelfin =  aov(lighting_score ~ intensity + hue, data = data1)
summary(modelfin)
coef(modelfin)
plot(modelfin)
ggqqplot(residuals(modelfin))

#from the anova results, we can see that the interaction between intensity and 
#background is not significant (p-value = 0.06). On the other hand, the interaction
#between intensity and hue is significant (p-value = 0.02). The interaction between
#background and hue is also significant (p-value = 0.02). The main effects of intensity,
#background, and hue are all significant (p-value < 0.05). The results suggest that the
#mean lighting score is significantly different across the levels of intensity, background,
#and hue. The interaction effects indicate that the effect of one factor on the lighting
#score depends on the level of another factor.

#the best model is model2, which includes the interaction between intensity and hue, and it will be the one we will use going forward.

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
