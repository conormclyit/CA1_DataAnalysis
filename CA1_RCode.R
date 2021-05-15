##############################
## CA1 - Heart Attack Analysis
##############################

# import the heart data frame
# Store file into working directory
# and then read into a data frame. 
heart_data <- read.csv("heart.csv")

# view top records
head(heart_data)

# View data type is this file
class(heart_data)

# Structure of the file
str(heart_data)

# View any records with NA
na_records <- heart_data[!complete.cases(heart_data),]
na_records

# Dataset has no null values

##############################
## Data Prep
##############################

# rename column names to more readable version
# rename cp (3rd item in dataframe) to ChestPainType
names(heart_data)[3] <- "ChestPainType"
# rename trtbps (4th item in dataframe) to RestingBloodPressure
names(heart_data)[4] <- "RestingBloodPressure"
# rename chol (5th item in dataframe) to Cholestoral
names(heart_data)[5] <- "Cholestoral"
# rename fbs (6th item in dataframe) to FastingBloodSugar
names(heart_data)[6] <- "FastingBloodSugar"
# rename rest_ecg (7th item in dataframe) to RestECGResults
names(heart_data)[7] <- "RestECGResults"
# rename thalachh  (8th item in dataframe) to MaxHeartRate
names(heart_data)[8] <- "MaxHeartRate"
# rename exang (9th item in dataframe) to ExerciseInducedAngina
names(heart_data)[9] <- "ExerciseInducedAngina"
# rename slp (11th item in dataframe) to Slope
names(heart_data)[11] <- "Slope"
# rename caa (12th item in dataframe) to NumMajVessels
names(heart_data)[12] <- "NumMajVessels"
# rename output (14th item in dataframe) to HeartAttackRisk
names(heart_data)[14] <- "HeartAttackRisk"

# Remove Coloumns not in assignment handout - thall and oldpeak 
heart_data$thall <- NULL
heart_data$oldpeak <- NULL

# Test Results
head(heart_data)

# View the summary of the data
summary(heart_data)

##################################
# Hypothesis & Statistical Methods
##################################

# The heart dataset contains data on heart attack stats

# to examine correlations between variables
pairs(heart_data, labels = colnames(heart_data), main = "Heart Attack dataset correlation plot")

##############
# Hypothesis 1
##############
# Find correlation between sex and HeartAttackRisk
# H0: Males are greater risk of heart attack.
# H1: Males are not a greater risk of heart attack.

# Variables used for hypothesis test:
# Sex - sex of patient - Dichotomous variable type
# HeartAttackRisk - More Chance / Less Chance - Nominal variable type
# Convert variables to numeric

# resolve error: Error in plot.new() : figure margins too large, Scatter plot
par("mar")
par(mar=c(1,1,1,1))

# look at correlation between both of these variables
attach(heart_data)
plot(sex, HeartAttackRisk, pch = 19, col = "lightblue")

# converting veriables to numeric
as.numeric(sex)
as.numeric(HeartAttackRisk)

# summarise the medians and interquartile range
tapply(sex, HeartAttackRisk, median)

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(sex)
qqline(sex, col = "red")

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(HeartAttackRisk)
qqline(HeartAttackRisk, col = "red")

# Formal test of normality for colestoral using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$sex)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (2.75)
# = normally distributed

# Formal test of normality for age using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$HeartAttackRisk)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (5.67)
# = normally distributed

# Relationship between 2 categorical variables
# Parametric test
# Chi-squared test

# create table with the 2 categorical variables
tbl = table(heart_data$sex, heart_data$HeartAttackRisk)

chisq <- chisq.test(tbl)
chisq # p-value = 1.877

# p-value is > 0.05 so we retain the null hypothesis and reject the alternative hypothesis
# Males are greater risk of heart attack.

# data analysis to confirm
library(ggplot2)
ggplot(data=heart_data, aes(x=sex, y=HeartAttackRisk)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=sex), vjust=-0.3, size=3.5)+
  theme_minimal()
# 0 - female
# 1 - Male

##############
# Hypothesis 2
##############
# Find correlation between resting blood pressure and cholesterol levels
# H0: Resting blood pressure is not a factor of cholesterol.
# H1: Resting blood pressure is a factor of cholesterol levels.

# Variables used for hypothesis test:
# RestingBloodPressure - Resting blood pressure (in mm Hg) - Continuous variable type
# Cholesterol - cholesterol in mg/dl fetched via BMI sensor - Continuous variable Type

# look at correlation between both of these variables
attach(heart_data)
plot(Cholestoral, RestingBloodPressure, pch = 19, col = "lightblue")

# summarise the medians and interquartile range
tapply(Cholestoral, RestingBloodPressure, median)

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(Cholestoral)
qqline(Cholestoral, col = "red")

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(RestingBloodPressure)
qqline(RestingBloodPressure, col = "red")

# Formal test of normality for colestoral using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$Cholestoral)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (5.364)
# = normally distributed

# Formal test of normality for age using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$RestingBloodPressure)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (1.45)
# = normally distributed

# Relationship between 2 continuous variables
# Parametric test
# Pearsons Correlation Co-eff
res1 <-cor.test(heart_data$RestingBloodPressure, heart_data$Cholestoral,  method = "pearson")
res1 # p-value = 0.03

# p-value is <0.05 so we reject the H0 and conclude that
# Resting blood pressure is not a factor of cholesterol.

# data analysis to confirm
ggplot(data=heart_data, aes(x=RestingBloodPressure, y=Cholestoral)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Cholestoral), vjust=-0.3, size=3.5)+
  theme_minimal()
# high cholestrol does not mean high blood pressure

##############
# Hypothesis 3
##############
# Find correlation between cholesterol and age
# H0: Age is a factor of Cholesterol levels.
# H1: Age is not a factor of Cholesterol level

# Variables used for hypothesis test:
# Age - Age of the patient - Continuous variable type
# Cholesterol - cholestoral in mg/dl fetched via BMI sensor - Continuous variable Type

# look at correlation between both of these variables
attach(heart_data)
plot(Cholestoral, age, pch = 19, col = "lightblue")

# summarise the medians and interquartile range
tapply(Cholestoral, age, median)

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(Cholestoral)
qqline(Cholestoral, col = "red")

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(age)
qqline(age, col = "red")

# Formal test of normality for colestoral using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$Cholestoral)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (5.364)
# = normally distributed

# Formal test of normality for age using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$age)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is lower than 0.05 (0.005)
# = not normally distributed

# Relationship between 2 continuous variables
# Non-parametric test
# Spearmans Coorelation Co-eff
res1 <-cor.test(heart_data$age, heart_data$Cholestoral,  method = "spearman")
res1 # p-value = 0.0006

# p-value is <0.05 so we reject the H0 and conclude that
# Age is not a factor of cholesterol levels

# data analysis to confirm
ggplot(data=heart_data, aes(x=age, y=Cholestoral)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Cholestoral), vjust=-0.3, size=3.5)+
  theme_minimal()
# older age does not mean higher blood pressure

##############
# Hypothesis 4
##############
# Find correlation between blood pressure and HeartAttackRisk
# H0: Max heart rate is not a factor of High resting blood pressure.
# H1: Max heart rate is a factor of High resting blood pressure.

# Variables used for hypothesis test:
# MaxHeartRate - maximum heart rate achieved - Continuous variable type
# RestingBloodPressure - resting blood pressure (in mm Hg) - Continuous variable Type

# look at correlation between both of these variables
attach(heart_data)
plot(MaxHeartRate, RestingBloodPressure, pch = 19, col = "lightblue")

# summarise the medians and interquartile range
tapply(MaxHeartRate, RestingBloodPressure, median)

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(MaxHeartRate)
qqline(MaxHeartRate, col = "red")

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(RestingBloodPressure)
qqline(RestingBloodPressure, col = "red")

# Formal test of normality for colestoral using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$MaxHeartRate)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (6.6208)
# = normally distributed

# Formal test of normality for age using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$RestingBloodPressure)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (1.45)
# = normally distributed

# Relationship between 2 continuous variables
# Parametric test
# Pearsons Correlation Co-eff
res1 <-cor.test(heart_data$RestingBloodPressure, heart_data$MaxHeartRate,  method = "pearson")
res1 # p-value = 0.418

# p-value is > 0.05 so we retain the null hypothesis and reject the alternative hypothesis
# Max heart rate is not a factor of High resting blood pressure.

# data analysis to confirm
ggplot(data=heart_data, aes(x=RestingBloodPressure, y=MaxHeartRate)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=MaxHeartRate), vjust=-0.3, size=3.5)+
  theme_minimal()
# Max heart rate is not a factor of High resting blood pressure.

##############
# Hypothesis 5
##############
# Find correlation between angina and chest pain type
# H0: Exercise angina cannot determine one particular chest pain type.
# H1: Exercise angina can determine one particular chest pain type.

# Variables used for hypothesis test:
# ExerciseInducedAngina - exercise induced angina (1 = yes; 0 = no) - Categorical variable type
# ChestPainType - : Chest Pain type chest pain type
# Value 1: typical angina
# Value 2: atypical angina
# Value 3: non-anginal pain
# Value 4: asymptomatic- 
# Categorical variable Type

# convert variables to numeric
as.numeric(ExerciseInducedAngina)
as.numeric(ChestPainType)

# summarise the medians and interquartile range
tapply(ExerciseInducedAngina, ChestPainType, median)

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(ExerciseInducedAngina)
qqline(ExerciseInducedAngina, col = "red")

# look at Quantile-quantile plot to check if nominally distributed
attach(heart_data)
qqnorm(ChestPainType)
qqline(ChestPainType, col = "red")

# Formal test of normality for colestoral using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$ExerciseInducedAngina)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (3.84)
# = normally distributed

# Formal test of normality for age using Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$ChestPainType)
normality_test$p.value
# p-value shows the if sample comes from a normal distribution 
# In this example, p-value is higher than 0.05 (1.85)
# = normally distributed

# Relationship between 2 categorical variables
# Parametric test
# Chi-squared test

# create table with the 2 categorical variables
tbl = table(heart_data$ExerciseInducedAngina, heart_data$ChestPainType)

chisq <- chisq.test(tbl)
chisq # p-value = 1.577

# p-value is > 0.05 so we retain the null hypothesis and reject the alternative hypothesis
# Exercise angina cannot determine one particular chest pain type