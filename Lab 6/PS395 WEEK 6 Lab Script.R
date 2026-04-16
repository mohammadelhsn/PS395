### 1) INSTALL PACKAGES

library(rstatix) #for cohen's d
library(psych) #for group level descriptives
library(afex) #to run ANOVAs
library(emmeans) #for follow-up comparisons
library(ggplot2) #for plots
library(ggpubr) #for plots
library (ggbeeswarm) #for means plot
library(performance) #for assumptions

### 2) SET WORKING DIRECTORY

getwd() #get working directory
setwd("C:/Users/mbarlow/Dropbox/WLU/Classes/395/Data/") #set YOUR working directory

### 3) LOAD LAB DATASET

data <-read.csv('PS395_dataset.csv') #load the lab dataset

options(scipen = 999) #option to turn off scientific notation

### 4) EXPLORE DATA

summary(data) #can see format of variables, with our group variable not read as a factor
data$group_Treat2 <- as.factor(data$group_Treat2) #tell R that group_Treat2 is a factor
summary(data) #can see counts per group now that group_Treat2 variable is read as a factor

mean(data$SC_mean) #to get overall self-compassion mean
sd(data$SC_mean) #to get overall self-compassion standard deviation

describeBy(SC_mean ~ group_Treat2, data = data) #to get descriptives by group (High, Low, Medium) 

### 5) RUN BETWEEN-PARTICIPANTS ANOVA

#Run the overall model

Model1 <- aov_car(SC_mean ~ group_Treat2 + Error(ID), data = data) #to set up the model
Model1 #to run the model
Model1$Anova #to run the model and show results in ANOVA table format

#Plot the group means

afex_plot(Model1, x = "group_Treat2") +
  labs(y = "SC_mean", x = "group_Treat2") +
  geom_line(aes(group = 1)) + #adds the line between groups when you have a one-way ANOVA 
  theme_pubr() #modifies the plot to be more APA-like

### 6) CONDUCT FOLLOW-UP COMPARISONS

Model1.emm<-emmeans(Model1, ~group_Treat2) #to set up the comparisons
Model1.emm #notice that the SEs are the same across all groups. This is because emmeans uses a pooled error term

#Run the pairwise comparisons

pairs(Model1.emm, adjust = "none") #no correction
pairs(Model1.emm, adjust = "bon") #bonferroni correction
pairs(Model1.emm, adjust = "holm") #holm correction
pairs(Model1.emm, adjust = "tukey") #tukey correction

#Run the confidence intervals for the pairwise comparisons
confint(pairs(Model1.emm, adjust = "none")) 

#Run the effect size for the pairwise comparisons
eff_size(Model1.emm, sigma = sigma(Model1$lm), edf = df.residual(Model1$lm))

### 7) CHECK ASSUMPTIONS

# A)Normality 

# Using the performance package to check our assumptions
check_normality(Model1$lm) #Shapiro-Wilk test
plot(check_normality(Model1), type = "qq") #QQ plot 

# B) Homogeneity of Variance (HOV)

check_homogeneity(Model1) #to run Levene's test
plot(check_homogeneity(Model1))  #to generate violin plot
