### 1) INSTALL PACKAGES

library(rstatix) #for cohen's d
library(psych) #for group level descriptives
library(afex) #for ANOVAs
library(emmeans) #for follow-up comparisons
library(ggplot2) #for plots
library(ggpubr) #for plots
library(performance) #for assumptions

### 2) SET WORKING DIRECTORY

getwd() #to get working directory
setwd("C:/Users/dweise/OneDrive - Wilfrid Laurier University/STATS/PS395/AAA PS395 W2026") #set to YOUR working directory

### 3) LOAD LAB DATASET

data <-read.csv('PS395_dataset.csv') #load the PS395 dataset

options(scipen = 999) #option to turn off scientific notation

### 4) EXPLORE DATA

summary(data) #can see format of variables, with our group variable not read as a factor
data$group_Treat2 <- as.factor(data$group_Treat2) #telling R group_Treat2 is a factor
data$group_Age <- as.factor(data$group_Age) #telling R Group_Age is a factor
mean(data$SWLS_mean) #get overall mean
sd(data$SWLS_mean) #get overall sd

describeBy(SWLS_mean ~ group_Treat2, data = data) #get descriptives by group_Treat2
describeBy(SWLS_mean ~ group_Age, data = data) #get descriptives by group_Age
describeBy(SWLS_mean ~ group_Treat2*group_Age, data = data) #get descriptives by conditions
describeBy(SWLS_mean ~ ID_WP, data = data) #get descriptives by participant

# We end up with the following table of cell means and marginal means:

#         | Old   | Young   |  Total

# High    |  5.35 |  5.18   |  5.27
# Low     |  4.68 |  4.20   |  4.44
# Med     |  4.37 |  4.08   |  4.23

# Total   |  4.80 |  4.49   |  4.64 (grand mean)

### 5) RUN MIXED ANOVA

#Run model

Model1 <- aov_car(SWLS_mean ~ group_Treat2*group_Age + Error(ID_WP/(group_Treat2)), data = data)
Model1

Model1a<- aov_car(SWLS_mean ~ group_Treat2*group_Age + Error(ID_WP/(group_Treat2)), data = data,
                  anova_table = list(correction = "none")) #across all ANOVAs, by default certain corrections are applied (e.g., sphericity correction leads to empirical dfs) - can add this command to suppress the corrections
Model1a #notice difference in degrees of freedom

#Plot findings

afex_plot(Model1, x = "group_Treat2", trace = "group_Age", error = "none") +
  labs(y = "SWLS_mean", x = "group_Treat2") +  
  theme_pubr() # modifying the plot to be more APA-like

### 6) RUN FOLLOW-UP COMPARISONS AND EFFECT SIZE

Model1.emm<-emmeans(Model1, ~group_Treat2*group_Age)
Model1.emm  #for marginal means and CIs for each condition

#Pairwise comparisons

pairs(Model1.emm, adjust = "holm") #holm correction
confint(pairs(Model1.emm, adjust = "holm")) #confidence intervals with holm correction
eff_size(pairs(Model1.emm, adjust = "holm"), sigma = sqrt(mean(sigma(Model1$lm)^2)), #note different standardizer relative to BP Factorial 
         edf = df.residual(Model1$lm), method = "identity") #effect size for our comparisons

#Simple effects of group_Treat2 - follow procedure/code from WP Factorial, if BP - follow procedure/code from BP Factorial

test(pairs(Model1.emm, adjust = "holm", simple = "group_Treat2"), joint = TRUE) #tests if there's an omnibus effects of group_Treat2 at each level of group_Age
pairs(Model1.emm, adjust = "holm", simple = "group_Treat2") # run the simple effect
confint(pairs(Model1.emm, adjust = "holm", simple = "group_Treat2")) #confidence intervals for simple effects
eff_size(pairs(Model1.emm, adjust = "holm", simple = "group_Treat2"), #effect sizes for simple effect
         sigma = sqrt(mean(sigma(Model1$lm)^2)), #notice different computation for standardizer relative to BP Factorial
         edf = df.residual(Model1$lm), method = "identity")
##note: could also get the simple effect of group_Age by swapping "group_Treat2" for "group_Age" throughout. Importantly, you should only do it one way, not both. 

Model1B.emm<-emmeans(Model1, ~group_Treat2) #create a model to test the main effect of group_Treat on its own - which groups are different from each other?
pairs(Model1B.emm, adjust = "holm")

### 7) CHECK ASSUMPTIONS

# A) Normality 

# Using the performance package to check our assumptions
check_normality(Model1$lm) #Shapiro-Wilk test
plot(check_normality(Model1), type = "qq") # qq plot 

# B) Homogeneity of Variance (HOV)

check_homogeneity(Model1) # Levene's test
plot(check_homogeneity(Model1))  #violin plots

# C) Sphericity
check_sphericity(Model1)

# But let's look in more detail with the "SPSS" style output from afex
Model1a <- aov_car(SWLS_mean ~ group_Treat2*group_Age + Error(ID_WP/(group_Treat2)), data = data, return = "univariate")
Model1a # Notice we don't get a Mauchly test for group_Age. This is because sphericity is only a concern for WP factors with more than 2 levels. Mauchly's tests are significant, so we need apply a correction for sphericity. Note that GG is the more conservative option.

# D) Homogeneity of Covariance (HOCV)

#you have to restructure the data to test Box's M
library (tidyverse) #need to restructure

data_wide <- data %>% select(ID_WP, group_Treat2, SWLS_mean, group_Age) %>% pivot_wider(names_from = group_Treat2, values_from = SWLS_mean) #create sub-dataset with wide data
data_matrix <- data_wide %>% select('High', 'Low', 'Med') #select only our outcome variable columns
data_group <- data_wide$group_Age #select only our grouping variable
box_m(data_matrix, data_group) #test Box's M, which is a Chi-Square statistic

