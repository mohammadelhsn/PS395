### 1) INSTALL PACKAGES
library(rstatix) #for cohen's d
library(psych) #for group level descriptives
library(afex) #for ANOVAs
library(emmeans) #for follow-up comparisons
library(ggplot2) #for plots
library(ggpubr) #for plots
library(performance) #for assumptions

### 3) LOAD LAB DATASET

data <-read.csv('PS395_datasetW7LAB.csv') #load the lab 7 dataset

options(scipen = 999) #option to turn off scientific notation

### 4) EXPLORE DATA

summary(data) #can see format of variables as-is, without the group variable read as a factor
data$ID_Dep <- as.factor(data$ID_Dep) #tells R that ID_Dep is a factor
data$Time <- as.factor(data$Time) #tells R that Time is a factor
summary(data) ##can see counts per group now that ID_Dep & Time are read as factors

mean(data$SWLS_mean) #get overall mean for SWLS_mean
sd(data$SWLS_mean) #get overall sd for SWLS_mean

describeBy(SWLS_mean ~ ID_Dep, data = data) #get descriptives for each participant 
describeBy(SWLS_mean ~ Time, data = data) #get descriptives at each value of Time 

### 5) RUN WITHIN-PARTICIPANTS ANOVA

#Run model

Model1 <- aov_car(SWLS_mean ~ Time + Error(ID_Dep/Time), data = data) 
Model1 # note adjustment to DF - GG correction applied by default
Model1$Anova 

Model1a <- aov_car(SWLS_mean ~ Time + Error(ID_Dep/Time), data = data,
                   anova_table = list(correction = "none")) #run without GG correction
Model1a
Model1a$Anova

#Plot findings

afex_plot(Model1, x = "Time", error = "within", error_ci = TRUE) +
  labs(y = "SWLS_mean", x = "Time") +
  geom_line(aes(group = 1)) + # adds the line between groups (i.e., conditions)
  theme_pubr() # modifies the plot to be more APA-like

### 6) RUN FOLLOW-UP COMPARISONS AND EFFECT SIZE

Model1.emm<-emmeans(Model1, ~Time)
Model1.emm # see SEs for all groups. 

#Pairwise comparisons

pairs(Model1.emm, adjust = "none") #no correction
pairs(Model1.emm, adjust = "bon") #bonferroni correction
pairs(Model1.emm, adjust = "holm") #holm correction
pairs(Model1.emm, adjust = "tukey") #tukey correction

#Confidence intervals for comparisons
confint(pairs(Model1.emm, adjust = "none")) 

#Effect size for our comparisons

eff_size(Model1.emm,
         sigma = sqrt(mean(sigma(Model1$lm)^2)), #note we had to change the standardizer
         edf = df.residual(Model1$lm))

### 7) CHECK ASSUMPTIONS

# A) Normality 

# Using the performance package to check our assumptions
check_normality(Model1$lm) #Shapiro-Wilk test
plot(check_normality(Model1), type = "qq") #qq plot 

# B) Sphericity

#re-run the model with "return = "univariate" added to get see the Mauchly's test of Sphericity and GG/HF corrections for each 
Model2 <- aov_car(SWLS_mean ~ Time + Error(ID_Dep/Time), data = data, return = "univariate") 
Model2 
