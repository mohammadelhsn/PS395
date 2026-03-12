### 1) INSTALL PACKAGES

library(rstatix) #for cohen's d
library(psych) #for group level descriptives
library(afex) #for ANOVAs
library(emmeans) #for follow-up comparisons
library(ggplot2) #for plots
library(ggpubr) #for plots
library(performance) #for assumptions

data <-read.csv('PS395_dataset.csv') #load the PS395 dataset

options(scipen = 999) #option to turn off scientific notation

### 4) EXPLORE DATA
data$Extra_mean<- rowMeans(cbind(data$BFI_1, data$BFI_8r, data$BFI_10, data$BFI_13r, data$BFI_20, data$BFI_23r))
summary(data) #can see format of variables, with the group variables not read as factors
data$group_Treat2 <- as.factor(data$group_Treat2) #tell R that group_Treat2 is a factor
data$group_Age <- as.factor(data$group_Age) #tell R that group_Age is a factor
summary(data) #can see descriptives with group_Treat2 and group_Age as factors

mean(data$Extra_mean) #get overall extraversion mean
sd(data$Extra_mean) #get overall extraversion sd

describeBy(Extra_mean ~ group_Treat2, data = data) #get descriptives by group_Treat2 - high, low, med
describeBy(Extra_mean ~ group_Age, data = data) #get descriptives by group_Age - old, young
describeBy(Extra_mean ~ group_Treat2*group_Age, data = data) #get descriptives by conditions - 6 in total

# We end up with the following table of cell means and marginal means:

#         | Old   | Young   |  Total

# High    |  4.26 |  4.43   |  4.34
# Low     |  4.24 |  3.60   |  3.92
# Med     |  3.93 |  3.94   |  3.94

# Total   |  4.14 |  3.99   |  4.07 (grand mean)

### 5) RUN BP FACTORIAL ANOVA

#Run model

Model1 <- aov_car(Extra_mean ~ group_Treat2*group_Age + Error(ID), data = data) 
Model1 
Model1$Anova

#Plot findings

afex_plot(Model1, x = "group_Treat2", trace = "group_Age") +
  labs(y = "Extra_mean", x = "group_Treat2") +  
  theme_pubr() # modifying the plot to be more APA-like

### 6) RUN FOLLOW-UP COMPARISONS AND EFFECT SIZE

Model1.emm<-emmeans(Model1, ~group_Treat2*group_Age)
Model1.emm # Notice the SEs are the same across all groups. This is because emmeans uses a pooled error term

#Pairwise comparisons

pairs(Model1.emm, adjust = "holm") #holm correction
confint(pairs(Model1.emm, adjust = "holm")) #confidence intervals with holm correction
eff_size(pairs(Model1.emm, adjust = "holm"), sigma = sigma(Model1$lm), edf = df.residual(Model1$lm), method = "identity") #effect size for our comparisons

#Simple effects of group_Treat2

test(pairs(Model1.emm, adjust = "holm", simple = "group_Treat2"), joint = TRUE) #tests if there's an omnibus effect of group_Treat2 at each level of the other factor (group_Age)
pairs(Model1.emm, adjust = "holm", simple = "group_Treat2") # run the simple effect
confint(pairs(Model1.emm, adjust = "holm", simple = "group_Treat2")) #confidence intervals for simple effects
eff_size(pairs(Model1.emm, adjust = "holm", simple = "group_Treat2"), #effect sizes for simple effect
         sigma = sigma(Model1$lm), edf = df.residual(Model1$lm), method = "identity")
##note: you could also get the simple effect of group_Age by swapping "group_Treat2" with "group_Age" throughout. Importantly, you should only do it one way, not both! 

### 7) CHECK ASSUMPTIONS

# A) Normality 

# Using the performance package to check our assumptions
check_normality(Model1$lm) #Shapiro-Wilk test
plot(check_normality(Model1), type = "qq") # qq plot 

# B) Homogeneity of Variance (HOV)

check_homogeneity(Model1) # Levene's test
plot(check_homogeneity(Model1))  #violin plots
