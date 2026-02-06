### 1) INSTALL PACKAGES

library(rstatix) #for cohen's d
library(psych) #for group level descriptives
library(ggpubr) #for QQ plots

data <- read.csv('') 

options(scipen = 999) #option to turn off scientific notation

### 4) EXPLORE THE DATA - DESCRIPTIVE STATISTICS

summary(data) #see format of variables with our group_Age variable not read as a factor
data$group_Age <- as.factor(data$group_Age) #tells R that group_Age is a factor
summary(data) #can now see counts per group now that group_Age is read as a factor

mean(data$SWLS_mean) #to get overall mean for SWLS_mean
sd(data$SWLS_mean) #to get overall sd for SWLS_mean

describeBy(SWLS_mean ~ group_Age, data = data) #to get SWLS_mean descriptives by group

### 5) RUN IND SAMPLES T-TEST & EFFECT SIZE

t.test(SWLS_mean~group_Age, data = data, var.equal = TRUE) #to run ind samples t-test
t.test(SWLS_mean~group_Age, data = data, var.equal = FALSE) #to run Welch's t-test (if HOV is violated), df will adjust proportionally to violation

cohens_d(SWLS_mean~group_Age, data = data, var.equal = TRUE, ci = TRUE) #to run effect size
cohens_d(SWLS_mean~group_Age, data = data, var.equal = FALSE, ci = TRUE) #to run effect size with correction if HOV is violated

### 6) CHECK ASSUMPTIONS

ggqqplot(data, x = "SWLS_mean", facet.by = "group_Age", na.rm=TRUE) #to generate QQ plots

shapiro.test(data$SWLS_mean[data$group_Age == "Old"]) #to run SW for Old group
shapiro.test(data$SWLS_mean[data$group_Age == "Young"]) #to run SW for Young group

levene_test(SWLS_mean ~ group_Age, data = data) #to run levene's test 
