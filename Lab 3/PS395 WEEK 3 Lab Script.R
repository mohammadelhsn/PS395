### Scale means computed in Week 2 Lab, but note correction to SC_mean as SC_7 was missing. ## The code is correct below.


data$SC_1r<- (6-data$SC_1)
data$SC_2r<- (6-data$SC_2)
data$SC_3r<- (6-data$SC_3)
data$SC_6r<- (6-data$SC_6)
data$SC_8r<- (6-data$SC_8)
data$SC_11r<- (6-data$SC_11)
data$SC_mean<- rowMeans(cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_7, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12))
data$SWLS_mean<- rowMeans(cbind(data$SWLS_1, data$SWLS_2, data$SWLS_3, data$SWLS_4, data$SWLS_5))
data$BFI_8r<- (8-data$BFI_8)
data$BFI_13r<- (8-data$BFI_13)
data$BFI_23r<- (8-data$BFI_23)
data$Extra_mean<- rowMeans(cbind(data$BFI_1, data$BFI_8r, data$BFI_10, data$BFI_13r, data$BFI_20, data$BFI_23r))
data$BFI_7r<- (8-data$BFI_7)
data$BFI_18r<- (8-data$BFI_18)
data$BFI_22r<- (8-data$BFI_22)
data$Agree_mean<- rowMeans(cbind(data$BFI_2, data$BFI_6, data$BFI_7r, data$BFI_18r, data$BFI_22r, data$BFI_26))
data$BFI_3r<- (8-data$BFI_3)
data$BFI_11r<- (8-data$BFI_11)
data$BFI_14r<- (8-data$BFI_14)
data$Conscien_mean<- rowMeans(cbind(data$BFI_3r, data$BFI_11r, data$BFI_14r, data$BFI_21))
data$BFI_4r<- (8-data$BFI_4)
data$BFI_12r<- (8-data$BFI_12)
data$BFI_15r<- (8-data$BFI_15)
data$Neurot_mean<- rowMeans(cbind(data$BFI_4r, data$BFI_12r, data$BFI_15r, data$BFI_17, data$BFI_24, data$BFI_27))
data$BFI_5r<- (8-data$BFI_5)
data$BFI_16r<- (8-data$BFI_16)
data$BFI_25r<- (8-data$BFI_25)
data$Open_mean<- rowMeans(cbind(data$BFI_5r, data$BFI_9, data$BFI_16r, data$BFI_19, data$BFI_25r, data$BFI_28))

### PS395 WEEK 3 LAB

### 1) INSTALL PACKAGES

library(rstatix) #load package for cohen's d to assess effect size
library(ggpubr) #load package to run QQ plots to assess normality

options(scipen = 999) #run this to turn off scientific notation

### 4) EXPLORE DATA

mean(data$SC_mean) #to generate the sample mean
sd(data$SC_mean) #to generate the sample standard deviation

### 5) RUN SINGLE SAMPLE T-TEST AND EFFECT SIZE

#Is our sample mean different than zero? mu = 0

t.test(data$SC_mean)
t.test(data$SC_mean, alternative = "greater") #use the alternative argument for directional hypotheses
t.test(data$SC_mean, alternative = "less") #use the alternative argument for directional hypotheses
cohens_d(SC_mean ~ 1, data = data, ci = TRUE) #effect size

#Is our sample mean different than a known population value? mu = 2.75

t.test(data$SC_mean, mu = 2.75)
t.test(data$SC_mean, mu = 2.75, alternative = "greater") #use the alternative argument for directional hypotheses
t.test(data$SC_mean, mu = 2.75, alternative = "less") #use the alternative argument for directional hypotheses
cohens_d(SC_mean ~ 1, mu = 2.75, data = data, ci = TRUE) #effect size

### 6) CHECK ASSUMPTIONS

ggqqplot(data, x = "SC_mean") #QQ plot to visually assess normality

shapiro_test(data$SC_mean) #shapiro-wilks to statistically assess normality



### PRACTICE ####
data$



