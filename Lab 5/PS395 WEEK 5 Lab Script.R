### 1) INSTALL PACKAGES

library(rstatix) # for cohen's d
library(psych) # for group level descriptives
library(ggpubr) # for QQ plots
library(stats)

### 3) LOAD LAB DATASET

data <-read.csv('PS395_datasetW5LAB.csv') #load the lab dataset

options(scipen = 999) #option to turn off scientific notation

### 4) EXPLORE DATA

summary(data) #can see format of variables as-is, without the group variable read as a factor
data$ID_Dep <- as.factor(data$ID_Dep) #tells R that ID_Dep is a factor
data$Time <- as.factor(data$Time) #tells R that Time is a factor
summary(data) #can see counts per group now that our group variable is read as a factor

##notice in the data, we actually have the data formatted two ways. First, we have it in long format, with each dog ID appearing twice (having two rows - one happiness score after no belly rubs, and one after belly rubs). This is the data we will use for MOST of the analysis. We also however have the same data in wide format - one column for the no belly rubs data, and one for te belly rubs data - this is need for assumptions testing only. 

mean(data$SWLS_mean) #get overall mean for SWLS_mean
sd(data$SWLS_mean) #get overall sd for SWLS_mean

describeBy(SWLS_mean ~ ID_Dep, data = data) #get descriptives for each pair of scores (long data)
describeBy(SWLS_mean ~ Time, data = data) #get descriptives at each value of Time (long data)

mean(data$Before_SWLS, na.rm = TRUE) #get mean of Before_SWLS group (wide data) - need na.rm command so it computes despite missing data (NAs) 
mean(data$After_SWLS, na.rm = TRUE) #get mean of After_SWLS group (wide data) - need na.rm command so it computes despite missing data (NAs) 

### 5) RUN DEPENDENT SAMPLES T-TEST AND EFFECT SIZE

wide_data <- data[complete.cases(data), ] #create a subset including only  wide data

t.test(wide_data$Before_SWLS, wide_data$After_SWLS, data = wide_data, paired = TRUE) #to run t-test with wide data group columns

cohens_d(SWLS_mean ~ Time, data = data, paired = TRUE, ci = TRUE) #just like the independent test, but with paired = TRUE

### 6) CHECK ASSUMPTIONS

wide_data$SWLS_change <- (wide_data$Before_SWLS - wide_data$After_SWLS) #compute change scores with wide data

ggqqplot(wide_data, x = "SWLS_change") #dependent - run on change scores

shapiro.test(wide_data$SWLS_change) #shapiro wilk for change scores


data2 <- read.csv('PS395_WEEK5_Data.csv')
