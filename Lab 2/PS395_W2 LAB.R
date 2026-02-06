###1) SETTING YOUR WORKING DIRECTORY
library(psych)

#Check and/or set up your working directory
getwd()

###2) CHECK INSTALLED PACKAGES

#Look in the Packages pane in the bottom right of the screen.

###3) LOADING THE DATA

#Load the PS395 Dataset on R
data <- read.csv("PS395_dataset.csv")
View(data)

###4) INSPECTING THE DATA

#View a summary of the data
summary(data)
#Run some descriptive statistics for the SC_1 variable
hist(data$SC_1, na.rm=TRUE)
mean(data$SC_1, na.rm=TRUE)
median(data$SC_1, na.rm=TRUE)
range(data$SC_1, na.rm=TRUE)
var(data$SC_1, na.rm=TRUE)  
sd(data$SC_1, na.rm=TRUE)
#With the psych package, you can generate descriptive stats for all variables
describe(data)

###5) CHANGING THE DATA

#Reverse code the SCS-SF scale items
data$SC_1r<- (6-data$SC_1)
data$SC_2r<- (6-data$SC_2)
data$SC_3r<- (6-data$SC_3)
data$SC_6r<- (6-data$SC_6)
data$SC_8r<- (6-data$SC_8)
data$SC_11r<- (6-data$SC_11)
#Compute the overall mean for the SCS-SF
data$SC_mean<- rowMeans(cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12))
#Compute the overall mean for the SWLS
data$SWLS_mean<- rowMeans(cbind(data$SWLS_1, data$SWLS_2, data$SWLS_3, data$SWLS_4, data$SWLS_5))

###6) CORRELATION

#Run a simple linear correlation analysis with SC_mean and SWLS_mean
cor.test(data$SC_mean, data$SWLS_mean)
#Install Hmisc package and generate a correlation matrix with SC_mean and SWLS_mean
library(Hmisc)
rcorr(cbind(data$SC_mean, data$SWLS_mean))

###7) PRACTICE

#Use the appropriate code to run descriptive stats for SWLS_1, run a correlation with SWLS_1 and SWLS_2, and generate a correlation matrix for SWLS_1, SWLS_2, and SWLS_3. Create/modify the R Script on your own!

#For additional practice, reverse code the appropriate BFI items and calculate the means for the five personality subscales. The script is listed below.

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


describe(data$SWLS_1)
corr.test(data$SWLS_1, data$SWL2)
rcorr(data$SWLS_1)
rcorr(data$SWLS_2)
rcorr(data$SWLS_3)
