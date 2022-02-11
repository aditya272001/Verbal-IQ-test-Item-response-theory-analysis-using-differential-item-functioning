#_Packages_#
library(dplyr)
library(eRm)

#---Data-Cleaning---# 
VBIQ <- read.csv("D:/Verbal_IQ/VIQT_data/VIQT_data/VIQT_data.csv", head = T, sep =  ",")

IQR_1 <- quantile(VBIQ$testelapse, 0.25)
IQR_3 <- quantile(VBIQ$testelapse, 0.75)
IQR_survey <- IQR(VBIQ$testelapse)

VBIQ <- subset(VBIQ, VBIQ$testelapse > (IQR_1 - 1.5 * IQR_survey) & VBIQ$testelapse < (IQR_3 + 1.5 * IQR_survey))

five_percentile <- quantile(VBIQ$testelapse, 0.05)

VBIQ <- subset(VBIQ, VBIQ$testelapse > five_percentile)
VBIQ$Q1[VBIQ$Q1 == 2] <- 0
VBIQ$Q1[VBIQ$Q1 == 4] <- 0
VBIQ$Q1[VBIQ$Q1 == 8] <- 0
VBIQ$Q40[VBIQ$Q40 == 2] <- 0
VBIQ$Q40[VBIQ$Q40 == 4] <- 0
VBIQ$Q40[VBIQ$Q40 == 8] <- 0
VBIQ$Q36[VBIQ$Q36 == 4] <- 0
VBIQ$Q36[VBIQ$Q36 == 8] <- 0
boxplot(VBIQ$testelapse)

VBIQ <- mutat
table(VBIQ$gender)
table(VBIQ$engnat)
#---------------------------------------# 

#-----Rasch-Model------# 
Rasch_VBIQ <- RM(VBIQ[,c(1:45)])
summary(Rasch_VBIQ)
