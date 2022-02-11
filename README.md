# Verbal IQ test : Item response theory analysis using differential item functioning 

This project was done on data retrieved from OpenPsychometrics.org on Verbal IQ test scores of participants from wide range 
of demographics. The aim of this analysis is to assess the bias in the items and if they function 
similarly between the gender (Male and Female) and Native v/s Non-Native speakers. Majority of items failed the differential item functioning (Choi et al., 2011) and were biased towards one or the other group. Two different types of estimators have been used to perform very differntial item functioning, Mantel haztel estimator (Holland & Thayer, 1986), 
and Lord test (Langer, 2008). 
The demo code and results are discussed below: 

## Demo code

-------packages------

library(dplyr)

library(eRm)

library(ggplot2)


#-----Rasch-Model-Gender-Natives-------# 
VBIQ_natives <- filter(VBIQ, engnat == "1") # 1 = Yes, 2 = No

VBIQ_Rasch_N <- RM(VBIQ[,c(1:45)])
summary(VBIQ_Rasch_N)

Gender_natives <- VBIQ$gender # 2 = Female, 1 = Male
VBIQ_natives$gender[VBIQ_natives$gender == "Female"] <- 0
VBIQ_natives$gender[VBIQ_natives$gender == "Male"] <- 1

VBIQ_Rasch_N_Group <- Waldtest(VBIQ_Rasch_N, 
                               splitcr = Gender_natives)
#---Objects_Specific_Sub_groups---# 
Subgroup_1_Natives <- VBIQ_Rasch_N_Group$betapar1
Subgroup_2_Natives <- VBIQ_Rasch_N_Group$betapar2

Comparing_groups <- as.data.frame(VBIQ_Rasch_N_Group$coef.table)

#----Visualizing-using-Scatter-Plot-----# 

ggplotobject <- as.data.frame(cbind(Subgroup_1_Natives, Subgroup_2_Natives))
ggplot(ggplotobject, aes(x = Subgroup_1_Natives, y = Subgroup_2_Natives)) + 
  geom_point(color = "red", size = 3) + 
  geom_smooth(method = lm, se = F, linetype = "dashed", color = "blue", size = 1.2) + xlim(-4, 4) + ylim(-4, 4) + 
  theme_bw() + labs(y = "Female", x = "Male", size = 5)
![DIF_gender](https://user-images.githubusercontent.com/96023170/153632048-baae23d7-2b4d-49aa-81d6-0520f7a129db.png)

#----Differential-Item-Functioning-Using-MH-estimator----# 
library(ltm)

VBIQ_natives_MH <- difMH(Data = VBIQ_natives[,c(1:45, 52)], group = "gender", 
                         focal.name = 1, purify = T, nrIter = 25)
VBIQ_natives_MH
pdf("Native-IRT_MH.pdf", height = 12, width = 16, paper = "USr")
plot(VBIQ_natives_MH)
dev.off()
![Screenshot 2022-02-11 221258](https://user-images.githubusercontent.com/96023170/153632484-1186c289-3c24-40aa-86d1-cff9e374cc53.png)

VBIQ_natives_LORD <- difLord(Data = VBIQ_natives[,c(1:45, 52)], group = "gender", 
                             focal.name = 1, purify = T, nrIter = 25, model = "2PL")

VBIQ_natives_LORD
pdf("IRT_Lord_Native.pdf", height = 12, width = 16, paper = "USr")
plot(VBIQ_natives_LORD)
dev.off()
![Screenshot 2022-02-11 221327](https://user-images.githubusercontent.com/96023170/153632495-f4120f29-5367-408c-90ac-094f8db02376.png)


#------Differential_Item_Functioning_between_Native-V/S-Non-Native------# 
VBIQ_NAN_MH <- difMH(Data = VBIQ[,c(1:45, 53)], group = "engnat", 
                     focal.name = 1, purify = T, nrIter = 25)
VBIQ_NAN_MH
pdf("IRT_MH_Non.pdf", height = 12, width = 16, paper = "USr")
plot(VBIQ_NAN_MH)
dev.off()
![Screenshot 2022-02-11 221536](https://user-images.githubusercontent.com/96023170/153633200-c526216b-29a4-4174-a973-2dc8a2bd47a5.png)

VBIQ_NAN_Lord <- difMH(Data = VBIQ[,c(1:45, 53)], group = "engnat", 
                     focal.name = 1, purify = T, nrIter = 25)
VBIQ_NAN_Lord

pdf("IRT_Lord_Non.pdf", height = 12, width = 16, paper = "USr")
plot(VBIQ_NAN_Lord)
dev.off()
![Screenshot 2022-02-11 221853](https://user-images.githubusercontent.com/96023170/153633211-cd351880-8da8-4a2e-bdff-00d4aa589db0.png)

## References

Choi, S. W., Gibbons, L. E., & Crane, P. K. (2011). Lordif: An R package for detecting differential item functioning using iterative hybrid ordinal logistic regression/item response theory and Monte Carlo simulations. Journal of statistical software, 39(8), 1.

Holland, P. W., & Thayer, D. T. (1986). Differential item functioning and the Mantel‐Haenszel procedure. ETS Research Report Series, 1986(2), i-24.

Langer, M. M. (2008). A reexamination of Lord's Wald test for differential item functioning using item response theory and modern error estimation (Doctoral dissertation, The University of North Carolina at Chapel Hill).



