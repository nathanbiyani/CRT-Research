library("readxl")
library("ggplot2")
library("dplyr")
library(caTools)
library(caret)
library(glmnet)
library(data.table)
library(doSNOW)
library(ROCR)

Spinale <- read_excel("SpinaleDataSet.xlsx")


SpinaleResponses <- Spinale[!is.na(Spinale$response),]

S <- SpinaleResponses %>%
  filter(time == 0)

S$response <- as.factor(S$response)

ggplot(S, aes(x = response, y = lvesv_delta)) +
  geom_point()




S <- S %>%
  filter(mmp2 > 0)

colSums(is.na(S))


#standardizing MMP2 and checking its distribution/relationship with 
#target variable



boxplot(S$mmp2)


n <- length(S$mmp2)
sort(S$mmp2,partial=n)[n]
which(S$mmp2 == 4881676)

sort(S$mmp2,partial=n-1)[n-1]
which(S$mmp2 > 3336170)

sort(S$mmp2,partial=n-2)[n-2]
which(S$mmp2 >= 2448172)

S$mmp2[c(38, 181)] <- 2448172


meanMMP2 <- mean(S$mmp2)
sdMMP2 <- sd(S$mmp2)



S <- S %>% 
    mutate(MMP2_standardized = (mmp2 - meanMMP2)/sdMMP2)

ggplot(S, aes(x = S)) +
  geom_histogram()

ggplot(S, aes(x = response, y = MMP2_standardized)) +
  geom_point() 

boxplot(S$MMP2_standardized)





#standardizing MMP9 and checking its distribution/relationship with 
#target variable

boxplot(S$mmp9)
sort(S$mmp9,partial=n)[n]
which(S$mmp9 == 1979866)

sort(S$mmp9,partial=n - 1)[n - 1]

S$mmp9[181] <- 990026



meanMMP9 <- mean(S$mmp9)
sdMMP9 <- sd(S$mmp9)



S <- S %>% 
  mutate(MMP9_standardized = (mmp9 - meanMMP9)/sdMMP9)

ggplot(S, aes(x = MMP9_standardized)) +
  geom_histogram()

ggplot(S, aes(x = mmp9)) +
  geom_()

ggplot(S, aes(x = response, y = mmp9)) +
  geom_boxplot() 

boxplot(S$MMP9_standardized)


#standardizing ST2 and checking its distribution/relationship with 
#target variable

boxplot(S$st2)




meanST2 <- mean(S$st2)
sdST2 <- sd(S$st2)



S <- S %>% 
  mutate(ST2_standardized = (st2 - meanST2)/sdST2)

ggplot(S, aes(x = ST2_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = ST2_standardized)) +
  geom_point() 



#standardizing CRP and checking its distribution/relationship with 
#target variable

boxplot(S$crp)



meanCRP <- mean(S$crp)
sdCRP <- sd(S$crp)



S <- S %>% 
  mutate(CRP_standardized = (crp - meanCRP)/sdCRP)

ggplot(S, aes(x = CRP_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = CRP_standardized)) +
  geom_point() 



#standardizing bnp and checking its distribution/relationship with 
#target variable

boxplot(S$bnp)



meanBNP <- mean(S$bnp)
sdBNP <- sd(S$bnp)



S <- S %>% 
  mutate(BNP_standardized = (bnp - meanBNP)/sdBNP)

ggplot(S, aes(x = BNP_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = BNP_standardized)) +
  geom_point() 



#standardizing timp1 and checking its distribution/relationship with 
#target variable


boxplot(S$timp1)
sort(S$timp1,partial=n)[n]
which(S$timp1 > 527820)

sort(S$timp1,partial=n - 1)[n - 1]

S$timp1[711] <- 406871



meanTIMP1 <- mean(S$timp1)
sdTIMP1 <- sd(S$timp1)



S <- S %>% 
  mutate(TIMP1_standardized = (timp1 - meanTIMP1)/sdTIMP1)

ggplot(S, aes(x = TIMP1_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = TIMP1_standardized)) +
  geom_point() 

boxplot(S$TIMP1_standardized)


#standardizing timp2 and checking its distribution/relationship with 
#target variable

boxplot(S$timp2)
sort(S$timp2,partial=n)[n]
sort(S$timp2,partial=n-1)[n-1]
sort(S$timp2,partial=n-2)[n-2]
sort(S$timp2,partial=n-3)[n-3]

which(S$timp2 > 238780)

S$timp2[c(12,21,29)] <- 202051




meanTIMP2 <- mean(S$timp2)
sdTIMP2 <- sd(S$timp2)



S <- S %>% 
  mutate(TIMP2_standardized = (timp2 - meanTIMP2)/sdTIMP2)

ggplot(S, aes(x = TIMP2_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = TIMP2_standardized)) +
  geom_point() 

boxplot(S$TIMP2_standardized)


#standardizing tim4 and checking its distribution/relationship with 
#target variable

boxplot(S$tim4)

sort(S$tim4,partial=n)[n]
sort(S$tim4,partial=n-1)[n-1]
sort(S$tim4,partial=n-2)[n-2]



which(S$tim4 > 10390)

S$tim4[c(24, 545)] <- 9008


meanTIM4 <- mean(S$tim4)
sdTIMP4 <- sd(S$tim4)



S <- S %>% 
  mutate(TIM4_standardized = (tim4 - meanTIM4)/sdTIMP4)

ggplot(S, aes(x = TIM4_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = TIM4_standardized)) +
  geom_point() 

boxplot(S$TIM4_standardized)


#standardizing spg130 and checking its distribution/relationship with 
#target variable

boxplot(S$spg130)
sort(S$spg130,partial=n)[n]
sort(S$spg130,partial=n-1)[n-1]
sort(S$spg130,partial=n-2)[n-2]
sort(S$spg130,partial=n-3)[n-3]

which(S$spg130 > 436570)

S$spg130[c(24, 545)] <- 389689.6


meanSPG130 <- mean(S$spg130)
sdSPG130 <- sd(S$spg130)



S <- S %>% 
  mutate(SPG130_standardized = (spg130 - meanSPG130)/sdSPG130)

ggplot(S, aes(x = SPG130_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = SPG130_standardized)) +
  geom_point() 

boxplot(S$SPG130_standardized)


#standardizing sil2ra and checking its distribution/relationship with 
#target variable


boxplot(S$sil2ra)
sort(S$sil2ra,partial=n)[n]
sort(S$sil2ra,partial=n-1)[n-1]
sort(S$sil2ra,partial=n-2)[n-2]
sort(S$sil2ra,partial=n-3)[n-3]
sort(S$sil2ra,partial=n-4)[n-4]
sort(S$sil2ra,partial=n-5)[n-5]

which(S$sil2ra > 4000)

S$sil2ra[c(21, 29, 44, 257, 304)] <- 3397


meanSIL2RA <- mean(S$sil2ra)
sdSIL2RA <- sd(S$sil2ra)



S <- S %>% 
  mutate(SIL2RA_standardized = (sil2ra - meanSIL2RA)/sdSIL2RA)

ggplot(S, aes(x = SIL2RA_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = SIL2RA_standardized)) +
  geom_point() 

boxplot(S$SIL2RA_standardized)


#standardizing tnfrii and checking its distribution/relationship with 
#target variable

boxplot(S$tnfrii)
sort(S$tnfrii,partial=n)[n]
sort(S$tnfrii,partial=n-1)[n-1]
sort(S$tnfrii,partial=n-2)[n-2]
sort(S$tnfrii,partial=n-3)[n-3]
sort(S$tnfrii,partial=n-4)[n-4]




which(S$tnfrii == 57589)

S$tnfrii[c(19, 21, 212, 472)] <- 40660

meanTNFRII <- mean(S$tnfrii)
sdTNFRII <- sd(S$tnfrii)



S <- S %>% 
  mutate(TIFRII_standardized = (tnfrii - meanTNFRII)/sdTNFRII)

ggplot(S, aes(x = TIFRII_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = TIFRII_standardized)) +
  geom_point() 

boxplot(S$TIFRII_standardized)


#standardizing ifng and checking its distribution/relationship with 
#target variable


boxplot(S$ifng)
sort(S$ifng,partial=n)[n]
sort(S$ifng,partial=n-1)[n-1]
sort(S$ifng,partial=n-2)[n-2]
sort(S$ifng,partial=n-3)[n-3]
sort(S$ifng,partial=n-4)[n-4]
sort(S$ifng,partial=n-5)[n-5]
sort(S$ifng,partial=n-6)[n-6]



which(S$ifng > 24)

S$ifng[c(102, 145, 157, 409, 439, 650, 651)] <- 24.97271

meanIFNG <- mean(S$ifng)
sdIFNG <- sd(S$ifng)



S <- S %>% 
  mutate(IFNG_standardized = (ifng - meanIFNG)/sdIFNG)

ggplot(S, aes(x = IFNG_standardized)) +
  geom_histogram()

ggplot(S, aes(x = response, y = IFNG_standardized)) +
  geom_point() 


boxplot(S$IFNG_standardized)

#standardizing diastolic blood pressure and checking its distribution/relationship with 
#target variable


sort(S$bpdia,partial=n)[n]
sort(S$bpdia,partial=n-1)[n-1]


boxplot(S$bpdia)



meanDiastolic <- mean(S$bpdia)
sdDiastolic <- sd(S$bpdia)



S <- S %>% 
  mutate(standardized_dystolicBloodPressure = (bpdia - meanDiastolic)/sdDiastolic)

ggplot(S, aes(x = response, y = standardized_dystolicBloodPressure)) +
  geom_point() 


#standardizing systolic blood pressure and checking its 
#distribution/relationship with target variable


boxplot(S$bpsys)


meanSystolic <- mean(S$bpsys)
sdSystolic <- sd(S$bpsys)

S <- S %>% 
  mutate(standardized_systolicBloodPressure = (bpsys - meanSystolic)/sdSystolic)

ggplot(S, aes(x = response, y = standardized_systolicBloodPressure)) +
  geom_point() 

#standardizing LVESV and checking its 
#distribution/relationship with target variable

boxplot(S$LVESV)

sort(S$LVESV,partial=n)[n]
sort(S$LVESV,partial=n-1)[n-1]

which(S$LVESV > 500)
S$LVESV[c(685)] <- 447

meanLVESV <- mean(S$LVESV)
sdLVESV <- sd(S$LVESV)

S <- S %>% 
  mutate(LVESV_standardized = (LVESV - meanLVESV)/sdLVESV)

ggplot(S, aes(x = response, y = LVESV_standardized)) +
  geom_point() 

#standardizing LVEDV and checking its 
#distribution/relationship with target variable

boxplot(S$LVEDV)

sort(S$LVEDV,partial=n)[n]
sort(S$LVEDV,partial=n-1)[n-1]

which(S$LVEDV > 600)
S$LVEDV[c(685)] <- 513

meanLVEDV <- mean(S$LVEDV)
sdLVEDV <- sd(S$LVEDV)

S <- S %>% 
  mutate(LVEDV_standardized = (LVEDV - meanLVEDV)/sdLVEDV)

ggplot(S, aes(x = response, y = LVEDV_standardized)) +
  geom_point() 


#creating new variables
S <- S %>% 
  mutate(PulsePressure = bpsys - bpdia)

ggplot(S, aes(x = response, y = PulsePressure)) +
  geom_point() 

meanPulsePressure <- mean(S$PulsePressure)
sdPulsePressure <- sd(S$PulsePressure)

S <- S %>% 
  mutate(pulsePressure_standardized = (PulsePressure - meanPulsePressure)/sdPulsePressure)


S <- S %>% 
  mutate(StrokeVolume = LVEDV - LVESV)

ggplot(S, aes(x = response, y = StrokeVolume)) +
  geom_point() 

meanStrokeVolume <- mean(S$StrokeVolume)
sdStrokeVolume <- sd(S$StrokeVolume)

S <- S %>% 
  mutate(StrokeVolume_standardized = (StrokeVolume - meanStrokeVolume)/sdStrokeVolume)


S <- S %>% 
  mutate(EjectionFraction = StrokeVolume - LVEDV)

ggplot(S, aes(x = response, y = EjectionFraction)) +
  geom_point() 

meanEjectionFraction <- mean(S$EjectionFraction)
sdEjectionFraction <- sd(S$EjectionFraction)

S <- S %>% 
  mutate(EjectionFraction_standardized = (EjectionFraction - meanEjectionFraction)/sdEjectionFraction)


S <- S %>% 
  mutate(Stretch = LVEDV / LVESV)

ggplot(S, aes(x = response, y = Stretch)) +
  geom_point()

meanStretch <- mean(S$Stretch)
sdStretch <- sd(S$Stretch)

S <- S %>% 
  mutate(Stretch_standardized = (Stretch - meanStretch)/sdStretch)

Final <- S %>%
    select(response,Stretch_standardized, ST2_standardized,
             EjectionFraction_standardized, TIFRII_standardized, 
             LVESV_standardized, CRP_standardized, BNP_standardized,
           lvesv_delta)


ctrl_10_fold_CV <- trainControl(method = "repeatedcv",
                     number = 10,
                     verboseIter = TRUE)

ctrl_5_fold_CV <- trainControl(method = "repeatedcv",
                               number = 5,
                               verboseIter = TRUE)

#------------------------------------------------------------------------------

#creating both datasets for under -30 and over -30
LVESV_delta_under_negative_thirty <- Final %>%
    filter(lvesv_delta < -30)

LVESV_delta_above_negative_thirty <- Final %>%
    filter(lvesv_delta >= -30)



index_for_under <- createDataPartition(LVESV_delta_under_negative_thirty$response,
                                       times = 1,
                                       p = 0.7,
                                       list = FALSE)

index_for_over <- createDataPartition(LVESV_delta_above_negative_thirty$response,
                                       times = 1,
                                       p = 0.7,
                                       list = FALSE)


LVESV_delta_under_negative_thirty_training <- LVESV_delta_under_negative_thirty[index_for_under,]
LVESV_delta_under_negative_thirty_test <- LVESV_delta_under_negative_thirty[-index_for_under,]

LVESV_delta_above_negative_thirty_training <- LVESV_delta_above_negative_thirty[index_for_over,]
LVESV_delta_above_negative_thirty_test <- LVESV_delta_above_negative_thirty[-index_for_over,]

underAccuracies_10CV <- data.frame("parameter" = "a",
                                   Accuracy = 5,
                                   Kappa = 5,
                                   AccuracySD = 5,
                                   KappaSD = 5)

aboveAccuracies_10CV <- data.frame("parameter" = "a",
                                   Accuracy = 5,
                                   Kappa = 5,
                                   AccuracySD = 5,
                                   KappaSD = 5)

underAccuracies_5CV <- data.frame("parameter" = "a",
                                  Accuracy = 5,
                                  Kappa = 5,
                                  AccuracySD = 5,
                                  KappaSD = 5)

aboveAccuracies_5CV <- data.frame("parameter" = "a",
                                  Accuracy = 5,
                                  Kappa = 5,
                                  AccuracySD = 5,
                                  KappaSD = 5)


#CVs
for (i in 1:5)
{
LVESV_delta_under_CV <- LVESV_delta_under_negative_thirty_training[sample(nrow(LVESV_delta_under_negative_thirty_training)),]
LVESV_delta_above_CV <- LVESV_delta_above_negative_thirty_training[sample(nrow(LVESV_delta_above_negative_thirty_training)),]

#10 fold CVs
#under
logisticRegression_under_10foldCV <- train(response~.-lvesv_delta,
                                  LVESV_delta_under_CV,
                                  family = "binomial",
                                  method = "glm",
                                  trControl = ctrl_10_fold_CV,
                                  metric = "Accuracy")

logisticRegression_under_results_10foldCV <- as.data.frame(logisticRegression_under_10foldCV$results)

underAccuracies_10CV <- rbind(underAccuracies_10CV,
                              logisticRegression_under_results_10foldCV)


#above
logisticRegression_above_10foldCV <- train(response~Stretch_standardized + ST2_standardized +
                                             EjectionFraction_standardized + TIFRII_standardized + 
                                             LVESV_standardized + CRP_standardized + BNP_standardized,
                                           LVESV_delta_above_CV,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl_10_fold_CV,
                                           metric = "Accuracy")

logisticRegression_above_results_10foldCV <- as.data.frame(logisticRegression_above_10foldCV$results)

aboveAccuracies_10CV <- rbind(aboveAccuracies_10CV,
                             logisticRegression_above_results_10foldCV)






#5 fold CVs
#under
logisticRegression_under_5foldCV <- train(response~Stretch_standardized + ST2_standardized +
                                             EjectionFraction_standardized + TIFRII_standardized + 
                                             LVESV_standardized + CRP_standardized + BNP_standardized,
                                           LVESV_delta_under_CV,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl_5_fold_CV,
                                           metric = "Accuracy")

logisticRegression_under_results_5foldCV <- as.data.frame(logisticRegression_under_5foldCV$results)

underAccuracies_5CV <- rbind(underAccuracies_5CV,
                              logisticRegression_under_results_5foldCV)


#above
logisticRegression_above_5foldCV <- train(response~Stretch_standardized + ST2_standardized +
                                             EjectionFraction_standardized + TIFRII_standardized + 
                                             LVESV_standardized + CRP_standardized + BNP_standardized,
                                           LVESV_delta_above_CV,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl_5_fold_CV,
                                           metric = "Accuracy")

logisticRegression_above_results_5foldCV <- as.data.frame(logisticRegression_above_5foldCV$results)

aboveAccuracies_5CV <- rbind(aboveAccuracies_5CV,
                            logisticRegression_above_results_5foldCV)






}

