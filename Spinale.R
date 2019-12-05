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



#------------------------------------------------------------------------------



results_for_each_ML_algorithm <- data.frame(nameOfRun = "a",
                                            Accuracy = 1,
                                            Kappa = 1,
                                            AccuracyLower = 1,
                                            AccuracyUpper = 1,
                                            AccuracyNull = 1,
                                            AccuracyPValue = 1,
                                            McnemarPValue = 1,
                                            Sensitivity = 1,
                                            Specificity = 1,
                                            "Pos Pred Value" = 1,
                                            "Neg Pred Value" = 1,
                                            Precision = 1,
                                            Recall = 1,
                                            F1 = 1, 
                                            Prevalence = 1,
                                            "Detection Rate" = 1,
                                            "Detection Prevalence" = 1,
                                            "Balanced Accuracy" = 1,
                                            FalseNegative = 1,
                                            FalsePositive = 1,
                                            check.names = FALSE)
                                            



SpinaleProteinsAndResponses2 <- S %>% 
  select(MMP2_standardized, MMP9_standardized, ST2_standardized, CRP_standardized, BNP_standardized, TIMP1_standardized, TIMP2_standardized, TIM4_standardized, SPG130_standardized, SIL2RA_standardized, TIFRII_standardized, IFNG_standardized, response)




ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     verboseIter = TRUE)

ctrl_5_fold_CV <- trainControl(method = "repeatedcv",
                               number = 5,
                               verboseIter = TRUE)

variables_biochemical <- as.data.frame(c(1, 2, 3))

for (i in 1:50)
{
  SpinaleProteinsAndResponses_variables <- SpinaleProteinsAndResponses2[sample(nrow(SpinaleProteinsAndResponses2)),]
  
  indexes_variables <- createDataPartition(SpinaleProteinsAndResponses_variables$response,
                                 times = 1,
                                 p = 0.8,
                                 list = FALSE)
  
  SpinaleProteinsAndResponses_variables_training <- SpinaleProteinsAndResponses_variables[indexes_variables,]
  SpinaleProteinsAndResponses_variables_test <- SpinaleProteinsAndResponses_variables[-indexes_variables,]
  
  
  #random forest variables
  rf_predictors <- train(response ~.,
                         SpinaleProteinsAndResponses_variables_training,
                         method = "rf",
                         trControl = ctrl,
                         tuneLength = 12,
                         metric = "Accuracy")
  
  
  plot(varImp(rf_predictors, scale = TRUE))
  
  varImp(rf_predictors)
  
  imps_rf <- as.data.frame(rf_predictors$coefnames)
  important_variables_rf <- cbind(imps_rf, varImp(rf_predictors)$importance)
  
  important_variables_rf %>% arrange(desc(Overall))
  
  important_variables_over_60_rf <- important_variables_rf %>%
      filter(Overall >= 60) %>% arrange(desc(Overall))
  

  variables_biochemical <- merge(data.frame(variables_biochemical, row.names=NULL), data.frame(important_variables_over_60_rf, row.names=NULL), by = 0, all = TRUE)[-1]
  
  
  
  #lasso variables
  lasso_variables <- train(response ~.,
                           SpinaleProteinsAndResponses_variables_training,
                           family = "binomial",
                           method = "glmnet",
                           tuneGrid = expand.grid(alpha = 1,
                                                  lambda = seq(0.0000001, 0.2, length = 20)),
                           trControl = ctrl)
  

  plot(varImp(lasso_variables, scale = TRUE))
  
  imps_lasso <- as.data.frame(lasso_variables$coefnames)
  
  important_variables_lasso <- cbind(imps_lasso, varImp(lasso_variables)$importance)
  
  important_variables_lasso %>% arrange(desc(Overall))
  
  important_variables_over_60_lasso <- important_variables_lasso %>%
    filter(Overall >= 60) %>% arrange(desc(Overall))
  
  
  variables_biochemical <- merge(data.frame(variables_biochemical, row.names=NULL), data.frame(important_variables_over_60_lasso, row.names=NULL), by = 0, all = TRUE)[-1]
  
  
  
}

#the most important variables are ST2, CRP and TIFRII



#----------------------------------------------------------------------------
#standardizing clinical variables, removing outliers and finding important variables

#standardizing diastolic blood pressure and checking its 
#distribution/relationship with target variable
n1 <- length(S$bpdia)

sort(S$bpdia,partial=n1)[n1]
sort(S$bpdia,partial=n1-1)[n1-1]


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

sort(S$LVESV,partial=n1)[n1]
sort(S$LVESV,partial=n1-1)[n1-1]

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

sort(S$LVEDV,partial=n1)[n1]
sort(S$LVEDV,partial=n1-1)[n1-1]

which(S$LVEDV > 600)
S$LVEDV[c(685)] <- 513

meanLVEDV <- mean(S$LVEDV)
sdLVEDV <- sd(S$LVEDV)

S <- S %>% 
  mutate(LVEDV_standardized = (LVEDV - meanLVEDV)/sdLVEDV)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = LVEDV_standardized)) +
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









SpinaleClinicalMarkersAndResponseStandardized <- S %>% 
  select(standardized_dystolicBloodPressure, standardized_systolicBloodPressure, 
         LVESV_standardized, LVEDV_standardized, pulsePressure_standardized, 
         StrokeVolume_standardized, Stretch_standardized, EjectionFraction_standardized,
         response)




variables_clinical <- as.data.frame(c(1, 2, 3))

for (i in 1:50)
{
  Spinale_clinical_variables <- SpinaleClinicalMarkersAndResponseStandardized[sample(nrow(SpinaleClinicalMarkersAndResponseStandardized)),]
  
  indexes_clinical_variables <- createDataPartition(Spinale_clinical_variables$response,
                                                    times = 1,
                                                    p = 0.8,
                                                    list = FALSE)
  
  Spinale_clinical_variables_training <- Spinale_clinical_variables[indexes_clinical_variables,]
  Spinale_clinical_variables_test <- Spinale_clinical_variables[-indexes_clinical_variables,]
  
  
  #random forest variables
  rf_predictors_clinical <- train(response ~.,
                                  Spinale_clinical_variables_training,
                                  method = "rf",
                                  trControl = ctrl,
                                  tuneLength = 12,
                                  metric = "Accuracy")
  
  
  plot(varImp(rf_predictors_clinical, scale = TRUE))
  
  varImp(rf_predictors_clinical)
  
  imps_rf_clinical <- as.data.frame(rf_predictors_clinical$coefnames)
  important_variables_rf_clinical <- cbind(imps_rf_clinical, varImp(rf_predictors_clinical)$importance)
  
  important_variables_rf_clinical %>% arrange(desc(Overall))
  
  important_variables_over_60_rf_clinical <- important_variables_rf_clinical %>%
    filter(Overall >= 60) %>% arrange(desc(Overall))
  
  
  variables_clinical <- merge(data.frame(variables_clinical, row.names=NULL), data.frame(important_variables_over_60_rf_clinical, row.names=NULL), by = 0, all = TRUE)[-1]
  
  
  
  #lasso variables
  lasso_variables_clinical <- train(response ~.,
                                    Spinale_clinical_variables_training,
                                    family = "binomial",
                                    method = "glmnet",
                                    tuneGrid = expand.grid(alpha = 1,
                                                           lambda = seq(0.0000001, 0.2, length = 20)),
                                    trControl = ctrl)
  
  
  plot(varImp(lasso_variables_clinical, scale = TRUE))
  
  imps_lasso_clinical <- as.data.frame(lasso_variables_clinical$coefnames)
  
  important_variables_lasso <- cbind(imps_lasso_clinical, varImp(lasso_variables_clinical)$importance)
  
  important_variables_lasso %>% arrange(desc(Overall))
  
  important_variables_over_60_lasso_clinical <- important_variables_lasso %>%
    filter(Overall >= 60) %>% arrange(desc(Overall))
  
  
  variables_clinical <- merge(data.frame(variables_clinical, row.names=NULL), data.frame(important_variables_over_60_lasso_clinical, row.names=NULL), by = 0, all = TRUE)[-1]
  
  
  
}



#stroke volume really only important one

#------------------------------------------------------------------------------
#finding important variables for all standardized variables
Spinale_all_standardized <- S%>%
  select(standardized_dystolicBloodPressure, standardized_systolicBloodPressure, 
         LVESV_standardized, LVEDV_standardized, pulsePressure_standardized, 
         StrokeVolume_standardized, Stretch_standardized, EjectionFraction_standardized,
         MMP2_standardized, MMP9_standardized, ST2_standardized, CRP_standardized, BNP_standardized, 
         TIMP1_standardized, TIMP2_standardized, TIM4_standardized, SPG130_standardized,
         SIL2RA_standardized, TIFRII_standardized, IFNG_standardized, response)


variables_all <- as.data.frame(c(1,2,3))


for (i in 1:50)
{
  Spinale_all_variables <- Spinale_all_standardized[sample(nrow(Spinale_all_standardized)),]
  
  indexes_all_variables <- createDataPartition(Spinale_all_variables$response,
                                               times = 1,
                                               p = 0.8,
                                               list = FALSE)
  
  Spinale_all_train <- Spinale_all_variables[indexes_all_variables,]
  Spinale_all_test <- Spinale_all_variables[-indexes_all_variables,]
  
  
  #random forest variables
  rf_predictors_All <- train(response ~.,
                             Spinale_all_train,
                             method = "rf",
                             trControl = ctrl,
                             tuneLength = 21,
                             metric = "Accuracy")
  
  
  plot(varImp(rf_predictors_All, scale = TRUE))
  
  imps_rf_all <- as.data.frame(rf_predictors_All$coefnames)
  important_variables_rf_all <- cbind(imps_rf_all, varImp(rf_predictors_All)$importance)
  
  important_variables_rf_all %>% arrange(desc(Overall))
  
  important_variables_over_60_rf_all <- important_variables_rf_all %>%
    filter(Overall >= 60) %>% arrange(desc(Overall))
  
  
  variables_all <- merge(data.frame(variables_all, row.names=NULL), data.frame(important_variables_over_60_rf_all, row.names=NULL), by = 0, all = TRUE)[-1]
  
  
  
  #lasso variables
  lasso_variables_all <- train(response ~.,
                               Spinale_all_train,
                               family = "binomial",
                               method = "glmnet",
                               tuneGrid = expand.grid(alpha = 1,
                                                      lambda = seq(0.0000001, 0.2, length = 20)),
                               trControl = ctrl)
  
  
  plot(varImp(lasso_variables_all, scale = TRUE))
  
  imps_lasso_all <- as.data.frame(lasso_variables_all$coefnames)
  
  important_variables_lasso_all <- cbind(imps_lasso_all, varImp(lasso_variables_all)$importance)
  
  important_variables_lasso_all %>% arrange(desc(Overall))
  
  important_variables_over_60_lasso_all <- important_variables_lasso_all %>%
    filter(Overall >= 60) %>% arrange(desc(Overall))
  
  
  variables_all <- merge(data.frame(variables_all, row.names=NULL), data.frame(important_variables_over_60_lasso_all, row.names=NULL), by = 0, all = TRUE)[-1]
  
  
  
}



#------------------------------------------------------------------------------
indexes_for_final_biochemical <- createDataPartition(SpinaleProteinsAndResponses2$response,
                                         times = 1,
                                         p = 0.7,
                                         list = FALSE)

Spinale_biochemical_training <- SpinaleProteinsAndResponses2[indexes_for_final_biochemical,]
Spinale_biochemical_test <- SpinaleProteinsAndResponses2[-indexes_for_final_biochemical,]

grid <- expand.grid(C = c(2 ** -10, 2 ** -9, 2 ** -8, 2 ** -7, 2 ** -6, 2 ** -5, 2 ** -4, 2 ** -3, 2 ** -2, 2 ** -1, 2 ** 0, 
                          2, 2 ** 2, 2 ** 3, 2 ** 4, 2 ** 5, 2 ** 6, 2 ** 7, 2 **8, 2 ** 9, 2 ** 10))

grid_radial <- expand.grid(sigma = c(2 ** -10, 2 ** -9, 2 ** -8, 2 ** -7, 2 ** -6, 2 ** -5, 2 ** -4, 2 ** -3, 2 ** -2, 2 ** -1, 2 ** 0, 
                                     2, 2 ** 2, 2 ** 3, 2 ** 4, 2 ** 5, 2 ** 6, 2 ** 7, 2 **8, 2 ** 9, 2 ** 10),
                           C = c(2 ** -10, 2 ** -9, 2 ** -8, 2 ** -7, 2 ** -6, 2 ** -5, 2 ** -4, 2 ** -3, 2 ** -2, 2 ** -1, 2 ** 0, 
                                 2, 2 ** 2, 2 ** 3, 2 ** 4, 2 ** 5, 2 ** 6, 2 ** 7, 2 **8, 2 ** 9, 2 ** 10))


#all the biochemical variables first -- random forest, logistic regression,
#knn, linear SVM then radial SVM

#first 10-fold CV
Spinale_biochemical_training_1 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_1 <- train(response~.,
                          Spinale_biochemical_training_1,
                    method = "rf",
                    trControl = ctrl,
                    tuneLength = 12,
                    ntrees = 1000,
                    metric = "Accuracy")


randomForest_all_variables_results_1 <- as.data.frame(randomForest_all_variables_1$results)

randomForest_allVariables_maxAccuracies <- randomForest_all_variables_results_1 %>%
    filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_allVariables_10FoldCV_1 <- train(response ~.,
                                             data = Spinale_biochemical_training_1,
                                             family = "binomial",
                                             method = "glm",
                                             trControl = ctrl,
                                             metric = "Accuracy")

logisticRegression_all_variables_results_1 <- as.data.frame(logisticRegression_allVariables_10FoldCV_1$results)

logisticRegression_all_variables_results <- logisticRegression_all_variables_results_1

#K-NEAREST NEIGHBORS
KNN_all_variables_10FOLDCV_1 <-  train(response~.,
                                     Spinale_biochemical_training_1,
                              method = "knn",
                              trControl = ctrl,
                              tuneLength = 20,
                              metric = "Accuracy")

KNN_all_variables_results_10FOLDCV_1 <- as.data.frame(KNN_all_variables_10FOLDCV_1$results)

KNN_allVariables_10FOLDCVmaxAccuracies <- KNN_all_variables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_all_variables_10FOLDCV_1 <- train(response~.,
                             Spinale_biochemical_training_1,
                             method = "svmLinear",
                             trControl = ctrl,
                             tuneGrid = grid,
                             tuneLength = 10,
                             metric = "Accuracy")

LinearSVM_all_variables_results_10FOLDCV_1 <- as.data.frame(LinearSVM_all_variables_10FOLDCV_1$results)

LinearSVM_allVariables_10FOLDCVmaxAccuracies <- LinearSVM_all_variables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_all_variables_10FOLDCV_1 <- train(response ~.,
                                            Spinale_biochemical_training_1,
                                    method = "svmRadial",
                                    trControl = ctrl,
                                    tuneGrid = grid_radial,
                                    tuneLength = 10,
                                    metric = "Accuracy")

RadialSVM_all_variables_results_10FOLDCV_1 <- as.data.frame(RadialSVM_all_variables_10FOLDCV_1$results)

RadialSVM_allVariables_10FOLDCVmaxAccuracies <- RadialSVM_all_variables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))


#second 10-fold CV
Spinale_biochemical_training_2 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_2 <- train(response~.,
                                      Spinale_biochemical_training_2,
                                      method = "rf",
                                      trControl = ctrl,
                                      tuneLength = 12,
                                      ntrees = 1000,
                                      metric = "Accuracy")


randomForest_all_variables_results_2 <- as.data.frame(randomForest_all_variables_2$results)

randomForest_allVariables_maxAccuracies2 <- randomForest_all_variables_results_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies <- rbind(randomForest_allVariables_maxAccuracies, 
                                                 randomForest_allVariables_maxAccuracies2)

#LOGISTIC REGRESSION
logisticRegression_allVariables_10FoldCV_2 <- train(response ~.,
                                                    data = Spinale_biochemical_training_2,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_all_variables_results_2 <- as.data.frame(logisticRegression_allVariables_10FoldCV_2$results)

logisticRegression_all_variables_results <- rbind(logisticRegression_all_variables_results,
                                                  logisticRegression_all_variables_results_2)

#K-NEAREST NEIGHBORS

KNN_all_variables_10FOLDCV_2 <-  train(response~.,
                                       Spinale_biochemical_training_2,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_all_variables_results_10FOLDCV_2 <- as.data.frame(KNN_all_variables_10FOLDCV_2$results)

KNN_allVariables_10FOLDCVmaxAccuracies2 <- KNN_all_variables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_10FOLDCVmaxAccuracies <- rbind(KNN_allVariables_10FOLDCVmaxAccuracies, 
                                                 KNN_allVariables_10FOLDCVmaxAccuracies2)

#LINEAR SVM
LinearSVM_all_variables_10FOLDCV_2 <- train(response~.,
                                            Spinale_biochemical_training_2,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_all_variables_results_10FOLDCV_2 <- as.data.frame(LinearSVM_all_variables_10FOLDCV_2$results)

LinearSVM_allVariables_10FOLDCVmaxAccuracies2 <- LinearSVM_all_variables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      LinearSVM_allVariables_10FOLDCVmaxAccuracies2)

#RADIAL SVM
RadialSVM_all_variables_10FOLDCV_2 <- train(response ~.,
                                            Spinale_biochemical_training_2,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_all_variables_results_10FOLDCV_2 <- as.data.frame(RadialSVM_all_variables_10FOLDCV_2$results)

RadialSVM_allVariables_10FOLDCVmaxAccuracies2 <- RadialSVM_all_variables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      RadialSVM_allVariables_10FOLDCVmaxAccuracies2)

#third 10-fold CV
Spinale_biochemical_training_3 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_3 <- train(response~.,
                                      Spinale_biochemical_training_3,
                                      method = "rf",
                                      trControl = ctrl,
                                      tuneLength = 12,
                                      ntrees = 1000,
                                      metric = "Accuracy")

randomForest_all_variables_results_3 <- as.data.frame(randomForest_all_variables_3$results)

randomForest_allVariables_maxAccuracies3 <- randomForest_all_variables_results_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies <- rbind(randomForest_allVariables_maxAccuracies, 
                                                 randomForest_allVariables_maxAccuracies3)

#LOGISTIC REGRESSION
logisticRegression_allVariables_10FoldCV_3 <- train(response ~.,
                                                    data = Spinale_biochemical_training_3,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_all_variables_results_3 <- as.data.frame(logisticRegression_allVariables_10FoldCV_3$results)

logisticRegression_all_variables_results <- rbind(logisticRegression_all_variables_results,
                                                  logisticRegression_all_variables_results_3)

#K-NEAREST NEIGHBORS

KNN_all_variables_10FOLDCV_3 <-  train(response~.,
                                       Spinale_biochemical_training_3,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_all_variables_results_10FOLDCV_3 <- as.data.frame(KNN_all_variables_10FOLDCV_3$results)

KNN_allVariables_10FOLDCVmaxAccuracies3 <- KNN_all_variables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_10FOLDCVmaxAccuracies <- rbind(KNN_allVariables_10FOLDCVmaxAccuracies, 
                                                KNN_allVariables_10FOLDCVmaxAccuracies3)

#LINEAR SVM
LinearSVM_all_variables_10FOLDCV_3 <- train(response~.,
                                            Spinale_biochemical_training_3,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_all_variables_results_10FOLDCV_3 <- as.data.frame(LinearSVM_all_variables_10FOLDCV_3$results)

LinearSVM_allVariables_10FOLDCVmaxAccuracies3 <- LinearSVM_all_variables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      LinearSVM_allVariables_10FOLDCVmaxAccuracies3)

#RADIAL SVM
RadialSVM_all_variables_10FOLDCV_3 <- train(response ~.,
                                            Spinale_biochemical_training_3,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_all_variables_results_10FOLDCV_3 <- as.data.frame(RadialSVM_all_variables_10FOLDCV_3$results)

RadialSVM_allVariables_10FOLDCVmaxAccuracies3 <- RadialSVM_all_variables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      RadialSVM_allVariables_10FOLDCVmaxAccuracies3)

#fourth 10-fold CV
Spinale_biochemical_training_4 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_4 <- train(response~.,
                                      Spinale_biochemical_training_4,
                                      method = "rf",
                                      trControl = ctrl,
                                      tuneLength = 12,
                                      ntrees = 1000,
                                      metric = "Accuracy")

randomForest_all_variables_results_4 <- as.data.frame(randomForest_all_variables_4$results)

randomForest_allVariables_maxAccuracies4 <- randomForest_all_variables_results_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies <- rbind(randomForest_allVariables_maxAccuracies, 
                                                 randomForest_allVariables_maxAccuracies4)

#LOGISTIC REGRESSION
logisticRegression_allVariables_10FoldCV_4 <- train(response ~.,
                                                    data = Spinale_biochemical_training_4,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_all_variables_results_4 <- as.data.frame(logisticRegression_allVariables_10FoldCV_4$results)

logisticRegression_all_variables_results <- rbind(logisticRegression_all_variables_results,
                                                  logisticRegression_all_variables_results_4)

#K-NEAREST NEIGHBORS
KNN_all_variables_10FOLDCV_4 <-  train(response~.,
                                       Spinale_biochemical_training_4,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_all_variables_results_10FOLDCV_4 <- as.data.frame(KNN_all_variables_10FOLDCV_4$results)

KNN_allVariables_10FOLDCVmaxAccuracies4 <- KNN_all_variables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_10FOLDCVmaxAccuracies <- rbind(KNN_allVariables_10FOLDCVmaxAccuracies, 
                                                KNN_allVariables_10FOLDCVmaxAccuracies4)

#LINEAR SVM
LinearSVM_all_variables_10FOLDCV_4 <- train(response~.,
                                            Spinale_biochemical_training_4,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_all_variables_results_10FOLDCV_4 <- as.data.frame(LinearSVM_all_variables_10FOLDCV_4$results)

LinearSVM_allVariables_10FOLDCVmaxAccuracies4 <- LinearSVM_all_variables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      LinearSVM_allVariables_10FOLDCVmaxAccuracies4)

#RADIAL SVM
RadialSVM_all_variables_10FOLDCV_4 <- train(response ~.,
                                            Spinale_biochemical_training_4,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_all_variables_results_10FOLDCV_4 <- as.data.frame(RadialSVM_all_variables_10FOLDCV_4$results)

RadialSVM_allVariables_10FOLDCVmaxAccuracies4 <- RadialSVM_all_variables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      RadialSVM_allVariables_10FOLDCVmaxAccuracies4)

#fifth 10-fold CV
Spinale_biochemical_training_5 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_5 <- train(response~.,
                                      Spinale_biochemical_training_5,
                                      method = "rf",
                                      trControl = ctrl,
                                      tuneLength = 12,
                                      ntrees = 1000,
                                      metric = "Accuracy")

randomForest_all_variables_results_5 <- as.data.frame(randomForest_all_variables_5$results)

randomForest_allVariables_maxAccuracies5 <- randomForest_all_variables_results_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies <- rbind(randomForest_allVariables_maxAccuracies, 
                                                 randomForest_allVariables_maxAccuracies5)

#LOGISTIC REGRESSION
logisticRegression_allVariables_10FoldCV_5 <- train(response ~.,
                                                    data = Spinale_biochemical_training_5,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_all_variables_results_5 <- as.data.frame(logisticRegression_allVariables_10FoldCV_5$results)

logisticRegression_all_variables_results <- rbind(logisticRegression_all_variables_results,
                                                  logisticRegression_all_variables_results_5)

#K-NEAREST NEIGHBORS
KNN_all_variables_10FOLDCV_5 <-  train(response~.,
                                       Spinale_biochemical_training_5,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_all_variables_results_10FOLDCV_5 <- as.data.frame(KNN_all_variables_10FOLDCV_5$results)

KNN_allVariables_10FOLDCVmaxAccuracies5 <- KNN_all_variables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_10FOLDCVmaxAccuracies <- rbind(KNN_allVariables_10FOLDCVmaxAccuracies, 
                                                KNN_allVariables_10FOLDCVmaxAccuracies5)

#LINEAR SVM
LinearSVM_all_variables_10FOLDCV_5 <- train(response~.,
                                            Spinale_biochemical_training_5,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_all_variables_results_10FOLDCV_5 <- as.data.frame(LinearSVM_all_variables_10FOLDCV_5$results)

LinearSVM_allVariables_10FOLDCVmaxAccuracies5 <- LinearSVM_all_variables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      LinearSVM_allVariables_10FOLDCVmaxAccuracies5)
#RADIAL SVM
RadialSVM_all_variables_10FOLDCV_5 <- train(response ~.,
                                            Spinale_biochemical_training_5,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_all_variables_results_10FOLDCV_5 <- as.data.frame(RadialSVM_all_variables_10FOLDCV_5$results)

RadialSVM_allVariables_10FOLDCVmaxAccuracies5 <- RadialSVM_all_variables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_10FOLDCVmaxAccuracies, 
                                                      RadialSVM_allVariables_10FOLDCVmaxAccuracies5)

#-----------------------------------------------------------------------------------------
#now let's do 5 5-fold CVs 
Spinale_biochemical_training_5FoldCV_1 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_5FoldCV_1 <- train(response~.,
                                        Spinale_biochemical_training_5FoldCV_1,
                                      method = "rf",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 12,
                                      ntrees = 1000,
                                      metric = "Accuracy")


randomForest_all_variables_results_5FoldCV_1 <- as.data.frame(randomForest_all_variables_5FoldCV_1$results)

randomForest_allVariables_maxAccuracies_5FoldCV <- randomForest_all_variables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_allVariables_5FoldCV_1 <- train(response ~.,
                                                    data = Spinale_biochemical_training_5FoldCV_1,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl_5_fold_CV,
                                                    metric = "Accuracy")

logisticRegression_all_variables_5FOLDCV_results_1 <- as.data.frame(logisticRegression_allVariables_5FoldCV_1$results)

logisticRegression_all_variables_results_5FOLDCV <- logisticRegression_all_variables_5FOLDCV_results_1

#K-NEAREST NEIGHBORS
KNN_all_variables_5FOLDCV_1 <-  train(response~.,
                                      Spinale_biochemical_training_5FoldCV_1,
                                       method = "knn",
                                       trControl = ctrl_5_fold_CV,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_all_variables_results_5FOLDCV_1 <- as.data.frame(KNN_all_variables_5FOLDCV_1$results)

KNN_allVariables_5FOLDCVmaxAccuracies <- KNN_all_variables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_all_variables_5FOLDCV_1 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_1,
                                            method = "svmLinear",
                                            trControl = ctrl_5_fold_CV,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_all_variables_results_5FOLDCV_1 <- as.data.frame(LinearSVM_all_variables_5FOLDCV_1$results)

LinearSVM_allVariables_5FOLDCVmaxAccuracies <- LinearSVM_all_variables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_all_variables_5FOLDCV_1 <- train(response ~.,
                                           Spinale_biochemical_training_5FoldCV_1,
                                            method = "svmRadial",
                                            trControl = ctrl_5_fold_CV,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_all_variables_results_5FOLDCV_1 <- as.data.frame(RadialSVM_all_variables_5FOLDCV_1$results)

RadialSVM_allVariables_5FOLDCVmaxAccuracies <- RadialSVM_all_variables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#second 5-fold CV
Spinale_biochemical_training_5FoldCV_2 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_5FoldCV_2 <- train(response~.,
                                      Spinale_biochemical_training_5FoldCV_2,
                                      method = "rf",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 12,
                                      ntrees = 1000,
                                      metric = "Accuracy")


randomForest_all_variables_results_5FoldCV_2 <- as.data.frame(randomForest_all_variables_5FoldCV_2$results)

randomForest_allVariables_maxAccuracies_5FoldCV_2 <- randomForest_all_variables_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allVariables_maxAccuracies_5FoldCV, 
                                                         randomForest_allVariables_maxAccuracies_5FoldCV_2)

#LOGISTIC REGRESSION
logisticRegression_allVariables_5FoldCV_2 <- train(response ~.,
                                                    data = Spinale_biochemical_training_5FoldCV_2,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl_5_fold_CV,
                                                    metric = "Accuracy")

logisticRegression_all_variables_5FOLDCV_results_2 <- as.data.frame(logisticRegression_allVariables_5FoldCV_2$results)

logisticRegression_all_variables_results_5FOLDCV <- rbind(logisticRegression_all_variables_results_5FOLDCV,
                                                  logisticRegression_all_variables_5FOLDCV_results_2)

#K-NEAREST NEIGHBORS
KNN_all_variables_5FOLDCV_2 <-  train(response~.,
                                      Spinale_biochemical_training_5FoldCV_2,
                                       method = "knn",
                                       trControl = ctrl_5_fold_CV,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_all_variables_results_5FOLDCV_2 <- as.data.frame(KNN_all_variables_5FOLDCV_2$results)

KNN_allVariables_5FOLDCVmaxAccuracies2 <- KNN_all_variables_results_5FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_5FOLDCVmaxAccuracies <- rbind(KNN_allVariables_5FOLDCVmaxAccuracies, 
                                               KNN_allVariables_5FOLDCVmaxAccuracies2)

#LINEAR SVM
LinearSVM_all_variables_5FOLDCV_2 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_2,
                                            method = "svmLinear",
                                            trControl = ctrl_5_fold_CV,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_all_variables_results_5FOLDCV_2 <- as.data.frame(LinearSVM_all_variables_5FOLDCV_2$results)

LinearSVM_allVariables_5FOLDCVmaxAccuracies2 <- LinearSVM_all_variables_results_5FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_allVariables_5FOLDCVmaxAccuracies2)

#RADIAL SVM
RadialSVM_all_variables_5FOLDCV_2 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_2,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_all_variables_results_5FOLDCV_2 <- as.data.frame(RadialSVM_all_variables_5FOLDCV_2$results)

RadialSVM_allVariables_5FOLDCVmaxAccuracies2 <- RadialSVM_all_variables_results_5FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     RadialSVM_allVariables_5FOLDCVmaxAccuracies2)

#third 5-fold CV

Spinale_biochemical_training_5FoldCV_3 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_5FoldCV_3 <- train(response~.,
                                              Spinale_biochemical_training_5FoldCV_3,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 12,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_all_variables_results_5FoldCV_3 <- as.data.frame(randomForest_all_variables_5FoldCV_3$results)

randomForest_allVariables_maxAccuracies_5FoldCV_3 <- randomForest_all_variables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allVariables_maxAccuracies_5FoldCV, 
                                                         randomForest_allVariables_maxAccuracies_5FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_allVariables_5FoldCV_3 <- train(response ~.,
                                                   data = Spinale_biochemical_training_5FoldCV_3,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_all_variables_5FOLDCV_results_3 <- as.data.frame(logisticRegression_allVariables_5FoldCV_3$results)

logisticRegression_all_variables_results_5FOLDCV <- rbind(logisticRegression_all_variables_results_5FOLDCV,
                                                          logisticRegression_all_variables_5FOLDCV_results_3)

#K-NEAREST NEIGHBORS
KNN_all_variables_5FOLDCV_3 <-  train(response~.,
                                      Spinale_biochemical_training_5FoldCV_3,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_all_variables_results_5FOLDCV_3 <- as.data.frame(KNN_all_variables_5FOLDCV_3$results)

KNN_allVariables_5FOLDCVmaxAccuracies3 <- KNN_all_variables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_5FOLDCVmaxAccuracies <- rbind(KNN_allVariables_5FOLDCVmaxAccuracies, 
                                               KNN_allVariables_5FOLDCVmaxAccuracies3)
#LINEAR SVM
LinearSVM_all_variables_5FOLDCV_3 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_3,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_all_variables_results_5FOLDCV_3 <- as.data.frame(LinearSVM_all_variables_5FOLDCV_3$results)

LinearSVM_allVariables_5FOLDCVmaxAccuracies3 <- LinearSVM_all_variables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     LinearSVM_allVariables_5FOLDCVmaxAccuracies3)

#RADIAL SVM
RadialSVM_all_variables_5FOLDCV_3 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_3,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_all_variables_results_5FOLDCV_3 <- as.data.frame(RadialSVM_all_variables_5FOLDCV_3$results)

RadialSVM_allVariables_5FOLDCVmaxAccuracies3 <- RadialSVM_all_variables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     RadialSVM_allVariables_5FOLDCVmaxAccuracies3)


#fourth 5-CV 
Spinale_biochemical_training_5FoldCV_4 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_5FoldCV_4 <- train(response~.,
                                              Spinale_biochemical_training_5FoldCV_4,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 12,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_all_variables_results_5FoldCV_4 <- as.data.frame(randomForest_all_variables_5FoldCV_4$results)

randomForest_allVariables_maxAccuracies_5FoldCV_4 <- randomForest_all_variables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allVariables_maxAccuracies_5FoldCV, 
                                                         randomForest_allVariables_maxAccuracies_5FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_allVariables_5FoldCV_4 <- train(response ~.,
                                                   data = Spinale_biochemical_training_5FoldCV_4,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_all_variables_5FOLDCV_results_4 <- as.data.frame(logisticRegression_allVariables_5FoldCV_4$results)

logisticRegression_all_variables_results_5FOLDCV <- rbind(logisticRegression_all_variables_results_5FOLDCV,
                                                          logisticRegression_all_variables_5FOLDCV_results_4)

#K-NEAREST NEIGHBORS
KNN_all_variables_5FOLDCV_4 <-  train(response~.,
                                      Spinale_biochemical_training_5FoldCV_4,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_all_variables_results_5FOLDCV_4 <- as.data.frame(KNN_all_variables_5FOLDCV_4$results)

KNN_allVariables_5FOLDCVmaxAccuracies4 <- KNN_all_variables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_5FOLDCVmaxAccuracies <- rbind(KNN_allVariables_5FOLDCVmaxAccuracies, 
                                               KNN_allVariables_5FOLDCVmaxAccuracies4)

#LINEAR SVM
LinearSVM_all_variables_5FOLDCV_4 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_4,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_all_variables_results_5FOLDCV_4 <- as.data.frame(LinearSVM_all_variables_5FOLDCV_4$results)

LinearSVM_allVariables_5FOLDCVmaxAccuracies4 <- LinearSVM_all_variables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     LinearSVM_allVariables_5FOLDCVmaxAccuracies4)

#RADIAL SVM
RadialSVM_all_variables_5FOLDCV_4 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_4,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_all_variables_results_5FOLDCV_4 <- as.data.frame(RadialSVM_all_variables_5FOLDCV_4$results)

RadialSVM_allVariables_5FOLDCVmaxAccuracies4 <- RadialSVM_all_variables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     RadialSVM_allVariables_5FOLDCVmaxAccuracies4)

#fifth 10-CV
Spinale_biochemical_training_5FoldCV_5 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_all_variables_5FoldCV_5 <- train(response~.,
                                              Spinale_biochemical_training_5FoldCV_5,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 12,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_all_variables_results_5FoldCV_5 <- as.data.frame(randomForest_all_variables_5FoldCV_5$results)

randomForest_allVariables_maxAccuracies_5FoldCV_5 <- randomForest_all_variables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allVariables_maxAccuracies_5FoldCV, 
                                                         randomForest_allVariables_maxAccuracies_5FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_allVariables_5FoldCV_5 <- train(response ~.,
                                                   data = Spinale_biochemical_training_5FoldCV_5,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_all_variables_5FOLDCV_results_5 <- as.data.frame(logisticRegression_allVariables_5FoldCV_5$results)

logisticRegression_all_variables_results_5FOLDCV <- rbind(logisticRegression_all_variables_results_5FOLDCV,
                                                          logisticRegression_all_variables_5FOLDCV_results_5)

#K-NEAREST NEIGHBORS
KNN_all_variables_5FOLDCV_5 <-  train(response~.,
                                      Spinale_biochemical_training_5FoldCV_5,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_all_variables_results_5FOLDCV_5 <- as.data.frame(KNN_all_variables_5FOLDCV_5$results)

KNN_allVariables_5FOLDCVmaxAccuracies5 <- KNN_all_variables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_allVariables_5FOLDCVmaxAccuracies <- rbind(KNN_allVariables_5FOLDCVmaxAccuracies, 
                                               KNN_allVariables_5FOLDCVmaxAccuracies5)

#LINEAR SVM
LinearSVM_all_variables_5FOLDCV_5 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_5,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_all_variables_results_5FOLDCV_5 <- as.data.frame(LinearSVM_all_variables_5FOLDCV_5$results)

LinearSVM_allVariables_5FOLDCVmaxAccuracies5 <- LinearSVM_all_variables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     LinearSVM_allVariables_5FOLDCVmaxAccuracies5)

#RADIAL SVM
RadialSVM_all_variables_5FOLDCV_5 <- train(response~.,
                                           Spinale_biochemical_training_5FoldCV_5,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_all_variables_results_5FOLDCV_5 <- as.data.frame(RadialSVM_all_variables_5FOLDCV_5$results)

RadialSVM_allVariables_5FOLDCVmaxAccuracies5 <- RadialSVM_all_variables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_allVariables_5FOLDCVmaxAccuracies, 
                                                     RadialSVM_allVariables_5FOLDCVmaxAccuracies5)

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

#now we do 10 fold and 5 fold CV for the base variables (MMP2, CRP, ST2, TIFRII)

#first 10-fold CV
Spinale_biochemical_training_baseVariables_1 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_base_variables_10FOLDCV_1 <- train(response~ MMP2_standardized + CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_baseVariables_1,
                                      method = "rf",
                                      trControl = ctrl,
                                      tuneLength = 4,
                                      ntrees = 1000,
                                      metric = "Accuracy")

randomForest_baseVariables_results_10FOLDCV_1 <- as.data.frame(randomForest_base_variables_10FOLDCV_1$results)

randomForest_baseVariables_maxAccuracies_10FOLDCV <- randomForest_baseVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_baseVariables_10FoldCV_1 <- train(response ~MMP2_standardized + CRP_standardized + 
                                                       ST2_standardized + TIFRII_standardized,
                                                    data = Spinale_biochemical_training_baseVariables_1,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_baseVariables_results_10FOLDCV_1 <- as.data.frame(logisticRegression_baseVariables_10FoldCV_1$results)

logisticRegression_baseVariables_results_10FOLDCV <- logisticRegression_baseVariables_results_10FOLDCV_1

#K-NEAREST NEIGHBORS
KNN_baseVariables_10FoldCV_1 <-  train(response ~MMP2_standardized + CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       data = Spinale_biochemical_training_baseVariables_1,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_baseVariables_results_10FOLDCV_1 <- as.data.frame(KNN_baseVariables_10FoldCV_1$results)

KNN_baseVariables_maxAccuracies_10FOLDCV <- KNN_baseVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_baseVariables_10FOLDCV_1 <- train(response~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            data = Spinale_biochemical_training_baseVariables_1,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_baseVariables_results_10FOLDCV_1 <- as.data.frame(LinearSVM_baseVariables_10FOLDCV_1$results)

LinearSVM_baseVariables_maxAccuracies_10FOLDCV <- LinearSVM_baseVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_baseVariables_10FOLDCV_1 <- train(response~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            data = Spinale_biochemical_training_baseVariables_1,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_baseVariables_results_10FOLDCV_1 <- as.data.frame(RadialSVM_baseVariables_10FOLDCV_1$results)

RadialSVM_baseVariables_maxAccuracies_10FOLDCV <- RadialSVM_baseVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#second 10-fold CV
Spinale_biochemical_training_baseVariables_2 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_base_variables_10FOLDCV_2 <- train(response~MMP2_standardized + CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_baseVariables_2,
                                      method = "rf",
                                      trControl = ctrl,
                                      tuneLength = 4,
                                      ntrees = 1000,
                                      metric = "Accuracy")


randomForest_baseVariables_results_10FOLDCV_2 <- as.data.frame(randomForest_base_variables_10FOLDCV_2$results)

randomForest_allVariables_maxAccuracies2 <- randomForest_baseVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_10FOLDCV <- rbind(randomForest_baseVariables_maxAccuracies_10FOLDCV, 
                                                           randomForest_allVariables_maxAccuracies2)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_10FoldCV_2 <- train(response ~MMP2_standardized + CRP_standardized + 
                                                       ST2_standardized + TIFRII_standardized,
                                                    data = Spinale_biochemical_training_baseVariables_2,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_basevariables_results_2 <- as.data.frame(logisticRegression_baseVariables_10FoldCV_2$results)

logisticRegression_baseVariables_results_10FOLDCV <- rbind(logisticRegression_baseVariables_results_10FOLDCV,
                                                  logisticRegression_basevariables_results_2)

#K-NEAREST NEIGHBORS

KNN_baseVariables_10FOLDCV_2 <-  train(response ~MMP2_standardized + CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_baseVariables_2,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_baseVariables_results_10FOLDCV_2 <- as.data.frame(KNN_baseVariables_10FOLDCV_2$results)

KNN_baseVariables_10FOLDCVmaxAccuracies2 <- KNN_baseVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_maxAccuracies_10FOLDCV <- rbind(KNN_baseVariables_maxAccuracies_10FOLDCV, 
                                                KNN_baseVariables_10FOLDCVmaxAccuracies2)

#LINEAR SVM
LinearSVM_baseVariables_10FOLDCV_2 <- train(response ~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_2,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_baseVariables_results_10FOLDCV_2 <- as.data.frame(LinearSVM_baseVariables_10FOLDCV_2$results)

LinearSVM_baseVariables_10FOLDCVmaxAccuracies2 <- LinearSVM_baseVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(LinearSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                      LinearSVM_baseVariables_10FOLDCVmaxAccuracies2)

#RADIAL SVM
RadialSVM_baseVariables_10FOLDCV_2 <- train(response~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_2,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_baseVariables_results_10FOLDCV_2 <- as.data.frame(RadialSVM_baseVariables_10FOLDCV_2$results)

RadialSVM_baseVariables_10FOLDCVmaxAccuracies2 <- RadialSVM_baseVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(RadialSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                      RadialSVM_baseVariables_10FOLDCVmaxAccuracies2)

#third 10-fold CV
Spinale_biochemical_training_baseVariables_3 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_base_variables_10FOLDCV_3 <- train(response~MMP2_standardized + CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_baseVariables_3,
                                                method = "rf",
                                                trControl = ctrl,
                                                tuneLength = 4,
                                                ntrees = 1000,
                                                metric = "Accuracy")


randomForest_baseVariables_results_10FOLDCV_3 <- as.data.frame(randomForest_base_variables_10FOLDCV_3$results)

randomForest_allVariables_maxAccuracies3 <- randomForest_baseVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_10FOLDCV <- rbind(randomForest_baseVariables_maxAccuracies_10FOLDCV, 
                                                           randomForest_allVariables_maxAccuracies3)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_10FoldCV_3 <- train(response ~MMP2_standardized + CRP_standardized + 
                                                       ST2_standardized + TIFRII_standardized,
                                                     data = Spinale_biochemical_training_baseVariables_3,
                                                     family = "binomial",
                                                     method = "glm",
                                                     trControl = ctrl,
                                                     metric = "Accuracy")

logisticRegression_basevariables_results_3 <- as.data.frame(logisticRegression_baseVariables_10FoldCV_3$results)

logisticRegression_baseVariables_results_10FOLDCV <- rbind(logisticRegression_baseVariables_results_10FOLDCV,
                                                           logisticRegression_basevariables_results_3)

#K-NEAREST NEIGHBORS

KNN_baseVariables_10FOLDCV_3 <-  train(response ~MMP2_standardized + CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_baseVariables_3,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_baseVariables_results_10FOLDCV_3 <- as.data.frame(KNN_baseVariables_10FOLDCV_3$results)

KNN_baseVariables_10FOLDCVmaxAccuracies3 <- KNN_baseVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_maxAccuracies_10FOLDCV <- rbind(KNN_baseVariables_maxAccuracies_10FOLDCV, 
                                                  KNN_baseVariables_10FOLDCVmaxAccuracies3)

#LINEAR SVM
LinearSVM_baseVariables_10FOLDCV_3 <- train(response ~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_3,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_baseVariables_results_10FOLDCV_3 <- as.data.frame(LinearSVM_baseVariables_10FOLDCV_3$results)

LinearSVM_baseVariables_10FOLDCVmaxAccuracies3 <- LinearSVM_baseVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(LinearSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                        LinearSVM_baseVariables_10FOLDCVmaxAccuracies3)

#RADIAL SVM
RadialSVM_baseVariables_10FOLDCV_3 <- train(response~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_3,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_baseVariables_results_10FOLDCV_3 <- as.data.frame(RadialSVM_baseVariables_10FOLDCV_3$results)

RadialSVM_baseVariables_10FOLDCVmaxAccuracies3 <- RadialSVM_baseVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(RadialSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                        RadialSVM_baseVariables_10FOLDCVmaxAccuracies3)


#fourth 10-fold CV
Spinale_biochemical_training_baseVariables_4 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_base_variables_10FOLDCV_4 <- train(response~MMP2_standardized + CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_baseVariables_4,
                                                method = "rf",
                                                trControl = ctrl,
                                                tuneLength = 4,
                                                ntrees = 1000,
                                                metric = "Accuracy")


randomForest_baseVariables_results_10FOLDCV_4 <- as.data.frame(randomForest_base_variables_10FOLDCV_4$results)

randomForest_allVariables_maxAccuracies4 <- randomForest_baseVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_10FOLDCV <- rbind(randomForest_baseVariables_maxAccuracies_10FOLDCV, 
                                                           randomForest_allVariables_maxAccuracies4)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_10FoldCV_4 <- train(response ~MMP2_standardized + CRP_standardized + 
                                                       ST2_standardized + TIFRII_standardized,
                                                     data = Spinale_biochemical_training_baseVariables_4,
                                                     family = "binomial",
                                                     method = "glm",
                                                     trControl = ctrl,
                                                     metric = "Accuracy")

logisticRegression_basevariables_results_4 <- as.data.frame(logisticRegression_baseVariables_10FoldCV_4$results)

logisticRegression_baseVariables_results_10FOLDCV <- rbind(logisticRegression_baseVariables_results_10FOLDCV,
                                                           logisticRegression_basevariables_results_4)

#K-NEAREST NEIGHBORS

KNN_baseVariables_10FOLDCV_4 <-  train(response ~MMP2_standardized + CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_baseVariables_4,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_baseVariables_results_10FOLDCV_4 <- as.data.frame(KNN_baseVariables_10FOLDCV_4$results)

KNN_baseVariables_10FOLDCVmaxAccuracies4 <- KNN_baseVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_maxAccuracies_10FOLDCV <- rbind(KNN_baseVariables_maxAccuracies_10FOLDCV, 
                                                  KNN_baseVariables_10FOLDCVmaxAccuracies4)

#LINEAR SVM
LinearSVM_baseVariables_10FOLDCV_4 <- train(response ~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_4,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_baseVariables_results_10FOLDCV_4 <- as.data.frame(LinearSVM_baseVariables_10FOLDCV_4$results)

LinearSVM_baseVariables_10FOLDCVmaxAccuracies4 <- LinearSVM_baseVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(LinearSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                        LinearSVM_baseVariables_10FOLDCVmaxAccuracies4)

#RADIAL SVM
RadialSVM_baseVariables_10FOLDCV_4 <- train(response~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_4,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_baseVariables_results_10FOLDCV_4 <- as.data.frame(RadialSVM_baseVariables_10FOLDCV_4$results)

RadialSVM_baseVariables_10FOLDCVmaxAccuracies4 <- RadialSVM_baseVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(RadialSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                        RadialSVM_baseVariables_10FOLDCVmaxAccuracies4)


#fifth 10-fold CV
Spinale_biochemical_training_baseVariables_5 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_base_variables_10FOLDCV_5 <- train(response~MMP2_standardized + CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_baseVariables_5,
                                                method = "rf",
                                                trControl = ctrl,
                                                tuneLength = 4,
                                                ntrees = 1000,
                                                metric = "Accuracy")


randomForest_baseVariables_results_10FOLDCV_5 <- as.data.frame(randomForest_base_variables_10FOLDCV_5$results)

randomForest_allVariables_maxAccuracies5 <- randomForest_baseVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_10FOLDCV <- rbind(randomForest_baseVariables_maxAccuracies_10FOLDCV, 
                                                           randomForest_allVariables_maxAccuracies5)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_10FoldCV_5 <- train(response ~MMP2_standardized + CRP_standardized + 
                                                       ST2_standardized + TIFRII_standardized,
                                                     data = Spinale_biochemical_training_baseVariables_5,
                                                     family = "binomial",
                                                     method = "glm",
                                                     trControl = ctrl,
                                                     metric = "Accuracy")

logisticRegression_basevariables_results_5 <- as.data.frame(logisticRegression_baseVariables_10FoldCV_5$results)

logisticRegression_baseVariables_results_10FOLDCV <- rbind(logisticRegression_baseVariables_results_10FOLDCV,
                                                           logisticRegression_basevariables_results_5)

#K-NEAREST NEIGHBORS

KNN_baseVariables_10FOLDCV_5 <-  train(response ~MMP2_standardized + CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_baseVariables_5,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_baseVariables_results_10FOLDCV_5 <- as.data.frame(KNN_baseVariables_10FOLDCV_5$results)

KNN_baseVariables_10FOLDCVmaxAccuracies5 <- KNN_baseVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_maxAccuracies_10FOLDCV <- rbind(KNN_baseVariables_maxAccuracies_10FOLDCV, 
                                                  KNN_baseVariables_10FOLDCVmaxAccuracies5)

#LINEAR SVM
LinearSVM_baseVariables_10FOLDCV_5 <- train(response ~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_5,
                                            method = "svmLinear",
                                            trControl = ctrl,
                                            tuneGrid = grid,
                                            tuneLength = 10,
                                            metric = "Accuracy")

LinearSVM_baseVariables_results_10FOLDCV_5 <- as.data.frame(LinearSVM_baseVariables_10FOLDCV_5$results)

LinearSVM_baseVariables_10FOLDCVmaxAccuracies5 <- LinearSVM_baseVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(LinearSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                        LinearSVM_baseVariables_10FOLDCVmaxAccuracies5)

#RADIAL SVM
RadialSVM_baseVariables_10FOLDCV_5 <- train(response~MMP2_standardized + CRP_standardized + 
                                              ST2_standardized + TIFRII_standardized,
                                            Spinale_biochemical_training_baseVariables_5,
                                            method = "svmRadial",
                                            trControl = ctrl,
                                            tuneGrid = grid_radial,
                                            tuneLength = 10,
                                            metric = "Accuracy")

RadialSVM_baseVariables_results_10FOLDCV_5 <- as.data.frame(RadialSVM_baseVariables_10FOLDCV_5$results)

RadialSVM_baseVariables_10FOLDCVmaxAccuracies5 <- RadialSVM_baseVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_maxAccuracies_10FOLDCV <- rbind(RadialSVM_baseVariables_maxAccuracies_10FOLDCV, 
                                                        RadialSVM_baseVariables_10FOLDCVmaxAccuracies5)

#-----------------------------------------------------------------------------------------
#now let's do 5 5-fold CVs 
Spinale_biochemical_training_baseVariables_5FoldCV_1 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_baseVariables_5FoldCV_1 <- train(response~MMP2_standardized + CRP_standardized + 
                                                ST2_standardized + TIFRII_standardized,
                                              Spinale_biochemical_training_baseVariables_5FoldCV_1,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 12,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_baseVariables_results_5FoldCV_1 <- as.data.frame(randomForest_baseVariables_5FoldCV_1$results)

randomForest_baseVariables_maxAccuracies_5FoldCV <- randomForest_baseVariables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_baseVariables_5FoldCV_1 <- train(response~MMP2_standardized + CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                   data = Spinale_biochemical_training_baseVariables_5FoldCV_1,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_baseVariables_5FOLDCV_results_1 <- as.data.frame(logisticRegression_baseVariables_5FoldCV_1$results)

logisticRegression_baseVariables_results_5FOLDCV <- logisticRegression_baseVariables_5FOLDCV_results_1

#K-NEAREST NEIGHBORS
KNN_baseVariables_5FOLDCV_1 <-  train(response~MMP2_standardized + CRP_standardized + 
                                        ST2_standardized + TIFRII_standardized,
                                      Spinale_biochemical_training_baseVariables_5FoldCV_1,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_baseVariables_results_5FOLDCV_1 <- as.data.frame(KNN_baseVariables_5FOLDCV_1$results)

KNN_baseVariables_5FOLDCVmaxAccuracies <- KNN_baseVariables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_baseVariables_5FOLDCV_1 <- train(response~MMP2_standardized + CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_1,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_baseVariables_results_5FOLDCV_1 <- as.data.frame(LinearSVM_baseVariables_5FOLDCV_1$results)

LinearSVM_baseVariables_5FOLDCVmaxAccuracies <- LinearSVM_baseVariables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_baseVariables_5FOLDCV_1 <- train(response ~MMP2_standardized + CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_1,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_baseVariables_results_5FOLDCV_1 <- as.data.frame(RadialSVM_baseVariables_5FOLDCV_1$results)

RadialSVM_baseVariables_5FOLDCVmaxAccuracies <- RadialSVM_baseVariables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#second 5-fold CV
Spinale_biochemical_training_baseVariables_5FoldCV_3 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_baseVariables_5FoldCV_3 <- train(response~MMP2_standardized + CRP_standardized + 
                                                ST2_standardized + TIFRII_standardized,
                                              Spinale_biochemical_training_baseVariables_5FoldCV_3,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 4,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_baseVariables_results_5FoldCV_3 <- as.data.frame(randomForest_baseVariables_5FoldCV_3$results)

randomForest_baseVariables_maxAccuracies_5FoldCV_3 <- randomForest_baseVariables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_5FoldCV <- rbind(randomForest_baseVariables_maxAccuracies_5FoldCV, 
                                                          randomForest_baseVariables_maxAccuracies_5FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_5FoldCV_3 <- train(response ~ MMP2_standardized + CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                   data = Spinale_biochemical_training_baseVariables_5FoldCV_3,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_baseVariables_5FOLDCV_results_3 <- as.data.frame(logisticRegression_baseVariables_5FoldCV_3$results)

logisticRegression_baseVariables_results_5FOLDCV <- rbind(logisticRegression_baseVariables_results_5FOLDCV,
                                                          logisticRegression_baseVariables_5FOLDCV_results_3)

#K-NEAREST NEIGHBORS
KNN_baseVariables_5FOLDCV_3 <-  train(response ~ MMP2_standardized + CRP_standardized + 
                                        ST2_standardized + TIFRII_standardized,
                                      Spinale_biochemical_training_baseVariables_5FoldCV_3,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_baseVariables_results_5FOLDCV_3 <- as.data.frame(KNN_baseVariables_5FOLDCV_3$results)

KNN_baseVariables_5FOLDCVmaxAccuracies3 <- KNN_baseVariables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_5FOLDCVmaxAccuracies <- rbind(KNN_baseVariables_5FOLDCVmaxAccuracies, 
                                                KNN_baseVariables_5FOLDCVmaxAccuracies3)

#LINEAR SVM
LinearSVM_baseVariables_5FOLDCV_3 <- train(response~.,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_3,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_baseVariables_results_5FOLDCV_3 <- as.data.frame(LinearSVM_baseVariables_5FOLDCV_3$results)

LinearSVM_baseVariables_5FOLDCVmaxAccuracies3 <- LinearSVM_baseVariables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_baseVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_baseVariables_5FOLDCVmaxAccuracies3)

#RADIAL SVM
RadialSVM_baseVariables_5FOLDCV_3 <- train(response~.,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_3,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_baseVariables_results_5FOLDCV_3 <- as.data.frame(RadialSVM_baseVariables_5FOLDCV_3$results)

RadialSVM_baseVariables_5FOLDCVmaxAccuracies3 <- RadialSVM_baseVariables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_baseVariables_5FOLDCVmaxAccuracies, 
                                                      RadialSVM_baseVariables_5FOLDCVmaxAccuracies3)

#fourth 5-fold CV
Spinale_biochemical_training_baseVariables_5FoldCV_4 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_baseVariables_5FoldCV_4 <- train(response~MMP2_standardized + CRP_standardized + 
                                                ST2_standardized + TIFRII_standardized,
                                              Spinale_biochemical_training_baseVariables_5FoldCV_4,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 4,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_baseVariables_results_5FoldCV_4 <- as.data.frame(randomForest_baseVariables_5FoldCV_4$results)

randomForest_baseVariables_maxAccuracies_5FoldCV_4 <- randomForest_baseVariables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_5FoldCV <- rbind(randomForest_baseVariables_maxAccuracies_5FoldCV, 
                                                          randomForest_baseVariables_maxAccuracies_5FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_5FoldCV_4 <- train(response ~ MMP2_standardized + CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    data = Spinale_biochemical_training_baseVariables_5FoldCV_4,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl_5_fold_CV,
                                                    metric = "Accuracy")

logisticRegression_baseVariables_5FOLDCV_results_4 <- as.data.frame(logisticRegression_baseVariables_5FoldCV_4$results)

logisticRegression_baseVariables_results_5FOLDCV <- rbind(logisticRegression_baseVariables_results_5FOLDCV,
                                                          logisticRegression_baseVariables_5FOLDCV_results_4)

#K-NEAREST NEIGHBORS
KNN_baseVariables_5FOLDCV_4 <-  train(response ~ MMP2_standardized + CRP_standardized + 
                                        ST2_standardized + TIFRII_standardized,
                                      Spinale_biochemical_training_baseVariables_5FoldCV_4,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_baseVariables_results_5FOLDCV_4 <- as.data.frame(KNN_baseVariables_5FOLDCV_4$results)

KNN_baseVariables_5FOLDCVmaxAccuracies4 <- KNN_baseVariables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_5FOLDCVmaxAccuracies <- rbind(KNN_baseVariables_5FOLDCVmaxAccuracies, 
                                                KNN_baseVariables_5FOLDCVmaxAccuracies4)

#LINEAR SVM
LinearSVM_baseVariables_5FOLDCV_4 <- train(response~.,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_4,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_baseVariables_results_5FOLDCV_4 <- as.data.frame(LinearSVM_baseVariables_5FOLDCV_4$results)

LinearSVM_baseVariables_5FOLDCVmaxAccuracies4 <- LinearSVM_baseVariables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_baseVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_baseVariables_5FOLDCVmaxAccuracies4)

#RADIAL SVM
RadialSVM_baseVariables_5FOLDCV_4 <- train(response~.,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_4,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_baseVariables_results_5FOLDCV_4 <- as.data.frame(RadialSVM_baseVariables_5FOLDCV_4$results)

RadialSVM_baseVariables_5FOLDCVmaxAccuracies4 <- RadialSVM_baseVariables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_baseVariables_5FOLDCVmaxAccuracies, 
                                                      RadialSVM_baseVariables_5FOLDCVmaxAccuracies4)

#fifth 5-fold CV
Spinale_biochemical_training_baseVariables_5FoldCV_5 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_baseVariables_5FoldCV_5 <- train(response~MMP2_standardized + CRP_standardized + 
                                                ST2_standardized + TIFRII_standardized,
                                              Spinale_biochemical_training_baseVariables_5FoldCV_5,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 4,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_baseVariables_results_5FoldCV_5 <- as.data.frame(randomForest_baseVariables_5FoldCV_5$results)

randomForest_baseVariables_maxAccuracies_5FoldCV_5 <- randomForest_baseVariables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_baseVariables_maxAccuracies_5FoldCV <- rbind(randomForest_baseVariables_maxAccuracies_5FoldCV, 
                                                          randomForest_baseVariables_maxAccuracies_5FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_baseVariables_5FoldCV_5 <- train(response ~ MMP2_standardized + CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    data = Spinale_biochemical_training_baseVariables_5FoldCV_5,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl_5_fold_CV,
                                                    metric = "Accuracy")

logisticRegression_baseVariables_5FOLDCV_results_5 <- as.data.frame(logisticRegression_baseVariables_5FoldCV_5$results)

logisticRegression_baseVariables_results_5FOLDCV <- rbind(logisticRegression_baseVariables_results_5FOLDCV,
                                                          logisticRegression_baseVariables_5FOLDCV_results_5)

#K-NEAREST NEIGHBORS
KNN_baseVariables_5FOLDCV_5 <-  train(response ~ MMP2_standardized + CRP_standardized + 
                                        ST2_standardized + TIFRII_standardized,
                                      Spinale_biochemical_training_baseVariables_5FoldCV_5,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_baseVariables_results_5FOLDCV_5 <- as.data.frame(KNN_baseVariables_5FOLDCV_5$results)

KNN_baseVariables_5FOLDCVmaxAccuracies5 <- KNN_baseVariables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_baseVariables_5FOLDCVmaxAccuracies <- rbind(KNN_baseVariables_5FOLDCVmaxAccuracies, 
                                                KNN_baseVariables_5FOLDCVmaxAccuracies5)

#LINEAR SVM
LinearSVM_baseVariables_5FOLDCV_5 <- train(response~MMP2_standardized + CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_5,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_baseVariables_results_5FOLDCV_5 <- as.data.frame(LinearSVM_baseVariables_5FOLDCV_5$results)

LinearSVM_baseVariables_5FOLDCVmaxAccuracies5 <- LinearSVM_baseVariables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_baseVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_baseVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_baseVariables_5FOLDCVmaxAccuracies5)

#RADIAL SVM
RadialSVM_baseVariables_5FOLDCV_5 <- train(response~MMP2_standardized + CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                           Spinale_biochemical_training_baseVariables_5FoldCV_5,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_baseVariables_results_5FOLDCV_5 <- as.data.frame(RadialSVM_baseVariables_5FOLDCV_5$results)

RadialSVM_baseVariables_5FOLDCVmaxAccuracies5 <- RadialSVM_baseVariables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_baseVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_baseVariables_5FOLDCVmaxAccuracies, 
                                                      RadialSVM_baseVariables_5FOLDCVmaxAccuracies5)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#now we do 10 fold and 5 fold CV for the selected variables (CRP, ST2, TIFRII)

#first 10-fold CV
Spinale_biochemical_training_selectedVariables_1 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_10FOLDCV_1 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                  Spinale_biochemical_training_selectedVariables_1,
                                                method = "rf",
                                                trControl = ctrl,
                                                tuneLength = 3,
                                                ntrees = 1000,
                                                metric = "Accuracy")


randomForest_selectedVariables_results_10FoldCV_1 <- as.data.frame(randomForest_selected_variables_10FOLDCV_1$results)

randomForest_selectedVariables_maxAccuracies_10FoldCV <- randomForest_selectedVariables_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_selectedVariables_10FoldCV_1 <- train(response ~ CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    data = Spinale_biochemical_training_selectedVariables_1,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl,
                                                    metric = "Accuracy")

logisticRegression_selectedVariables_10FOLDCV_results_1 <- as.data.frame(logisticRegression_selectedVariables_10FoldCV_1$results)

logisticRegression_selectedVariables_results_10FOLDCV <- logisticRegression_selectedVariables_10FOLDCV_results_1

#K-NEAREST NEIGHBORS
KNN_selectedVariables_10FoldCV_1 <-  train(response ~ CRP_standardized + 
                                        ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_selectedVariables_1,
                                      method = "knn",
                                      trControl = ctrl,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_selectedVariables_results_10FOLDCV_1 <- as.data.frame(KNN_selectedVariables_10FoldCV_1$results)

KNN_selectedVariables_10FOLDCVmaxAccuracies <- KNN_selectedVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LINEARSVM_selectedVariables_10FOLDCV_1 <- train(response~ CRP_standardized + 
                                          ST2_standardized + TIFRII_standardized,
                                          Spinale_biochemical_training_selectedVariables_1,
                                          method = "svmLinear",
                                          trControl = ctrl,
                                          tuneGrid = grid,
                                          metric = "Accuracy")

LinearSVM_selectedVariables_results_10FOLDCV_1 <- as.data.frame(LINEARSVM_selectedVariables_10FOLDCV_1$results)

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- LinearSVM_selectedVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_selectedVariables_10FOLDCV_1 <- train(response~CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_1,
                                                method = "svmRadial",
                                                trControl = ctrl,
                                                tuneGrid = grid_radial,
                                                metric = "Accuracy")

RadialSVM_selectedVariables_results_10FOLDCV_1 <- as.data.frame(RadialSVM_selectedVariables_10FOLDCV_1$results)
RadialSVM_selectedVariables_10FOLDCVmaxAccuracies <- RadialSVM_selectedVariables_results_10FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#2nd 10-fold CV
Spinale_biochemical_training_selectedVariables_2 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_10FOLDCV_2 <- train(response~ CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    Spinale_biochemical_training_selectedVariables_2,
                                                    method = "rf",
                                                    trControl = ctrl,
                                                    tuneLength = 3,
                                                    ntrees = 1000,
                                                    metric = "Accuracy")

randomForest_selectedVariables_results_10FoldCV_2 <- as.data.frame(randomForest_selected_variables_10FOLDCV_2$results)

randomForest_selectedVariables_maxAccuracies_10FoldCV_2 <- randomForest_selectedVariables_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_10FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_10FoldCV, 
                                                          randomForest_selectedVariables_maxAccuracies_10FoldCV_2)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_10FoldCV_2 <- train(response ~ CRP_standardized + 
                                                       ST2_standardized + TIFRII_standardized,
                                                     data = Spinale_biochemical_training_selectedVariables_2,
                                                     family = "binomial",
                                                     method = "glm",
                                                     trControl = ctrl,
                                                     metric = "Accuracy")

logisticRegression_selectedVariables_results_2 <- as.data.frame(logisticRegression_selectedVariables_10FoldCV_2$results)

logisticRegression_selectedVariables_results_10FOLDCV <- rbind(logisticRegression_selectedVariables_results_10FOLDCV,
                                                           logisticRegression_selectedVariables_results_2)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_10FoldCV_2 <-  train(response ~ CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_selectedVariables_2,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_selectedVariables_results_10FOLDCV_2 <- as.data.frame(KNN_selectedVariables_10FoldCV_2$results)

KNN_selectedVariables_10FOLDCVmaxAccuracies2 <- KNN_selectedVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_10FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_10FOLDCVmaxAccuracies, 
                                                KNN_selectedVariables_10FOLDCVmaxAccuracies2)

#LINEAR SVM
LINEARSVM_selectedVariables_10FOLDCV_2 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_2,
                                                method = "svmLinear",
                                                trControl = ctrl,
                                                tuneGrid = grid,
                                                metric = "Accuracy")

LinearSVM_selectedVariables_results_10FOLDCV_2 <- as.data.frame(LINEARSVM_selectedVariables_10FOLDCV_2$results)

LinearSVM_selectedVariables_10FOLDCVmaxAccuracies2 <- LinearSVM_selectedVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_selectedVariables_10FOLDCVmaxAccuracies2)

#RADIAL SVM
RadialSVM_selectedVariables_10FOLDCV_2 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_2,
                                                method = "svmRadial",
                                                trControl = ctrl,
                                                tuneGrid = grid_radial,
                                                metric = "Accuracy")

RadialSVM_selectedVariables_results_10FOLDCV_2 <- as.data.frame(RadialSVM_selectedVariables_10FOLDCV_2$results)

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies2 <- RadialSVM_selectedVariables_results_10FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_10FOLDCVmaxAccuracies, 
                                                           RadialSVM_selectedVariables_10FOLDCVmaxAccuracies2)

#Third 10-fold CV
Spinale_biochemical_training_selectedVariables_3 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_10FOLDCV_3 <- train(response~ CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    Spinale_biochemical_training_selectedVariables_3,
                                                    method = "rf",
                                                    trControl = ctrl,
                                                    tuneLength = 3,
                                                    ntrees = 1000,
                                                    metric = "Accuracy")

randomForest_selectedVariables_results_10FoldCV_3 <- as.data.frame(randomForest_selected_variables_10FOLDCV_3$results)

randomForest_selectedVariables_maxAccuracies_10FoldCV_3 <- randomForest_selectedVariables_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_10FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_10FoldCV, 
                                                               randomForest_selectedVariables_maxAccuracies_10FoldCV_3)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_10FoldCV_3 <- train(response ~ CRP_standardized + 
                                                           ST2_standardized + TIFRII_standardized,
                                                         data = Spinale_biochemical_training_selectedVariables_3,
                                                         family = "binomial",
                                                         method = "glm",
                                                         trControl = ctrl,
                                                         metric = "Accuracy")

logisticRegression_selectedVariables_results_3 <- as.data.frame(logisticRegression_selectedVariables_10FoldCV_3$results)

logisticRegression_selectedVariables_results_10FOLDCV <- rbind(logisticRegression_selectedVariables_results_10FOLDCV,
                                                               logisticRegression_selectedVariables_results_3)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_10FoldCV_3 <-  train(response ~ CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_selectedVariables_3,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_selectedVariables_results_10FOLDCV_3 <- as.data.frame(KNN_selectedVariables_10FoldCV_3$results)

KNN_selectedVariables_10FOLDCVmaxAccuracies3 <- KNN_selectedVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_10FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_10FOLDCVmaxAccuracies, 
                                                     KNN_selectedVariables_10FOLDCVmaxAccuracies3)

#LINEAR SVM
LINEARSVM_selectedVariables_10FOLDCV_3 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_3,
                                                method = "svmLinear",
                                                trControl = ctrl,
                                                tuneGrid = grid,
                                                metric = "Accuracy")

LinearSVM_selectedVariables_results_10FOLDCV_3 <- as.data.frame(LINEARSVM_selectedVariables_10FOLDCV_3$results)

LinearSVM_selectedVariables_10FOLDCVmaxAccuracies3 <- LinearSVM_selectedVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_selectedVariables_10FOLDCVmaxAccuracies3)

#RADIAL SVM
RadialSVM_selectedVariables_10FOLDCV_3 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_3,
                                                method = "svmRadial",
                                                trControl = ctrl,
                                                tuneGrid = grid_radial,
                                                metric = "Accuracy")

RadialSVM_selectedVariables_results_10FOLDCV_3 <- as.data.frame(RadialSVM_selectedVariables_10FOLDCV_3$results)

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies3 <- RadialSVM_selectedVariables_results_10FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_10FOLDCVmaxAccuracies, 
                                                       RadialSVM_selectedVariables_10FOLDCVmaxAccuracies3)

#Fourth 10-fold CV
Spinale_biochemical_training_selectedVariables_4 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_10FOLDCV_4 <- train(response~ CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    Spinale_biochemical_training_selectedVariables_4,
                                                    method = "rf",
                                                    trControl = ctrl,
                                                    tuneLength = 3,
                                                    ntrees = 1000,
                                                    metric = "Accuracy")

randomForest_selectedVariables_results_10FoldCV_4 <- as.data.frame(randomForest_selected_variables_10FOLDCV_4$results)

randomForest_selectedVariables_maxAccuracies_10FoldCV_4 <- randomForest_selectedVariables_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_10FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_10FoldCV, 
                                                               randomForest_selectedVariables_maxAccuracies_10FoldCV_4)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_10FoldCV_4 <- train(response ~ CRP_standardized + 
                                                           ST2_standardized + TIFRII_standardized,
                                                         data = Spinale_biochemical_training_selectedVariables_4,
                                                         family = "binomial",
                                                         method = "glm",
                                                         trControl = ctrl,
                                                         metric = "Accuracy")

logisticRegression_selectedVariables_results_4 <- as.data.frame(logisticRegression_selectedVariables_10FoldCV_4$results)

logisticRegression_selectedVariables_results_10FOLDCV <- rbind(logisticRegression_selectedVariables_results_10FOLDCV,
                                                               logisticRegression_selectedVariables_results_4)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_10FoldCV_4 <-  train(response ~ CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_selectedVariables_4,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_selectedVariables_results_10FOLDCV_4 <- as.data.frame(KNN_selectedVariables_10FoldCV_4$results)

KNN_selectedVariables_10FOLDCVmaxAccuracies4 <- KNN_selectedVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_10FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_10FOLDCVmaxAccuracies, 
                                                     KNN_selectedVariables_10FOLDCVmaxAccuracies4)

#LINEAR SVM
LINEARSVM_selectedVariables_10FOLDCV_4 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_4,
                                                method = "svmLinear",
                                                trControl = ctrl,
                                                tuneGrid = grid,
                                                metric = "Accuracy")

LinearSVM_selectedVariables_results_10FOLDCV_4 <- as.data.frame(LINEARSVM_selectedVariables_10FOLDCV_4$results)

LinearSVM_selectedVariables_10FOLDCVmaxAccuracies4 <- LinearSVM_selectedVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_selectedVariables_10FOLDCVmaxAccuracies4)

#RADIAL SVM
RadialSVM_selectedVariables_10FOLDCV_4 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_4,
                                                method = "svmRadial",
                                                trControl = ctrl,
                                                tuneGrid = grid_radial,
                                                metric = "Accuracy")

RadialSVM_selectedVariables_results_10FOLDCV_4 <- as.data.frame(RadialSVM_selectedVariables_10FOLDCV_4$results)

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies4 <- RadialSVM_selectedVariables_results_10FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_10FOLDCVmaxAccuracies, 
                                                       RadialSVM_selectedVariables_10FOLDCVmaxAccuracies4)

#Fifth 10-fold CV
Spinale_biochemical_training_selectedVariables_5 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_10FOLDCV_5 <- train(response~ CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    Spinale_biochemical_training_selectedVariables_5,
                                                    method = "rf",
                                                    trControl = ctrl,
                                                    tuneLength = 3,
                                                    ntrees = 1000,
                                                    metric = "Accuracy")

randomForest_selectedVariables_results_10FoldCV_5 <- as.data.frame(randomForest_selected_variables_10FOLDCV_5$results)

randomForest_selectedVariables_maxAccuracies_10FoldCV_5 <- randomForest_selectedVariables_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_10FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_10FoldCV, 
                                                               randomForest_selectedVariables_maxAccuracies_10FoldCV_5)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_10FoldCV_5 <- train(response ~ CRP_standardized + 
                                                           ST2_standardized + TIFRII_standardized,
                                                         data = Spinale_biochemical_training_selectedVariables_5,
                                                         family = "binomial",
                                                         method = "glm",
                                                         trControl = ctrl,
                                                         metric = "Accuracy")

logisticRegression_selectedVariables_results_5 <- as.data.frame(logisticRegression_selectedVariables_10FoldCV_5$results)

logisticRegression_selectedVariables_results_10FOLDCV <- rbind(logisticRegression_selectedVariables_results_10FOLDCV,
                                                               logisticRegression_selectedVariables_results_5)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_10FoldCV_5 <-  train(response ~ CRP_standardized + 
                                         ST2_standardized + TIFRII_standardized,
                                       Spinale_biochemical_training_selectedVariables_5,
                                       method = "knn",
                                       trControl = ctrl,
                                       tuneLength = 20,
                                       metric = "Accuracy")

KNN_selectedVariables_results_10FOLDCV_5 <- as.data.frame(KNN_selectedVariables_10FoldCV_5$results)

KNN_selectedVariables_10FOLDCVmaxAccuracies5 <- KNN_selectedVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_10FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_10FOLDCVmaxAccuracies, 
                                                     KNN_selectedVariables_10FOLDCVmaxAccuracies5)

#LINEAR SVM
LINEARSVM_selectedVariables_10FOLDCV_5 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_5,
                                                method = "svmLinear",
                                                trControl = ctrl,
                                                tuneGrid = grid,
                                                metric = "Accuracy")

LinearSVM_selectedVariables_results_10FOLDCV_5 <- as.data.frame(LINEARSVM_selectedVariables_10FOLDCV_5$results)

LinearSVM_selectedVariables_10FOLDCVmaxAccuracies5 <- LinearSVM_selectedVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                      LinearSVM_selectedVariables_10FOLDCVmaxAccuracies5)

#RADIAL SVM
RadialSVM_selectedVariables_10FOLDCV_5 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                                Spinale_biochemical_training_selectedVariables_5,
                                                method = "svmRadial",
                                                trControl = ctrl,
                                                tuneGrid = grid_radial,
                                                metric = "Accuracy")

RadialSVM_selectedVariables_results_10FOLDCV_5 <- as.data.frame(RadialSVM_selectedVariables_10FOLDCV_5$results)

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies5 <- RadialSVM_selectedVariables_results_10FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_10FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_10FOLDCVmaxAccuracies, 
                                                       RadialSVM_selectedVariables_10FOLDCVmaxAccuracies5)

#-----------------------------------------------------------------------------------------
#now let's do 5 5-fold CVs 
Spinale_biochemical_training_selectedVariables_5FoldCV_1 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selectedVariables_5FoldCV_1 <- train(response~CRP_standardized + 
                                                ST2_standardized + TIFRII_standardized,
                                              Spinale_biochemical_training_selectedVariables_5FoldCV_1,
                                              method = "rf",
                                              trControl = ctrl_5_fold_CV,
                                              tuneLength = 12,
                                              ntrees = 1000,
                                              metric = "Accuracy")


randomForest_selectedVariables_results_5FoldCV_1 <- as.data.frame(randomForest_selectedVariables_5FoldCV_1$results)

randomForest_selectedVariables_maxAccuracies_5FoldCV <- randomForest_selectedVariables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_selectedVariables_5FoldCV_1 <- train(response~CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                    data = Spinale_biochemical_training_selectedVariables_5FoldCV_1,
                                                    family = "binomial",
                                                    method = "glm",
                                                    trControl = ctrl_5_fold_CV,
                                                    metric = "Accuracy")

logisticRegression_selectedVariables_5FOLDCV_results_1 <- as.data.frame(logisticRegression_selectedVariables_5FoldCV_1$results)

logisticRegression_selectedVariables_results_5FOLDCV <- logisticRegression_selectedVariables_5FOLDCV_results_1

#K-NEAREST NEIGHBORS
KNN_selectedVariables_5FOLDCV_1 <-  train(response~CRP_standardized + 
                                        ST2_standardized + TIFRII_standardized,
                                        Spinale_biochemical_training_selectedVariables_5FoldCV_1,
                                      method = "knn",
                                      trControl = ctrl_5_fold_CV,
                                      tuneLength = 20,
                                      metric = "Accuracy")

KNN_selectedVariables_results_5FOLDCV_1 <- as.data.frame(KNN_selectedVariables_5FOLDCV_1$results)

KNN_selectedVariables_5FOLDCVmaxAccuracies <- KNN_selectedVariables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_selectedVariables_5FOLDCV_1 <- train(response~CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                             Spinale_biochemical_training_selectedVariables_5FoldCV_1,
                                           method = "svmLinear",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid,
                                           tuneLength = 10,
                                           metric = "Accuracy")

LinearSVM_selectedVariables_results_5FOLDCV_1 <- as.data.frame(LinearSVM_selectedVariables_5FOLDCV_1$results)

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- LinearSVM_selectedVariables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_selectedVariables_5FOLDCV_1 <- train(response ~CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                             Spinale_biochemical_training_selectedVariables_5FoldCV_1,
                                           method = "svmRadial",
                                           trControl = ctrl_5_fold_CV,
                                           tuneGrid = grid_radial,
                                           tuneLength = 10,
                                           metric = "Accuracy")

RadialSVM_selectedVariables_results_5FOLDCV_1 <- as.data.frame(RadialSVM_selectedVariables_5FOLDCV_1$results)

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies <- RadialSVM_selectedVariables_results_5FOLDCV_1 %>%
  filter(Accuracy == max(Accuracy))


#2nd 5-fold CV
Spinale_biochemical_training_selectedVariables_5FoldCV_2 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_5FOLDCV_2 <- train(response~ CRP_standardized + 
                                                      ST2_standardized + TIFRII_standardized,
                                                   Spinale_biochemical_training_selectedVariables_5FoldCV_2,
                                                    method = "rf",
                                                    trControl = ctrl_5_fold_CV,
                                                    tuneLength = 3,
                                                    ntrees = 1000,
                                                    metric = "Accuracy")

randomForest_selectedVariables_results_5FoldCV_2 <- as.data.frame(randomForest_selected_variables_5FOLDCV_2$results)

randomForest_selectedVariables_maxAccuracies_5FoldCV_2 <- randomForest_selectedVariables_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_5FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_5FoldCV, 
                                                               randomForest_selectedVariables_maxAccuracies_5FoldCV_2)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_5FoldCV_2 <- train(response ~ CRP_standardized + 
                                                           ST2_standardized + TIFRII_standardized,
                                                         data = Spinale_biochemical_training_selectedVariables_5FoldCV_2,
                                                         family = "binomial",
                                                         method = "glm",
                                                         trControl = ctrl_5_fold_CV,
                                                         metric = "Accuracy")

logisticRegression_selectedVariables_results_5FOLDCV_2 <- as.data.frame(logisticRegression_selectedVariables_5FoldCV_2$results)


logisticRegression_selectedVariables_results_5FOLDCV <- rbind(logisticRegression_selectedVariables_results_5FOLDCV,
                                                               logisticRegression_selectedVariables_results_5FOLDCV_2)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_5FoldCV_2 <-  train(response ~ CRP_standardized + 
                                             ST2_standardized + TIFRII_standardized,
                                          Spinale_biochemical_training_selectedVariables_5FoldCV_2,
                                           method = "knn",
                                           trControl = ctrl_5_fold_CV,
                                           tuneLength = 20,
                                           metric = "Accuracy")

KNN_selectedVariables_results_5FOLDCV_2 <- as.data.frame(KNN_selectedVariables_5FoldCV_2$results)

KNN_selectedVariables_5FOLDCVmaxAccuracies2 <- KNN_selectedVariables_results_5FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_5FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_5FOLDCVmaxAccuracies, 
                                                     KNN_selectedVariables_5FOLDCVmaxAccuracies2)

#LINEAR SVM
LINEARSVM_selectedVariables_5FOLDCV_2 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_2,
                                                method = "svmLinear",
                                                trControl = ctrl_5_fold_CV,
                                                tuneGrid = grid,
                                                metric = "Accuracy")

LinearSVM_selectedVariables_results_5FOLDCV_2 <- as.data.frame(LINEARSVM_selectedVariables_5FOLDCV_2$results)

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies2 <- LinearSVM_selectedVariables_results_5FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          LinearSVM_selectedVariables_5FOLDCVmaxAccuracies2)

#RADIAL SVM
RadialSVM_selectedVariables_5FOLDCV_2 <- train(response~ CRP_standardized + 
                                                  ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_2,
                                                method = "svmRadial",
                                                trControl = ctrl_5_fold_CV,
                                                tuneGrid = grid_radial,
                                                metric = "Accuracy")

RadialSVM_selectedVariables_results_5FOLDCV_2 <- as.data.frame(RadialSVM_selectedVariables_5FOLDCV_2$results)

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies2 <- RadialSVM_selectedVariables_results_5FOLDCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                           RadialSVM_selectedVariables_5FOLDCVmaxAccuracies2)


#Third 5-fold CV
Spinale_biochemical_training_selectedVariables_5FoldCV_3 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_5FOLDCV_3 <- train(response~ CRP_standardized + 
                                                     ST2_standardized + TIFRII_standardized,
                                                   Spinale_biochemical_training_selectedVariables_5FoldCV_3,
                                                   method = "rf",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneLength = 3,
                                                   ntrees = 1000,
                                                   metric = "Accuracy")

randomForest_selectedVariables_results_5FoldCV_3 <- as.data.frame(randomForest_selected_variables_5FOLDCV_3$results)

randomForest_selectedVariables_maxAccuracies_5FoldCV_3 <- randomForest_selectedVariables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_5FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_5FoldCV, 
                                                              randomForest_selectedVariables_maxAccuracies_5FoldCV_3)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_5FoldCV_3 <- train(response ~ CRP_standardized + 
                                                          ST2_standardized + TIFRII_standardized,
                                                        data = Spinale_biochemical_training_selectedVariables_5FoldCV_3,
                                                        family = "binomial",
                                                        method = "glm",
                                                        trControl = ctrl_5_fold_CV,
                                                        metric = "Accuracy")

logisticRegression_selectedVariables_results_5FOLDCV_3 <- as.data.frame(logisticRegression_selectedVariables_5FoldCV_3$results)


logisticRegression_selectedVariables_results_5FOLDCV <- rbind(logisticRegression_selectedVariables_results_5FOLDCV,
                                                              logisticRegression_selectedVariables_results_5FOLDCV_3)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_5FoldCV_3 <-  train(response ~ CRP_standardized + 
                                            ST2_standardized + TIFRII_standardized,
                                          Spinale_biochemical_training_selectedVariables_5FoldCV_3,
                                          method = "knn",
                                          trControl = ctrl_5_fold_CV,
                                          tuneLength = 20,
                                          metric = "Accuracy")

KNN_selectedVariables_results_5FOLDCV_3 <- as.data.frame(KNN_selectedVariables_5FoldCV_3$results)

KNN_selectedVariables_5FOLDCVmaxAccuracies3 <- KNN_selectedVariables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_5FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_5FOLDCVmaxAccuracies, 
                                                    KNN_selectedVariables_5FOLDCVmaxAccuracies3)

#LINEAR SVM
LINEARSVM_selectedVariables_5FOLDCV_3 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_3,
                                               method = "svmLinear",
                                               trControl = ctrl_5_fold_CV,
                                               tuneGrid = grid,
                                               metric = "Accuracy")

LinearSVM_selectedVariables_results_5FOLDCV_3 <- as.data.frame(LINEARSVM_selectedVariables_5FOLDCV_3$results)

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies3 <- LinearSVM_selectedVariables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          LinearSVM_selectedVariables_5FOLDCVmaxAccuracies3)

#RADIAL SVM
RadialSVM_selectedVariables_5FOLDCV_3 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_3,
                                               method = "svmRadial",
                                               trControl = ctrl_5_fold_CV,
                                               tuneGrid = grid_radial,
                                               metric = "Accuracy")

RadialSVM_selectedVariables_results_5FOLDCV_3 <- as.data.frame(RadialSVM_selectedVariables_5FOLDCV_3$results)

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies3 <- RadialSVM_selectedVariables_results_5FOLDCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          RadialSVM_selectedVariables_5FOLDCVmaxAccuracies3)

#Fourth 5-fold CV
Spinale_biochemical_training_selectedVariables_5FoldCV_4 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_5FOLDCV_4 <- train(response~ CRP_standardized + 
                                                     ST2_standardized + TIFRII_standardized,
                                                   Spinale_biochemical_training_selectedVariables_5FoldCV_4,
                                                   method = "rf",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneLength = 3,
                                                   ntrees = 1000,
                                                   metric = "Accuracy")

randomForest_selectedVariables_results_5FoldCV_4 <- as.data.frame(randomForest_selected_variables_5FOLDCV_4$results)

randomForest_selectedVariables_maxAccuracies_5FoldCV_4 <- randomForest_selectedVariables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_5FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_5FoldCV, 
                                                              randomForest_selectedVariables_maxAccuracies_5FoldCV_4)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_5FoldCV_4 <- train(response ~ CRP_standardized + 
                                                          ST2_standardized + TIFRII_standardized,
                                                        data = Spinale_biochemical_training_selectedVariables_5FoldCV_4,
                                                        family = "binomial",
                                                        method = "glm",
                                                        trControl = ctrl_5_fold_CV,
                                                        metric = "Accuracy")

logisticRegression_selectedVariables_results_5FOLDCV_4 <- as.data.frame(logisticRegression_selectedVariables_5FoldCV_4$results)


logisticRegression_selectedVariables_results_5FOLDCV <- rbind(logisticRegression_selectedVariables_results_5FOLDCV,
                                                              logisticRegression_selectedVariables_results_5FOLDCV_4)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_5FoldCV_4 <-  train(response ~ CRP_standardized + 
                                            ST2_standardized + TIFRII_standardized,
                                          Spinale_biochemical_training_selectedVariables_5FoldCV_4,
                                          method = "knn",
                                          trControl = ctrl_5_fold_CV,
                                          tuneLength = 20,
                                          metric = "Accuracy")

KNN_selectedVariables_results_5FOLDCV_4 <- as.data.frame(KNN_selectedVariables_5FoldCV_4$results)

KNN_selectedVariables_5FOLDCVmaxAccuracies4 <- KNN_selectedVariables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_5FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_5FOLDCVmaxAccuracies, 
                                                    KNN_selectedVariables_5FOLDCVmaxAccuracies4)

#LINEAR SVM
LINEARSVM_selectedVariables_5FOLDCV_4 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_4,
                                               method = "svmLinear",
                                               trControl = ctrl_5_fold_CV,
                                               tuneGrid = grid,
                                               metric = "Accuracy")

LinearSVM_selectedVariables_results_5FOLDCV_4 <- as.data.frame(LINEARSVM_selectedVariables_5FOLDCV_4$results)

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies4 <- LinearSVM_selectedVariables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          LinearSVM_selectedVariables_5FOLDCVmaxAccuracies4)

#RADIAL SVM
RadialSVM_selectedVariables_5FOLDCV_4 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_4,
                                               method = "svmRadial",
                                               trControl = ctrl_5_fold_CV,
                                               tuneGrid = grid_radial,
                                               metric = "Accuracy")

RadialSVM_selectedVariables_results_5FOLDCV_4 <- as.data.frame(RadialSVM_selectedVariables_5FOLDCV_4$results)

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies4 <- RadialSVM_selectedVariables_results_5FOLDCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          RadialSVM_selectedVariables_5FOLDCVmaxAccuracies4)


#Fifth 5-fold CV
Spinale_biochemical_training_selectedVariables_5FoldCV_5 <- Spinale_biochemical_training[sample(nrow(Spinale_biochemical_training)),]

#RANDOM FOREST
randomForest_selected_variables_5FOLDCV_5 <- train(response~ CRP_standardized + 
                                                     ST2_standardized + TIFRII_standardized,
                                                   Spinale_biochemical_training_selectedVariables_5FoldCV_5,
                                                   method = "rf",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneLength = 3,
                                                   ntrees = 1000,
                                                   metric = "Accuracy")

randomForest_selectedVariables_results_5FoldCV_5 <- as.data.frame(randomForest_selected_variables_5FOLDCV_5$results)

randomForest_selectedVariables_maxAccuracies_5FoldCV_5 <- randomForest_selectedVariables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_selectedVariables_maxAccuracies_5FoldCV <- rbind(randomForest_selectedVariables_maxAccuracies_5FoldCV, 
                                                              randomForest_selectedVariables_maxAccuracies_5FoldCV_5)
#LOGISTIC REGRESSION
logisticRegression_selectedVariables_5FoldCV_5 <- train(response ~ CRP_standardized + 
                                                          ST2_standardized + TIFRII_standardized,
                                                        data = Spinale_biochemical_training_selectedVariables_5FoldCV_5,
                                                        family = "binomial",
                                                        method = "glm",
                                                        trControl = ctrl_5_fold_CV,
                                                        metric = "Accuracy")

logisticRegression_selectedVariables_results_5FOLDCV_5 <- as.data.frame(logisticRegression_selectedVariables_5FoldCV_5$results)


logisticRegression_selectedVariables_results_5FOLDCV <- rbind(logisticRegression_selectedVariables_results_5FOLDCV,
                                                              logisticRegression_selectedVariables_results_5FOLDCV_5)

#K-NEAREST NEIGHBORS
KNN_selectedVariables_5FoldCV_5 <-  train(response ~ CRP_standardized + 
                                            ST2_standardized + TIFRII_standardized,
                                          Spinale_biochemical_training_selectedVariables_5FoldCV_5,
                                          method = "knn",
                                          trControl = ctrl_5_fold_CV,
                                          tuneLength = 20,
                                          metric = "Accuracy")

KNN_selectedVariables_results_5FOLDCV_5 <- as.data.frame(KNN_selectedVariables_5FoldCV_5$results)

KNN_selectedVariables_5FOLDCVmaxAccuracies5 <- KNN_selectedVariables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_selectedVariables_5FOLDCVmaxAccuracies <- rbind(KNN_selectedVariables_5FOLDCVmaxAccuracies, 
                                                    KNN_selectedVariables_5FOLDCVmaxAccuracies5)

#LINEAR SVM
LINEARSVM_selectedVariables_5FOLDCV_5 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_5,
                                               method = "svmLinear",
                                               trControl = ctrl_5_fold_CV,
                                               tuneGrid = grid,
                                               metric = "Accuracy")

LinearSVM_selectedVariables_results_5FOLDCV_5 <- as.data.frame(LINEARSVM_selectedVariables_5FOLDCV_5$results)

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies5 <- LinearSVM_selectedVariables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(LinearSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          LinearSVM_selectedVariables_5FOLDCVmaxAccuracies5)

#RADIAL SVM
RadialSVM_selectedVariables_5FOLDCV_5 <- train(response~ CRP_standardized + 
                                                 ST2_standardized + TIFRII_standardized,
                                               Spinale_biochemical_training_selectedVariables_5FoldCV_5,
                                               method = "svmRadial",
                                               trControl = ctrl_5_fold_CV,
                                               tuneGrid = grid_radial,
                                               metric = "Accuracy")

RadialSVM_selectedVariables_results_5FOLDCV_5 <- as.data.frame(RadialSVM_selectedVariables_5FOLDCV_5$results)

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies5 <- RadialSVM_selectedVariables_results_5FOLDCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_selectedVariables_5FOLDCVmaxAccuracies <- rbind(RadialSVM_selectedVariables_5FOLDCVmaxAccuracies, 
                                                          RadialSVM_selectedVariables_5FOLDCVmaxAccuracies5)


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#finally done with just biochemicals, let's do just clinical now
#for this, because Stretch was the only important variable, we can do all the variables
#and  call it a day
indexes_for_clinical <- createDataPartition(SpinaleClinicalMarkersAndResponseStandardized$response,
                                                     times = 1,
                                                     p = 0.7,
                                                     list = FALSE)
Spinale_clinical_training <- SpinaleClinicalMarkersAndResponseStandardized[indexes_for_clinical,]
Spinale_clinical_test <- SpinaleProteinsAndResponses2[-indexes_for_clinical,]

#first 10 fold CV
SpinaleClinical_10FOLDCV_1<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST

randomForest_allClinical_10FoldCV_1 <- train(response~.,
                                             SpinaleClinical_10FOLDCV_1,
                                                  method = "rf",
                                                  trControl = ctrl,
                                                  tuneLength = 9,
                                                  ntrees = 1000,
                                                  metric = "Accuracy")


randomForest_allClinical_results_10FoldCV_1 <- as.data.frame(randomForest_allClinical_10FoldCV_1$results)

randomForest_allClinical_maxAccuracies_10FoldCV <- randomForest_allClinical_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_allClinical_10FoldCV_1 <- train(response~.,
                                                        data = SpinaleClinical_10FOLDCV_1,
                                                        family = "binomial",
                                                        method = "glm",
                                                        trControl = ctrl,
                                                        metric = "Accuracy")

logisticRegression_allClinical_results_10FoldCV_1 <- as.data.frame(logisticRegression_allClinical_10FoldCV_1$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- logisticRegression_allClinical_results_10FoldCV_1


#K-NEAREST NEIGHBORS
KNN_allClinical_10FoldCV_1 <-  train(response~.,
                                     SpinaleClinical_10FOLDCV_1,
                                          method = "knn",
                                          trControl = ctrl,
                                          tuneLength = 20,
                                          metric = "Accuracy")

KNN_allClinical_results_10FoldCV_1 <- as.data.frame(KNN_allClinical_10FoldCV_1$results)

KNN_allClinical_maxAccuracies_10FoldCV <- KNN_allClinical_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_allClinical_10FoldCV_1 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_1,
                                               method = "svmLinear",
                                               trControl = ctrl,
                                               tuneGrid = grid,
                                               metric = "Accuracy")

LinearSVM_allClinical_results_10FoldCV_1 <- as.data.frame(LinearSVM_allClinical_10FoldCV_1$results)

LinearSVM_allClinical_maxAccuracies_10FoldCV <- LinearSVM_allClinical_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_allClinical_10FoldCV_1 <- train(response ~.,
                                          SpinaleClinical_10FOLDCV_1,
                                               method = "svmRadial",
                                               trControl = ctrl,
                                               tuneGrid = grid_radial,
                                               metric = "Accuracy")

RadialSVM_allClinical_results_10FoldCV_1 <- as.data.frame(RadialSVM_allClinical_10FoldCV_1$results)

RadialSVM_allClinical_maxAccuracies_10FoldCV <- RadialSVM_allClinical_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Second 10-Fold CV
SpinaleClinical_10FOLDCV_2<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_10FoldCV_2 <- train(response~.,
                                             SpinaleClinical_10FOLDCV_2,
                                             method = "rf",
                                             trControl = ctrl,
                                             tuneLength = 9,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_allClinical_results_10FoldCV_2 <- as.data.frame(randomForest_allClinical_10FoldCV_2$results)

randomForest_allClinical_maxAccuracies_10FoldCV_2 <- randomForest_allClinical_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_10FoldCV <- rbind(randomForest_allClinical_maxAccuracies_10FoldCV, 
                                                         randomForest_allClinical_maxAccuracies_10FoldCV_2)

#LOGISTIC REGRESSION
logisticRegression_allClinical_10FoldCV_2 <- train(response~.,
                                                   data = SpinaleClinical_10FOLDCV_2,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl,
                                                   metric = "Accuracy")

logisticRegression_allClinical_results_10FoldCV_2 <- as.data.frame(logisticRegression_allClinical_10FoldCV_2$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_allClinical_results_10FoldCV_2)

#K-NEAREST NEIGHBORS
KNN_allClinical_10FoldCV_2 <-  train(response ~.,
                                     SpinaleClinical_10FOLDCV_2,
                                          method = "knn",
                                          trControl = ctrl,
                                          tuneLength = 20,
                                          metric = "Accuracy")

KNN_allClinical_results_10FoldCV_2 <- as.data.frame(KNN_allClinical_10FoldCV_2$results)

KNN_allClinical_maxAccuracies_10FoldCV_2 <- KNN_allClinical_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_10FoldCV <- rbind(KNN_allClinical_maxAccuracies_10FoldCV, 
                                                KNN_allClinical_maxAccuracies_10FoldCV_2)

#LINEAR SVM
LinearSVM_allClinical_10FoldCV_2 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_2,
                                               method = "svmLinear",
                                               trControl = ctrl,
                                               tuneGrid = grid,
                                               metric = "Accuracy")

LinearSVM_allClinical_results_10FoldCV_2 <- as.data.frame(LinearSVM_allClinical_10FoldCV_2$results)

LinearSVM_allClinical_maxAccuracies_10FoldCV_2 <- LinearSVM_allClinical_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_10FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_10FoldCV, 
                                                          LinearSVM_allClinical_maxAccuracies_10FoldCV_2)

#RADIAL SVM
RadialSVM_allClinical_10FoldCV_2 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_2,
                                          method = "svmRadial",
                                          trControl = ctrl,
                                          tuneGrid = grid_radial,
                                          metric = "Accuracy")

RadialSVM_allClinical_results_10FoldCV_2 <- as.data.frame(RadialSVM_allClinical_10FoldCV_2$results)

RadialSVM_allClinical_maxAccuracies_10FoldCV_2 <- RadialSVM_allClinical_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_10FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      RadialSVM_allClinical_maxAccuracies_10FoldCV_2)

#Third 10-Fold CV
SpinaleClinical_10FOLDCV_3 <- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_10FoldCV_3 <- train(response~.,
                                             SpinaleClinical_10FOLDCV_3,
                                             method = "rf",
                                             trControl = ctrl,
                                             tuneLength = 9,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_allClinical_results_10FoldCV_3 <- as.data.frame(randomForest_allClinical_10FoldCV_3$results)

randomForest_allClinical_maxAccuracies_10FoldCV_3 <- randomForest_allClinical_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_10FoldCV <- rbind(randomForest_allClinical_maxAccuracies_10FoldCV, 
                                                         randomForest_allClinical_maxAccuracies_10FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_allClinical_10FoldCV_3 <- train(response~.,
                                                   data = SpinaleClinical_10FOLDCV_3,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl,
                                                   metric = "Accuracy")

logisticRegression_allClinical_results_10FoldCV_3 <- as.data.frame(logisticRegression_allClinical_10FoldCV_3$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_allClinical_results_10FoldCV_3)

#K-NEAREST NEIGHBORS
KNN_allClinical_10FoldCV_3 <-  train(response ~.,
                                     SpinaleClinical_10FOLDCV_3,
                                     method = "knn",
                                     trControl = ctrl,
                                     tuneLength = 20,
                                     metric = "Accuracy")

KNN_allClinical_results_10FoldCV_3 <- as.data.frame(KNN_allClinical_10FoldCV_3$results)

KNN_allClinical_maxAccuracies_10FoldCV_3 <- KNN_allClinical_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_10FoldCV <- rbind(KNN_allClinical_maxAccuracies_10FoldCV, 
                                                KNN_allClinical_maxAccuracies_10FoldCV_3)

#LINEAR SVM
LinearSVM_allClinical_10FoldCV_3 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_3,
                                          method = "svmLinear",
                                          trControl = ctrl,
                                          tuneGrid = grid,
                                          metric = "Accuracy")

LinearSVM_allClinical_results_10FoldCV_3 <- as.data.frame(LinearSVM_allClinical_10FoldCV_3$results)

LinearSVM_allClinical_maxAccuracies_10FoldCV_3 <- LinearSVM_allClinical_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_10FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      LinearSVM_allClinical_maxAccuracies_10FoldCV_3)

#RADIAL SVM
RadialSVM_allClinical_10FoldCV_3 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_3,
                                          method = "svmRadial",
                                          trControl = ctrl,
                                          tuneGrid = grid_radial,
                                          metric = "Accuracy")

RadialSVM_allClinical_results_10FoldCV_3 <- as.data.frame(RadialSVM_allClinical_10FoldCV_3$results)

RadialSVM_allClinical_maxAccuracies_10FoldCV_3 <- RadialSVM_allClinical_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_10FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      RadialSVM_allClinical_maxAccuracies_10FoldCV_3)

#Fourth 10-Fold CV
SpinaleClinical_10FOLDCV_4 <- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_10FoldCV_4 <- train(response~.,
                                             SpinaleClinical_10FOLDCV_4,
                                             method = "rf",
                                             trControl = ctrl,
                                             tuneLength = 9,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_allClinical_results_10FoldCV_4 <- as.data.frame(randomForest_allClinical_10FoldCV_4$results)

randomForest_allClinical_maxAccuracies_10FoldCV_4 <- randomForest_allClinical_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_10FoldCV <- rbind(randomForest_allClinical_maxAccuracies_10FoldCV, 
                                                         randomForest_allClinical_maxAccuracies_10FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_allClinical_10FoldCV_4 <- train(response~.,
                                                   data = SpinaleClinical_10FOLDCV_4,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl,
                                                   metric = "Accuracy")

logisticRegression_allClinical_results_10FoldCV_4 <- as.data.frame(logisticRegression_allClinical_10FoldCV_4$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_allClinical_results_10FoldCV_4)

#K-NEAREST NEIGHBORS
KNN_allClinical_10FoldCV_4 <-  train(response ~.,
                                     SpinaleClinical_10FOLDCV_4,
                                     method = "knn",
                                     trControl = ctrl,
                                     tuneLength = 20,
                                     metric = "Accuracy")

KNN_allClinical_results_10FoldCV_4 <- as.data.frame(KNN_allClinical_10FoldCV_4$results)

KNN_allClinical_maxAccuracies_10FoldCV_4 <- KNN_allClinical_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_10FoldCV <- rbind(KNN_allClinical_maxAccuracies_10FoldCV, 
                                                KNN_allClinical_maxAccuracies_10FoldCV_4)

#LINEAR SVM
LinearSVM_allClinical_10FoldCV_4 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_4,
                                          method = "svmLinear",
                                          trControl = ctrl,
                                          tuneGrid = grid,
                                          metric = "Accuracy")

LinearSVM_allClinical_results_10FoldCV_4 <- as.data.frame(LinearSVM_allClinical_10FoldCV_4$results)

LinearSVM_allClinical_maxAccuracies_10FoldCV_4 <- LinearSVM_allClinical_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_10FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      LinearSVM_allClinical_maxAccuracies_10FoldCV_4)

#RADIAL SVM
RadialSVM_allClinical_10FoldCV_4 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_4,
                                          method = "svmRadial",
                                          trControl = ctrl,
                                          tuneGrid = grid_radial,
                                          metric = "Accuracy")

RadialSVM_allClinical_results_10FoldCV_4 <- as.data.frame(RadialSVM_allClinical_10FoldCV_4$results)

RadialSVM_allClinical_maxAccuracies_10FoldCV_4 <- RadialSVM_allClinical_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_10FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      RadialSVM_allClinical_maxAccuracies_10FoldCV_4)

#Fifth 10-Fold CV
SpinaleClinical_10FOLDCV_5 <- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_10FoldCV_5 <- train(response~.,
                                             SpinaleClinical_10FOLDCV_5,
                                             method = "rf",
                                             trControl = ctrl,
                                             tuneLength = 9,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_allClinical_results_10FoldCV_5 <- as.data.frame(randomForest_allClinical_10FoldCV_5$results)

randomForest_allClinical_maxAccuracies_10FoldCV_5 <- randomForest_allClinical_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_10FoldCV <- rbind(randomForest_allClinical_maxAccuracies_10FoldCV, 
                                                         randomForest_allClinical_maxAccuracies_10FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_allClinical_10FoldCV_5 <- train(response~.,
                                                   data = SpinaleClinical_10FOLDCV_5,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl,
                                                   metric = "Accuracy")

logisticRegression_allClinical_results_10FoldCV_5 <- as.data.frame(logisticRegression_allClinical_10FoldCV_5$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_allClinical_results_10FoldCV_5)

#K-NEAREST NEIGHBORS
KNN_allClinical_10FoldCV_5 <-  train(response ~.,
                                     SpinaleClinical_10FOLDCV_5,
                                     method = "knn",
                                     trControl = ctrl,
                                     tuneLength = 20,
                                     metric = "Accuracy")

KNN_allClinical_results_10FoldCV_5 <- as.data.frame(KNN_allClinical_10FoldCV_5$results)

KNN_allClinical_maxAccuracies_10FoldCV_5 <- KNN_allClinical_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_10FoldCV <- rbind(KNN_allClinical_maxAccuracies_10FoldCV, 
                                                KNN_allClinical_maxAccuracies_10FoldCV_5)

#LINEAR SVM
LinearSVM_allClinical_10FoldCV_5 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_5,
                                          method = "svmLinear",
                                          trControl = ctrl,
                                          tuneGrid = grid,
                                          metric = "Accuracy")

LinearSVM_allClinical_results_10FoldCV_5 <- as.data.frame(LinearSVM_allClinical_10FoldCV_5$results)

LinearSVM_allClinical_maxAccuracies_10FoldCV_5 <- LinearSVM_allClinical_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_10FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      LinearSVM_allClinical_maxAccuracies_10FoldCV_5)

#RADIAL SVM
RadialSVM_allClinical_10FoldCV_5 <- train(response~.,
                                          SpinaleClinical_10FOLDCV_5,
                                          method = "svmRadial",
                                          trControl = ctrl,
                                          tuneGrid = grid_radial,
                                          metric = "Accuracy")

RadialSVM_allClinical_results_10FoldCV_5 <- as.data.frame(RadialSVM_allClinical_10FoldCV_5$results)

RadialSVM_allClinical_maxAccuracies_10FoldCV_5 <- RadialSVM_allClinical_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_10FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_10FoldCV, 
                                                      RadialSVM_allClinical_maxAccuracies_10FoldCV_5)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#first 5 fold CV
SpinaleClinical_5FOLDCV_1<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST

randomForest_allClinical_5FoldCV_1 <- train(response~.,
                                            SpinaleClinical_5FOLDCV_1,
                                             method = "rf",
                                             trControl = ctrl_5_fold_CV,
                                             tuneLength = 9,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_allClinical_results_5FoldCV_1 <- as.data.frame(randomForest_allClinical_5FoldCV_1$results)

randomForest_allClinical_maxAccuracies_5FoldCV <- randomForest_allClinical_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_allClinical_5FoldCV_1 <- train(response~.,
                                                   data = SpinaleClinical_5FOLDCV_1,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_allClinical_results_5FoldCV_1 <- as.data.frame(logisticRegression_allClinical_5FoldCV_1$results)

logisticRegression_allClinical_maxAccuracies_5FoldCV <- logisticRegression_allClinical_results_5FoldCV_1


#K-NEAREST NEIGHBORS
KNN_allClinical_5FoldCV_1 <-  train(response~.,
                                     SpinaleClinical_5FOLDCV_1,
                                     method = "knn",
                                     trControl = ctrl_5_fold_CV,
                                     tuneLength = 20,
                                     metric = "Accuracy")

KNN_allClinical_results_5FoldCV_1 <- as.data.frame(KNN_allClinical_5FoldCV_1$results)

KNN_allClinical_maxAccuracies_5FoldCV <- KNN_allClinical_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_allClinical_5FoldCV_1 <- train(response~.,
                                          SpinaleClinical_5FOLDCV_1,
                                          method = "svmLinear",
                                          trControl = ctrl_5_fold_CV,
                                          tuneGrid = grid,
                                          metric = "Accuracy")

LinearSVM_allClinical_results_5FoldCV_1 <- as.data.frame(LinearSVM_allClinical_5FoldCV_1$results)

LinearSVM_allClinical_maxAccuracies_5FoldCV <- LinearSVM_allClinical_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_allClinical_5FoldCV_1 <- train(response ~.,
                                          SpinaleClinical_5FOLDCV_1,
                                          method = "svmRadial",
                                          trControl = ctrl_5_fold_CV,
                                          tuneGrid = grid_radial,
                                          metric = "Accuracy")

RadialSVM_allClinical_results_5FoldCV_1 <- as.data.frame(RadialSVM_allClinical_5FoldCV_1$results)

RadialSVM_allClinical_maxAccuracies_5FoldCV <- RadialSVM_allClinical_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Second 5-Fold CV
SpinaleClinical_5FOLDCV_2<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_5FoldCV_2 <- train(response~.,
                                             SpinaleClinical_5FOLDCV_2,
                                             method = "rf",
                                             trControl = ctrl_5_fold_CV,
                                             tuneLength = 9,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_allClinical_results_5FoldCV_2 <- as.data.frame(randomForest_allClinical_5FoldCV_2$results)

randomForest_allClinical_maxAccuracies_5FoldCV_2 <- randomForest_allClinical_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_5FoldCV <- rbind(randomForest_allClinical_maxAccuracies_5FoldCV, 
                                                         randomForest_allClinical_maxAccuracies_5FoldCV_2)

#LOGISTIC REGRESSION
logisticRegression_allClinical_5FoldCV_2 <- train(response~.,
                                                   data = SpinaleClinical_5FOLDCV_2,
                                                   family = "binomial",
                                                   method = "glm",
                                                   trControl = ctrl_5_fold_CV,
                                                   metric = "Accuracy")

logisticRegression_allClinical_results_5FoldCV_2 <- as.data.frame(logisticRegression_allClinical_5FoldCV_2$results)

logisticRegression_allClinical_maxAccuracies_5FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_5FoldCV,
                                                               logisticRegression_allClinical_results_5FoldCV_2)

#K-NEAREST NEIGHBORS
KNN_allClinical_5FoldCV_2 <-  train(response ~.,
                                     SpinaleClinical_5FOLDCV_2,
                                     method = "knn",
                                     trControl = ctrl_5_fold_CV,
                                     tuneLength = 20,
                                     metric = "Accuracy")

KNN_allClinical_results_5FoldCV_2 <- as.data.frame(KNN_allClinical_5FoldCV_2$results)

KNN_allClinical_maxAccuracies_5FoldCV_2 <- KNN_allClinical_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_5FoldCV <- rbind(KNN_allClinical_maxAccuracies_5FoldCV, 
                                                KNN_allClinical_maxAccuracies_5FoldCV_2)

#LINEAR SVM
LinearSVM_allClinical_5FoldCV_2 <- train(response~.,
                                          SpinaleClinical_5FOLDCV_2,
                                          method = "svmLinear",
                                          trControl = ctrl_5_fold_CV,
                                          tuneGrid = grid,
                                          metric = "Accuracy")

LinearSVM_allClinical_results_5FoldCV_2 <- as.data.frame(LinearSVM_allClinical_5FoldCV_2$results)

LinearSVM_allClinical_maxAccuracies_5FoldCV_2 <- LinearSVM_allClinical_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_5FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_5FoldCV, 
                                                      LinearSVM_allClinical_maxAccuracies_5FoldCV_2)

#RADIAL SVM
RadialSVM_allClinical_5FoldCV_2 <- train(response~.,
                                          SpinaleClinical_5FOLDCV_2,
                                          method = "svmRadial",
                                          trControl = ctrl_5_fold_CV,
                                          tuneGrid = grid_radial,
                                          metric = "Accuracy")

RadialSVM_allClinical_results_5FoldCV_2 <- as.data.frame(RadialSVM_allClinical_5FoldCV_2$results)

RadialSVM_allClinical_maxAccuracies_5FoldCV_2 <- RadialSVM_allClinical_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_5FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_5FoldCV, 
                                                      RadialSVM_allClinical_maxAccuracies_5FoldCV_2)

#Third 5-Fold CV
SpinaleClinical_5FOLDCV_3<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_5FoldCV_3 <- train(response~.,
                                            SpinaleClinical_5FOLDCV_3,
                                            method = "rf",
                                            trControl = ctrl_5_fold_CV,
                                            tuneLength = 9,
                                            ntrees = 1000,
                                            metric = "Accuracy")


randomForest_allClinical_results_5FoldCV_3 <- as.data.frame(randomForest_allClinical_5FoldCV_3$results)

randomForest_allClinical_maxAccuracies_5FoldCV_3 <- randomForest_allClinical_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_5FoldCV <- rbind(randomForest_allClinical_maxAccuracies_5FoldCV, 
                                                        randomForest_allClinical_maxAccuracies_5FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_allClinical_5FoldCV_3 <- train(response~.,
                                                  data = SpinaleClinical_5FOLDCV_3,
                                                  family = "binomial",
                                                  method = "glm",
                                                  trControl = ctrl_5_fold_CV,
                                                  metric = "Accuracy")

logisticRegression_allClinical_results_5FoldCV_3 <- as.data.frame(logisticRegression_allClinical_5FoldCV_3$results)

logisticRegression_allClinical_maxAccuracies_5FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_5FoldCV,
                                                              logisticRegression_allClinical_results_5FoldCV_3)

#K-NEAREST NEIGHBORS
KNN_allClinical_5FoldCV_3 <-  train(response ~.,
                                    SpinaleClinical_5FOLDCV_3,
                                    method = "knn",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 20,
                                    metric = "Accuracy")

KNN_allClinical_results_5FoldCV_3 <- as.data.frame(KNN_allClinical_5FoldCV_3$results)

KNN_allClinical_maxAccuracies_5FoldCV_3 <- KNN_allClinical_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_5FoldCV <- rbind(KNN_allClinical_maxAccuracies_5FoldCV, 
                                               KNN_allClinical_maxAccuracies_5FoldCV_3)

#LINEAR SVM
LinearSVM_allClinical_5FoldCV_3 <- train(response~.,
                                         SpinaleClinical_5FOLDCV_3,
                                         method = "svmLinear",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid,
                                         metric = "Accuracy")

LinearSVM_allClinical_results_5FoldCV_3 <- as.data.frame(LinearSVM_allClinical_5FoldCV_3$results)

LinearSVM_allClinical_maxAccuracies_5FoldCV_3 <- LinearSVM_allClinical_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_5FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_5FoldCV, 
                                                     LinearSVM_allClinical_maxAccuracies_5FoldCV_3)

#RADIAL SVM
RadialSVM_allClinical_5FoldCV_3 <- train(response~.,
                                         SpinaleClinical_5FOLDCV_3,
                                         method = "svmRadial",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid_radial,
                                         metric = "Accuracy")

RadialSVM_allClinical_results_5FoldCV_3 <- as.data.frame(RadialSVM_allClinical_5FoldCV_3$results)

RadialSVM_allClinical_maxAccuracies_5FoldCV_3 <- RadialSVM_allClinical_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_5FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_5FoldCV, 
                                                     RadialSVM_allClinical_maxAccuracies_5FoldCV_3)

#Fourth 5-Fold CV
SpinaleClinical_5FOLDCV_4<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_5FoldCV_4 <- train(response~.,
                                            SpinaleClinical_5FOLDCV_4,
                                            method = "rf",
                                            trControl = ctrl_5_fold_CV,
                                            tuneLength = 9,
                                            ntrees = 1000,
                                            metric = "Accuracy")


randomForest_allClinical_results_5FoldCV_4 <- as.data.frame(randomForest_allClinical_5FoldCV_4$results)

randomForest_allClinical_maxAccuracies_5FoldCV_4 <- randomForest_allClinical_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_5FoldCV <- rbind(randomForest_allClinical_maxAccuracies_5FoldCV, 
                                                        randomForest_allClinical_maxAccuracies_5FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_allClinical_5FoldCV_4 <- train(response~.,
                                                  data = SpinaleClinical_5FOLDCV_4,
                                                  family = "binomial",
                                                  method = "glm",
                                                  trControl = ctrl_5_fold_CV,
                                                  metric = "Accuracy")

logisticRegression_allClinical_results_5FoldCV_4 <- as.data.frame(logisticRegression_allClinical_5FoldCV_4$results)

logisticRegression_allClinical_maxAccuracies_5FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_5FoldCV,
                                                              logisticRegression_allClinical_results_5FoldCV_4)

#K-NEAREST NEIGHBORS
KNN_allClinical_5FoldCV_4 <-  train(response ~.,
                                    SpinaleClinical_5FOLDCV_4,
                                    method = "knn",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 20,
                                    metric = "Accuracy")

KNN_allClinical_results_5FoldCV_4 <- as.data.frame(KNN_allClinical_5FoldCV_4$results)

KNN_allClinical_maxAccuracies_5FoldCV_4 <- KNN_allClinical_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_5FoldCV <- rbind(KNN_allClinical_maxAccuracies_5FoldCV, 
                                               KNN_allClinical_maxAccuracies_5FoldCV_4)

#LINEAR SVM
LinearSVM_allClinical_5FoldCV_4 <- train(response~.,
                                         SpinaleClinical_5FOLDCV_4,
                                         method = "svmLinear",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid,
                                         metric = "Accuracy")

LinearSVM_allClinical_results_5FoldCV_4 <- as.data.frame(LinearSVM_allClinical_5FoldCV_4$results)

LinearSVM_allClinical_maxAccuracies_5FoldCV_4 <- LinearSVM_allClinical_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_5FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_5FoldCV, 
                                                     LinearSVM_allClinical_maxAccuracies_5FoldCV_4)

#RADIAL SVM
RadialSVM_allClinical_5FoldCV_4 <- train(response~.,
                                         SpinaleClinical_5FOLDCV_4,
                                         method = "svmRadial",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid_radial,
                                         metric = "Accuracy")

RadialSVM_allClinical_results_5FoldCV_4 <- as.data.frame(RadialSVM_allClinical_5FoldCV_4$results)

RadialSVM_allClinical_maxAccuracies_5FoldCV_4 <- RadialSVM_allClinical_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_5FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_5FoldCV, 
                                                     RadialSVM_allClinical_maxAccuracies_5FoldCV_4)

#Fifth 5-Fold CV
SpinaleClinical_5FOLDCV_5<- Spinale_clinical_training[sample(nrow(Spinale_clinical_training)),]

#RANDOM FOREST
randomForest_allClinical_5FoldCV_5 <- train(response~.,
                                            SpinaleClinical_5FOLDCV_5,
                                            method = "rf",
                                            trControl = ctrl_5_fold_CV,
                                            tuneLength = 9,
                                            ntrees = 1000,
                                            metric = "Accuracy")


randomForest_allClinical_results_5FoldCV_5 <- as.data.frame(randomForest_allClinical_5FoldCV_5$results)

randomForest_allClinical_maxAccuracies_5FoldCV_5 <- randomForest_allClinical_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allClinical_maxAccuracies_5FoldCV <- rbind(randomForest_allClinical_maxAccuracies_5FoldCV, 
                                                        randomForest_allClinical_maxAccuracies_5FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_allClinical_5FoldCV_5 <- train(response~.,
                                                  data = SpinaleClinical_5FOLDCV_5,
                                                  family = "binomial",
                                                  method = "glm",
                                                  trControl = ctrl_5_fold_CV,
                                                  metric = "Accuracy")

logisticRegression_allClinical_results_5FoldCV_5 <- as.data.frame(logisticRegression_allClinical_5FoldCV_5$results)

logisticRegression_allClinical_maxAccuracies_5FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_5FoldCV,
                                                              logisticRegression_allClinical_results_5FoldCV_5)

#K-NEAREST NEIGHBORS
KNN_allClinical_5FoldCV_5 <-  train(response ~.,
                                    SpinaleClinical_5FOLDCV_5,
                                    method = "knn",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 20,
                                    metric = "Accuracy")

KNN_allClinical_results_5FoldCV_5 <- as.data.frame(KNN_allClinical_5FoldCV_5$results)

KNN_allClinical_maxAccuracies_5FoldCV_5 <- KNN_allClinical_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_allClinical_maxAccuracies_5FoldCV <- rbind(KNN_allClinical_maxAccuracies_5FoldCV, 
                                               KNN_allClinical_maxAccuracies_5FoldCV_5)

#LINEAR SVM
LinearSVM_allClinical_5FoldCV_5 <- train(response~.,
                                         SpinaleClinical_5FOLDCV_5,
                                         method = "svmLinear",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid,
                                         metric = "Accuracy")

LinearSVM_allClinical_results_5FoldCV_5 <- as.data.frame(LinearSVM_allClinical_5FoldCV_5$results)

LinearSVM_allClinical_maxAccuracies_5FoldCV_5 <- LinearSVM_allClinical_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allClinical_maxAccuracies_5FoldCV <- rbind(LinearSVM_allClinical_maxAccuracies_5FoldCV, 
                                                     LinearSVM_allClinical_maxAccuracies_5FoldCV_5)

#RADIAL SVM
RadialSVM_allClinical_5FoldCV_5 <- train(response~.,
                                         SpinaleClinical_5FOLDCV_5,
                                         method = "svmRadial",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid_radial,
                                         metric = "Accuracy")

RadialSVM_allClinical_results_5FoldCV_5 <- as.data.frame(RadialSVM_allClinical_5FoldCV_5$results)

RadialSVM_allClinical_maxAccuracies_5FoldCV_5 <- RadialSVM_allClinical_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allClinical_maxAccuracies_5FoldCV <- rbind(RadialSVM_allClinical_maxAccuracies_5FoldCV, 
                                                     RadialSVM_allClinical_maxAccuracies_5FoldCV_5)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#now we do combined biochemical and clinical -- we do all the variables first then
#the relevant ones
indexes_for_both <- createDataPartition(Spinale_all_standardized$response,
                                            times = 1,
                                            p = 0.7,
                                            list = FALSE)
Spinale_both_training <- Spinale_all_standardized[indexes_for_both,]
Spinale_both_test <- Spinale_all_standardized[-indexes_for_both,]

SpinaleAll_10FOLDCV_1<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_All_10FoldCV_1 <- train(response~.,
                                             SpinaleAll_10FOLDCV_1,
                                             method = "rf",
                                             trControl = ctrl,
                                             tuneLength = 21,
                                             ntrees = 1000,
                                             metric = "Accuracy")


randomForest_all_results_10FoldCV_1 <- as.data.frame(randomForest_All_10FoldCV_1$results)

randomForest_all_maxAccuracies_10FoldCV <- randomForest_all_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_all_10FoldCV_1 <- train(response~.,
                                           SpinaleAll_10FOLDCV_1,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl,
                                           metric = "accuracy")

logisticRegression_all_results_10FoldCV_1 <- as.data.frame(logisticRegression_all_10FoldCV_1$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- logisticRegression_all_results_10FoldCV_1

#K-NEAREST NEIGHBORS
KNN_all_10FOLDCV_1 <- train(response~.,
                            SpinaleAll_10FOLDCV_1,
                            method = "knn",
                            trControl = ctrl,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_all_results_10FoldCV_1 <- as.data.frame(KNN_all_10FOLDCV_1$results)

KNN_all_maxAccuracies_10FoldCV <- KNN_all_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_all_10FoldCV_1 <- train(response~.,
                                  SpinaleAll_10FOLDCV_1,
                                  method = "svmLinear",
                                  trControl = ctrl,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_all_results_10FoldCV_1 <- as.data.frame(LinearSVM_all_10FoldCV_1$results)

LinearSVM_all_maxAccuracies_10FoldCV <- LinearSVM_all_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Radial SVM
RadialSVM_all_10FoldCV_1 <- train(response~.,
                                  SpinaleAll_10FOLDCV_1,
                                  method = "svmRadial",
                                  trControl = ctrl,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_10FoldCV_1 <- as.data.frame(RadialSVM_all_10FoldCV_1$results)

RadialSVM_all_maxAccuracies_10FoldCV <- RadialSVM_all_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Second 10-Fold CV
SpinaleAll_10FOLDCV_2<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#Random Forest
randomForest_All_10FoldCV_2 <- train(response~.,
                                     SpinaleAll_10FOLDCV_2,
                                     method = "rf",
                                     trControl = ctrl,
                                     tuneLength = 21,
                                     ntrees = 1000,
                                     metric = "Accuracy")

randomForest_all_results_10FoldCV_2 <- as.data.frame(randomForest_All_10FoldCV_2$results)

randomForest_all_maxAccuracy_10FoldCV_2 <- randomForest_all_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_10FoldCV <- rbind(randomForest_all_maxAccuracies_10FoldCV,
                                                 randomForest_all_maxAccuracy_10FoldCV_2)

#Logistic Regression
logisticRegression_all_10FoldCV_2 <- train(response~.,
                                           SpinaleAll_10FOLDCV_2,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl,
                                           metric = "accuracy")

logisticRegression_all_results_10FoldCV_2 <- as.data.frame(logisticRegression_all_10FoldCV_2$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_all_results_10FoldCV_2)

#K-NEAREST NEIGHBORS

KNN_all_10FOLDCV_2 <- train(response~.,
                            SpinaleAll_10FOLDCV_2,
                            method = "knn",
                            trControl = ctrl,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_all_results_10FoldCV_2 <- as.data.frame(KNN_all_10FOLDCV_2$results)

KNN_all_maxAccuracy_10FoldCV_2 <- KNN_all_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_10FoldCV <- rbind(KNN_all_maxAccuracies_10FoldCV,
                                        KNN_all_maxAccuracy_10FoldCV_2)


#LINEAR SVM
LinearSVM_all_10FoldCV_2 <- train(response~.,
                                  SpinaleAll_10FOLDCV_2,
                                  method = "svmLinear",
                                  trControl = ctrl,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_all_results_10FoldCV_2 <- as.data.frame(LinearSVM_all_10FoldCV_2$results)

LinearSVM_all_maxAccuracy_10FoldCV_2 <- LinearSVM_all_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_10FoldCV <- rbind(LinearSVM_all_maxAccuracies_10FoldCV, 
                                              LinearSVM_all_maxAccuracy_10FoldCV_2)

#RADIAL SVM
RadialSVM_all_10FoldCV_2 <- train(response~.,
                                  SpinaleAll_10FOLDCV_2,
                                  method = "svmRadial",
                                  trControl = ctrl,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_10FoldCV_2 <- as.data.frame(RadialSVM_all_10FoldCV_2$results)

RadialSVM_all_maxAccuracy_10FoldCV_2 <- RadialSVM_all_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_10FoldCV <- rbind(RadialSVM_all_maxAccuracies_10FoldCV, 
                                              RadialSVM_all_maxAccuracy_10FoldCV_2)

#Third 10-Fold CV
SpinaleAll_10FOLDCV_3<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_All_10FoldCV_3 <- train(response~.,
                                     SpinaleAll_10FOLDCV_3,
                                     method = "rf",
                                     trControl = ctrl,
                                     tuneLength = 21,
                                     ntrees = 1000,
                                     metric = "Accuracy")

randomForest_all_results_10FoldCV_3 <- as.data.frame(randomForest_All_10FoldCV_3$results)

randomForest_all_maxAccuracy_10FoldCV_3 <- randomForest_all_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_10FoldCV <- rbind(randomForest_all_maxAccuracies_10FoldCV,
                                                 randomForest_all_maxAccuracy_10FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_all_10FoldCV_3 <- train(response~.,
                                           SpinaleAll_10FOLDCV_3,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl,
                                           metric = "accuracy")

logisticRegression_all_results_10FoldCV_3 <- as.data.frame(logisticRegression_all_10FoldCV_3$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_all_results_10FoldCV_3)

#K-NEAREST NEIGHBORS
KNN_all_10FOLDCV_3 <- train(response~.,
                            SpinaleAll_10FOLDCV_3,
                            method = "knn",
                            trControl = ctrl,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_all_results_10FoldCV_3 <- as.data.frame(KNN_all_10FOLDCV_3$results)

KNN_all_maxAccuracy_10FoldCV_3 <- KNN_all_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_10FoldCV <- rbind(KNN_all_maxAccuracies_10FoldCV,
                                        KNN_all_maxAccuracy_10FoldCV_3)

#LINEAR SVM
LinearSVM_all_10FoldCV_3 <- train(response~.,
                                  SpinaleAll_10FOLDCV_3,
                                  method = "svmLinear",
                                  trControl = ctrl,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_all_results_10FoldCV_3 <- as.data.frame(LinearSVM_all_10FoldCV_3$results)

LinearSVM_all_maxAccuracy_10FoldCV_3 <- LinearSVM_all_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_10FoldCV <- rbind(LinearSVM_all_maxAccuracies_10FoldCV, 
                                              LinearSVM_all_maxAccuracy_10FoldCV_3)

#RADIAL SVM
RadialSVM_all_10FoldCV_3 <- train(response~.,
                                  SpinaleAll_10FOLDCV_3,
                                  method = "svmRadial",
                                  trControl = ctrl,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_10FoldCV_3 <- as.data.frame(RadialSVM_all_10FoldCV_3$results)

RadialSVM_all_maxAccuracy_10FoldCV_3 <- RadialSVM_all_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_10FoldCV <- rbind(RadialSVM_all_maxAccuracies_10FoldCV, 
                                              RadialSVM_all_maxAccuracy_10FoldCV_3)

#Fourth 10-FOLD CV
SpinaleAll_10FOLDCV_4<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_All_10FoldCV_4 <- train(response~.,
                                     SpinaleAll_10FOLDCV_4,
                                     method = "rf",
                                     trControl = ctrl,
                                     tuneLength = 21,
                                     ntrees = 1000,
                                     metric = "Accuracy")

randomForest_all_results_10FoldCV_4 <- as.data.frame(randomForest_All_10FoldCV_4$results)

randomForest_all_maxAccuracy_10FoldCV_4 <- randomForest_all_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_10FoldCV <- rbind(randomForest_all_maxAccuracies_10FoldCV,
                                                 randomForest_all_maxAccuracy_10FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_all_10FoldCV_4 <- train(response~.,
                                           SpinaleAll_10FOLDCV_4,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl,
                                           metric = "accuracy")

logisticRegression_all_results_10FoldCV_4 <- as.data.frame(logisticRegression_all_10FoldCV_4$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_all_results_10FoldCV_4)

#K-NEAREST NEIGHBORS
KNN_all_10FOLDCV_4 <- train(response~.,
                            SpinaleAll_10FOLDCV_4,
                            method = "knn",
                            trControl = ctrl,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_all_results_10FoldCV_4 <- as.data.frame(KNN_all_10FOLDCV_4$results)

KNN_all_maxAccuracy_10FoldCV_4 <- KNN_all_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_10FoldCV <- rbind(KNN_all_maxAccuracies_10FoldCV,
                                        KNN_all_maxAccuracy_10FoldCV_4)

#LINEAR SVM
LinearSVM_all_10FoldCV_4 <- train(response~.,
                                  SpinaleAll_10FOLDCV_4,
                                  method = "svmLinear",
                                  trControl = ctrl,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_all_results_10FoldCV_4 <- as.data.frame(LinearSVM_all_10FoldCV_4$results)

LinearSVM_all_maxAccuracy_10FoldCV_4 <- LinearSVM_all_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_10FoldCV <- rbind(LinearSVM_all_maxAccuracies_10FoldCV, 
                                              LinearSVM_all_maxAccuracy_10FoldCV_4)

#RADIAL SVM
RadialSVM_all_10FoldCV_4 <- train(response~.,
                                  SpinaleAll_10FOLDCV_4,
                                  method = "svmRadial",
                                  trControl = ctrl,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_10FoldCV_4 <- as.data.frame(RadialSVM_all_10FoldCV_4$results)

RadialSVM_all_maxAccuracy_10FoldCV_4 <- RadialSVM_all_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_10FoldCV <- rbind(RadialSVM_all_maxAccuracies_10FoldCV, 
                                              RadialSVM_all_maxAccuracy_10FoldCV_4)

#Fifth 10-FOLD CV
SpinaleAll_10FOLDCV_5<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_All_10FoldCV_5 <- train(response~.,
                                     SpinaleAll_10FOLDCV_5,
                                     method = "rf",
                                     trControl = ctrl,
                                     tuneLength = 21,
                                     ntrees = 1000,
                                     metric = "Accuracy")

randomForest_all_results_10FoldCV_5 <- as.data.frame(randomForest_All_10FoldCV_5$results)

randomForest_all_maxAccuracy_10FoldCV_5 <- randomForest_all_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_10FoldCV <- rbind(randomForest_all_maxAccuracies_10FoldCV,
                                                 randomForest_all_maxAccuracy_10FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_all_10FoldCV_5 <- train(response~.,
                                           SpinaleAll_10FOLDCV_5,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl,
                                           metric = "accuracy")

logisticRegression_all_results_10FoldCV_5 <- as.data.frame(logisticRegression_all_10FoldCV_5$results)

logisticRegression_allClinical_maxAccuracies_10FoldCV <- rbind(logisticRegression_allClinical_maxAccuracies_10FoldCV,
                                                               logisticRegression_all_results_10FoldCV_5)

#K-NEAREST NEIGHBORS
KNN_all_10FOLDCV_5 <- train(response~.,
                            SpinaleAll_10FOLDCV_5,
                            method = "knn",
                            trControl = ctrl,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_all_results_10FoldCV_5 <- as.data.frame(KNN_all_10FOLDCV_5$results)

KNN_all_maxAccuracy_10FoldCV_5 <- KNN_all_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_10FoldCV <- rbind(KNN_all_maxAccuracies_10FoldCV,
                                        KNN_all_maxAccuracy_10FoldCV_5)

#LINEAR SVM
LinearSVM_all_10FoldCV_5 <- train(response~.,
                                  SpinaleAll_10FOLDCV_5,
                                  method = "svmLinear",
                                  trControl = ctrl,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_all_results_10FoldCV_5 <- as.data.frame(LinearSVM_all_10FoldCV_5$results)

LinearSVM_all_maxAccuracy_10FoldCV_5 <- LinearSVM_all_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_10FoldCV <- rbind(LinearSVM_all_maxAccuracies_10FoldCV, 
                                              LinearSVM_all_maxAccuracy_10FoldCV_5)

#RADIAL SVM
RadialSVM_all_10FoldCV_5 <- train(response~.,
                                  SpinaleAll_10FOLDCV_5,
                                  method = "svmRadial",
                                  trControl = ctrl,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_10FoldCV_5 <- as.data.frame(RadialSVM_all_10FoldCV_5$results)

RadialSVM_all_maxAccuracy_10FoldCV_5 <- RadialSVM_all_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_10FoldCV <- rbind(RadialSVM_all_maxAccuracies_10FoldCV, 
                                              RadialSVM_all_maxAccuracy_10FoldCV_5)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#first 5 fold CV
SpinaleAll_5FOLDCV_1<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_All_5FoldCV_1 <- train(response~.,
                                    SpinaleAll_5FOLDCV_1,
                                            method = "rf",
                                            trControl = ctrl_5_fold_CV,
                                            tuneLength = 9,
                                            ntrees = 1000,
                                            metric = "Accuracy")


randomForest_all_results_5FoldCV_1 <- as.data.frame(randomForest_All_5FoldCV_1$results)

randomForest_all_maxAccuracies_5FoldCV <- randomForest_all_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_All_5FoldCV_1 <- train(response~.,
                                    data = SpinaleAll_5FOLDCV_1,
                                    family = "binomial",
                                    method = "glm",
                                    trControl = ctrl_5_fold_CV,
                                    metric = "Accuracy")

logisticRegression_all_results_5FoldCV_1 <- as.data.frame(logisticRegression_All_5FoldCV_1$results)

logisticRegression_all_maxAccuracies_5FoldCV <- logisticRegression_all_results_5FoldCV_1


#K-NEAREST NEIGHBORS
KNN_all_5FoldCV_1 <-  train(response~.,
                                    SpinaleClinical_5FOLDCV_1,
                                    method = "knn",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 20,
                                    metric = "Accuracy")

KNN_all_results_5FoldCV_1 <- as.data.frame(KNN_all_5FoldCV_1$results)

KNN_all_maxAccuracies_5FoldCV <- KNN_all_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_all_5FoldCV_1 <- train(response~.,
                                  SpinaleClinical_5FOLDCV_1,
                                         method = "svmLinear",
                                         trControl = ctrl_5_fold_CV,
                                         tuneGrid = grid,
                                         metric = "Accuracy")

LinearSVM_all_results_5FoldCV_1 <- as.data.frame(LinearSVM_all_5FoldCV_1$results)

LinearSVM_all_maxAccuracies_5FoldCV <- LinearSVM_all_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_all_5FoldCV_1 <- train(response~.,
                                 SpinaleClinical_5FOLDCV_1,
                                  method = "svmRadial",
                                  trControl = ctrl_5_fold_CV,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_5FoldCV_1 <- as.data.frame(RadialSVM_all_5FoldCV_1$results)

RadialSVM_all_maxAccuracies_5FoldCV <- RadialSVM_all_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Second 5-Fold CV
SpinaleAll_5FOLDCV_2<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_All_5FoldCV_2 <- train(response~.,
                                    SpinaleAll_5FOLDCV_2,
                                     method = "rf",
                                     trControl = ctrl_5_fold_CV,
                                     tuneLength = 21,
                                     ntrees = 1000,
                                     metric = "Accuracy")

randomForest_all_results_5FoldCV_2 <- as.data.frame(randomForest_All_5FoldCV_2$results)

randomForest_all_maxAccuracy_5FoldCV_2 <- randomForest_all_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_5FoldCV <- rbind(randomForest_all_maxAccuracies_5FoldCV,
                                                 randomForest_all_maxAccuracy_5FoldCV_2)

#LOGISTIC REGRESSION
logisticRegression_all_5FoldCV_2 <- train(response~.,
                                          SpinaleAll_5FOLDCV_2,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl_5_fold_CV,
                                           metric = "accuracy")

logisticRegression_all_results_5FoldCV_2 <- as.data.frame(logisticRegression_all_5FoldCV_2$results)

logisticRegression_all_maxAccuracies_5FoldCV <- rbind(logisticRegression_all_maxAccuracies_5FoldCV,
                                                               logisticRegression_all_results_5FoldCV_2)

#K-NEAREST NEIGHBORS
KNN_all_5FOLDCV_2 <- train(response~.,
                           SpinaleAll_5FOLDCV_2,
                            method = "knn",
                            trControl = ctrl_5_fold_CV,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_all_results_5FoldCV_2 <- as.data.frame(KNN_all_5FOLDCV_2$results)

KNN_all_maxAccuracy_5FoldCV_2 <- KNN_all_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_5FoldCV <- rbind(KNN_all_maxAccuracies_5FoldCV,
                                        KNN_all_maxAccuracy_5FoldCV_2)

#LINEAR SVM
LinearSVM_all_5FOLDCV_2 <- train(response~.,
                                 SpinaleAll_5FOLDCV_2,
                                  method = "svmLinear",
                                  trControl = ctrl_5_fold_CV,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_all_results_5FoldCV_2 <- as.data.frame(LinearSVM_all_5FOLDCV_2$results)

LinearSVM_all_maxAccuracy_5FoldCV_2 <- LinearSVM_all_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_5FoldCV <- rbind(LinearSVM_all_maxAccuracies_5FoldCV, 
                                             LinearSVM_all_maxAccuracy_5FoldCV_2)

#RADIAL SVM
RadialSVM_all_5FoldCV_2 <- train(response~.,
                                 SpinaleAll_5FOLDCV_2,
                                  method = "svmRadial",
                                  trControl = ctrl_5_fold_CV,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_all_results_5FoldCV_2 <- as.data.frame(RadialSVM_all_5FoldCV_2$results)

RadialSVM_all_maxAccuracy_5FoldCV_2 <- RadialSVM_all_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_5FoldCV <- rbind(RadialSVM_all_maxAccuracies_5FoldCV, 
                                              RadialSVM_all_maxAccuracy_5FoldCV_2)

#Third 5-Fold CV
SpinaleAll_5FOLDCV_3<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_All_5FoldCV_3 <- train(response~.,
                                    SpinaleAll_5FOLDCV_3,
                                    method = "rf",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 21,
                                    ntrees = 1000,
                                    metric = "Accuracy")

randomForest_all_results_5FoldCV_3 <- as.data.frame(randomForest_All_5FoldCV_3$results)

randomForest_all_maxAccuracy_5FoldCV_3 <- randomForest_all_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_5FoldCV <- rbind(randomForest_all_maxAccuracies_5FoldCV,
                                                randomForest_all_maxAccuracy_5FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_all_5FoldCV_3 <- train(response~.,
                                          SpinaleAll_5FOLDCV_3,
                                          family = "binomial",
                                          method = "glm",
                                          trControl = ctrl_5_fold_CV,
                                          metric = "accuracy")

logisticRegression_all_results_5FoldCV_3 <- as.data.frame(logisticRegression_all_5FoldCV_3$results)

logisticRegression_all_maxAccuracies_5FoldCV <- rbind(logisticRegression_all_maxAccuracies_5FoldCV,
                                                      logisticRegression_all_results_5FoldCV_3)

#K-NEAREST NEIGHBORS
KNN_all_5FOLDCV_3 <- train(response~.,
                           SpinaleAll_5FOLDCV_3,
                           method = "knn",
                           trControl = ctrl_5_fold_CV,
                           tuneLength = 20,
                           metric = "Accuracy")

KNN_all_results_5FoldCV_3 <- as.data.frame(KNN_all_5FOLDCV_3$results)

KNN_all_maxAccuracy_5FoldCV_3 <- KNN_all_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_5FoldCV <- rbind(KNN_all_maxAccuracies_5FoldCV,
                                       KNN_all_maxAccuracy_5FoldCV_3)

#LINEAR SVM
LinearSVM_all_5FOLDCV_3 <- train(response~.,
                                 SpinaleAll_5FOLDCV_3,
                                 method = "svmLinear",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid,
                                 metric = "Accuracy")

LinearSVM_all_results_5FoldCV_3 <- as.data.frame(LinearSVM_all_5FOLDCV_3$results)

LinearSVM_all_maxAccuracy_5FoldCV_3 <- LinearSVM_all_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_5FoldCV <- rbind(LinearSVM_all_maxAccuracies_5FoldCV, 
                                             LinearSVM_all_maxAccuracy_5FoldCV_3)

#RADIAL SVM
RadialSVM_all_5FoldCV_3 <- train(response~.,
                                 SpinaleAll_5FOLDCV_3,
                                 method = "svmRadial",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid_radial,
                                 metric = "Accuracy")

RadialSVM_all_results_5FoldCV_3 <- as.data.frame(RadialSVM_all_5FoldCV_3$results)

RadialSVM_all_maxAccuracy_5FoldCV_3 <- RadialSVM_all_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_5FoldCV <- rbind(RadialSVM_all_maxAccuracies_5FoldCV, 
                                             RadialSVM_all_maxAccuracy_5FoldCV_3)

#Fourth 5-Fold CV
SpinaleAll_5FOLDCV_4<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_All_5FoldCV_4 <- train(response~.,
                                    SpinaleAll_5FOLDCV_4,
                                    method = "rf",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 21,
                                    ntrees = 1000,
                                    metric = "Accuracy")

randomForest_all_results_5FoldCV_4 <- as.data.frame(randomForest_All_5FoldCV_4$results)

randomForest_all_maxAccuracy_5FoldCV_4 <- randomForest_all_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_5FoldCV <- rbind(randomForest_all_maxAccuracies_5FoldCV,
                                                randomForest_all_maxAccuracy_5FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_all_5FoldCV_4 <- train(response~.,
                                          SpinaleAll_5FOLDCV_4,
                                          family = "binomial",
                                          method = "glm",
                                          trControl = ctrl_5_fold_CV,
                                          metric = "accuracy")

logisticRegression_all_results_5FoldCV_4 <- as.data.frame(logisticRegression_all_5FoldCV_4$results)

logisticRegression_all_maxAccuracies_5FoldCV <- rbind(logisticRegression_all_maxAccuracies_5FoldCV,
                                                      logisticRegression_all_results_5FoldCV_4)

#K-NEAREST NEIGHBORS
KNN_all_5FOLDCV_4 <- train(response~.,
                           SpinaleAll_5FOLDCV_4,
                           method = "knn",
                           trControl = ctrl_5_fold_CV,
                           tuneLength = 20,
                           metric = "Accuracy")

KNN_all_results_5FoldCV_4 <- as.data.frame(KNN_all_5FOLDCV_4$results)

KNN_all_maxAccuracy_5FoldCV_4 <- KNN_all_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_5FoldCV <- rbind(KNN_all_maxAccuracies_5FoldCV,
                                       KNN_all_maxAccuracy_5FoldCV_4)

#LINEAR SVM
LinearSVM_all_5FOLDCV_4 <- train(response~.,
                                 SpinaleAll_5FOLDCV_4,
                                 method = "svmLinear",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid,
                                 metric = "Accuracy")

LinearSVM_all_results_5FoldCV_4 <- as.data.frame(LinearSVM_all_5FOLDCV_4$results)

LinearSVM_all_maxAccuracy_5FoldCV_4 <- LinearSVM_all_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_5FoldCV <- rbind(LinearSVM_all_maxAccuracies_5FoldCV, 
                                             LinearSVM_all_maxAccuracy_5FoldCV_4)

#RADIAL SVM
RadialSVM_all_5FoldCV_4 <- train(response~.,
                                 SpinaleAll_5FOLDCV_4,
                                 method = "svmRadial",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid_radial,
                                 metric = "Accuracy")

RadialSVM_all_results_5FoldCV_4 <- as.data.frame(RadialSVM_all_5FoldCV_4$results)

RadialSVM_all_maxAccuracy_5FoldCV_4 <- RadialSVM_all_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_5FoldCV <- rbind(RadialSVM_all_maxAccuracies_5FoldCV, 
                                             RadialSVM_all_maxAccuracy_5FoldCV_4)

#Fifth 5-Fold CV
SpinaleAll_5FOLDCV_5<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_All_5FoldCV_5 <- train(response~.,
                                    SpinaleAll_5FOLDCV_5,
                                    method = "rf",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 21,
                                    ntrees = 1000,
                                    metric = "Accuracy")

randomForest_all_results_5FoldCV_5 <- as.data.frame(randomForest_All_5FoldCV_5$results)

randomForest_all_maxAccuracy_5FoldCV_5 <- randomForest_all_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_all_maxAccuracies_5FoldCV <- rbind(randomForest_all_maxAccuracies_5FoldCV,
                                                randomForest_all_maxAccuracy_5FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_all_5FoldCV_5 <- train(response~.,
                                          SpinaleAll_5FOLDCV_5,
                                          family = "binomial",
                                          method = "glm",
                                          trControl = ctrl_5_fold_CV,
                                          metric = "accuracy")

logisticRegression_all_results_5FoldCV_5 <- as.data.frame(logisticRegression_all_5FoldCV_5$results)

logisticRegression_all_maxAccuracies_5FoldCV <- rbind(logisticRegression_all_maxAccuracies_5FoldCV,
                                                      logisticRegression_all_results_5FoldCV_5)

#K-NEAREST NEIGHBORS
KNN_all_5FOLDCV_5 <- train(response~.,
                           SpinaleAll_5FOLDCV_5,
                           method = "knn",
                           trControl = ctrl_5_fold_CV,
                           tuneLength = 20,
                           metric = "Accuracy")

KNN_all_results_5FoldCV_5 <- as.data.frame(KNN_all_5FOLDCV_5$results)

KNN_all_maxAccuracy_5FoldCV_5 <- KNN_all_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_all_maxAccuracies_5FoldCV <- rbind(KNN_all_maxAccuracies_5FoldCV,
                                       KNN_all_maxAccuracy_5FoldCV_5)

#LINEAR SVM
LinearSVM_all_5FOLDCV_5 <- train(response~.,
                                 SpinaleAll_5FOLDCV_5,
                                 method = "svmLinear",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid,
                                 metric = "Accuracy")

LinearSVM_all_results_5FoldCV_5 <- as.data.frame(LinearSVM_all_5FOLDCV_5$results)

LinearSVM_all_maxAccuracy_5FoldCV_5 <- LinearSVM_all_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_all_maxAccuracies_5FoldCV <- rbind(LinearSVM_all_maxAccuracies_5FoldCV, 
                                             LinearSVM_all_maxAccuracy_5FoldCV_5)

#RADIAL SVM
RadialSVM_all_5FoldCV_5 <- train(response~.,
                                 SpinaleAll_5FOLDCV_5,
                                 method = "svmRadial",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid_radial,
                                 metric = "Accuracy")

RadialSVM_all_results_5FoldCV_5 <- as.data.frame(RadialSVM_all_5FoldCV_5$results)

RadialSVM_all_maxAccuracy_5FoldCV_5 <- RadialSVM_all_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_all_maxAccuracies_5FoldCV <- rbind(RadialSVM_all_maxAccuracies_5FoldCV, 
                                             RadialSVM_all_maxAccuracy_5FoldCV_5)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#10-Fold CV
SpinaleAll_10FOLDCV_1<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_AllImportantVariables_10FoldCV_1 <- train(response~ Stretch_standardized + ST2_standardized +
                                      EjectionFraction_standardized + TIFRII_standardized + 
                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                    SpinaleAll_10FOLDCV_1,
                                     method = "rf",
                                     trControl = ctrl,
                                     tuneLength = 7,
                                     ntrees = 1000,
                                     metric = "Accuracy")


randomForest_allImportantVariables_results_10FoldCV_1 <- as.data.frame(randomForest_AllImportantVariables_10FoldCV_1$results)

randomForest_allImportantVariables_maxAccuracies_10FoldCV <- randomForest_allImportantVariables_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_AllImportantVariables_10FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                             EjectionFraction_standardized + TIFRII_standardized + 
                                             LVESV_standardized + CRP_standardized + BNP_standardized,
                                           SpinaleAll_10FOLDCV_1,
                                           family = "binomial",
                                           method = "glm",
                                           trControl = ctrl,
                                           metric = "accuracy")

logisticRegression_allImportantVariables_results_10FoldCV_1 <- as.data.frame(logisticRegression_AllImportantVariables_10FoldCV_1$results)

logisticRegression_allImportantVariables_maxAccuracies_10FoldCV <- logisticRegression_allImportantVariables_results_10FoldCV_1

#K-NEAREST NEIGHBORS
KNN_AllImportantVariables_10FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                              EjectionFraction_standardized + TIFRII_standardized + 
                              LVESV_standardized + CRP_standardized + BNP_standardized,
                            SpinaleAll_10FOLDCV_1,
                            method = "knn",
                            trControl = ctrl,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_allImportantVariables_results_10FoldCV_1 <- as.data.frame(KNN_AllImportantVariables_10FoldCV_1$results)

KNN_allImportantVariables_maxAccuracies_10FoldCV <- KNN_allImportantVariables_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_AllImportantVariables_10FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                    EjectionFraction_standardized + TIFRII_standardized + 
                                    LVESV_standardized + CRP_standardized + BNP_standardized,
                                  SpinaleAll_10FOLDCV_1,
                                  method = "svmLinear",
                                  trControl = ctrl,
                                  tuneGrid = grid,
                                  metric = "Accuracy")

LinearSVM_allImportantVariables_results_10FoldCV_1 <- as.data.frame(LinearSVM_AllImportantVariables_10FoldCV_1$results)

LinearSVM_allImportantVariables_maxAccuracies_10FoldCV <- LinearSVM_allImportantVariables_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Radial SVM
RadialSVM_AllImportantVariables_10FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                    EjectionFraction_standardized + TIFRII_standardized + 
                                    LVESV_standardized + CRP_standardized + BNP_standardized,
                                  SpinaleAll_10FOLDCV_1,
                                  method = "svmRadial",
                                  trControl = ctrl,
                                  tuneGrid = grid_radial,
                                  metric = "Accuracy")

RadialSVM_allImportantVariables_results_10FoldCV_1 <- as.data.frame(RadialSVM_AllImportantVariables_10FoldCV_1$results)

RadialSVM_allImportantVariables_maxAccuracies_10FoldCV <- RadialSVM_allImportantVariables_results_10FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Second 10-Fold CV
SpinaleAll_10FOLDCV_2<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_10FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                                         EjectionFraction_standardized + TIFRII_standardized + 
                                                         LVESV_standardized + CRP_standardized + BNP_standardized,
                                    SpinaleAll_10FOLDCV_2,
                                    method = "rf",
                                    trControl = ctrl,
                                    tuneLength = 21,
                                    ntrees = 1000,
                                    metric = "Accuracy")

randomForest_allImportantVariables_results_10FoldCV_2 <- as.data.frame(randomForest_AllImportantVariables_10FoldCV_2$results)

randomForest_allImportantVariables_maxAccuracy_10FoldCV_2 <- randomForest_allImportantVariables_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_10FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_10FoldCV,
                                                randomForest_allImportantVariables_maxAccuracy_10FoldCV_2)

#LOGISTIC REGRESSION
logisticRegression_AllImportantVariables_10FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                                               EjectionFraction_standardized + TIFRII_standardized + 
                                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                          SpinaleAll_10FOLDCV_2,
                                          family = "binomial",
                                          method = "glm",
                                          trControl = ctrl,
                                          metric = "accuracy")

logisticRegression_allImportantVariables_results_10FoldCV_2 <- as.data.frame(logisticRegression_AllImportantVariables_10FoldCV_2$results)

logisticRegression_allImportantVariables_maxAccuracies_10FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_10FoldCV,
                                                                         logisticRegression_allImportantVariables_results_10FoldCV_2)

#K-NEAREST NEIGHBORS
KNN_AllImportantVariables_10FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                             EjectionFraction_standardized + TIFRII_standardized + 
                             LVESV_standardized + CRP_standardized + BNP_standardized,
                           SpinaleAll_10FOLDCV_2,
                           method = "knn",
                           trControl = ctrl,
                           tuneLength = 20,
                           metric = "Accuracy")

KNN_allImportantVariables_results_10FoldCV_2 <- as.data.frame(KNN_AllImportantVariables_10FoldCV_2$results)

KNN_allImportantVariables_maxAccuracy_10FoldCV_2 <- KNN_allImportantVariables_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_10FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_10FoldCV,
                                                          KNN_allImportantVariables_maxAccuracy_10FoldCV_2)

#LINEAR SVM
LinearSVM_AllImportantVariables_10FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                   EjectionFraction_standardized + TIFRII_standardized + 
                                   LVESV_standardized + CRP_standardized + BNP_standardized,
                                 SpinaleAll_10FOLDCV_2,
                                 method = "svmLinear",
                                 trControl = ctrl,
                                 tuneGrid = grid,
                                 metric = "Accuracy")

LinearSVM_allImportantVariables_results_10FoldCV_2 <- as.data.frame(LinearSVM_AllImportantVariables_10FoldCV_2$results)

LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_2 <- LinearSVM_allImportantVariables_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                             LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_2)

#RADIAL SVM
RadialSVM_AllImportantVariables_10FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                   EjectionFraction_standardized + TIFRII_standardized + 
                                   LVESV_standardized + CRP_standardized + BNP_standardized,
                                 SpinaleAll_10FOLDCV_2,
                                 method = "svmRadial",
                                 trControl = ctrl,
                                 tuneGrid = grid_radial,
                                 metric = "Accuracy")

RadialSVM_allImportantVariables_results_10FoldCV_2 <- as.data.frame(RadialSVM_AllImportantVariables_10FoldCV_2$results)

RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_2 <- RadialSVM_allImportantVariables_results_10FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                             RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_2)

#Third 10-Fold CV
SpinaleAll_10FOLDCV_3<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_10FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                         EjectionFraction_standardized + TIFRII_standardized + 
                                                         LVESV_standardized + CRP_standardized + BNP_standardized,
                                                       SpinaleAll_10FOLDCV_3,
                                                       method = "rf",
                                                       trControl = ctrl,
                                                       tuneLength = 21,
                                                       ntrees = 1000,
                                                       metric = "Accuracy")

randomForest_allImportantVariables_results_10FoldCV_3 <- as.data.frame(randomForest_AllImportantVariables_10FoldCV_3$results)

randomForest_allImportantVariables_maxAccuracy_10FoldCV_3 <- randomForest_allImportantVariables_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_10FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_10FoldCV,
                                                                   randomForest_allImportantVariables_maxAccuracy_10FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_AllImportantVariables_10FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                               EjectionFraction_standardized + TIFRII_standardized + 
                                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                                             SpinaleAll_10FOLDCV_3,
                                                             family = "binomial",
                                                             method = "glm",
                                                             trControl = ctrl,
                                                             metric = "accuracy")

logisticRegression_allImportantVariables_results_10FoldCV_3 <- as.data.frame(logisticRegression_AllImportantVariables_10FoldCV_3$results)

logisticRegression_allImportantVariables_maxAccuracies_10FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_10FoldCV,
                                                                         logisticRegression_allImportantVariables_results_10FoldCV_3)

#K-NEAREST NEIGHBORS
KNN_AllImportantVariables_10FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                EjectionFraction_standardized + TIFRII_standardized + 
                                                LVESV_standardized + CRP_standardized + BNP_standardized,
                                              SpinaleAll_10FOLDCV_3,
                                              method = "knn",
                                              trControl = ctrl,
                                              tuneLength = 20,
                                              metric = "Accuracy")

KNN_allImportantVariables_results_10FoldCV_3 <- as.data.frame(KNN_AllImportantVariables_10FoldCV_3$results)

KNN_allImportantVariables_maxAccuracy_10FoldCV_3 <- KNN_allImportantVariables_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_10FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_10FoldCV,
                                                          KNN_allImportantVariables_maxAccuracy_10FoldCV_3)

#LINEAR SVM
LinearSVM_AllImportantVariables_10FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                      EjectionFraction_standardized + TIFRII_standardized + 
                                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                                    SpinaleAll_10FOLDCV_3,
                                                    method = "svmLinear",
                                                    trControl = ctrl,
                                                    tuneGrid = grid,
                                                    metric = "Accuracy")

LinearSVM_allImportantVariables_results_10FoldCV_3 <- as.data.frame(LinearSVM_AllImportantVariables_10FoldCV_3$results)

LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_3 <- LinearSVM_allImportantVariables_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                                                LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_3)

#RADIAL SVM
RadialSVM_AllImportantVariables_10FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                      EjectionFraction_standardized + TIFRII_standardized + 
                                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                                    SpinaleAll_10FOLDCV_3,
                                                    method = "svmRadial",
                                                    trControl = ctrl,
                                                    tuneGrid = grid_radial,
                                                    metric = "Accuracy")

RadialSVM_allImportantVariables_results_10FoldCV_3 <- as.data.frame(RadialSVM_AllImportantVariables_10FoldCV_3$results)

RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_3 <- RadialSVM_allImportantVariables_results_10FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                                                RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_3)

#Fourth 10-Fold CV
SpinaleAll_10FOLDCV_4<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_10FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                         EjectionFraction_standardized + TIFRII_standardized + 
                                                         LVESV_standardized + CRP_standardized + BNP_standardized,
                                                       SpinaleAll_10FOLDCV_4,
                                                       method = "rf",
                                                       trControl = ctrl,
                                                       tuneLength = 21,
                                                       ntrees = 1000,
                                                       metric = "Accuracy")

randomForest_allImportantVariables_results_10FoldCV_4 <- as.data.frame(randomForest_AllImportantVariables_10FoldCV_4$results)

randomForest_allImportantVariables_maxAccuracy_10FoldCV_4 <- randomForest_allImportantVariables_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_10FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_10FoldCV,
                                                                   randomForest_allImportantVariables_maxAccuracy_10FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_AllImportantVariables_10FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                               EjectionFraction_standardized + TIFRII_standardized + 
                                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                                             SpinaleAll_10FOLDCV_4,
                                                             family = "binomial",
                                                             method = "glm",
                                                             trControl = ctrl,
                                                             metric = "accuracy")

logisticRegression_allImportantVariables_results_10FoldCV_4 <- as.data.frame(logisticRegression_AllImportantVariables_10FoldCV_4$results)

logisticRegression_allImportantVariables_maxAccuracies_10FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_10FoldCV,
                                                                         logisticRegression_allImportantVariables_results_10FoldCV_4)

#K-NEAREST NEIGHBORS
KNN_AllImportantVariables_10FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                EjectionFraction_standardized + TIFRII_standardized + 
                                                LVESV_standardized + CRP_standardized + BNP_standardized,
                                              SpinaleAll_10FOLDCV_4,
                                              method = "knn",
                                              trControl = ctrl,
                                              tuneLength = 20,
                                              metric = "Accuracy")

KNN_allImportantVariables_results_10FoldCV_4 <- as.data.frame(KNN_AllImportantVariables_10FoldCV_4$results)

KNN_allImportantVariables_maxAccuracy_10FoldCV_4 <- KNN_allImportantVariables_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_10FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_10FoldCV,
                                                          KNN_allImportantVariables_maxAccuracy_10FoldCV_4)

#LINEAR SVM
LinearSVM_AllImportantVariables_10FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                      EjectionFraction_standardized + TIFRII_standardized + 
                                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                                    SpinaleAll_10FOLDCV_4,
                                                    method = "svmLinear",
                                                    trControl = ctrl,
                                                    tuneGrid = grid,
                                                    metric = "Accuracy")

LinearSVM_allImportantVariables_results_10FoldCV_4 <- as.data.frame(LinearSVM_AllImportantVariables_10FoldCV_4$results)

LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_4 <- LinearSVM_allImportantVariables_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                                                LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_4)

#RADIAL SVM
RadialSVM_AllImportantVariables_10FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                      EjectionFraction_standardized + TIFRII_standardized + 
                                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                                    SpinaleAll_10FOLDCV_4,
                                                    method = "svmRadial",
                                                    trControl = ctrl,
                                                    tuneGrid = grid_radial,
                                                    metric = "Accuracy")

RadialSVM_allImportantVariables_results_10FoldCV_4 <- as.data.frame(RadialSVM_AllImportantVariables_10FoldCV_4$results)

RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_4 <- RadialSVM_allImportantVariables_results_10FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                                                RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_4)

#Fifth 10-Fold CV
SpinaleAll_10FOLDCV_5<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_10FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                         EjectionFraction_standardized + TIFRII_standardized + 
                                                         LVESV_standardized + CRP_standardized + BNP_standardized,
                                                       SpinaleAll_10FOLDCV_5,
                                                       method = "rf",
                                                       trControl = ctrl,
                                                       tuneLength = 21,
                                                       ntrees = 1000,
                                                       metric = "Accuracy")

randomForest_allImportantVariables_results_10FoldCV_5 <- as.data.frame(randomForest_AllImportantVariables_10FoldCV_5$results)

randomForest_allImportantVariables_maxAccuracy_10FoldCV_5 <- randomForest_allImportantVariables_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_10FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_10FoldCV,
                                                                   randomForest_allImportantVariables_maxAccuracy_10FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_AllImportantVariables_10FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                               EjectionFraction_standardized + TIFRII_standardized + 
                                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                                             SpinaleAll_10FOLDCV_5,
                                                             family = "binomial",
                                                             method = "glm",
                                                             trControl = ctrl,
                                                             metric = "accuracy")

logisticRegression_allImportantVariables_results_10FoldCV_5 <- as.data.frame(logisticRegression_AllImportantVariables_10FoldCV_5$results)

logisticRegression_allImportantVariables_maxAccuracies_10FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_10FoldCV,
                                                                         logisticRegression_allImportantVariables_results_10FoldCV_5)

#K-NEAREST NEIGHBORS
KNN_AllImportantVariables_10FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                EjectionFraction_standardized + TIFRII_standardized + 
                                                LVESV_standardized + CRP_standardized + BNP_standardized,
                                              SpinaleAll_10FOLDCV_5,
                                              method = "knn",
                                              trControl = ctrl,
                                              tuneLength = 20,
                                              metric = "Accuracy")

KNN_allImportantVariables_results_10FoldCV_5 <- as.data.frame(KNN_AllImportantVariables_10FoldCV_5$results)

KNN_allImportantVariables_maxAccuracy_10FoldCV_5 <- KNN_allImportantVariables_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_10FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_10FoldCV,
                                                          KNN_allImportantVariables_maxAccuracy_10FoldCV_5)

#LINEAR SVM
LinearSVM_AllImportantVariables_10FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                      EjectionFraction_standardized + TIFRII_standardized + 
                                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                                    SpinaleAll_10FOLDCV_5,
                                                    method = "svmLinear",
                                                    trControl = ctrl,
                                                    tuneGrid = grid,
                                                    metric = "Accuracy")

LinearSVM_allImportantVariables_results_10FoldCV_5 <- as.data.frame(LinearSVM_AllImportantVariables_10FoldCV_5$results)

LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_5 <- LinearSVM_allImportantVariables_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                                                LinearSVM_allImportantVariables_maxAccuracy_10FoldCV_5)

#RADIAL SVM
RadialSVM_AllImportantVariables_10FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                      EjectionFraction_standardized + TIFRII_standardized + 
                                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                                    SpinaleAll_10FOLDCV_5,
                                                    method = "svmRadial",
                                                    trControl = ctrl,
                                                    tuneGrid = grid_radial,
                                                    metric = "Accuracy")

RadialSVM_allImportantVariables_results_10FoldCV_5 <- as.data.frame(RadialSVM_AllImportantVariables_10FoldCV_5$results)

RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_5 <- RadialSVM_allImportantVariables_results_10FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_10FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_10FoldCV, 
                                                                RadialSVM_allImportantVariables_maxAccuracy_10FoldCV_5)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#first 5 fold CV
SpinaleAllVariables_5FOLDCV_1 <- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST

randomForest_AllImportantVariables_5FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                                        EjectionFraction_standardized + TIFRII_standardized + 
                                                        LVESV_standardized + CRP_standardized + BNP_standardized,
                                    SpinaleAllVariables_5FOLDCV_1,
                                    method = "rf",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 9,
                                    ntrees = 1000,
                                    metric = "Accuracy")


randomForest_allImportantVariables_results_5FoldCV_1 <- as.data.frame(randomForest_AllImportantVariables_5FoldCV_1$results)

randomForest_allImportantVariables_maxAccuracies_5FoldCV <- randomForest_allImportantVariables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LOGISTIC REGRESSION
logisticRegression_AllImportantVariables_5FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                            EjectionFraction_standardized + TIFRII_standardized + 
                                            LVESV_standardized + CRP_standardized + BNP_standardized,
                                          data = SpinaleAllVariables_5FOLDCV_1,
                                          family = "binomial",
                                          method = "glm",
                                          trControl = ctrl_5_fold_CV,
                                          metric = "Accuracy")

logisticRegression_allImportantVariables_results_5FoldCV_1 <- as.data.frame(logisticRegression_AllImportantVariables_5FoldCV_1$results)

logisticRegression_allImportantVariables_maxAccuracies_5FoldCV <- logisticRegression_allImportantVariables_results_5FoldCV_1


#K-NEAREST NEIGHBORS
KNN_allImportantVariables_5FoldCV_1 <-  train(response~Stretch_standardized + ST2_standardized +
                                                EjectionFraction_standardized + TIFRII_standardized + 
                                                LVESV_standardized + CRP_standardized + BNP_standardized,
                            SpinaleAllVariables_5FOLDCV_1,
                            method = "knn",
                            trControl = ctrl_5_fold_CV,
                            tuneLength = 20,
                            metric = "Accuracy")

KNN_allImportantVariables_results_5FoldCV_1 <- as.data.frame(KNN_allImportantVariables_5FoldCV_1$results)

KNN_allImportantVariables_maxAccuracies_5FoldCV <- KNN_allImportantVariables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#LINEAR SVM
LinearSVM_allImportant_5FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                   EjectionFraction_standardized + TIFRII_standardized + 
                                   LVESV_standardized + CRP_standardized + BNP_standardized,
                                 SpinaleAllVariables_5FOLDCV_1,
                                 method = "svmLinear",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid,
                                 metric = "Accuracy")

LinearSVM_allImportantVariables_results_5FoldCV_1 <- as.data.frame(LinearSVM_allImportant_5FoldCV_1$results)

LinearSVM_allImportantVariables_maxAccuracies_5FoldCV <- LinearSVM_allImportantVariables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#RADIAL SVM
RadialSVM_allImportantVariables_5FoldCV_1 <- train(response~Stretch_standardized + ST2_standardized +
                                   EjectionFraction_standardized + TIFRII_standardized + 
                                   LVESV_standardized + CRP_standardized + BNP_standardized,
                                 SpinaleAllVariables_5FOLDCV_1,
                                 method = "svmRadial",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid_radial,
                                 metric = "Accuracy")

RadialSVM_allImportantVariables_results_5FoldCV_1 <- as.data.frame(RadialSVM_allImportantVariables_5FoldCV_1$results)

RadialSVM_allImportantVariables_maxAccuracies_5FoldCV <- RadialSVM_allImportantVariables_results_5FoldCV_1 %>%
  filter(Accuracy == max(Accuracy))

#Second 5-Fold CV
SpinaleAllImportantVariables_5FOLDCV_2<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_5FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                      EjectionFraction_standardized + TIFRII_standardized + 
                                      LVESV_standardized + CRP_standardized + BNP_standardized,
                                    SpinaleAllImportantVariables_5FOLDCV_2,
                                    method = "rf",
                                    trControl = ctrl_5_fold_CV,
                                    tuneLength = 21,
                                    ntrees = 1000,
                                    metric = "Accuracy")

randomForest_allImportantVariables_results_5FoldCV_2 <- as.data.frame(randomForest_AllImportantVariables_5FoldCV_2$results)

randomForest_allImportantVariables_maxAccuracy_5FoldCV_2 <- randomForest_allImportantVariables_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_5FoldCV,
                                                randomForest_allImportantVariables_maxAccuracy_5FoldCV_2)

#LOGISTIC REGRESSION
logisticRegression_allImportantVariables_5FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                            EjectionFraction_standardized + TIFRII_standardized + 
                                            LVESV_standardized + CRP_standardized + BNP_standardized,
                                          SpinaleAllImportantVariables_5FOLDCV_2,
                                          family = "binomial",
                                          method = "glm",
                                          trControl = ctrl_5_fold_CV,
                                          metric = "accuracy")

logisticRegression_allImportantVariables_results_5FoldCV_2 <- as.data.frame(logisticRegression_allImportantVariables_5FoldCV_2$results)

logisticRegression_allImportantVariables_maxAccuracies_5FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_5FoldCV,
                                                      logisticRegression_allImportantVariables_results_5FoldCV_2)

#K-NEAREST NEIGHBORS
KNN_allImportantVariables_5FOLDCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                             EjectionFraction_standardized + TIFRII_standardized + 
                             LVESV_standardized + CRP_standardized + BNP_standardized,
                           SpinaleAllImportantVariables_5FOLDCV_2,
                           method = "knn",
                           trControl = ctrl_5_fold_CV,
                           tuneLength = 20,
                           metric = "Accuracy")

KNN_allImportantVariables_results_5FoldCV_2 <- as.data.frame(KNN_allImportantVariables_5FOLDCV_2$results)

KNN_allImportantVariables_maxAccuracy_5FoldCV_2 <- KNN_allImportantVariables_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_5FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_5FoldCV,
                                       KNN_allImportantVariables_maxAccuracy_5FoldCV_2)

#LINEAR SVM
LinearSVM_allImportantVariables_5FOLDCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                   EjectionFraction_standardized + TIFRII_standardized + 
                                   LVESV_standardized + CRP_standardized + BNP_standardized,
                                 SpinaleAllImportantVariables_5FOLDCV_2,
                                 method = "svmLinear",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid,
                                 metric = "Accuracy")

LinearSVM_allImportantVariables_results_5FoldCV_2 <- as.data.frame(LinearSVM_allImportantVariables_5FOLDCV_2$results)

LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_2 <- LinearSVM_allImportantVariables_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                             LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_2)

#RADIAL SVM
RadialSVM_allImportantVariables_5FoldCV_2 <- train(response~Stretch_standardized + ST2_standardized +
                                   EjectionFraction_standardized + TIFRII_standardized + 
                                   LVESV_standardized + CRP_standardized + BNP_standardized,
                                 SpinaleAllImportantVariables_5FOLDCV_2,
                                 method = "svmRadial",
                                 trControl = ctrl_5_fold_CV,
                                 tuneGrid = grid_radial,
                                 metric = "Accuracy")

RadialSVM_allImportantVariables_results_5FoldCV_2 <- as.data.frame(RadialSVM_allImportantVariables_5FoldCV_2$results)

RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_2 <- RadialSVM_allImportantVariables_results_5FoldCV_2 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                             RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_2)

#Third 5-Fold CV
SpinaleAllImportantVariables_5FOLDCV_3<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_5FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                        EjectionFraction_standardized + TIFRII_standardized + 
                                                        LVESV_standardized + CRP_standardized + BNP_standardized,
                                                      SpinaleAllImportantVariables_5FOLDCV_3,
                                                      method = "rf",
                                                      trControl = ctrl_5_fold_CV,
                                                      tuneLength = 21,
                                                      ntrees = 1000,
                                                      metric = "Accuracy")

randomForest_allImportantVariables_results_5FoldCV_3 <- as.data.frame(randomForest_AllImportantVariables_5FoldCV_3$results)

randomForest_allImportantVariables_maxAccuracy_5FoldCV_3 <- randomForest_allImportantVariables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_5FoldCV,
                                                                  randomForest_allImportantVariables_maxAccuracy_5FoldCV_3)

#LOGISTIC REGRESSION
logisticRegression_allImportantVariables_5FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                              EjectionFraction_standardized + TIFRII_standardized + 
                                                              LVESV_standardized + CRP_standardized + BNP_standardized,
                                                            SpinaleAllImportantVariables_5FOLDCV_3,
                                                            family = "binomial",
                                                            method = "glm",
                                                            trControl = ctrl_5_fold_CV,
                                                            metric = "accuracy")

logisticRegression_allImportantVariables_results_5FoldCV_3 <- as.data.frame(logisticRegression_allImportantVariables_5FoldCV_3$results)

logisticRegression_allImportantVariables_maxAccuracies_5FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_5FoldCV,
                                                                        logisticRegression_allImportantVariables_results_5FoldCV_3)

#K-NEAREST NEIGHBORS
KNN_allImportantVariables_5FOLDCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                               EjectionFraction_standardized + TIFRII_standardized + 
                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                             SpinaleAllImportantVariables_5FOLDCV_3,
                                             method = "knn",
                                             trControl = ctrl_5_fold_CV,
                                             tuneLength = 20,
                                             metric = "Accuracy")

KNN_allImportantVariables_results_5FoldCV_3 <- as.data.frame(KNN_allImportantVariables_5FOLDCV_3$results)

KNN_allImportantVariables_maxAccuracy_5FoldCV_3 <- KNN_allImportantVariables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_5FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_5FoldCV,
                                       KNN_allImportantVariables_maxAccuracy_5FoldCV_3)

#LINEAR SVM
LinearSVM_allImportantVariables_5FOLDCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                     EjectionFraction_standardized + TIFRII_standardized + 
                                                     LVESV_standardized + CRP_standardized + BNP_standardized,
                                                   SpinaleAllImportantVariables_5FOLDCV_3,
                                                   method = "svmLinear",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneGrid = grid,
                                                   metric = "Accuracy")

LinearSVM_allImportantVariables_results_5FoldCV_3 <- as.data.frame(LinearSVM_allImportantVariables_5FOLDCV_3$results)

LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_3 <- LinearSVM_allImportantVariables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                                               LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_3)

#RADIAL SVM
RadialSVM_allImportantVariables_5FoldCV_3 <- train(response~Stretch_standardized + ST2_standardized +
                                                     EjectionFraction_standardized + TIFRII_standardized + 
                                                     LVESV_standardized + CRP_standardized + BNP_standardized,
                                                   SpinaleAllImportantVariables_5FOLDCV_3,
                                                   method = "svmRadial",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneGrid = grid_radial,
                                                   metric = "Accuracy")

RadialSVM_allImportantVariables_results_5FoldCV_3 <- as.data.frame(RadialSVM_allImportantVariables_5FoldCV_3$results)

RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_3 <- RadialSVM_allImportantVariables_results_5FoldCV_3 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                                               RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_3)
#Fourth 5-Fold CV
SpinaleAllImportantVariables_5FOLDCV_4<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_5FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                        EjectionFraction_standardized + TIFRII_standardized + 
                                                        LVESV_standardized + CRP_standardized + BNP_standardized,
                                                      SpinaleAllImportantVariables_5FOLDCV_4,
                                                      method = "rf",
                                                      trControl = ctrl_5_fold_CV,
                                                      tuneLength = 21,
                                                      ntrees = 1000,
                                                      metric = "Accuracy")

randomForest_allImportantVariables_results_5FoldCV_4 <- as.data.frame(randomForest_AllImportantVariables_5FoldCV_4$results)

randomForest_allImportantVariables_maxAccuracy_5FoldCV_4 <- randomForest_allImportantVariables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_5FoldCV,
                                                                  randomForest_allImportantVariables_maxAccuracy_5FoldCV_4)

#LOGISTIC REGRESSION
logisticRegression_allImportantVariables_5FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                              EjectionFraction_standardized + TIFRII_standardized + 
                                                              LVESV_standardized + CRP_standardized + BNP_standardized,
                                                            SpinaleAllImportantVariables_5FOLDCV_4,
                                                            family = "binomial",
                                                            method = "glm",
                                                            trControl = ctrl_5_fold_CV,
                                                            metric = "accuracy")

logisticRegression_allImportantVariables_results_5FoldCV_4 <- as.data.frame(logisticRegression_allImportantVariables_5FoldCV_4$results)

logisticRegression_allImportantVariables_maxAccuracies_5FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_5FoldCV,
                                                                        logisticRegression_allImportantVariables_results_5FoldCV_4)

#K-NEAREST NEIGHBORS
KNN_allImportantVariables_5FOLDCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                               EjectionFraction_standardized + TIFRII_standardized + 
                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                             SpinaleAllImportantVariables_5FOLDCV_4,
                                             method = "knn",
                                             trControl = ctrl_5_fold_CV,
                                             tuneLength = 20,
                                             metric = "Accuracy")

KNN_allImportantVariables_results_5FoldCV_4 <- as.data.frame(KNN_allImportantVariables_5FOLDCV_4$results)

KNN_allImportantVariables_maxAccuracy_5FoldCV_4 <- KNN_allImportantVariables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_5FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_5FoldCV,
                                       KNN_allImportantVariables_maxAccuracy_5FoldCV_4)

#LINEAR SVM
LinearSVM_allImportantVariables_5FOLDCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                     EjectionFraction_standardized + TIFRII_standardized + 
                                                     LVESV_standardized + CRP_standardized + BNP_standardized,
                                                   SpinaleAllImportantVariables_5FOLDCV_4,
                                                   method = "svmLinear",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneGrid = grid,
                                                   metric = "Accuracy")

LinearSVM_allImportantVariables_results_5FoldCV_4 <- as.data.frame(LinearSVM_allImportantVariables_5FOLDCV_4$results)

LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_4 <- LinearSVM_allImportantVariables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                                               LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_4)

#RADIAL SVM
RadialSVM_allImportantVariables_5FoldCV_4 <- train(response~Stretch_standardized + ST2_standardized +
                                                     EjectionFraction_standardized + TIFRII_standardized + 
                                                     LVESV_standardized + CRP_standardized + BNP_standardized,
                                                   SpinaleAllImportantVariables_5FOLDCV_4,
                                                   method = "svmRadial",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneGrid = grid_radial,
                                                   metric = "Accuracy")

RadialSVM_allImportantVariables_results_5FoldCV_4 <- as.data.frame(RadialSVM_allImportantVariables_5FoldCV_4$results)

RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_4 <- RadialSVM_allImportantVariables_results_5FoldCV_4 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                                               RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_4)
#Fifth 5-Fold CV
SpinaleAllImportantVariables_5FOLDCV_5<- Spinale_both_training[sample(nrow(Spinale_both_training)),]

#RANDOM FOREST
randomForest_AllImportantVariables_5FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                        EjectionFraction_standardized + TIFRII_standardized + 
                                                        LVESV_standardized + CRP_standardized + BNP_standardized,
                                                      SpinaleAllImportantVariables_5FOLDCV_5,
                                                      method = "rf",
                                                      trControl = ctrl_5_fold_CV,
                                                      tuneLength = 21,
                                                      ntrees = 1000,
                                                      metric = "Accuracy")

randomForest_allImportantVariables_results_5FoldCV_5 <- as.data.frame(randomForest_AllImportantVariables_5FoldCV_5$results)

randomForest_allImportantVariables_maxAccuracy_5FoldCV_5 <- randomForest_allImportantVariables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

randomForest_allImportantVariables_maxAccuracies_5FoldCV <- rbind(randomForest_allImportantVariables_maxAccuracies_5FoldCV,
                                                                  randomForest_allImportantVariables_maxAccuracy_5FoldCV_5)

#LOGISTIC REGRESSION
logisticRegression_allImportantVariables_5FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                              EjectionFraction_standardized + TIFRII_standardized + 
                                                              LVESV_standardized + CRP_standardized + BNP_standardized,
                                                            SpinaleAllImportantVariables_5FOLDCV_5,
                                                            family = "binomial",
                                                            method = "glm",
                                                            trControl = ctrl_5_fold_CV,
                                                            metric = "accuracy")

logisticRegression_allImportantVariables_results_5FoldCV_5 <- as.data.frame(logisticRegression_allImportantVariables_5FoldCV_5$results)

logisticRegression_allImportantVariables_maxAccuracies_5FoldCV <- rbind(logisticRegression_allImportantVariables_maxAccuracies_5FoldCV,
                                                                        logisticRegression_allImportantVariables_results_5FoldCV_5)

#K-NEAREST NEIGHBORS
KNN_allImportantVariables_5FOLDCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                               EjectionFraction_standardized + TIFRII_standardized + 
                                               LVESV_standardized + CRP_standardized + BNP_standardized,
                                             SpinaleAllImportantVariables_5FOLDCV_5,
                                             method = "knn",
                                             trControl = ctrl_5_fold_CV,
                                             tuneLength = 20,
                                             metric = "Accuracy")

KNN_allImportantVariables_results_5FoldCV_5 <- as.data.frame(KNN_allImportantVariables_5FOLDCV_5$results)

KNN_allImportantVariables_maxAccuracy_5FoldCV_5 <- KNN_allImportantVariables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

KNN_allImportantVariables_maxAccuracies_5FoldCV <- rbind(KNN_allImportantVariables_maxAccuracies_5FoldCV,
                                       KNN_allImportantVariables_maxAccuracy_5FoldCV_5)

#LINEAR SVM
LinearSVM_allImportantVariables_5FOLDCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                     EjectionFraction_standardized + TIFRII_standardized + 
                                                     LVESV_standardized + CRP_standardized + BNP_standardized,
                                                   SpinaleAllImportantVariables_5FOLDCV_5,
                                                   method = "svmLinear",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneGrid = grid,
                                                   metric = "Accuracy")

LinearSVM_allImportantVariables_results_5FoldCV_5 <- as.data.frame(LinearSVM_allImportantVariables_5FOLDCV_5$results)

LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_5 <- LinearSVM_allImportantVariables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

LinearSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(LinearSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                                               LinearSVM_allImportantVariables_maxAccuracy_5FoldCV_5)

#RADIAL SVM
RadialSVM_allImportantVariables_5FoldCV_5 <- train(response~Stretch_standardized + ST2_standardized +
                                                     EjectionFraction_standardized + TIFRII_standardized + 
                                                     LVESV_standardized + CRP_standardized + BNP_standardized,
                                                   SpinaleAllImportantVariables_5FOLDCV_5,
                                                   method = "svmRadial",
                                                   trControl = ctrl_5_fold_CV,
                                                   tuneGrid = grid_radial,
                                                   metric = "Accuracy")

RadialSVM_allImportantVariables_results_5FoldCV_5 <- as.data.frame(RadialSVM_allImportantVariables_5FoldCV_5$results)

RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_5 <- RadialSVM_allImportantVariables_results_5FoldCV_5 %>%
  filter(Accuracy == max(Accuracy))

RadialSVM_allImportantVariables_maxAccuracies_5FoldCV <- rbind(RadialSVM_allImportantVariables_maxAccuracies_5FoldCV, 
                                                               RadialSVM_allImportantVariables_maxAccuracy_5FoldCV_5)
