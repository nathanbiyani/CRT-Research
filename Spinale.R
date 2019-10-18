library("readxl")
library("ggplot2")
library("dplyr")
library(caTools)
library(caret)
library(glmnet)
library(data.table)
library(doSNOW)

Spinale <- read_excel("SpinaleDataSet.xlsx")


SpinaleResponses <- Spinale[!is.na(Spinale$response),]

S <- SpinaleResponses %>%
  filter(time == 0)

S$response <- as.factor(S$response)

ggplot(S, aes(x = response, y = lvesv_delta)) +
  geom_point()



SpinaleProteinsAndResponses <- S %>% 
  select(mmp2, mmp9, st2, crp, bnp, timp1, timp2, tim4, spg130, sil2ra, tnfrii, ifng, response)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>%
  filter(mmp2 > 0)

colSums(is.na(SpinaleProteinsAndResponses))


#standardizing MMP2 and checking its distribution/relationship with 
#target variable



boxplot(SpinaleProteinsAndResponses$mmp2)


n <- length(SpinaleProteinsAndResponses$mmp2)
sort(SpinaleProteinsAndResponses$mmp2,partial=n)[n]
which(SpinaleProteinsAndResponses$mmp2 == 4881676)

sort(SpinaleProteinsAndResponses$mmp2,partial=n-1)[n-1]
which(SpinaleProteinsAndResponses$mmp2 > 3336170)

sort(SpinaleProteinsAndResponses$mmp2,partial=n-2)[n-2]
which(SpinaleProteinsAndResponses$mmp2 >= 2448172)

SpinaleProteinsAndResponses$mmp2[c(38, 181)] <- 2448172


meanMMP2 <- mean(SpinaleProteinsAndResponses$mmp2)
sdMMP2 <- sd(SpinaleProteinsAndResponses$mmp2)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
    mutate(MMP2_standardized = (mmp2 - meanMMP2)/sdMMP2)

ggplot(SpinaleProteinsAndResponses, aes(x = MMP2_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = MMP2_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$MMP2_standardized)





#standardizing MMP9 and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$mmp9)
sort(SpinaleProteinsAndResponses$mmp9,partial=n)[n]
which(SpinaleProteinsAndResponses$mmp9 == 1979866)

sort(SpinaleProteinsAndResponses$mmp9,partial=n - 1)[n - 1]

SpinaleProteinsAndResponses$mmp9[181] <- 990026



meanMMP9 <- mean(SpinaleProteinsAndResponses$mmp9)
sdMMP9 <- sd(SpinaleProteinsAndResponses$mmp9)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(MMP9_standardized = (mmp9 - meanMMP9)/sdMMP9)

ggplot(SpinaleProteinsAndResponses, aes(x = MMP9_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = mmp9)) +
  geom_()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = mmp9)) +
  geom_boxplot() 

boxplot(SpinaleProteinsAndResponses$MMP9_standardized)


#standardizing ST2 and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$st2)




meanST2 <- mean(SpinaleProteinsAndResponses$st2)
sdST2 <- sd(SpinaleProteinsAndResponses$st2)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(ST2_standardized = (st2 - meanST2)/sdST2)

ggplot(SpinaleProteinsAndResponses, aes(x = ST2_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = ST2_standardized)) +
  geom_point() 



#standardizing CRP and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$crp)



meanCRP <- mean(SpinaleProteinsAndResponses$crp)
sdCRP <- sd(SpinaleProteinsAndResponses$crp)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(CRP_standardized = (crp - meanCRP)/sdCRP)

ggplot(SpinaleProteinsAndResponses, aes(x = CRP_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = CRP_standardized)) +
  geom_point() 



#standardizing bnp and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$bnp)



meanBNP <- mean(SpinaleProteinsAndResponses$bnp)
sdBNP <- sd(SpinaleProteinsAndResponses$bnp)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(BNP_standardized = (bnp - meanBNP)/sdBNP)

ggplot(SpinaleProteinsAndResponses, aes(x = BNP_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = BNP_standardized)) +
  geom_point() 



#standardizing timp1 and checking its distribution/relationship with 
#target variable


boxplot(SpinaleProteinsAndResponses$timp1)
sort(SpinaleProteinsAndResponses$timp1,partial=n)[n]
which(SpinaleProteinsAndResponses$timp1 > 527820)

sort(SpinaleProteinsAndResponses$timp1,partial=n - 1)[n - 1]

SpinaleProteinsAndResponses$timp1[711] <- 406871



meanTIMP1 <- mean(SpinaleProteinsAndResponses$timp1)
sdTIMP1 <- sd(SpinaleProteinsAndResponses$timp1)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(TIMP1_standardized = (timp1 - meanTIMP1)/sdTIMP1)

ggplot(SpinaleProteinsAndResponses, aes(x = TIMP1_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = TIMP1_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$TIMP1_standardized)


#standardizing timp2 and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$timp2)
sort(SpinaleProteinsAndResponses$timp2,partial=n)[n]
sort(SpinaleProteinsAndResponses$timp2,partial=n-1)[n-1]
sort(SpinaleProteinsAndResponses$timp2,partial=n-2)[n-2]
sort(SpinaleProteinsAndResponses$timp2,partial=n-3)[n-3]

which(SpinaleProteinsAndResponses$timp2 > 238780)

SpinaleProteinsAndResponses$timp2[c(12,21,29)] <- 202051




meanTIMP2 <- mean(SpinaleProteinsAndResponses$timp2)
sdTIMP2 <- sd(SpinaleProteinsAndResponses$timp2)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(TIMP2_standardized = (timp2 - meanTIMP2)/sdTIMP2)

ggplot(SpinaleProteinsAndResponses, aes(x = TIMP2_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = TIMP2_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$TIMP2_standardized)


#standardizing tim4 and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$tim4)

sort(SpinaleProteinsAndResponses$tim4,partial=n)[n]
sort(SpinaleProteinsAndResponses$tim4,partial=n-1)[n-1]
sort(SpinaleProteinsAndResponses$tim4,partial=n-2)[n-2]



which(SpinaleProteinsAndResponses$tim4 > 10390)

SpinaleProteinsAndResponses$tim4[c(24, 545)] <- 9008


meanTIM4 <- mean(SpinaleProteinsAndResponses$tim4)
sdTIMP4 <- sd(SpinaleProteinsAndResponses$tim4)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(TIM4_standardized = (tim4 - meanTIM4)/sdTIMP4)

ggplot(SpinaleProteinsAndResponses, aes(x = TIM4_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = TIM4_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$TIM4_standardized)


#standardizing spg130 and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$spg130)
sort(SpinaleProteinsAndResponses$spg130,partial=n)[n]
sort(SpinaleProteinsAndResponses$spg130,partial=n-1)[n-1]
sort(SpinaleProteinsAndResponses$spg130,partial=n-2)[n-2]
sort(SpinaleProteinsAndResponses$spg130,partial=n-3)[n-3]

which(SpinaleProteinsAndResponses$spg130 > 436570)

SpinaleProteinsAndResponses$spg130[c(24, 545)] <- 389689.6


meanSPG130 <- mean(SpinaleProteinsAndResponses$spg130)
sdSPG130 <- sd(SpinaleProteinsAndResponses$spg130)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(SPG130_standardized = (spg130 - meanSPG130)/sdSPG130)

ggplot(SpinaleProteinsAndResponses, aes(x = SPG130_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = SPG130_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$SPG130_standardized)


#standardizing sil2ra and checking its distribution/relationship with 
#target variable


boxplot(SpinaleProteinsAndResponses$sil2ra)
sort(SpinaleProteinsAndResponses$sil2ra,partial=n)[n]
sort(SpinaleProteinsAndResponses$sil2ra,partial=n-1)[n-1]
sort(SpinaleProteinsAndResponses$sil2ra,partial=n-2)[n-2]
sort(SpinaleProteinsAndResponses$sil2ra,partial=n-3)[n-3]
sort(SpinaleProteinsAndResponses$sil2ra,partial=n-4)[n-4]
sort(SpinaleProteinsAndResponses$sil2ra,partial=n-5)[n-5]

which(SpinaleProteinsAndResponses$sil2ra > 4000)

SpinaleProteinsAndResponses$sil2ra[c(21, 29, 44, 257, 304)] <- 3397


meanSIL2RA <- mean(SpinaleProteinsAndResponses$sil2ra)
sdSIL2RA <- sd(SpinaleProteinsAndResponses$sil2ra)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(SIL2RA_standardized = (sil2ra - meanSIL2RA)/sdSIL2RA)

ggplot(SpinaleProteinsAndResponses, aes(x = SIL2RA_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = SIL2RA_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$SIL2RA_standardized)


#standardizing tnfrii and checking its distribution/relationship with 
#target variable

boxplot(SpinaleProteinsAndResponses$tnfrii)
sort(SpinaleProteinsAndResponses$tnfrii,partial=n)[n]
sort(SpinaleProteinsAndResponses$tnfrii,partial=n-1)[n-1]
sort(SpinaleProteinsAndResponses$tnfrii,partial=n-2)[n-2]
sort(SpinaleProteinsAndResponses$tnfrii,partial=n-3)[n-3]
sort(SpinaleProteinsAndResponses$tnfrii,partial=n-4)[n-4]




which(SpinaleProteinsAndResponses$tnfrii == 57589)

SpinaleProteinsAndResponses$tnfrii[c(19, 21, 212, 472)] <- 40660

meanTNFRII <- mean(SpinaleProteinsAndResponses$tnfrii)
sdTNFRII <- sd(SpinaleProteinsAndResponses$tnfrii)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(TIFRII_standardized = (tnfrii - meanTNFRII)/sdTNFRII)

ggplot(SpinaleProteinsAndResponses, aes(x = TIFRII_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = TIFRII_standardized)) +
  geom_point() 

boxplot(SpinaleProteinsAndResponses$TIFRII_standardized)


#standardizing ifng and checking its distribution/relationship with 
#target variable


boxplot(SpinaleProteinsAndResponses$ifng)
sort(SpinaleProteinsAndResponses$ifng,partial=n)[n]
sort(SpinaleProteinsAndResponses$ifng,partial=n-1)[n-1]
sort(SpinaleProteinsAndResponses$ifng,partial=n-2)[n-2]
sort(SpinaleProteinsAndResponses$ifng,partial=n-3)[n-3]
sort(SpinaleProteinsAndResponses$ifng,partial=n-4)[n-4]
sort(SpinaleProteinsAndResponses$ifng,partial=n-5)[n-5]
sort(SpinaleProteinsAndResponses$ifng,partial=n-6)[n-6]



which(SpinaleProteinsAndResponses$ifng > 24)

SpinaleProteinsAndResponses$ifng[c(102, 145, 157, 409, 439, 650, 651)] <- 24.97271

meanIFNG <- mean(SpinaleProteinsAndResponses$ifng)
sdIFNG <- sd(SpinaleProteinsAndResponses$ifng)



SpinaleProteinsAndResponses <- SpinaleProteinsAndResponses %>% 
  mutate(IFNG_standardized = (ifng - meanIFNG)/sdIFNG)

ggplot(SpinaleProteinsAndResponses, aes(x = IFNG_standardized)) +
  geom_histogram()

ggplot(SpinaleProteinsAndResponses, aes(x = response, y = IFNG_standardized)) +
  geom_point() 


boxplot(SpinaleProteinsAndResponses$IFNG_standardized)

#------------------------------------------------------------------------------


#standardizing diastolic blood pressure and checking its 
#distribution/relationship with target variable

SpinaleClinicalMarkersAndResponse <- S %>% 
  select(bpdia, bpsys, LVEDV, LVESV, response)


n1 <- length(SpinaleClinicalMarkersAndResponse$bpdia)

sort(SpinaleClinicalMarkersAndResponse$bpdia,partial=n1)[n1]
sort(SpinaleClinicalMarkersAndResponse$bpdia,partial=n1-1)[n1-1]

which(SpinaleClinicalMarkersAndResponse$bpdia > 150)

SpinaleClinicalMarkersAndResponse$bpdia[c(220)] <- 119

boxplot(SpinaleClinicalMarkersAndResponse$bpdia)



meanDiastolic <- mean(SpinaleClinicalMarkersAndResponse$bpdia)
sdDiastolic <- sd(SpinaleClinicalMarkersAndResponse$bpdia)



SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(standardized_dystolicBloodPressure = (bpdia - meanDiastolic)/sdDiastolic)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = standardized_dystolicBloodPressure)) +
  geom_point() 


#standardizing diastolic blood pressure and checking its 
#distribution/relationship with target variable

boxplot(SpinaleClinicalMarkersAndResponse$bpsys)


meanSystolic <- mean(SpinaleClinicalMarkersAndResponse$bpsys)
sdSystolic <- sd(SpinaleClinicalMarkersAndResponse$bpsys)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(standardized_systolicBloodPressure = (bpsys - meanSystolic)/sdSystolic)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = standardized_systolicBloodPressure)) +
  geom_point() 

#standardizing LVESV and checking its 
#distribution/relationship with target variable

boxplot(SpinaleClinicalMarkersAndResponse$LVESV)

sort(SpinaleClinicalMarkersAndResponse$LVESV,partial=n1)[n1]
sort(SpinaleClinicalMarkersAndResponse$LVESV,partial=n1-1)[n1-1]

which(SpinaleClinicalMarkersAndResponse$LVESV > 500)
SpinaleClinicalMarkersAndResponse$LVESV[c(767)] <- 447

meanLVESV <- mean(SpinaleClinicalMarkersAndResponse$LVESV)
sdLVESV <- sd(SpinaleClinicalMarkersAndResponse$LVESV)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(LVESV_standardized = (LVESV - meanLVESV)/sdLVESV)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = LVESV_standardized)) +
  geom_point() 

#standardizing LVEDV and checking its 
#distribution/relationship with target variable

boxplot(SpinaleClinicalMarkersAndResponse$LVEDV)

sort(SpinaleClinicalMarkersAndResponse$LVEDV,partial=n1)[n1]
sort(SpinaleClinicalMarkersAndResponse$LVEDV,partial=n1-1)[n1-1]

which(SpinaleClinicalMarkersAndResponse$LVEDV > 600)
SpinaleClinicalMarkersAndResponse$LVEDV[c(767)] <- 513

meanLVEDV <- mean(SpinaleClinicalMarkersAndResponse$LVEDV)
sdLVEDV <- sd(SpinaleClinicalMarkersAndResponse$LVEDV)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(LVEDV_standardized = (LVEDV - meanLVEDV)/sdLVEDV)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = LVEDV_standardized)) +
  geom_point() 

#creating new variables
SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(PulsePressure = bpsys - bpdia)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = PulsePressure)) +
  geom_point() 

meanPulsePressure <- mean(SpinaleClinicalMarkersAndResponse$PulsePressure)
sdPulsePressure <- sd(SpinaleClinicalMarkersAndResponse$PulsePressure)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(pulsePressure_standardized = (PulsePressure - meanPulsePressure)/sdPulsePressure)


SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(StrokeVolume = LVEDV - LVESV)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = StrokeVolume)) +
  geom_point() 

meanStrokeVolume <- mean(SpinaleClinicalMarkersAndResponse$StrokeVolume)
sdStrokeVolume <- sd(SpinaleClinicalMarkersAndResponse$StrokeVolume)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(StrokeVolume_standardized = (StrokeVolume - meanStrokeVolume)/sdStrokeVolume)


SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(EjectionFraction = StrokeVolume - LVEDV)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = EjectionFraction)) +
  geom_point() 

meanEjectionFraction <- mean(SpinaleClinicalMarkersAndResponse$EjectionFraction)
sdEjectionFraction <- sd(SpinaleClinicalMarkersAndResponse$EjectionFraction)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(EjectionFraction_standardized = (EjectionFraction - meanEjectionFraction)/sdEjectionFraction)


SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(Stretch = LVEDV / LVESV)

ggplot(SpinaleClinicalMarkersAndResponse, aes(x = response, y = Stretch)) +
  geom_point()

meanStretch <- mean(SpinaleClinicalMarkersAndResponse$Stretch)
sdStretch <- sd(SpinaleClinicalMarkersAndResponse$Stretch)

SpinaleClinicalMarkersAndResponse <- SpinaleClinicalMarkersAndResponse %>% 
  mutate(Stretch_standardized = (Stretch - meanStretch)/sdStretch)


SpinaleClinicalMarkersAndResponse1 <- SpinaleClinicalMarkersAndResponse %>% 
  select(standardized_dystolicBloodPressure, standardized_systolicBloodPressure, 
         LVESV_standardized, LVEDV_standardized, pulsePressure_standardized, 
         StrokeVolume_standardized, Stretch_standardized, EjectionFraction_standardized,
         response)

indexes <- createDataPartition(SpinaleClinicalMarkersAndResponse1$response,
                               times = 1,
                               p = 0.8,
                               list = FALSE)

SpinaleClinicalMarkersAndResponses_training <- SpinaleClinicalMarkersAndResponse1[indexes,]
SpinaleClinicalMarkersAndResponses_test <- SpinaleClinicalMarkersAndResponse1[-indexes,]




ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     verboseIter = TRUE)

rf <- train(response ~ Stretch_standardized + pulsePressure_standardized + 
              standardized_dystolicBloodPressure + standardized_systolicBloodPressure,
            SpinaleClinicalMarkersAndResponses_training,
                       method = "rf",
                       trControl = ctrl,
                       tuneLength = 9,
                       metric = "Accuracy")



plot(varImp(rf, scale = TRUE))
varImp(rf)

preds_rf <- predict(rf, SpinaleClinicalMarkersAndResponses_test)
results_rf <- confusionMatrix(preds_rf, SpinaleClinicalMarkersAndResponses_test$response)
results_rf1 <- as.matrix(results_rf)
results_rf2 <- as.matrix(results_rf, what = "overall")
results_rf3 <- as.matrix(results_rf, what = "classes")

logistic_regression <- train(response ~ Stretch_standardized + pulsePressure_standardized + 
                               standardized_dystolicBloodPressure + standardized_systolicBloodPressure,
                                                     data = SpinaleClinicalMarkersAndResponses_training,
                                                     family = "binomial",
                                                     method = "glm",
                                                     trControl = ctrl,
                                                     metric = "Accuracy")

logistic_regression

preds_logistic_regression <- predict(logistic_regression, SpinaleClinicalMarkersAndResponses_test)
results_logistic_regression <- confusionMatrix(preds_logistic_regression, SpinaleClinicalMarkersAndResponses_test$response)
results_logistic_regression1 <- as.matrix(results_logistic_regression)
results_logistic_regression2 <- as.matrix(results_logistic_regression, what = "overall")
results_logistic_regression3 <- as.matrix(results_logistic_regression, what = "classes")






#------------------------------------------------------------------------------

most_important_variables <- data.frame(fromWhichModel = "a",
                  importantVariable1="a",
                 importantVariable2="a",
                 importantVariable3="a",
                 importantVariable4 = "a",
                 importantVariable5 = "a")


results_for_each_ML_algorithm <- data.frame(nameOfRun = "a",
                                            variable1 = "a",
                                            variable2 = "a",
                                            variable3 = "a", 
                                            variable4 = "a",
                                            variable5 = "a",
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
                                            



SpinaleProteinsAndResponses2 <- SpinaleProteinsAndResponses %>% 
  select(MMP2_standardized, MMP9_standardized, ST2_standardized, CRP_standardized, BNP_standardized, TIMP1_standardized, TIMP2_standardized, TIM4_standardized, SPG130_standardized, SIL2RA_standardized, TIFRII_standardized, IFNG_standardized, response)


cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)
for (i in 1:30)
  {
SpinaleProteinsAndResponses3 <- SpinaleProteinsAndResponses2[sample(nrow(SpinaleProteinsAndResponses2)),]

indexes <- createDataPartition(SpinaleProteinsAndResponses3$response,
                               times = 1,
                               p = 0.8,
                               list = FALSE)

SpinaleProteinsAndResponses_training <- SpinaleProteinsAndResponses3[indexes,]
SpinaleProteinsAndResponses_test <- SpinaleProteinsAndResponses3[-indexes,]




ctrl <- trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 3,
                    verboseIter = TRUE)

#ridge regression

ridge_variables <- train(response ~.,
                  SpinaleProteinsAndResponses_training,
                  family = "binomial",
                  method = "glmnet",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = seq(0.000001, 1, length = 10)),
                  trControl = ctrl)


plot(ridge_variables)

ridge_variables
plot(ridge_variables$finalModel, xvar = "lambda", label = TRUE)
plot(ridge_variables$finalModel, xvar = "dev", label = TRUE)
plot(varImp(ridge_variables, scale = TRUE))

imps_ridge <- as.data.frame(varImp(ridge_variables)$importance)


variable1_ridge <- rownames(imps_ridge)[order(imps_ridge$Overall, decreasing=TRUE)][1]
variable2_ridge <- rownames(imps_ridge)[order(imps_ridge$Overall, decreasing=TRUE)][2]
variable3_ridge <- rownames(imps_ridge)[order(imps_ridge$Overall, decreasing=TRUE)][3]
variable4_ridge <- rownames(imps_ridge)[order(imps_ridge$Overall, decreasing=TRUE)][4]
variable5_ridge <- rownames(imps_ridge)[order(imps_ridge$Overall, decreasing=TRUE)][5]

df_ridge_names <- data.frame(fromWhichModel = "Ridge Regression Variable Selection",
                             importantVariable1 = variable1_ridge,
                             importantVariable2 = variable2_ridge,
                             importantVariable3 = variable3_ridge,
                             importantVariable4 = variable4_ridge,
                             importantVariable5 = variable5_ridge)


most_important_variables <- rbind(most_important_variables, df_ridge_names)


important_variable_for_Ridge_Regression1 <- SpinaleProteinsAndResponses_training[, variable1_ridge]
important_variable_for_Ridge_Regression2 <- SpinaleProteinsAndResponses_training[, variable2_ridge]
important_variable_for_Ridge_Regression3 <- SpinaleProteinsAndResponses_training[, variable3_ridge]
important_variable_for_Ridge_Regression4 <- SpinaleProteinsAndResponses_training[, variable4_ridge]
important_variable_for_Ridge_Regression5 <- SpinaleProteinsAndResponses_training[, variable5_ridge]


df_of_important_variables_and_their_columns_ridge <- data.frame(importantVariableRidge1 = important_variable_for_Ridge_Regression1,
                                                importantVariableRidge2 = important_variable_for_Ridge_Regression2,
                                                importantVariableRidge3 = important_variable_for_Ridge_Regression3,
                                                importantVariableRidge4 = important_variable_for_Ridge_Regression4,
                                               importantVariableRidge5 = important_variable_for_Ridge_Regression5)

df_of_important_variables_and_their_columns_ridge$response <- SpinaleProteinsAndResponses_training$response

#lasso regression

lasso_variables <- train(response ~.,
                        SpinaleProteinsAndResponses_training,
                        family = "binomial",
                        method = "glmnet",
                        tuneGrid = expand.grid(alpha = 1,
                                               lambda = seq(0.0000001, 0.2, length = 20)),
                        trControl = ctrl)

plot(lasso_variables)
lasso_variables
plot(lasso_variables$finalModel, xvar = "lambda", label = TRUE)
plot(lasso_variables$finalModel, xvar = "dev", label = TRUE)
plot(varImp(lasso_variables, scale = TRUE))

imps_lasso <- as.data.frame(varImp(lasso_variables)$importance)


variable1_lasso <- rownames(imps_lasso)[order(imps_lasso$Overall, decreasing=TRUE)][1]
variable2_lasso <- rownames(imps_lasso)[order(imps_lasso$Overall, decreasing=TRUE)][2]
variable3_lasso <- rownames(imps_lasso)[order(imps_lasso$Overall, decreasing=TRUE)][3]
variable4_lasso <- rownames(imps_lasso)[order(imps_lasso$Overall, decreasing=TRUE)][4]
variable5_lasso <- rownames(imps_lasso)[order(imps_lasso$Overall, decreasing=TRUE)][5]

df_lasso_names <- data.frame(fromWhichModel = "Lasso Regression Variable Selection",
                             importantVariable1 = variable1_lasso,
                             importantVariable2 = variable2_lasso,
                             importantVariable3 = variable3_lasso,
                             importantVariable4 = variable4_lasso,
                             importantVariable5 = variable5_lasso)


most_important_variables <- rbind(most_important_variables, df_lasso_names)



important_variable_for_Lasso_Regression1 <- SpinaleProteinsAndResponses_training[, variable1_lasso]
important_variable_for_Lasso_Regression2 <- SpinaleProteinsAndResponses_training[, variable2_lasso]
important_variable_for_Lasso_Regression3 <- SpinaleProteinsAndResponses_training[, variable3_lasso]
important_variable_for_Lasso_Regression4 <- SpinaleProteinsAndResponses_training[, variable4_lasso]
important_variable_for_Lasso_Regression5 <- SpinaleProteinsAndResponses_training[, variable5_lasso]


df_of_important_variables_and_their_columns_lasso <- data.frame(importantVariableLasso1=important_variable_for_Lasso_Regression1,
                                                                importantVariableLasso2=important_variable_for_Lasso_Regression2,
                                                                importantVariableLasso3=important_variable_for_Lasso_Regression3,
                                                                importantVariableLasso4 = important_variable_for_Lasso_Regression4,
                                                                importantVariableLasso5 = important_variable_for_Lasso_Regression5)

df_of_important_variables_and_their_columns_lasso$response <- SpinaleProteinsAndResponses_training$response






#random forest predictors

rf_predictors <- train(response ~.,
                        SpinaleProteinsAndResponses_training,
                        method = "rf",
                        trControl = ctrl,
                        tuneLength = 12,
                        metric = "Accuracy")



plot(varImp(rf_predictors, scale = TRUE))
varImp(rf_predictors)
imps_rf <- as.data.frame(varImp(rf_predictors)$importance)


variable1_rf <- rownames(imps_rf)[order(imps_rf$Overall, decreasing=TRUE)][1]
variable2_rf <- rownames(imps_rf)[order(imps_rf$Overall, decreasing=TRUE)][2]
variable3_rf <- rownames(imps_rf)[order(imps_rf$Overall, decreasing=TRUE)][3]
variable4_rf <- rownames(imps_rf)[order(imps_rf$Overall, decreasing=TRUE)][4]
variable5_rf <- rownames(imps_rf)[order(imps_rf$Overall, decreasing=TRUE)][5]

df_rf_names <- data.frame(fromWhichModel = "Random Forest Variable Selection",
                          importantVariable1 = variable1_rf,
                          importantVariable2 = variable2_rf,
                          importantVariable3 = variable3_rf,
                          importantVariable4 = variable4_rf,
                          importantVariable5 = variable5_rf)


most_important_variables <- rbind(most_important_variables, df_rf_names)



important_variable_for_Random_Forest_1 <- SpinaleProteinsAndResponses_training[, variable1_rf]
important_variable_for_Random_Forest_2 <- SpinaleProteinsAndResponses_training[, variable2_rf]
important_variable_for_Random_Forest_3 <- SpinaleProteinsAndResponses_training[, variable3_rf]
important_variable_for_Random_Forest_4 <- SpinaleProteinsAndResponses_training[, variable4_rf]
important_variable_for_Random_Forest_5 <- SpinaleProteinsAndResponses_training[, variable5_rf]


df_of_important_variables_and_their_columns_random_forest <- data.frame(importantVariableRandomForest1=important_variable_for_Random_Forest_1,
                                                                importantVariableRandomForest2=important_variable_for_Random_Forest_2,
                                                                importantVariableRandomForest3=important_variable_for_Random_Forest_3,
                                                                importantVariableRandomForest4 = important_variable_for_Random_Forest_4,
                                                                importantVariableRandomForest5 = important_variable_for_Random_Forest_5)

df_of_important_variables_and_their_columns_random_forest$response <- SpinaleProteinsAndResponses_training$response






#random forests
#random forest for ridge regression

rf_ridge_variables <- train(response ~.,
              df_of_important_variables_and_their_columns_ridge,
                    method = "rf",
                    trControl = ctrl,
                    tuneLength = 5,
                    ntrees = 1000,
                    metric = "Accuracy")


preds_rf_ridge_variables <- predict(rf_ridge_variables, SpinaleProteinsAndResponses_test)
results_rf_ridge_variables <- confusionMatrix(preds_rf_ridge_variables, SpinaleProteinsAndResponses_test$response)
results_rf_ridge_1 <- as.matrix(results_rf_ridge_variables)
results_rf_ridge_2 <- as.matrix(results_rf_ridge_variables, what = "overall")
results_rf_ridge_3 <- as.matrix(results_rf_ridge_variables, what = "classes")

random_forest_ridge_variables_trues_falses <- as.data.frame(results_rf_ridge_1)

random_forest_ridge_variables_accuracy_numbers <- as.data.frame(results_rf_ridge_2)
random_forest_ridge_variables_accuracy_numbers = t(random_forest_ridge_variables_accuracy_numbers)
random_forest_ridge_variables_accuracy_numbers <- as.data.frame(random_forest_ridge_variables_accuracy_numbers)

random_forest_ridge_variables_precision_recall_F1 <- as.data.frame(results_rf_ridge_3)
random_forest_ridge_variables_precision_recall_F1 = t(random_forest_ridge_variables_precision_recall_F1)
random_forest_ridge_variables_precision_recall_F1 <- as.data.frame(random_forest_ridge_variables_precision_recall_F1)

big_rf_ridge = merge(random_forest_ridge_variables_accuracy_numbers,random_forest_ridge_variables_precision_recall_F1)
big_rf_ridge$nameOfRun <- "random forest with ridge predictors"
big_rf_ridge$variable1 <- variable1_ridge
big_rf_ridge$variable2 <- variable2_ridge
big_rf_ridge$variable3 <- variable3_ridge
big_rf_ridge$variable4 <- variable4_ridge
big_rf_ridge$variable5 <- variable5_ridge
big_rf_ridge <- big_rf_ridge %>%
  select(nameOfRun, everything())

big_rf_ridge$FalseNegative <- random_forest_ridge_variables_trues_falses[1,2]
big_rf_ridge$FalsePositive <- random_forest_ridge_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_rf_ridge)











#random forest for lasso regression

rf_lasso_variables <- train(response ~.,
                  df_of_important_variables_and_their_columns_lasso,
                    method = "rf",
                    trControl = ctrl,
                    tuneLength = 4,
                    ntrees = 1000,
                    metric = "Accuracy")


rf_lasso_variables

preds_rf_lasso_variables <- predict(rf_lasso_variables, SpinaleProteinsAndResponses_test)
results_rf_lasso_variables <- confusionMatrix(preds_rf_lasso_variables, SpinaleProteinsAndResponses_test$response)
results_rf_lasso_1 <- as.matrix(results_rf_lasso_variables)
results_rf_lasso_2 <- as.matrix(results_rf_lasso_variables, what = "overall")
results_rf_lasso_3 <- as.matrix(results_rf_lasso_variables, what = "classes")

random_forest_lasso_variables_trues_falses <- as.data.frame(results_rf_lasso_1)

random_forest_lasso_variables_accuracy_numbers <- as.data.frame(results_rf_lasso_2)
random_forest_lasso_variables_accuracy_numbers = t(random_forest_lasso_variables_accuracy_numbers)
random_forest_lasso_variables_accuracy_numbers <- as.data.frame(random_forest_lasso_variables_accuracy_numbers)

random_forest_lasso_variables_precision_recall_F1 <- as.data.frame(results_rf_lasso_3)
random_forest_lasso_variables_precision_recall_F1 = t(random_forest_lasso_variables_precision_recall_F1)
random_forest_lasso_variables_precision_recall_F1 <- as.data.frame(random_forest_lasso_variables_precision_recall_F1)

big_rf_lasso = merge(random_forest_lasso_variables_accuracy_numbers,random_forest_lasso_variables_precision_recall_F1)
big_rf_lasso$nameOfRun <- "random forest with lasso predictors"
big_rf_lasso$variable1 <- variable1_lasso
big_rf_lasso$variable2 <- variable2_lasso
big_rf_lasso$variable3 <- variable3_lasso
big_rf_lasso$variable4 <- variable4_lasso
big_rf_lasso$variable5 <- variable5_lasso
big_rf_lasso <- big_rf_lasso %>%
  select(nameOfRun, everything())

big_rf_lasso$FalseNegative <- random_forest_lasso_variables_trues_falses[1,2]
big_rf_lasso$FalsePositive <- random_forest_lasso_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_rf_lasso)





#random forest for random forest variables

rf_random_forest_variables <- train(response ~.,
                            df_of_important_variables_and_their_columns_random_forest,
                            method = "rf",
                            trControl = ctrl,
                            tuneLength = 4,
                            ntrees = 1000,
                            metric = "Accuracy")


rf_random_forest_variables

preds_rf_random_forest_variables <- predict(rf_random_forest_variables, SpinaleProteinsAndResponses_test)
results_rf_random_forest_variables <- confusionMatrix(preds_rf_random_forest_variables, SpinaleProteinsAndResponses_test$response)
results_rf_random_forest_1 <- as.matrix(results_rf_random_forest_variables)
results_rf_random_forest_2 <- as.matrix(results_rf_random_forest_variables, what = "overall")
results_rf_random_forest_3 <- as.matrix(results_rf_random_forest_variables, what = "classes")

random_forest_random_forest_variables_trues_falses <- as.data.frame(results_rf_random_forest_1)

random_forest_random_forest_variables_accuracy_numbers <- as.data.frame(results_rf_random_forest_2)
random_forest_random_forest_variables_accuracy_numbers = t(random_forest_random_forest_variables_accuracy_numbers)
random_forest_random_forest_variables_accuracy_numbers <- as.data.frame(random_forest_random_forest_variables_accuracy_numbers)

random_forest_random_forest_variables_precision_recall_F1 <- as.data.frame(results_rf_random_forest_3)
random_forest_random_forest_variables_precision_recall_F1 = t(random_forest_random_forest_variables_precision_recall_F1)
random_forest_random_forest_variables_precision_recall_F1 <- as.data.frame(random_forest_random_forest_variables_precision_recall_F1)

big_rf_random_forest = merge(random_forest_random_forest_variables_accuracy_numbers,random_forest_random_forest_variables_precision_recall_F1)
big_rf_random_forest$nameOfRun <- "random forest with random forest predictors"
big_rf_random_forest$variable1 <- variable1_rf
big_rf_random_forest$variable2 <- variable2_rf
big_rf_random_forest$variable3 <- variable3_rf
big_rf_random_forest$variable4 <- variable4_rf
big_rf_random_forest$variable5 <- variable5_rf
big_rf_random_forest <- big_rf_random_forest %>%
  select(nameOfRun, everything())

big_rf_random_forest$FalseNegative <- random_forest_random_forest_variables_trues_falses[1,2]
big_rf_random_forest$FalsePositive <- random_forest_random_forest_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_rf_random_forest)







#knn ridge regression
knn_ridge_variables <-  train(response ~.,
                  df_of_important_variables_and_their_columns_ridge,
              method = "knn",
              trControl = ctrl,
              tuneLength = 20,
              metric = "Accuracy")




preds_knn_ridge_variables <- predict(knn_ridge_variables, SpinaleProteinsAndResponses_test)
results_knn_ridge_variables <- confusionMatrix(preds_knn_ridge_variables, SpinaleProteinsAndResponses_test$response)
results_knn_ridge_1 <- as.matrix(results_knn_ridge_variables)
results_knn_ridge_2 <- as.matrix(results_knn_ridge_variables, what = "overall")
results_knn_ridge_3 <- as.matrix(results_knn_ridge_variables, what = "classes")

knn_ridge_variables_trues_falses <- as.data.frame(results_knn_ridge_1)

knn_ridge_variables_accuracy_numbers <- as.data.frame(results_knn_ridge_2)
knn_ridge_variables_accuracy_numbers = t(knn_ridge_variables_accuracy_numbers)
knn_ridge_variables_accuracy_numbers <- as.data.frame(knn_ridge_variables_accuracy_numbers)

knn_ridge_variables_precision_recall_F1 <- as.data.frame(results_knn_ridge_3)
knn_ridge_variables_precision_recall_F1 = t(knn_ridge_variables_precision_recall_F1)
knn_ridge_variables_precision_recall_F1 <- as.data.frame(knn_ridge_variables_precision_recall_F1)

big_knn_ridge = merge(knn_ridge_variables_accuracy_numbers,knn_ridge_variables_precision_recall_F1)
big_knn_ridge$nameOfRun <- "k-nearest-neighbors with ridge predictors"
big_knn_ridge$variable1 <- variable1_ridge
big_knn_ridge$variable2 <- variable2_ridge
big_knn_ridge$variable3 <- variable3_ridge
big_knn_ridge$variable4 <- variable4_ridge
big_knn_ridge$variable5 <- variable5_ridge
big_knn_ridge <- big_knn_ridge %>%
  select(nameOfRun, everything())

big_knn_ridge$FalseNegative <- random_forest_ridge_variables_trues_falses[1,2]
big_knn_ridge$FalsePositive <- random_forest_ridge_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_knn_ridge)










#knn lasso predictors
knn_lasso_variables <-  train(response ~.,
                df_of_important_variables_and_their_columns_lasso,
              method = "knn",
              trControl = ctrl,
              tuneLength = 20,
              metric = "Accuracy")


knn_lasso_variables


preds_knn_lasso_variables <- predict(knn_lasso_variables, SpinaleProteinsAndResponses_test)
results_knn_lasso_variables <- confusionMatrix(preds_knn_lasso_variables, SpinaleProteinsAndResponses_test$response)
results_knn_lasso_1 <- as.matrix(results_knn_lasso_variables)
results_knn_lasso_2 <- as.matrix(results_knn_lasso_variables, what = "overall")
results_knn_lasso_3 <- as.matrix(results_knn_lasso_variables, what = "classes")

knn_lasso_variables_trues_falses <- as.data.frame(results_knn_lasso_1)

knn_lasso_variables_accuracy_numbers <- as.data.frame(results_knn_lasso_2)
knn_lasso_variables_accuracy_numbers = t(knn_lasso_variables_accuracy_numbers)
knn_lasso_variables_accuracy_numbers <- as.data.frame(knn_lasso_variables_accuracy_numbers)

knn_lasso_variables_precision_recall_F1 <- as.data.frame(results_knn_lasso_3)
knn_lasso_variables_precision_recall_F1 = t(knn_lasso_variables_precision_recall_F1)
knn_lasso_variables_precision_recall_F1 <- as.data.frame(knn_lasso_variables_precision_recall_F1)

big_knn_lasso = merge(knn_lasso_variables_accuracy_numbers,knn_lasso_variables_precision_recall_F1)
big_knn_lasso$nameOfRun <- "k nearest neighbors with lasso predictors"
big_knn_lasso$variable1 <- variable1_lasso
big_knn_lasso$variable2 <- variable2_lasso
big_knn_lasso$variable3 <- variable3_lasso
big_knn_lasso$variable4 <- variable4_lasso
big_knn_lasso$variable5 <- variable5_lasso
big_knn_lasso <- big_knn_lasso %>%
  select(nameOfRun, everything())

big_knn_lasso$FalseNegative <- knn_lasso_variables_trues_falses[1,2]
big_knn_lasso$FalsePositive <- knn_lasso_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_knn_lasso)








#knn with random forest predictors
knn_random_forest_variables <-  train(response~.,
                              df_of_important_variables_and_their_columns_random_forest,
                              method = "knn",
                              trControl = ctrl,
                              tuneLength = 20,
                              metric = "Accuracy")

knn_random_forest_variables

preds_knn_random_forest_variables <- predict(knn_random_forest_variables, SpinaleProteinsAndResponses_test)
results_knn_random_forest_variables <- confusionMatrix(preds_knn_random_forest_variables, SpinaleProteinsAndResponses_test$response)
results_knn_random_forest_1 <- as.matrix(results_knn_random_forest_variables)
results_knn_random_forest_2 <- as.matrix(results_knn_random_forest_variables, what = "overall")
results_knn_random_forest_3 <- as.matrix(results_knn_random_forest_variables, what = "classes")

knn_random_forest_variables_trues_falses <- as.data.frame(results_knn_random_forest_1)

knn_random_forest_variables_accuracy_numbers <- as.data.frame(results_knn_random_forest_2)
knn_random_forest_variables_accuracy_numbers = t(knn_random_forest_variables_accuracy_numbers)
knn_random_forest_variables_accuracy_numbers <- as.data.frame(knn_random_forest_variables_accuracy_numbers)

knn_random_forest_variables_precision_recall_F1 <- as.data.frame(results_knn_random_forest_3)
knn_random_forest_variables_precision_recall_F1 = t(knn_random_forest_variables_precision_recall_F1)
knn_random_forest_variables_precision_recall_F1 <- as.data.frame(knn_random_forest_variables_precision_recall_F1)

big_knn_random_forest = merge(knn_random_forest_variables_accuracy_numbers,knn_random_forest_variables_precision_recall_F1)
big_knn_random_forest$nameOfRun <- "k nearest neighbors with random forest predictors"
big_knn_random_forest$variable1 <- variable1_rf
big_knn_random_forest$variable2 <- variable2_rf
big_knn_random_forest$variable3 <- variable3_rf
big_knn_random_forest$variable4 <- variable4_rf
big_knn_random_forest$variable5 <- variable5_rf
big_knn_random_forest <- big_knn_random_forest %>%
  select(nameOfRun, everything())

big_knn_random_forest$FalseNegative <- knn_random_forest_variables_trues_falses[1,2]
big_knn_random_forest$FalsePositive <- knn_random_forest_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_knn_random_forest)








#logistic regression with ridge regression predictors
logistic_regression_ridge_variables <- train(response ~.,
                      data = df_of_important_variables_and_their_columns_ridge,
                      family = "binomial",
                      method = "glm",
                      trControl = ctrl,
                      metric = "Accuracy")

logistic_regression_ridge_variables


preds_logistic_regression_ridge_variables <- predict(logistic_regression_ridge_variables, SpinaleProteinsAndResponses_test)
results_logistic_regression_ridge_variables <- confusionMatrix(preds_logistic_regression_ridge_variables, SpinaleProteinsAndResponses_test$response)
results_logistic_regression_ridge_1 <- as.matrix(results_logistic_regression_ridge_variables)
results_logistic_regression_ridge_2 <- as.matrix(results_logistic_regression_ridge_variables, what = "overall")
results_logistic_regression_ridge_3 <- as.matrix(results_logistic_regression_ridge_variables, what = "classes")

logistic_regression_ridge_variables_trues_falses <- as.data.frame(results_logistic_regression_ridge_1)

logistic_regression_ridge_variables_accuracy_numbers <- as.data.frame(results_logistic_regression_ridge_2)
logistic_regression_ridge_variables_accuracy_numbers = t(logistic_regression_ridge_variables_accuracy_numbers)
logistic_regression_ridge_variables_accuracy_numbers <- as.data.frame(logistic_regression_ridge_variables_accuracy_numbers)

logistic_regression_ridge_variables_precision_recall_F1 <- as.data.frame(results_logistic_regression_ridge_3)
logistic_regression_ridge_variables_precision_recall_F1 = t(logistic_regression_ridge_variables_precision_recall_F1)
logistic_regression_ridge_variables_precision_recall_F1 <- as.data.frame(logistic_regression_ridge_variables_precision_recall_F1)

big_logistic_regression_ridge = merge(logistic_regression_ridge_variables_accuracy_numbers,logistic_regression_ridge_variables_precision_recall_F1)
big_logistic_regression_ridge$nameOfRun <- "logistic regression with ridge predictors"
big_logistic_regression_ridge$variable1 <- variable1_ridge
big_logistic_regression_ridge$variable2 <- variable2_ridge
big_logistic_regression_ridge$variable3 <- variable3_ridge
big_logistic_regression_ridge$variable4 <- variable4_ridge
big_logistic_regression_ridge$variable5 <- variable5_ridge
big_logistic_regression_ridge <- big_logistic_regression_ridge %>%
  select(nameOfRun, everything())

big_logistic_regression_ridge$FalseNegative <- logistic_regression_ridge_variables_trues_falses[1,2]
big_logistic_regression_ridge$FalsePositive <- logistic_regression_ridge_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_logistic_regression_ridge)











#logistic regression with lasso predictors
logistic_regression_lasso_variables <- train(response ~.,
                             data = df_of_important_variables_and_their_columns_lasso,
                             family = "binomial",
                             method = "glm",
                             trControl = ctrl,
                             metric = "Accuracy")

logistic_regression_lasso_variables

preds_logistic_regression_lasso_variables <- predict(logistic_regression_lasso_variables, SpinaleProteinsAndResponses_test)
results_logistic_regression_lasso_variables <- confusionMatrix(preds_logistic_regression_lasso_variables, SpinaleProteinsAndResponses_test$response)
results_logistic_regression_lasso_1 <- as.matrix(results_logistic_regression_lasso_variables)
results_logistic_regression_lasso_2 <- as.matrix(results_logistic_regression_lasso_variables, what = "overall")
results_logistic_regression_lasso_3 <- as.matrix(results_logistic_regression_lasso_variables, what = "classes")

logistic_regression_lasso_variables_trues_falses <- as.data.frame(results_logistic_regression_lasso_1)

logistic_regression_lasso_variables_accuracy_numbers <- as.data.frame(results_logistic_regression_lasso_2)
logistic_regression_lasso_variables_accuracy_numbers = t(logistic_regression_lasso_variables_accuracy_numbers)
logistic_regression_lasso_variables_accuracy_numbers <- as.data.frame(logistic_regression_lasso_variables_accuracy_numbers)

logistic_regression_lasso_variables_precision_recall_F1 <- as.data.frame(results_logistic_regression_lasso_3)
logistic_regression_lasso_variables_precision_recall_F1 = t(logistic_regression_lasso_variables_precision_recall_F1)
logistic_regression_lasso_variables_precision_recall_F1 <- as.data.frame(logistic_regression_lasso_variables_precision_recall_F1)

big_logistic_regression_lasso = merge(logistic_regression_lasso_variables_accuracy_numbers,logistic_regression_lasso_variables_precision_recall_F1)
big_logistic_regression_lasso$nameOfRun <- "logistic regression with lasso predictors"
big_logistic_regression_lasso$variable1 <- variable1_lasso
big_logistic_regression_lasso$variable2 <- variable2_lasso
big_logistic_regression_lasso$variable3 <- variable3_lasso
big_logistic_regression_lasso$variable4 <- variable4_lasso
big_logistic_regression_lasso$variable5 <- variable5_lasso
big_logistic_regression_lasso <- big_logistic_regression_lasso %>%
  select(nameOfRun, everything())

big_logistic_regression_lasso$FalseNegative <- logistic_regression_lasso_variables_trues_falses[1,2]
big_logistic_regression_lasso$FalsePositive <- logistic_regression_lasso_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_logistic_regression_lasso)







#logistic regression with random forest predictors
logistic_regression_random_forest_variables <- train(response ~.,
                                             data = df_of_important_variables_and_their_columns_random_forest,
                                             family = "binomial",
                                             method = "glm",
                                             trControl = ctrl,
                                             metric = "Accuracy")

logistic_regression_random_forest_variables

preds_logistic_regression_random_forest_variables <- predict(logistic_regression_random_forest_variables, SpinaleProteinsAndResponses_test)
results_logistic_regression_random_forest_variables <- confusionMatrix(preds_logistic_regression_random_forest_variables, SpinaleProteinsAndResponses_test$response)
results_logistic_regression_random_forest_1 <- as.matrix(results_logistic_regression_random_forest_variables)
results_logistic_regression_random_forest_2 <- as.matrix(results_logistic_regression_random_forest_variables, what = "overall")
results_logistic_regression_random_forest_3 <- as.matrix(results_logistic_regression_random_forest_variables, what = "classes")

logistic_regression_random_forest_variables_trues_falses <- as.data.frame(results_logistic_regression_random_forest_1)

logistic_regression_random_forest_variables_accuracy_numbers <- as.data.frame(results_logistic_regression_random_forest_2)
logistic_regression_random_forest_variables_accuracy_numbers = t(logistic_regression_random_forest_variables_accuracy_numbers)
logistic_regression_random_forest_variables_accuracy_numbers <- as.data.frame(logistic_regression_random_forest_variables_accuracy_numbers)

logistic_regression_random_forest_variables_precision_recall_F1 <- as.data.frame(results_logistic_regression_random_forest_3)
logistic_regression_random_forest_variables_precision_recall_F1 = t(logistic_regression_random_forest_variables_precision_recall_F1)
logistic_regression_random_forest_variables_precision_recall_F1 <- as.data.frame(logistic_regression_random_forest_variables_precision_recall_F1)

big_logistic_regression_random_forest = merge(logistic_regression_random_forest_variables_accuracy_numbers,logistic_regression_random_forest_variables_precision_recall_F1)
big_logistic_regression_random_forest$nameOfRun <- "logistic regression with random forest predictors"
big_logistic_regression_random_forest$variable1 <- variable1_rf
big_logistic_regression_random_forest$variable2 <- variable2_rf
big_logistic_regression_random_forest$variable3 <- variable3_rf
big_logistic_regression_random_forest$variable4 <- variable4_rf
big_logistic_regression_random_forest$variable5 <- variable5_rf
big_logistic_regression_random_forest <- big_logistic_regression_random_forest %>%
  select(nameOfRun, everything())

big_logistic_regression_random_forest$FalseNegative <- logistic_regression_random_forest_variables_trues_falses[1,2]
big_logistic_regression_random_forest$FalsePositive <- logistic_regression_random_forest_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_logistic_regression_random_forest)







#SVM for ridge predictors

grid <- expand.grid(C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
SVM_ridge_variables <- train(response ~.,
                            df_of_important_variables_and_their_columns_ridge,
                            method = "svmLinear",
                            trControl = ctrl,
                            tuneGrid = grid,
                            tuneLength = 10,
                            metric = "Accuracy")


preds_svm_ridge_variables <- predict(SVM_ridge_variables, SpinaleProteinsAndResponses_test)
results_svm_ridge_variables <- confusionMatrix(preds_svm_ridge_variables, SpinaleProteinsAndResponses_test$response)
results_svm_ridge_1 <- as.matrix(results_svm_ridge_variables)
results_svm_ridge_2 <- as.matrix(results_svm_ridge_variables, what = "overall")
results_svm_ridge_3 <- as.matrix(results_svm_ridge_variables, what = "classes")

svm_ridge_variables_trues_falses <- as.data.frame(results_svm_ridge_1)

svm_ridge_variables_accuracy_numbers <- as.data.frame(results_svm_ridge_2)
svm_ridge_variables_accuracy_numbers = t(svm_ridge_variables_accuracy_numbers)
svm_ridge_variables_accuracy_numbers <- as.data.frame(svm_ridge_variables_accuracy_numbers)

svm_ridge_variables_precision_recall_F1 <- as.data.frame(results_svm_ridge_3)
svm_ridge_variables_precision_recall_F1 = t(svm_ridge_variables_precision_recall_F1)
svm_ridge_variables_precision_recall_F1 <- as.data.frame(svm_ridge_variables_precision_recall_F1)

big_svm_ridge = merge(svm_ridge_variables_accuracy_numbers,svm_ridge_variables_precision_recall_F1)
big_svm_ridge$nameOfRun <- "SVM with ridge predictors"
big_svm_ridge$variable1 <- variable1_ridge
big_svm_ridge$variable2 <- variable2_ridge
big_svm_ridge$variable3 <- variable3_ridge
big_svm_ridge$variable4 <- variable4_ridge
big_svm_ridge$variable5 <- variable5_ridge
big_svm_ridge <- big_svm_ridge %>%
  select(nameOfRun, everything())

big_svm_ridge$FalseNegative <- svm_ridge_variables_trues_falses[1,2]
big_svm_ridge$FalsePositive <- svm_ridge_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_svm_ridge)

#SVM with lasso predictors
#logistic regression with lasso predictors
SVM_ridge_variables <- train(response ~.,
                             df_of_important_variables_and_their_columns_lasso,
                             method = "svmLinear",
                             trControl = ctrl,
                             tuneGrid = grid,
                             tuneLength = 10,
                             metric = "Accuracy")
SVM_ridge_variables

preds_SVM_lasso_variables <- predict(SVM_ridge_variables, SpinaleProteinsAndResponses_test)
results_SVM_lasso_variables <- confusionMatrix(preds_SVM_lasso_variables, SpinaleProteinsAndResponses_test$response)
results_SVM_lasso_1 <- as.matrix(results_SVM_lasso_variables)
results_SVM_lasso_2 <- as.matrix(results_SVM_lasso_variables, what = "overall")
results_SVM_lasso_3 <- as.matrix(results_SVM_lasso_variables, what = "classes")

SVM_lasso_variables_trues_falses <- as.data.frame(results_SVM_lasso_1)

SVM_lasso_variables_accuracy_numbers <- as.data.frame(results_SVM_lasso_2)
SVM_lasso_variables_accuracy_numbers = t(SVM_lasso_variables_accuracy_numbers)
SVM_lasso_variables_accuracy_numbers <- as.data.frame(SVM_lasso_variables_accuracy_numbers)

SVM_lasso_variables_precision_recall_F1 <- as.data.frame(results_SVM_lasso_3)
SVM_lasso_variables_precision_recall_F1 = t(SVM_lasso_variables_precision_recall_F1)
SVM_lasso_variables_precision_recall_F1 <- as.data.frame(SVM_lasso_variables_precision_recall_F1)

big_SVM_lasso = merge(SVM_lasso_variables_accuracy_numbers,SVM_lasso_variables_precision_recall_F1)
big_SVM_lasso$nameOfRun <- "SVM with lasso predictors"
big_SVM_lasso$variable1 <- variable1_lasso
big_SVM_lasso$variable2 <- variable2_lasso
big_SVM_lasso$variable3 <- variable3_lasso
big_SVM_lasso$variable4 <- variable4_lasso
big_SVM_lasso$variable5 <- variable5_lasso
big_SVM_lasso <- big_SVM_lasso %>%
  select(nameOfRun, everything())

big_SVM_lasso$FalseNegative <- SVM_lasso_variables_trues_falses[1,2]
big_SVM_lasso$FalsePositive <- SVM_lasso_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_SVM_lasso)



#svm for random forest predictors
SVM_random_forest_variables <- train(response ~.,
                             df_of_important_variables_and_their_columns_random_forest,
                             method = "svmLinear",
                             trControl = ctrl,
                             tuneGrid = grid,
                             tuneLength = 10,
                             metric = "Accuracy")

SVM_random_forest_variables

preds_SVM_random_forest_variables <- predict(SVM_random_forest_variables, SpinaleProteinsAndResponses_test)
results_SVM_random_forest_variables <- confusionMatrix(preds_SVM_random_forest_variables, SpinaleProteinsAndResponses_test$response)
results_SVM_random_forest_1 <- as.matrix(results_SVM_random_forest_variables)
results_SVM_random_forest_2 <- as.matrix(results_SVM_random_forest_variables, what = "overall")
results_SVM_random_forest_3 <- as.matrix(results_SVM_random_forest_variables, what = "classes")

SVM_random_forest_variables_trues_falses <- as.data.frame(results_SVM_random_forest_1)

SVM_random_forest_variables_accuracy_numbers <- as.data.frame(results_SVM_random_forest_2)
SVM_random_forest_variables_accuracy_numbers = t(SVM_random_forest_variables_accuracy_numbers)
SVM_random_forest_variables_accuracy_numbers <- as.data.frame(SVM_random_forest_variables_accuracy_numbers)

SVM_random_forest_variables_precision_recall_F1 <- as.data.frame(results_SVM_random_forest_3)
SVM_random_forest_variables_precision_recall_F1 = t(SVM_random_forest_variables_precision_recall_F1)
SVM_random_forest_variables_precision_recall_F1 <- as.data.frame(SVM_random_forest_variables_precision_recall_F1)

big_SVM_random_forest = merge(SVM_random_forest_variables_accuracy_numbers,SVM_random_forest_variables_precision_recall_F1)
big_SVM_random_forest$nameOfRun <- "SVM with random forest predictors"
big_SVM_random_forest$variable1 <- variable1_rf
big_SVM_random_forest$variable2 <- variable2_rf
big_SVM_random_forest$variable3 <- variable3_rf
big_SVM_random_forest$variable4 <- variable4_rf
big_SVM_random_forest$variable5 <- variable5_rf
big_SVM_random_forest <- big_SVM_random_forest %>%
  select(nameOfRun, everything())

big_SVM_random_forest$FalseNegative <- SVM_random_forest_variables_trues_falses[1,2]
big_SVM_random_forest$FalsePositive <- SVM_random_forest_variables_trues_falses[2,1]

results_for_each_ML_algorithm <- rbind(results_for_each_ML_algorithm, big_SVM_random_forest)




}


stopCluster(cl)

ridgeResults <- results_for_each_ML_algorithm %>%
    filter(grepl('ridge predictors', nameOfRun)) %>%
  mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
         meanRecall = mean(Recall), meanF1 = mean(F1))


lassoResults <- results_for_each_ML_algorithm %>%
  filter(grepl('lasso predictors', nameOfRun)) %>%
    mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
           meanRecall = mean(Recall), meanF1 = mean(F1))


randomForestResults <- results_for_each_ML_algorithm %>%
  filter(grepl('random forest predictors', nameOfRun)) %>%
  mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
         meanRecall = mean(Recall), meanF1 = mean(F1))

logisticRegressionResults <- results_for_each_ML_algorithm %>%
  filter(grepl('logistic regression', nameOfRun)) %>%
  mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
         meanRecall = mean(Recall), meanF1 = mean(F1))

SVMResults <- results_for_each_ML_algorithm %>%
  filter(grepl('SVM', nameOfRun)) %>%
  mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
         meanRecall = mean(Recall), meanF1 = mean(F1))

knnResults <- results_for_each_ML_algorithm %>%
  filter(grepl('neighbors', nameOfRun)) %>%
  mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
         meanRecall = mean(Recall), meanF1 = mean(F1))

randomForestResults1 <- results_for_each_ML_algorithm %>%
  filter(grepl('random forest with', nameOfRun)) %>%
  mutate(meanAccuracy = mean(Accuracy), meanPrecision = mean(Precision), 
         meanRecall = mean(Recall), meanF1 = mean(F1))

#------------------------------------------------------------------------------

