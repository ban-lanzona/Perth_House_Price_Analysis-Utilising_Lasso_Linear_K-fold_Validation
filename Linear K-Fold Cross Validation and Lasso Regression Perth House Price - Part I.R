#Initializing the R file

##Clearing the environment and plots
rm(list = ls()) 
dev.off()  # But only if there IS a plot
cat("\014")  # ctrl+L

##Packages
library(tidyverse)
library(gridExtra)
library(broom)
require(pacman)
library(caTools)
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages('caret', dependencies = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, gmodels,caret,ROCR)
pacman::p_load(pacman, tidyverse, gmodels,ROCR, rpart, rpart.plot,caret)

if (!require("gt")) install.packages("gt")
library(gt)
library(ggplot2)
library(reshape2)
library(cowplot)
library(psych)
library(ipred)
library(car)
library(huxtable)
library(lmtest)
library(sandwich)
library(Hmisc)
library(caret)
library(jtools)
library(lmtest)
#______________________________________________________________________________________________________________#

perthprice_df_full = read.csv("Model Data.csv")

pp_factors = c("Area","SchoolRanked","City","Station.Line","NEAREST_STN")
perthprice_df_full[pp_factors] = lapply(perthprice_df_full[pp_factors],as.factor)
perthprice_df = subset(perthprice_df_full, select = -c(8, 15, 17))

str(perthprice_df_full)

#With ln

pp_numplot = list()
pp_numvars = as.list(colnames((select_if(perthprice_df[,1:10], is.numeric))))

for (var_name in pp_numvars) {
  plot = ggplot(data = perthprice_df, aes(x = .data[[var_name]], y = logPrice)) +
    geom_point() + geom_smooth(method=lm)
  pp_numplot[[var_name]] = plot
}

pp_numplot_plots = gridExtra::grid.arrange(grobs = pp_numplot, ncol = 3)

########____________________

set.seed(130)

price_trainpart = perthprice_df$logPrice %>% createDataPartition(p=0.70, list=FALSE)

#For LM without City

pp_train = perthprice_df[price_trainpart,]
pp_train_year = pp_train["Date.Sold.Year"] 
pp_test = perthprice_df[-price_trainpart,]
pp_test_year = pp_test["Date.Sold.Year"] 

#For Lasso with City
ppls_train = pp_train
ppls_test = pp_test

pp_train = subset(pp_train, select = -c(12,15,8))
pp_test = subset(pp_test, select = -c(12,15,8))

#______________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________

#Data partition for random forest method to get the most important factors

pp_trainbaseVAR = pp_train$logPrice %>% createDataPartition(p=0.66, list=FALSE)

pp_ABbase = pp_train[pp_trainbaseVAR,]
pp_C = pp_train[-pp_trainbaseVAR,]

pp_AB = pp_ABbase$logPrice %>% createDataPartition(p=0.50, list=FALSE)
pp_A = pp_ABbase[pp_AB,]
pp_B = pp_ABbase[-pp_AB,]

#______________________________________________________________________________________________________________#

ppforestA = randomForest(logPrice ~ ., data = pp_A, importance=T)
ppforestimpA = varImpPlot(ppforestA)
ppforestimpA

ppforestB = randomForest(logPrice ~ ., data = pp_B, importance=T)
ppforestimpB = varImpPlot(ppforestB)
ppforestimpB

ppforestC = randomForest(logPrice ~ ., data = pp_C, importance=T)
ppforestimpC = varImpPlot(ppforestC)
ppforestimpC

#______________________________________________________________________________________________________________#

#VAR IMP
FLOOR_AREA
School.Percentile
House.Age.at.Sell
CBD_DIST
LAND_AREA
Area
Station.Line
NEAREST_STN_DIST
BATHROOMS
NEAREST_SCH_DIST
BEDROOMS
GARAGE

#______________________________________________________________________________________________________________
#______________________________________________________________________________________________________________

#Models


formB = logPrice ~ FLOOR_AREA 
formC = logPrice ~ FLOOR_AREA + School.Percentile 
formD = logPrice ~ FLOOR_AREA + School.Percentile + House.Age.at.Sell
formE = logPrice ~ FLOOR_AREA + School.Percentile + House.Age.at.Sell + CBD_DIST 
formF = logPrice ~ FLOOR_AREA + School.Percentile + House.Age.at.Sell + CBD_DIST +  LAND_AREA 
formG = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area 
formH = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line 
formI = logPrice ~ FLOOR_AREA + School.Percentile + House.Age.at.Sell + CBD_DIST +  LAND_AREA + Area +  Station.Line +  NEAREST_STN_DIST
formJ = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS 
formK = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST 
formL = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS 
formM = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS +	GARAGE


lm_model1 = lm(formB, data=pp_train)
lm_model2 = lm(formC, data=pp_train)
lm_model3 = lm(formD, data=pp_train)
lm_model4 = lm(formE, data=pp_train)
lm_model5 = lm(formF, data=pp_train)
lm_model6 = lm(formG, data=pp_train)
lm_model7 = lm(formH, data=pp_train)
lm_model8 = lm(formI, data=pp_train)
lm_model9 = lm(formJ, data=pp_train)
lm_model10 = lm(formK, data=pp_train)
lm_model11 = lm(formL, data=pp_train)
lm_model12 = lm(formM, data=pp_train)

pp_train$logFloorArea = log(pp_train$FLOOR_AREA)
pp_train$logLandArea = log(pp_train$LAND_AREA)
pp_test$logFloorArea = log(pp_test$FLOOR_AREA)
pp_test$logLandArea = log(pp_test$LAND_AREA)

formB_l1 = logPrice ~ logFloorArea 
formC_l1 = logPrice ~ logFloorArea + School.Percentile 
formD_l1 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell
formE_l1 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell + CBD_DIST 
formF_l1 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell + CBD_DIST +  LAND_AREA 
formG_l1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area 
formH_l1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line 
formI_l1 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell + CBD_DIST +  LAND_AREA + Area +  Station.Line +  NEAREST_STN_DIST
formJ_l1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS 
formK_l1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST 
formL_l1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS 
formM_l1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS +	GARAGE

lm_model1_l1 = lm(formB_l1, data=pp_train)
lm_model2_l1 = lm(formC_l1, data=pp_train)
lm_model3_l1 = lm(formD_l1, data=pp_train)
lm_model4_l1 = lm(formE_l1, data=pp_train)
lm_model5_l1 = lm(formF_l1, data=pp_train)
lm_model6_l1 = lm(formG_l1, data=pp_train)
lm_model7_l1 = lm(formH_l1, data=pp_train)
lm_model8_l1 = lm(formI_l1, data=pp_train)
lm_model9_l1 = lm(formJ_l1, data=pp_train)
lm_model10_l1 = lm(formK_l1, data=pp_train)
lm_model11_l1 = lm(formL_l1, data=pp_train)
lm_model12_l1 = lm(formM_l1, data=pp_train)

formB_l15 = logPrice ~ logFloorArea 
formC_l15 = logPrice ~ logFloorArea + School.Percentile 
formD_l15 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell
formE_l15 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell + CBD_DIST 
formF_l15 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell + CBD_DIST +  logLandArea 
formG_l15 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea +	Area 
formH_l15 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea +	Area +	Station.Line 
formI_l15 = logPrice ~ logFloorArea + School.Percentile + House.Age.at.Sell + CBD_DIST +  logLandArea + Area +  Station.Line +  NEAREST_STN_DIST
formJ_l15 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS 
formK_l15 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST 
formL_l15 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS 
formM_l15 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS +	GARAGE

lm_model1_l15 = lm(formB_l15, data=pp_train)
lm_model2_l15 = lm(formC_l15, data=pp_train)
lm_model3_l15 = lm(formD_l15, data=pp_train)
lm_model4_l15 = lm(formE_l15, data=pp_train)
lm_model5_l15 = lm(formF_l15, data=pp_train)
lm_model6_l15 = lm(formG_l15, data=pp_train)
lm_model7_l15 = lm(formH_l15, data=pp_train)
lm_model8_l15 = lm(formI_l15, data=pp_train)
lm_model9_l15 = lm(formJ_l15, data=pp_train)
lm_model10_l15 = lm(formK_l15, data=pp_train)
lm_model11_l15 = lm(formL_l15, data=pp_train)
lm_model12_l15 = lm(formM_l15, data=pp_train)

#-------

summary(lm_model12_l15)

par(mfrow = c(2,2))
plot(lm_model12_l15)

#Prediction

predit_test_model9_l15 = predict(lm_model9_l15, newdata=pp_test)
plot(pp_test$logPrice, predit_test_model9_l15, main = "Model 9 L15", xlab = "Actual", ylab = "Predicted")
abline(lm(predit_test_model9_l15 ~ pp_test$logPrice, data = pp_test), col = "red")

predit_test_model10_l15 = predict(lm_model10_l15, newdata=pp_test)
plot(pp_test$logPrice, predit_test_model10_l15, main = "Model 10 L15", xlab = "Actual", ylab = "Predicted")
abline(lm(predit_test_model10_l15 ~ pp_test$logPrice, data = pp_test), col = "red")

predit_test_model11_l15 = predict(lm_model11_l15, newdata=pp_test)
plot(pp_test$logPrice, predit_test_model11_l15, main = "Model 11 L15", xlab = "Actual", ylab = "Predicted")
abline(lm(predit_test_model11_l15 ~ pp_test$logPrice, data = pp_test), col = "red")

predit_test_model12_l15 = predict(lm_model12_l15, newdata=pp_test)
plot(pp_test$logPrice, predit_test_model12_l15, main = "Model 12 L15", xlab = "Actual", ylab = "Predicted")
abline(lm(predit_test_model12_l15 ~ pp_test$logPrice, data = pp_test), col = "red")




simplem = huxreg("1"   = lm_model1, 
                  "2" = lm_model2,
                  "3" = lm_model3,
                  "4"  = lm_model4,
                  "5" = lm_model5,
                  "6"   = lm_model6, 
                  "7" = lm_model7,
                  "8" = lm_model8,
                  "9"  = lm_model9,
                  "10" = lm_model10,
                  "11"  = lm_model11,
                  "12" = lm_model12,
                 stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

log1m = huxreg("1"   = lm_model1_l1, 
               "2" = lm_model2_l1,
               "3" = lm_model3_l1,
               "4"  = lm_model4_l1,
               "5" = lm_model5_l1,
               "6"   = lm_model6_l1, 
               "7" = lm_model7_l1,
               "8" = lm_model8_l1,
               "9"  = lm_model9_l1,
               "10" = lm_model10_l1,
               "11"  = lm_model11_l1,
               "12" = lm_model12_l1,
               stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

log15m = huxreg("1"   = lm_model1_l15, 
                "2" = lm_model2_l15,
                "3" = lm_model3_l15,
                "4"  = lm_model4_l15,
                "5" = lm_model5_l15,
                "6"   = lm_model6_l15, 
                "7" = lm_model7_l15,
                "8" = lm_model8_l15,
                "9"  = lm_model9_l15,
                "10" = lm_model10_l15,
                "11"  = lm_model11_l15,
                "12" = lm_model12_l15,
                stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

library(openxlsx)

models_wb <- createWorkbook()

as_Workbook(simplem, Workbook = models_wb, sheet = "simple")
as_Workbook(log1m, Workbook = models_wb, sheet = "log1")
as_Workbook(log15m, Workbook = models_wb, sheet = "log15")

## Save workbook
saveWorkbook(models_wb, "models.xlsx", overwrite = TRUE)




formM = logPrice ~ FLOOR_AREA +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	LAND_AREA +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS +	NEAREST_SCH_DIST +	BEDROOMS +	GARAGE

logPrice ~ logFloorArea
logPrice ~ logLandArea

par(mfrow = c(3,3))
plot(pp_train$NEAREST_STN_DIST,pp_train$logPrice)
abline(lm(logPrice ~ NEAREST_STN_DIST, data = pp_train), col = "red")

