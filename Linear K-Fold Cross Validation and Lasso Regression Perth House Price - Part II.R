
#Linear k-fold Regression

lm_control_specs = trainControl(method="cv", number=20, savePredictions="all")

set.seed(130)

#------------------------------
lmk_model1 = train(formB_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model2 = train(formC_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model3 = train(formD_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model4 = train(formE_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model5 = train(formF_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model6 = train(formG_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model7 = train(formH_l15, 
               data=pp_train, 
               method="lm",
               trControl=lm_control_specs, 
               na.action=na.omit)
lmk_model8 = train(formI_l15, 
                   data=pp_train, 
                   method="lm",
                   trControl=lm_control_specs, 
                   na.action=na.omit)
lmk_model9 = train(formJ_l15, 
                   data=pp_train, 
                   method="lm",
                   trControl=lm_control_specs, 
                   na.action=na.omit)
lmk_model10 = train(formK_l15, 
                   data=pp_train, 
                   method="lm",
                   trControl=lm_control_specs, 
                   na.action=na.omit)
lmk_model11 = train(formL_l15, 
                   data=pp_train, 
                   method="lm",
                   trControl=lm_control_specs, 
                   na.action=na.omit)
lmk_model12 = train(formM_l15, 
                    data=pp_train, 
                    method="lm",
                    trControl=lm_control_specs, 
                    na.action=na.omit)

summary(lmk_model12)

#------------------------------

#Comparing Linear Regression

compare_models(lmk_model12,lmk_model1,metric="RMSE")
compare_models(lmk_model12,lmk_model1,metric="Rsquared")

compare_models(lmk_model12,lmk_model2,metric="RMSE")
compare_models(lmk_model12,lmk_model2,metric="Rsquared")

compare_models(lmk_model12,lmk_model3,metric="RMSE")
compare_models(lmk_model12,lmk_model3,metric="Rsquared")

compare_models(lmk_model12,lmk_model4,metric="RMSE")
compare_models(lmk_model12,lmk_model4,metric="Rsquared")

compare_models(lmk_model12,lmk_model5,metric="RMSE")
compare_models(lmk_model12,lmk_model5,metric="Rsquared")

compare_models(lmk_model12,lmk_model6,metric="RMSE")
compare_models(lmk_model12,lmk_model6,metric="Rsquared")

compare_models(lmk_model12,lmk_model7,metric="RMSE")
compare_models(lmk_model12,lmk_model7,metric="Rsquared")

compare_models(lmk_model12,lmk_model8,metric="RMSE")
compare_models(lmk_model12,lmk_model8,metric="Rsquared")

compare_models(lmk_model12,lmk_model9,metric="RMSE")
compare_models(lmk_model12,lmk_model9,metric="Rsquared")

compare_models(lmk_model12,lmk_model10,metric="RMSE")
compare_models(lmk_model12,lmk_model10,metric="Rsquared")

compare_models(lmk_model12,lmk_model11,metric="RMSE")
compare_models(lmk_model12,lmk_model11,metric="Rsquared")

compare_models(lmk_model9,lmk_model6,metric="RMSE")
compare_models(lmk_model9,lmk_model6,metric="Rsquared")

compare_models(lmk_model9,lmk_model7,metric="RMSE")
compare_models(lmk_model9,lmk_model7,metric="Rsquared")

compare_models(lmk_model9,lmk_model8,metric="RMSE")
compare_models(lmk_model9,lmk_model8,metric="Rsquared")

#_______

#After analysing and getting the final model

coef(lmk_model9$finalModel)
summary(lmk_model9)

#_____

model_list = list(lmk_model9,lmk_model12)
resamp = resamples(model_list)
summary(resamp)

#------------------------------

prediction9 = predict(lmk_model9, newdata=pp_test)
data.frame(RMSE=RMSE(prediction9, pp_test$logPrice), 
           Rsquared=R2(prediction9, pp_test$logPrice))

prediction10 = predict(lmk_model10, newdata=pp_test)
data.frame(RMSE=RMSE(prediction10, pp_test$logPrice), 
           Rsquared=R2(prediction10, pp_test$logPrice))

prediction11 = predict(lmk_model11, newdata=pp_test)
data.frame(RMSE=RMSE(prediction11, pp_test$logPrice), 
           Rsquared=R2(prediction11, pp_test$logPrice))

prediction12 = predict(lmk_model12, newdata=pp_test)
data.frame(RMSE=RMSE(prediction12, pp_test$logPrice), 
           Rsquared=R2(prediction12, pp_test$logPrice))

plot(pp_test$logPrice, prediction9, main = "Model 9 LMK: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction9 ~ pp_test$logPrice, data = pp_test), col = "red")

plot(pp_test$logPrice, prediction10, main = "Model 10 LMK: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction10 ~ pp_test$logPrice, data = pp_test), col = "red")

plot(pp_test$logPrice, prediction11, main = "Model 11 LMK: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction11 ~ pp_test$logPrice, data = pp_test), col = "red")

plot(pp_test$logPrice, prediction12, main = "Model 12 LMK: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction12 ~ pp_test$logPrice, data = pp_test), col = "red")

#Testing with Interaction between Floor and Land Area

formJ_l15_wIT = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea + logFloorArea*logLandArea +	Area +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS 

lmk_model9_wIT = train(formJ_l15_wIT, 
                   data=pp_train, 
                   method="lm",
                   trControl=lm_control_specs, 
                   na.action=na.omit)
compare_models(lmk_model9,lmk_model9_wIT,metric="RMSE")
compare_models(lmk_model9,lmk_model9_wIT,metric="Rsquared")
summary(lmk_model9_wIT)



plot(pp_test$logPrice, prediction9, main = "Model 9 LMK: Actual vs Predicted", xlab = "Actual", ylab = "Predicted",col="#f1c232")
abline(lm(prediction9 ~ pp_test$logPrice, data = pp_test), col = "white")

plotting = as.data.frame(pp_test$logPrice)
plotting = mutate(plotting, Predictions = prediction9)

view(plotting)
write.csv(plotting,file="Predict.csv")
