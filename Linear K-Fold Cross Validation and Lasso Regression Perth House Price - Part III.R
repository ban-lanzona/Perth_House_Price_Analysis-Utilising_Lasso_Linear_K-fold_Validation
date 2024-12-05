
#Lasso Regression

control_specs = trainControl(method="cv", number=20, savePredictions="all")
lambdavector = 20^seq(5,-5,length=500)

set.seed(130)

ppls_train = subset(ppls_train, select = -c(15))
ppls_test = subset(ppls_test, select = -c(15))

ppls_train$logFloorArea = log(ppls_train$FLOOR_AREA)
ppls_train$logLandArea = log(ppls_train$LAND_AREA)
ppls_test$logFloorArea = log(ppls_test$FLOOR_AREA)
ppls_test$logLandArea = log(ppls_test$LAND_AREA)

formJ_l15_Lasso1 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea + City +	Station.Line +	NEAREST_STN_DIST +	BATHROOMS 
formJ_l15_Lasso2 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea + Area +	NEAREST_STN +	NEAREST_STN_DIST +	BATHROOMS 
formJ_l15_Lasso3 = logPrice ~ logFloorArea +	School.Percentile +	House.Age.at.Sell +	CBD_DIST +	logLandArea + City +	NEAREST_STN +	NEAREST_STN_DIST +	BATHROOMS 

#------------------------------
ls_model9 = train(formJ_l15_Lasso, 
               data=ppls_train, 
               preProcess=c("center","scale"), 
               method="glmnet",
               tuneGrid=expand.grid(alpha=1,lambda=lambdavector), 
               trControl=control_specs, 
               na.action=na.omit)
summary(ls_model9)

ls_model9$bestTune$lambda
coef(ls_model9$finalModel,ls_model9$bestTune$lambda)
plot(log(ls_model9$results$lambda), ls_model9$results$RMSE, xlim=c(-5,0))

ls_model9_2 = train(formJ_l15_Lasso2, 
                  data=ppls_train, 
                  preProcess=c("center","scale"), 
                  method="glmnet",
                  tuneGrid=expand.grid(alpha=1,lambda=lambdavector), 
                  trControl=control_specs, 
                  na.action=na.omit)


ls_model9_2$bestTune$lambda
coef(ls_model9_2$finalModel,ls_model9_2$bestTune$lambda)
plot(log(ls_model9_2$results$lambda), ls_model9_2$results$RMSE, xlim=c(-5,0))

ls_model9_3 = train(formJ_l15_Lasso3, 
                    data=ppls_train, 
                    preProcess=c("center","scale"), 
                    method="glmnet",
                    tuneGrid=expand.grid(alpha=1,lambda=lambdavector), 
                    trControl=control_specs, 
                    na.action=na.omit)


ls_model9_3$bestTune$lambda
coef(ls_model9_3$finalModel,ls_model9_3$bestTune$lambda)
plot(log(ls_model9_3$results$lambda), ls_model9_3$results$RMSE, xlim=c(-5,0))

########____________________

#Comparing Lasso Regression and Linear Regression Model 9

compare_models(lmk_model9,ls_model9,metric="RMSE")
compare_models(lmk_model9,ls_model9,metric="Rsquared")

compare_models(lmk_model9,ls_model9_2,metric="RMSE")
compare_models(lmk_model9,ls_model9_2,metric="Rsquared")

compare_models(lmk_model9,ls_model9_3,metric="RMSE")
compare_models(lmk_model9,ls_model9_3,metric="Rsquared")

model_list = list(lmk_model9,ls_model9,ls_model9_2,ls_model9_3)
resamp = resamples(model_list)
summary(resamp)

#------------------------------

#Lasso Predictions

par(mfrow = c(2,2))

prediction9_lasso = predict(ls_model9, newdata=ppls_test)
data.frame(RMSE=RMSE(prediction9_lasso, ppls_test$logPrice), 
           Rsquared=R2(prediction9_lasso, ppls_test$logPrice))

plot(ppls_test$logPrice, prediction9_lasso, main = "Model 9 Lasso: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction9_lasso ~ ppls_test$logPrice, data = ppls_test), col = "red")

prediction9_2_lasso = predict(ls_model9_2, newdata=ppls_test)
data.frame(RMSE=RMSE(prediction9_2_lasso, ppls_test$logPrice), 
           Rsquared=R2(prediction9_2_lasso, ppls_test$logPrice))

plot(ppls_test$logPrice, prediction9_2_lasso, main = "Model 9_2 Lasso: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction9_2_lasso ~ ppls_test$logPrice, data = ppls_test), col = "red")

prediction9_3_lasso = predict(ls_model9_3, newdata=ppls_test)
data.frame(RMSE=RMSE(prediction9_3_lasso, ppls_test$logPrice), 
           Rsquared=R2(prediction9_3_lasso, ppls_test$logPrice))

plot(ppls_test$logPrice, prediction9_3_lasso, main = "Model 9_3 Lasso: Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
abline(lm(prediction9_3_lasso ~ ppls_test$logPrice, data = ppls_test), col = "red")


