install.packages("randomForest")


library(randomForest)

set.seed(1)
bag.expl= randomForest( AverageELAProficiency∼.,data=train,importance =TRUE)
bag.expl
yhat.bag = predict(bag.expl , newdata = test )
plot(yhat.bag, test$AverageELAProficiency)
mean((yhat.bag-.test)^2)

RMSE_calculator(yhat.bag, test$AverageELAProficiency)

importance(bag.expl)
varImpPlot(bag)

set.seed(42)
x <- cadets
x$RT..seconds. <- NULL
y <- cadets$RT..seconds.

#rf.cv <- rfcv(x, y, cv.fold=10)


# install.packages("caret")
# library(caret)
set.seed(99)
bestRf=tuneRF(train.mat,train$AverageELAProficiency, data=train
              , stepFactor = 1.5, improve = 0.05)

## 

bag.expl= randomForest( AverageELAProficiency∼.,data=train,importance =TRUE, mtry = 70)

runRF = function(formulataken, traindata, mtry = 70, ntree = 500){
  bag.expl= randomForest( formulataken ,data=traindata,importance =TRUE, mtry = mtry, ntree = ntree)
  yhat.bag.bestmtry = predict(bag.expl , newdata = test )
  rmse = RMSE_calculator(yhat.bag.bestmtry, test$EconomicNeedIndex)
  print(rmse)
  return (rmse)
}

## Grid Search Cv for random Forest
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(99)
tunegrid <- expand.grid(.mtry=c(40, 60, 70, 100))
rf_gridsearch <- train(AverageELAProficiency~., data=train, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

## CV
## trying with mtry and ntree
ntree.grid = c(100, 500, 1000)
mtry.grid = c(40,60,70,100)
CVdf = data.frame(mtry = integer(),
                  ntree = integer(),
#                   err = double())
# for (tr in ntree.grid)
# {
#   for(mt in mtry.grid){
#     err = runRF(formula(AverageELAProficiency~.), train, mtry = mt, ntree = tr)
#     CVdf = rbind(CVdf, data.frame(mt, tr, err))
#     
#   }
# }


rf_obj = tune.randomForest(x = train[, -5], y = train$EconomicNeedIndex,
                           mtry= c(40,60,70,100), ntree = c(100,500,1000),
                           tunecontrol = tune.control(sampling = "cross", cross = 3))

best.rf = rf_obj$best.parameters
rf_obj$best.model

grep("EconomicNeedIndex", colnames(train))



library(ggmap)
nyc_base <- ggmap::get_map("New York City", zoom = 11)
ggmap(nyc_base) + geom_point(data= target_schools, aes(x=as.numeric(Longitude), y=as.numeric(Latitude)), color="brown", size=3, alpha=0.8)
#+ coord_cartesian(ylim = c(40,42))




