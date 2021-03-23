#So we are building classification model based on the variable "Poraba"

modelEval <- function( data, formula, modelFun, evalFun, fType, modelType, 
                       toPrune=F) {
  localTrain <- rep(F, times = nrow(goodInfo))
  outPut <- vector()
  for(i in 1:11) {
    cat("ITERATION: ", i, "\n")
    flush.console()
    
    localTrain <- localTrain | goodInfo$month == i
    localTest <- goodInfo$month == i + 1
    
    model <- modelFun(formula, data[localTrain,])
    if(toPrune) {
      model <- modelFun(formula, data=data[localTrain,], cp=0)
      tab <- printcp(model)
      
      row <- which.min(tab[,"xerror"])
      th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
      model <- prune(model, cp=th)
    }
    
    if(modelType == "class") {
      observed <- data$norm_poraba[localTest]
      obsMat <- class.ind(data$norm_poraba[localTest])
      
      predicted <- predict(model, data[localTest,], type="class")
      predMat <- predict(model, data[localTest,], type = fType)

      if(evalFun == "CA") {
        outPut[i] <- CA(observed, predicted)
      }
      else if(evalFun == "brier") {
        outPut[i] <- brier.score(obsMat, predMat)
      }
    }
    else if (modelType == "reg") {
      predicted <- predict(model, data[localTest,])
      observed <- data$poraba[localTest]
      
      if (evalFun == "rmse") {
        outPut[i] <- rmse(observed, predicted, mean(data$poraba[localTrain]))
      }
      else if (evalFun == "rmae") {
        outPut[i] <- rmae(observed, predicted, mean(data$poraba[localTrain]))
      }
      else if (evalFun == "mae") {
        outPut[i] <- mae(observed, predicted)
      }
      else if (evalFun == "mse") {
        outPut[i] <- mse(observed, predicted)
      }
    }
  }
  
  outPut
}

modelEvalKNN <- function(data, formula, evalFun) {
  localTrain <- rep(F, times = nrow(goodInfo))
  outPut <- vector()
  for(i in 1:11) {
    cat("ITERATION: ", i, "\n")
    flush.console()
    
    localTrain <- localTrain | goodInfo$month == i
    localTest <- goodInfo$month == i + 1
    
    model <- kknn(formula, data[localTrain,], data[localTest,], k = 5)

    predicted <- fitted(model)
    observed <- data$poraba[localTest]
      
    if (evalFun == "rmse") {
      outPut[i] <- rmse(observed, predicted, mean(data$poraba[localTrain]))
    }
    else if (evalFun == "rmae") {
      outPut[i] <- rmae(observed, predicted, mean(data$poraba[localTrain]))
    }
    else if (evalFun == "mae") {
      outPut[i] <- mae(observed, predicted)
    }
    else if (evalFun == "mse") {
      outPut[i] <- mse(observed, predicted)
    }
  }
  
  outPut
}

porabat <-  read.csv("data.csv", sep=",", stringsAsFactors = T)
porabat <- na.omit(porabat)
porabat$datum <- NULL
porabat$poraba <- NULL
porabat$avgPoraba<-NULL
porabat$maxPoraba<-NULL
porabat$minPoraba<-NULL
porabat$sumPoraba<-NULL
porabat$month<-NULL
porabat$X <- NULL
porabat$ura <- as.factor(porabat$ura)


set.seed(0)

samplec<- sample(1:nrow(porabat), size = as.integer(nrow(porabat) * 0.7), replace = F)

train<- porabat[samplec,]
test<- porabat[-samplec,]

table(train$norm_poraba)
table(test$norm_poraba)

library(nnet)
obsMat <- class.ind(test$norm_poraba)

library(caret)
observed <- test$norm_poraba


CA <- function(observed, predicted)
{
	mean(observed == predicted)
}

brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}



#classification model 1 decision tree

library(rpart)
dt <- rpart(norm_poraba ~ .,  data = train, cp =0)

printcp(dt)
tab <- printcp(dt)

row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th

dt <- prune(dt, cp=th)

predicted <- predict(dt, test, type="class")
CA(observed, predicted) #0.815472

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat) #0.2818836
###
dt1 <- rpart(norm_poraba ~ namembnost + leto_izgradnje + povrsina,  data = train, cp = 0)

printcp(dt1)
tab1 <- printcp(dt1)

row1 <- which.min(tab1[,"xerror"])
th1 <- mean(c(tab1[row1, "CP"], tab1[row1-1, "CP"]))
th1

dt1 <- prune(dt1, cp=th1)

predicted1 <- predict(dt1, test, type="class")
CA(observed, predicted1) #0.6408792

predMat1 <- predict(dt1, test, type = "prob")
brier.score(obsMat, predMat1) #0.4768113

modelEval(data = porabat, formula = as.formula(norm_poraba ~ .), modelFun = rpart, 
          evalFun = "brier", fType = "prob", modelType = "class", toPrune = T)
modelEval(data = porabat, formula = as.formula(norm_poraba ~ namembnost + leto_izgradnje + povrsina), 
          modelFun = rpart, evalFun = "brier", fType = "prob", modelType = "class", toPrune = T)


#Classification model 2  with random forest
library(CORElearn)
rf <- CoreModel(norm_poraba ~ ., data = train, model="rf")
predicted <- predict(rf, test, type="class")
CA(observed, predicted) #0.8312868

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat) #0.2446596
###
rf1 <- CoreModel(norm_poraba ~ namembnost + leto_izgradnje + povrsina, data = train, model="rf")
predicted1 <- predict(rf1, test, type="class")
CA(observed, predicted1) #0.6408792

predMat1 <- predict(rf1, test, type = "prob")
brier.score(obsMat, predMat1) #0.4557486

modelEval(data = porabat, formula = as.formula(norm_poraba ~ .), modelFun = CoreModel, 
          evalFun = "brier", fType = "prob", modelType = "class")
modelEval(data = porabat, formula = as.formula(norm_poraba ~ namembnost + leto_izgradnje + povrsina), modelFun = CoreModel, 
          evalFun = "brier", fType = "prob", modelType = "class")

#Classification model 3 with naive Bayes


library(e1071)

nb <- naiveBayes(norm_poraba ~ ., data = train)
predicted <- predict(nb, test, type="class")
CA(observed, predicted) #0.3799943

predMat <- predict(nb, test, type = "raw")
brier.score(obsMat, predMat) #0.7411347
###
nb1 <- naiveBayes(norm_poraba ~ namembnost + leto_izgradnje + povrsina, data = train)
predicted1 <- predict(nb1, test, type="class")
CA(observed, predicted1) #0.3734106

predMat1 <- predict(nb1, test, type = "raw")
brier.score(obsMat, predMat1) #0.7360255


modelEval(data = porabat, formula = as.formula(norm_poraba ~ namembnost + leto_izgradnje + povrsina), modelFun = naiveBayes, 
          evalFun = "brier", fType = "raw", modelType = "class")
modelEval(data = porabat, formula = as.formula(norm_poraba ~ .), modelFun = naiveBayes, 
          evalFun = "brier", fType = "raw", modelType = "class")


##############################################Regresija############################################################################


# linearna regresija


porabac <- read.csv("data.csv", sep=",", stringsAsFactors = T)
porabac <- na.omit(porabac)
porabac$datum <- NULL
porabac$norm_poraba<-NULL
porabac$month<-NULL
porabac$season<-NULL
porabac$X <- NULL
porabac$weather <- NULL
porabac$ura <- as.factor(porabac$ura)

set.seed(0)

samplec<- sample(1:nrow(porabac), size = as.integer(nrow(porabac) * 0.7), replace = F)

train<- porabac[samplec,]
test<- porabac[-samplec,]


rmae <- function(obs, pred, mean.val) 
{  
	sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

mae <- function(obs, pred)
{
	mean(abs(obs - pred))
}

mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

##########################################
meanVal <- mean(train$poraba)
meanVal
predTrivial <- rep(meanVal, nrow(test))


#Linear regression model 1 


model <- lm(poraba ~ ., train)

predicted <- predict(model, test)

observed <- test$poraba

rmse(observed, predicted, mean(train$poraba)) #0.06430346
rmae(observed, predicted, mean(train$poraba)) #0.1989883
###
model1 <- lm(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba, train)

predicted <- predict(model1, test)

observed <- test$poraba

rmse(observed, predicted, mean(train$poraba)) #0.0713572
rmae(observed, predicted, mean(train$poraba)) #0.1962335

modelEval(data = porabac, formula = as.formula(poraba ~ .), modelFun = lm, 
          evalFun = "rmse", fType = "", modelType = "reg")
modelEval(data = porabac, formula = as.formula(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba),
          modelFun = lm, evalFun = "rmse", fType = "", modelType = "reg")


#################################################################################################

#Regression model 2  regression tree

library(rpart)
library(rpart.plot)

rt.model <- rpart(poraba ~ ., data=train)
predicted <- predict(rt.model, test)
rmse(test$poraba, predicted, mean(train$poraba)) #0.09481967
rmae(test$poraba, predicted, mean(train$poraba)) #0.2785586
###
rt.model1 <- rpart(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba, data=train)
predicted <- predict(rt.model1, test)
rmse(test$poraba, predicted, mean(train$poraba)) #0.09481967
rmae(test$poraba, predicted, mean(train$poraba)) #0.2785586

modelEval(data = porabac, formula = as.formula(poraba ~ .), modelFun = rpart, 
          evalFun = "rmse", fType = "", modelType = "reg")
modelEval(data = porabac, formula = as.formula(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba),
          modelFun = rpart, evalFun = "rmse", fType = "", modelType = "reg")

#Regression model 3  nevronska mreza

library(nnet)

set.seed(0)

min_vals <- apply(train[c(3, 5:14, 18:21)], 2, min)
max_vals <- apply(train[c(3, 5:14, 18:21)], 2, max)
normTrain <- as.data.frame(scale(train[c(3, 5:14, 18:21)], center = min_vals, scale = max_vals - min_vals))
normTrain$poraba <- train$poraba
normTest <- as.data.frame(scale(test[c(3, 5:14, 18:21)], center = min_vals, scale = max_vals - min_vals))
normTest$quality <- test$quality

nn.model <- nnet(poraba ~ ., normTrain, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, normTest)
rmse(test$poraba, predicted, mean(normTrain$poraba)) #0.06995541
rmae(test$poraba, predicted, mean(normTrain$poraba)) #0.1985665
###
nn.model1 <- nnet(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba, 
                  normTrain, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model1, normTest)
rmse(test$poraba, predicted, mean(normTrain$poraba)) #0.07099577
rmae(test$poraba, predicted, mean(normTrain$poraba)) #0.195312

##############k nearest neighbor######################################################################
library(kknn)

knn.model <- kknn(poraba ~ ., train, test, k = 5)
predicted <- fitted(knn.model)
rmse(test$poraba, predicted, mean(train$poraba)) #0.05474145
rmae(test$poraba, predicted, mean(train$poraba)) #0.1970113
###
knn.model1 <- kknn(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba, train, test, k = 5)
predicted <- fitted(knn.model1)
rmse(test$poraba, predicted, mean(train$poraba)) #0.08148342
rmae(test$poraba, predicted, mean(train$poraba)) #0.211432

modelEvalKNN(data = porabac, formula = as.formula(poraba ~ .), evalFun = "rmse")
modelEvalKNN(data = porabac, 
             formula = as.formula(poraba ~ maxPoraba + avgPoraba + minPoraba + sumPoraba), 
             evalFun = "rmse")
