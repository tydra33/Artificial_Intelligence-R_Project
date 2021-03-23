###############################################################################################################################################
#Kombiniranje modelov 


podatki <- read.csv("data.csv", stringsAsFactors = T)


set.seed(0)

sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki)*0.7), replace=F)
train <- podatki[sel,]
test <- podatki[-sel,]


library(CORElearn)

CA <- function(observed, predicted)
{
	mean(observed == predicted)
}



# Glasovanje


modelDT <- CoreModel(norm_poraba ~ ., train, model="tree")
modelNB <- CoreModel(norm_poraba ~ ., train, model="bayes")
modelKNN <- CoreModel(norm_poraba ~ ., train, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$norm_poraba, predDT)
caDT

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$norm_poraba, predNB)
caNB

predKNN <- predict(modelDT, test, type = "class")
caKNN <- CA(test$norm_poraba, predDT)
caKNN



pred <- data.frame(predDT, predNB, predKNN)

head(pred)


voting <- function(predictions)
{
	res <- vector()

  	for (i in 1 : nrow(predictions))  	
	{
		vec <- unlist(predictions[i,])
    	res[i] <- names(which.max(table(vec)))
  	}

  	res
}





predNormPoraba <- voting(pred)
head(predNormPoraba)

predicted <- factor(predNormPoraba, levels=levels(train$norm_poraba))
head(predicted)

CA(test$norm_poraba, predicted)




# Utezeno glasovanje


predDT.prob <- predict(modelDT, test, type="prob")
predNB.prob <- predict(modelNB, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")



predProb <- predDT.prob + predNB.prob + predKNN.prob



predNormPoraba <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predNormPoraba, levels(podatki$norm_poraba))
head(predicted)

CA(test$norm_poraba, predicted)


# Ccross-checking 
library(ipred)

mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict <- function(object, newdata) {pred <- predict(object, newdata, type="class"); destroyModels(object); pred}

res <- errorest(norm_poraba ~ ., train, model=mymodel, predict=mypredict, target.model="tree")
caDT.cv <- 1 - res$error
caDT.cv

res <- errorest(norm_poraba ~ ., train, model=mymodel, predict=mypredict, target.model="bayes")
caNB.cv <- 1 - res$error
caNB.cv

mymodelKNN <- function(formula, data, valK){CoreModel(formula, data, model="knn", kInNN=valK)}
res <- errorest(norm_poraba ~ ., train, model=mymodelKNN, predict=mypredict, valK=5)
caKNN.cv <- 1 - res$error
caKNN.cv

predProb <- caDT.cv * predDT.prob + caNB.cv * predNB.prob  + caKNN.cv * predKNN.prob


predNormPoraba <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predNormPoraba, levels(podatki$norm_poraba))

CA(test$norm_poraba, predicted)


#Bagging

n <- nrow(train)
m <- 30

models <- list()
for (i in 1:m)
{
	=
	sel <- sample(1:n, n, replace = T)
	bootstrap <- train[sel,]
	models[[i]] <- CoreModel(norm_poraba ~ ., bootstrap, model="tree", minNodeWeightTree=1)
}

pred <- NULL
for (i in 1:m)
	pred <- cbind(pred, as.character(predict(models[[i]], test, type="class")))

head(pred)

predNormPoraba <- voting(pred)
predicted <- factor(predNormPoraba, levels=levels(train$norm_poraba))
CA(test$norm_poraba, predicted)




library(ipred)

bag <- bagging(norm_poraba ~ ., train, nbagg=30)
predicted <- predict(bag, test, type="class")
CA(test$norm_poraba, predicted)


######################################################################################################################################

#Ocena 9  classification model 

datacsv <-  read.csv("data.csv", stringsAsFactors = T)




datacsv$datum <- NULL
datacsv$poraba<-NULL

datacsv1 = datacsv[datacsv$regija == "zahodna",]

set.seed(0)

samplec<- sample(1:nrow(datacsv), size = as.integer(nrow(datacsv) * 0.7), replace = F)

train<- datacsv[samplec,]
test<- datacsv[-samplec,]


samplec1<- sample(1:nrow(datacsv1), size = as.integer(nrow(datacsv1) * 0.7), replace = F)

train1<- datacsv1[samplec1,]
test1<- datacsv1[-samplec1,]


CA <- function(obs, pred)
{
	tab <- table(obs, pred)

	sum(diag(tab)) / sum(tab)
}

brier.score <- function(obsMat, predMat)
{
	sum((obsMat - predMat) ^ 2) / nrow(predMat)
}


CA(observed, predicted)
library(nnet)
obsMat <- class.ind(test$norm_poraba)
obsMat1 <- class.ind(test1$norm_poraba)


observed <- test$norm_poraba
observed1 <- test1$norm_poraba



# classification for datacsv 

library(rpart)
dt <- rpart("norm_poraba ~ povrsina + namembnost + temp_zraka",  data = train, cp =0)


tab <- printcp(dt)


row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))


dt <- prune(dt, cp=th)





predicted <- predict(dt, test, type="class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)




#Random forest classification for datacsv1
library(rpart)
dt1 <- rpart("norm_poraba ~ povrsina + namembnost + temp_zraka",  data = train1, cp =0)



tab1 <- printcp(dt1)


row1 <- which.min(tab1[,"xerror"])
th1 <- mean(c(tab1[row1, "CP"], tab1[row1-1, "CP"]))


dt1 <- prune(dt1, cp=th)


predicted1 <- predict(dt1, test1, type="class")
CA(observed1, predicted1)

predMat1 <- predict(dt, test1, type = "prob")
brier.score(obsMat1, predMat1)
#################################################################################################################
#Ocena 9 regresija

datacsv <-  read.csv("data.csv", stringsAsFactors = T)
datacsv$datum <- NULL
datacsv$norm_poraba<-NULL



datacsv1 = datacsv[datacsv$regija == "zahodna",]

set.seed(0)

samplec<- sample(1:nrow(datacsv), size = as.integer(nrow(datacsv) * 0.7), replace = F)

train<- datacsv[samplec,]
test<- datacsv[-samplec,]


samplec1<- sample(1:nrow(datacsv1), size = as.integer(nrow(datacsv1) * 0.7), replace = F)

train1<- datacsv1[samplec1,]
test1<- datacsv1[-samplec1,]




rmae <- function(obs, pred, mean.val) 
{  
	sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}


# srednja absolutna napaka
mae <- function(obs, pred)
{
	mean(abs(obs - pred))
}

meanVal <- mean(train$poraba)

predTrivial <- rep(meanVal, nrow(test))

meanVal1 <- mean(train1$poraba)

predTrivial1 <- rep(meanVal1, nrow(test))

#Linear regression model 1 


library(rpart)
library(rpart.plot)

rt.model <- rpart(poraba ~ ., data=train)
rpart.plot(rt.model)
predicted <- predict(rt.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))


#Linear regression model 2


library(rpart)
library(rpart.plot)

rt.model1 <- rpart(poraba ~ ., data=train1)
rpart.plot(rt.model1)
predicted1 <- predict(rt.model1, test1)
mae(test1$poraba, predicted1)
rmae(test1$poraba, predicted1, mean(train1$poraba))

