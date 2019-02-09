library(readxl)
library(OneR)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

brandPredict <- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx",sheet=2)
names(brandPredict)
str(brandPredict)

ggplot(data = brandPredict)+
  geom_point(aes(x=age, y=salary, color = brand))


Incomplete <- read.csv(file = file.choose())
head(Incomplete)
summary(Incomplete)


ft <- c("elevel", "car", "zipcode", "brand")
Incomplete[, ft] <- lapply(Incomplete[, ft], as.factor)


boxplot(Incomplete$salary)



brandPredict$brand[brandPredict$brand == 0] <- "Acer"
brandPredict$brand[brandPredict$brand == 1] <- "Sony"
brandPredict[,ft] <- lapply(brandPredict[,ft], as.factor)


ggplot(data = brandPredict)+
  geom_boxplot(aes(x=brand, y=salary))

#DONT'T do discretization for KNN and emsemble models(rf,xboost...)

#salaryCat <-  bin(brandPredict$salary, nbins = 4, method = "length", labels = c("[2000-5250]","[5251-8500]","[8501-11750]","[11750-∞]]"))
#ageCat <- bin(brandPredict$age, nbins = 4, method = "length", labels = c("[20-35]","[36-50]", "[51-65]", "[66-80]"))
#creditCat <- bin(brandPredict$credit, nbins = 5, method = "length", labels = c("[0-100000]", "[100001-200000]", "[200001-300000]","[300001-400000]","[400001-∞]"))


head(brandPredict)


featurePlot(x = brandPredict[, c("salary", "age", "credit")], 
                        y = brandPredict$brand, 
                        plot = "box",
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

#data split
set.seed(789)
forTrain <- createDataPartition(y=brandPredict$brand,
                                 p=0.75,
                                 list =FALSE)
trainData <- brandPredict[forTrain,]
testData <- brandPredict[-forTrain,]
nrow(trainData)

#variable selection
#two different ways to get different important variables
trCtrl2 <- rpart.control(cp = 0.0002)
DTFit <- rpart(brand~salary + age + elevel + car + zipcode + credit,
               method = "class", data = trainData, control = trCtrl2)
rpart.plot(DTFit)
varImp(DTFit)
printcp(DTFit)
plotcp(DTFit)
tree.DTFit <- prune(DTFit, cp=0.00308)
rpart.plot(tree.DTFit)

rfCtrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = FALSE)

rf <- rfe(x=trainData[, c("salary", "age", "credit")],
          y = trainData$brand, rfeControl = rfCtrl)


#data training  
trCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
                      # classProbs = TRUE, summaryFunction = twoClassSummary

knnALG1 <- train(brand~salary + age,
                 data = trainData, method = "knn",
                 trControl = trCtrl, 
                 preProcess = c("center", "scale"),tuneLength = 30)

#the recommended k = sqrt n
grid <- expand.grid(k=65:100)
knnALG2 <- train(brand~salary + age,
                 data = trainData, method = "knn",
                 trControl = trCtrl, 
                 preProcess = c("center", "scale"),tuneGrid = grid)

plot(knnALG2)


# default mtry???
rfFIT <-  randomForest(brand ~ age + salary + credit + elevel + car + zipcode , 
                       data = trainData, trControl = trCtrl)

start_time <- Sys.time()
rfFIT2 <- train(brand~., data = trainData, method = "rf",
                trControl = trCtrl)
end_time <- Sys.time()
running_time <- end_time - start_time

plot(knnALG2)
plot(rfFIT2)


# making prediction in testing dataset
testData1 <- testData[,1:2]
head(testData1)
testknn2 <- predict(knnALG2, testData1)
postResample(testknn2, testData$brand)
confusionMatrix(testknn2, testData$brand)
plot(testknn2, testData$brand)
summary(testknn2)

rfrs <- predict(rfFIT2, testData)
postResample(rfrs,testData$brand)
plot(rfrs, testData$brand)
confusionMatrix(rfrs, testData$brand)





predictResult <- predict(knnALG2,newdata  = Incomplete,  prbs=TRUE)
head(predictResult)
summary(predictResult)
head(Incomplete)
head(brandPredict)
Incomplete$brand <- predictResult

allDataSet <- c(brandPredict,Incomplete)
head(allDataSet)
class(allDataSet)
allDataSet <- as.data.frame(allDataSet)

ggplot(data = Incomplete)+
  geom_bar(aes(brand))

ggplot(data = Incomplete)+
  geom_point(aes(x=age, y=salary, color = brand))

ggplot(data = allDataSet)+
  geom_point(aes(x=age, y=salary, color = brand))

mdresample <- resamples(list(rfSample = rfFIT2, knnSampel = knnALG2))
summary(mdresample)
bwplot(mdresample)
diff <- diff(mdresample)
summary(diff)


