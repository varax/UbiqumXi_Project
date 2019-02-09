

library(caret)
library(fBasics)
library(dplyr)
library(lubridate)
library(tidyr)
library(animation)
library(devtools)
library(ggmap)
library(wesanderson)
source_gist("https://gist.github.com/mrdwab/6424112")
trainData <- read.csv("trainingData.csv")
validData <- read.csv("validationData.csv")






#NAs, Duplicate
#traindata : 19937, 529 ; ValidData : 1111/529
str(trainData, list.len = ncol(trainData))
str(validData)
names(validData)

any(is.na(trainData))
any(is.na(validData))
#19300 after remove duplicate in trainData
trainData <- distinct(trainData)
validData <- distinct(validData)



plot(trainData$LONGITUDE, trainData$LATITUDE)

#convert datatype
factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID")
trainData[,factors] <-lapply(trainData[,factors], as.factor)
validData[, factors] <-lapply(validData[,factors], as.factor)


trainData$TIMESTAMP <- as_datetime(trainData$TIMESTAMP,
                                   origin = "1970-01-01", tz="UTC")

validData$TIMESTAMP <- as_datetime(validData$TIMESTAMP,
                                   origin = "1970-01-01", tz="UTC")




WAPS_VarTrain<-nearZeroVar(trainData[,1:520], saveMetrics=TRUE)
WAPS_VarValid<-nearZeroVar(validData[,1:520], saveMetrics=TRUE)


#474 variables after remove zeroVar
trainData <-trainData[,-which(WAPS_VarTrain$zeroVar==TRUE)]  
validData <- validData[,-which(WAPS_VarValid$zeroVar==TRUE)]
names(trainData)
names(validData) 

#19227 / 474 after filter out the whole row without any signal.
trainData <- trainData %>% 
  dplyr::filter(apply(trainData[,1:465], 1, function(x) length(unique(x))>1))
#validation data : after remove
validData <- validData %>%
  dplyr::filter(apply(validData[,1:367], 1, function(x) length(unique(x)) >1))

#replace non-signal(100) to lowest number
trainData[,1:465] <- sapply(trainData[,1:465],function(x) ifelse(x==100,-120,x))
validData[, 1:367] <- sapply(validData[, 1:367], function(x) ifelse(x == 100, -120, x))

#find the strongest RSSI of the row
trainData <- trainData %>%
  mutate(highRSSI = rowMaxs(trainData[,1:312]))
#find the corresponding WAP of the strongest RSSI
trainData <- trainData %>%
  mutate(highWAP = colnames(trainData[,1:312]) [apply(trainData[,1:312],1,which.max)])



validData <- validData %>%
  mutate(highRSSI = rowMaxs(validData[,1:312]))

validData <- validData %>%
  mutate(highWAP = colnames(validData[,1:312]) [apply(validData[,1:312],1,which.max)])




#another way to extract column string
#trainData$highWAP2 <- gsub('WAP', '', trainData$highWAP)
#trainData$highWAP2 <- as.numeric(trainData$highWAP2)
#validData$highWAP2 <- gsub('WAP', '', validData$highWAP)
#validData$highWAP2 <- as.numeric(validData$highWAP2)

trainData$highWAP <- as.factor(trainData$highWAP)
validData$highWAP <- as.factor(validData$highWAP)



trainData <- trainData %>%
  unite(BUILDING_FLOOR, c("BUILDINGID", "FLOOR"), sep = "_", remove = FALSE) %>%
  mutate(BUILDING_FLOOR = paste0("build", BUILDING_FLOOR))

validData <- validData %>%
  unite(BUILDING_FLOOR, c("BUILDINGID", "FLOOR"), sep = "_", remove = FALSE) %>%
  mutate(BUILDING_FLOOR = paste0("build", BUILDING_FLOOR))
trainData$BUILDING_FLOOR <- as.factor(trainData$BUILDING_FLOOR)
validData$BUILDING_FLOOR <- as.factor(validData$BUILDING_FLOOR)


#remove USER6, all signal 0 comes from user6
trainData <- trainData[ -which(trainData$USERID == "6"),]
ggplot(data = trainData) +
  geom_point(aes(x= LONGITUDE, y = LATITUDE, color = BUILDINGID))


#only keep the variables shared by both valid and train dataset
coltrain <- colnames(trainData)
colvalid <- colnames(validData)
colAND1 <- combine(coltrain, colvalid)
dp<-colAND1[duplicated(colAND1)]
trainData<- trainData[,names(trainData) %in% dp]  
validData <- validData[,names(validData) %in% dp]
#312 WAPs, 321 total variables
#Sampling based on the Building_floor
trainSample<- stratified(trainData, "BUILDING_FLOOR", 0.3)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)

#Accuracy  Kappa   
#0.9998781  0.9998121
svmbm<-train(BUILDINGID~.,
             data=trainSample[, c(1:312, 317)],
             method='svmLinear',
             metric = "Accuracy",
             trControl = fitControl, 
             preProcess = c("center", "scale"))

# 0.999927  0.9998873 , plus strongest WAP
svmbm2<-train(BUILDINGID~.,
              data=trainSample[, c(1:312, 317, 325)],
              method='svmLinear',
              metric = "Accuracy",
              trControl = fitControl, 
              preProcess = c("center", "scale"))

#didn't finish running .....
rfbm<-train(BUILDINGID~.,
            data= trainSample[, c(1:312, 317)],
            method='rf',
            metric = "Accuracy",
            trControl = fitControl, 
            preProcess = c("center", "scale"))
#  Accuracy   Kappa    
# 0.9988438  0.9982153
knnbm<-train(BUILDINGID~.,
             data=trainSample[, c(1:312, 317)],
             method='knn',
             metric = "Accuracy",
             trControl = fitControl, 
             preProcess = c("center", "scale"))

#Accuracy     Kappa 
#0.9990999 0.9985774 
bdpredictsvm <- predict(svmbm, validData)
confusionMatrix(bdpredictsvm,validData$BUILDINGID) 
postResample(bdpredictsvm,validData$BUILDINGID)

#Accuracy     Kappa 
#0.9873987 0.9801301  
bdpredictknn <- predict(knnbm, validData)
confusionMatrix(bdpredictknn,validData$BUILDINGID) 
postResample(bdpredictknn,validData$BUILDINGID)

#for building predict choose svm
bdresample <- resamples(list(svmSample = svmbm, knnSampel = knnbm))
summary(bdresample)
bwplot(bdresample)




#try to see which one is not predicted correctly
validData %>%
  filter(BUILDINGID !=bdpre)
ggplot(validData)+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, color = check))
validData$ckbd <- factor(validData$ckbd)
validData$check <- ifelse(validData$BUILDINGID ==validData$bdpre, "correct", "wrong")






#  Accuracy   Kappa    
#  0.9611111  0.9486064
knnfl<-train(FLOOR~.,
             data = trainSample[, c(1:312, 316)],
             method='knn',
             metric = "Accuracy",
             trControl = fitControl, 
             preProcess = c("center", "scale"))

#adding building as variable will it increase the accuracy?
#0.9607465  0.9481203
knnfl2<-train(FLOOR~.,
              data= trainSample[, c(1:312, 316:317)],
              method='knn',
              metric = "Accuracy",
              trControl = fitControl, 
              preProcess = c("center", "scale"))


# 0.9855756  0.9809317
svmrfl <-train(FLOOR~.,
               data=
                 e[, c(1:312, 316)],
               method='svmRadial',
               metric = "Accuracy",
               trControl = fitControl, 
               preProcess = c("center", "scale"))

#0.9830822  0.9776391
svmrfl2 <-train(FLOOR~.,
                data=trainSample[, c(1:312, 316:317)],
                method='svmRadial',
                metric = "Accuracy",
                trControl = fitControl, 
                preProcess = c("center", "scale"))





# Accuracy   Kappa    
#0.9836325  0.9783654
svmlfl<-train(FLOOR~.,
              data=trainSample[, c(1:312, 316)],
              method='svmLinear',
              metric = "Accuracy",
              trControl = fitControl, 
              preProcess = c("center", "scale"))
# 0.9804604  0.9741707
svmlfl2<-train(FLOOR~.,
               data=trainSample[, c(1:312, 316:317)],
               method='svmLinear',
               metric = "Accuracy",
               trControl = fitControl, 
               preProcess = c("center", "scale"))

#model performance comparision
flresample <- resamples(list(svmflSam = svmrfl, svmlflSamp = svmlfl, knnflSam = knnfl))
summary(flresample)
bwplot(flresample)


#trying predict building combined with floor
bdflprevalid <- predict(svmbmfl, validData)
postResample(bdflprevalid,validData$BUILDING_FLOOR)

test2$BUILDING_FLOOR <- as.factor(test2$BUILDING_FLOOR)

bdflpredict <- predict(svmbmfl, test2)
confusionMatrix(bdflpredict,test2$BUILDING_FLOOR)
postResample(bdflpredict,test2$BUILDING_FLOOR)


#low accuracy of floor may due to overfitting? decrease the training fold
#0.82, 0.75
flpredictsvmr <- predict(svmrfl, validData)
confusionMatrix(flpredictsvmr,validData$FLOOR) 

#0.86 0.81
flpredictsvml <- predict(svmlfl, validData)
confusionMatrix(flpredictsvml,validData$FLOOR)

#0.76, 0.67
flpredictknn <- predict(knnfl, validData)
confusionMatrix(flpredictknn,validData$FLOOR)



#to predict generally (without seperating the buildings)
#6.167734  0.9975184  3.532409
longfitknn <-train(LONGITUDE~.,
                   data= trainSampe[,c(1:313)],
                   method='knn',
                   trControl = fitControl, 
                   preProcess = c("center", "scale"))



latfitknn <-train(LATITUDE~.,
                  data=trainSample[,c(1:312, 314)],
                  method='knn',
                  trControl = fitControl, 
                  preProcess = c("center", "scale"))

#17.7
postResample(latpreknn,validData$LATITUDE)

latpreknn <- predict(latfitknn, validData)
validData$latpre <-latpreknn
validData$lonpre <- longpreknn


ggplot(validData) + 
  geom_point(aes(x=latpreknn, y = longpreknn), color = "blue")+
  geom_point(aes(x=LATITUDE, y = LONGITUDE), color = "red")+
  facet_wrap(~FLOOR)



#28.83453  0.9468324  21.35013
longfitglm<-train(LONGITUDE~.,
                  data=trainSample[,c(1:313)],
                  method='glm',
                  trControl = fitControl, 
                  preProcess = c("center", "scale"))


#30.11131  0.9425549  21.32131
longfitsvm <-train(LONGITUDE~.,
                   data=trainSample[,c(1:313)],
                   method='svmLinear',
                   trControl = fitControl, 
                   preProcess = c("center", "scale"))

#51.79101  0.8713331  36.21891
longfitsvmr <- train(LONGITUDE~.,
                     data=trainSample[,c(1:313)],
                     method='svmRadial',
                     trControl = fitControl, 
                     preProcess = c("center", "scale"))

#17.87775  0.9795081  12.16602
longfitxgb <- train(LONGITUDE~.,
                    data=trainSample[,c(1:313)],
                    method='xgbTree',
                    trControl = fitControl, 
                    preProcess = c("center", "scale"))




# rmse: 21.57468
longpreknn <- predict(longfitknn, validData)
plot(longpreknn, validData$LONGITUDE)


#21.5746841  0.9683655  9.0915478 
postResample(longpreknn, validData$LONGITUDE)



#28.9040482  0.9438517 20.8343698 
longprexgbt <- predict(longfitxgb, validData)
postResample(longprexgbt, validData$LONGITUDE)



#trying to seperate the building
validData$prebd<-bdpredictsvm
bd1 <-trainSample %>%
  dplyr::filter(BUILDINGID == "0")
bd2 <- trainSample %>%
  dplyr::filter(BUILDINGID == "1")
bd3 <- trainSample %>%
  dplyr::filter(BUILDINGID == "2")

names(bd1)
bd1$FLOOR <- factor(bd1$FLOOR)
bd2$FLOOR <- factor(bd2$FLOOR)
bd3$FLOOR <- factor(bd3$FLOOR)
#0.9247759  0.8993665
bd1flknn<-train(FLOOR~.,
                data=bd1[, c(1:312, 316)],
                method='knn',
                metric = c("Accuracy"),
                trControl = fitControl, 
                preProcess = c("center", "scale"))


#0.9723009  0.9628830
bd1flsvmr<-train(FLOOR~.,
                 data=bd1[, c(1:312, 316)],
                 method='svmRadial',
                 metric = "Accuracy",
                 trControl = fitControl, 
                 preProcess = c("center", "scale"))

#157   0.9949192  0.9931946
bd1flrf<-train(FLOOR~.,
               data=bd1[, c(1:312, 316)],
               method='rf',
               metric = "Accuracy",
               trControl = fitControl, 
               preProcess = c("center", "scale"))



#using the predicted building instead of the real building
bdv1 <- validData%>%
  dplyr::filter(BUILDINGID == "0")
bdv1$BUILDINGID <- bdv1$predbd
bdv1$predbd <- NULL
bdv1$FLOOR <- factor(bdv1$FLOOR)

bdv2 <- validData%>%
  dplyr::filter(BUILDINGID == "1")
bdv2$BUILDINGID <- bdv2$predbd
bdv2$predbd <- NULL
bdv2$FLOOR <- factor(bdv2$FLOOR)
bdv3 <- validData%>%
  dplyr::filter(BUILDINGID == "2")
bdv3$BUILDINGID <- bdv3$predbd
bdv3$predbd <- NULL

# Accuracy     Kappa 
#0.9514925 0.9315548 
bd1prd <- predict(bd1flrf, bdv1)
postResample(bd1prd, bdv1$FLOOR)
#0.8059701 0.7253733 
bd1prdsvm <- predict(bd1flsvmr, bdv1)
postResample(bd1prdsvm, bdv1$FLOOR)

# 5  0.9651234  0.9531157
bd2flknn<-train(FLOOR~.,
                data=bd2[, c(1:312, 316)],
                method='knn',
                metric = c("Accuracy"),
                trControl = fitControl, 
                preProcess = c("center", "scale"))

#1.00  0.9757504  0.9673648
bd2flsvmr<-train(FLOOR~.,
                 data=bd2[, c(1:312, 316)],
                 method='svmRadial',
                 metric = "Accuracy",
                 trControl = fitControl, 
                 preProcess = c("center", "scale"))

#157   0.9861785  0.9814319
bd2flrf<-train(FLOOR~.,
               data=bd2[, c(1:312, 316)],
               method='rf',
               metric = "Accuracy",
               trControl = fitControl, 
               preProcess = c("center", "scale"))

#0.9927544  0.9902671
bd2flxgb<-train(FLOOR~.,
                data=bd2[, c(1:312, 316)],
                method='xgbTree',
                metric = c("Accuracy"),
                trControl = fitControl, 
                preProcess = c("center", "scale"))


bdflresample <- resamples(list(knn = bd2flknn, svmRadial = bd2flsvmr, randomForest = bd2flrf, xgbTree =bd2flxgb))
bwplot(bdflresample)

#Accuracy     Kappa 
#0.8045603 0.7175626 
bd2prd <- predict(bd2flrf, bdv2)
postResample(bd2prd, bdv2$FLOOR)

# Accuracy     Kappa 
#0.7817590 0.6879892 
bd2prdsvm <- predict(bd2flsvmr, bdv2)
postResample(bd2prdsvm, bdv2$FLOOR)

#Accuracy     Kappa 
#0.7198697 0.6066917 
bd2prdknn <- predict(bd2flknn, bdv2)
postResample(bd2prdknn, bdv2$FLOOR)

# Accuracy     Kappa 
#0.7850163 0.6915841 
bd2prdxgb <- predict(bd2flxgb, bdv2)
postResample(bd2prdxgb, bdv2$FLOOR)




# 0.9585984  0.9456190
bd3flknn<-train(FLOOR~.,
                data=bd3[, c(1:312, 316)],
                method='knn',
                metric = c("Accuracy"),
                trControl = fitControl, 
                preProcess = c("center", "scale"))

#0.9799797  0.9736413
bd3flsvmr<-train(FLOOR~.,
                 data=bd3[, c(1:312, 316)],
                 method='svmRadial',
                 metric = c("Accuracy"),
                 trControl = fitControl, 
                 preProcess = c("center", "scale"))

#0.9902662  0.9872039
bd3flrf<-train(FLOOR~.,
               data=bd3[, c(1:312, 316)],
               method='rf',
               metric = "Accuracy",
               trControl = fitControl, 
               preProcess = c("center", "scale"))


#0.7537313 0.6634511 
bd3prdknn <- predict(bd3flknn, bdv3)
postResample(bd3prdknn, bdv3$FLOOR)

#0.8246269 0.7592416
bd3prdsvm <- predict(bd3flsvmr, bdv3)
postResample(bd3prdsvm, bdv3$FLOOR)


#0.8320896 0.7717811 
bd3prdrf <- predict(bd3flrf, bdv3)
postResample(bd3prdrf, bdv3$FLOOR)



#4.886976  0.9785731  3.348517
latb1fitknn <-train(LATITUDE~.,
                    data=bd1[,c(1:312, 314)],
                    method='knn',
                    trControl = fitControl, 
                    preProcess = c("center", "scale"))
#4.613055  0.9806728  2.942507
latb1fitknn1 <-train(LATITUDE~.,
                     data=bd1[,c(1:312, 314)],
                     method='kknn',
                     trControl = fitControl, 
                     preProcess = c("center", "scale"))
#3.622997  0.9882640   2.383066
latb1fitrf <-train(LATITUDE~.,
                   data=bd1[,c(1:312, 314)],
                   method='ranger',
                   trControl = fitControl, 
                   preProcess = c("center", "scale"))

# 11.9731497  0.8590075  7.0614810                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    11.9731497  0.8590075  7.0614810 
bd1prdlat <- predict(latb1fitknn, bdv1)
postResample(bd1prdlat, bdv1$LATITUDE)

bd1prdlat2 <- predict(latb1fitknn1, bdv1)
postResample(bd1prdlat2, bdv1$LATITUDE)

#6.0301814 0.9659723 4.2755534
bd1prdlatrf <- predict(latb1fitrf, bdv1)
postResample(bd1prdlatrf, bdv1$LATITUDE)


bdv1$latpre<- bd1prdlatrf

#6.624374  0.9670244  4.328597
latb2fitknn <-train(LATITUDE~.,
                    data=bd2[,c(1:312, 314)],
                    method='knn',
                    trControl = fitControl, 
                    preProcess = c("center", "scale"))

#5.957493  0.9731486  3.407484
latb2fitknn2 <-train(LATITUDE~.,
                     data=bd2[,c(1:312, 314)],
                     method='kknn',
                     trControl = fitControl, 
                     preProcess = c("center", "scale"))

#4.810724  0.9826555   3.270566
latb2fitrf<-train(LATITUDE~.,
                  data=bd2[,c(1:312, 314)],
                  method='ranger',
                  trControl = fitControl, 
                  preProcess = c("center", "scale"))
#13.2442046  0.8599474  8.7887318 
bd2prdlat <- predict(latb2fitknn, bdv2)
postResample(bd2prdlat, bdv2$LATITUDE)

#14.2569311  0.8391639  9.0453313 
bd2prdlat2 <- predict(latb2fitknn2, bdv2)
postResample(bd2prdlat2, bdv2$LATITUDE)
#10.6118270  0.9131988  7.8330972 
bd2prdlatrf <- predict(latb2fitrf, bdv2)
postResample(bd2prdlatrf , bdv2$LATITUDE)
bdv2$latpre<-bd2prdlatrf
# 5.343198  0.9614718  3.324921
latb3fitknn <-train(LATITUDE~.,
                    data=bd3[,c(1:312, 314)],
                    method='knn',
                    trControl = fitControl, 
                    preProcess = c("center", "scale"))
#4.924196  0.9672817  2.769060

latb3fitknn2 <-train(LATITUDE~.,
                     data=bd3[,c(1:312, 314)],
                     method='kknn',
                     trControl = fitControl, 
                     preProcess = c("center", "scale"))
#  4.797986  0.9702217   3.089927
latb3fitrf <-train(LATITUDE~.,
                   data=bd3[,c(1:312, 314)],
                   method='ranger',
                   trControl = fitControl, 
                   preProcess = c("center", "scale"))




#15.5058405  0.7285755  9.4211646 
bd3prdlat <- predict(latb3fitknn, bdv3)
postResample(bd3prdlat, bdv3$LATITUDE)

#15.2515049  0.7367779  9.2164650
bd3prdlat2 <- predict(latb3fitknn2, bdv3)
postResample(bd3prdlat2, bdv3$LATITUDE)
#10.8434415  0.8595622  7.5136502 
bd3prdlatrf <- predict(latb3fitrf, bdv3)
postResample(bd3prdlatrf, bdv3$LATITUDE)

bdv3$latpre<- bd3prdlatrf
# 5.454857  0.9514685  3.673316
longbd1fit<-train(LONGITUDE~.,
                  data=bd1[,c(1:313)],
                  method='knn',
                  trControl = fitControl, 
                  preProcess = c("center", "scale"))

#10.5330082  0.8407089  6.6944146 
bd1prdlon <- predict(longbd1fit, bdv1)
postResample(bd1prdlon, bdv1$LONGITUDE)

bdv1$lonpre<- bd1prdlon
longbd2fit<-train(LONGITUDE~.,
                  data=bd2[,c(1:313)],
                  method='knn',
                  trControl = fitControl, 
                  preProcess = c("center", "scale"))

#10.0435177  0.9533815  6.9080757 
bd2prdlon <- predict(longbd2fit, bdv2)
postResample(bd2prdlon, bdv2$LONGITUDE)
bdv2$lonpre<- bd2prdlon
# 6.200571  0.9566774  3.817376
longbd3fit<-train(LONGITUDE~.,
                  data=bd3[,c(1:313)],
                  method='knn',
                  trControl = fitControl, 
                  preProcess = c("center", "scale"))

#6.194795  0.9587724   3.636926
longbd3fitrf<-train(LONGITUDE~.,
                    data=bd3[,c(1:313)],
                    method='ranger',
                    trControl = fitControl, 
                    preProcess = c("center", "scale"))
#13.9018138  0.8063019  9.2296687 
bd3prdlon <- predict(longbd3fit, bdv3)
postResample(bd3prdlon, bdv3$LONGITUDE)

#12.4450774  0.8463335  8.2933193 
bd3prdlonrf <- predict(longbd3fitrf, bdv3)
postResample(bd3prdlonrf, bdv3$LONGITUDE)
bdv3$lonpre<- bd3prdlonrf
names(bd3)

# predicted longitude and latitude in Valid Dataset
newvalid <- rbind(bdv1, bdv2, bdv3)


ggplot(newvalid) + 
  geom_point(aes(x=lonpre, y = latpre), color = "tan2")+
  geom_point(aes(x=LONGITUDE, y = LATITUDE), color = "royalblue4")+
  facet_wrap(~FLOOR)+
  labs(x = "Longitude", y = "Latitude", title = "Actual and predicted longitude and latitude in Valid Dataset")



ggplot(validData) + 
  geom_point(aes(x=LONGITUDE, y = LATITUDE), color = "royalblue4")+
  facet_wrap(~FLOOR)+
  labs(x = "Longitude", y = "Latitude", title = "Actual longitude and latitude in Valid Dataset")

longandlat <- rbind(trainSample[, c(313:314,316)], validData[, c(313:314,316)])

longandlat$source <- "trainData"
longandlat$source[5478:6588] <- "validData"


onlytrain <- longandlat[1:5477,]

ggplot(onlytrain)+
  geom_point(aes(x= onlytrain[, 1], y = onlytrain[, 2]), color = 'tomato3')+
  facet_wrap(~FLOOR)+
  labs(x = "Longitude", y = "Latitude", title = "Training Dataset")
ggplot(longandlat)+
  geom_point(aes(x= longandlat[, 1], y = longandlat[, 2], color = source)) + 
  scale_color_manual(values=c( 'tomato3','royalblue4'))+
  facet_wrap(~FLOOR)+
  labs(x = "Longitude", y = "Latitude", title = "Training Dataset and Valid DataSet")

#17.7 for lat, #21.5746841 for longitude
ggplot(validData) + 
  geom_point(aes(x=longpreknn, y = latpreknn), color = "tan2")+
  geom_point(aes(x=LONGITUDE, y = LATITUDE), color = "royalblue4")+
  facet_wrap(~FLOOR)+
  labs(x = "Longitude", y = "Latitude", title = "Actual and predicted longitude and latitude in Valid Dataset")







