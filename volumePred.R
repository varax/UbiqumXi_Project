
# Data preparation####
existingData <- read.csv("existing_Age_Professional_Competitors.csv",
                         header = T, sep = ";")
newData <- read.csv("newproduct_Age_Professional_Competitors.csv", sep = ";")
names(newData)
neData <- newData[, -c(1,3,13:17)]
names(neData)
neData <- neData[which (neData$Product_type %in%  c("PC", "Laptop", "Smartphone", "Netbook")),]
neData <- neData[,c(1,3,4,8,11,12,13)]
str(neData)
neData$Profit_margin <- as.numeric(neData$Profit_margin )
neData$Competitors <- as.factor(neData$Competitors)

str(existingData)
names(existingData)
View(existingData)
Data_cl <- existingData[, -c(1, 3,14:17)]
Data_cl$Best_seller_rank <- NULL
Data_cl$Prices <- as.numeric(Data_cl$Prices)
Data_cl$Would_consumer_recomend__product <- as.numeric(Data_cl$Would_consumer_recomend__product)
Data_cl$Profit_margin <- as.numeric(Data_cl$Profit_margin)
Data_cl$Competitors <- as.factor(Data_cl$Competitors)
Data_cl$Professional <- as.factor(Data_cl$Professional)
Data_cl$Age <- as.factor(Data_cl$Age)
#(the same) Data_s <- Data_cl[ which(Data_cl$Product_type %in%  c("PC", "Laptop", "Smartphone", "Netbook")),]
DataFour <-subset.data.frame(Data_cl,Product_type =="PC"|
                               Product_type =="Laptop"|
                               Product_type =="Smartphone"|
                               Product_type =="Netbook", drop = TRUE ) 
rownames(DataFour) <- seq(length=nrow(DataFour))
head(DataFour)
ggplot(data = DataFour) + 
  geom_bar(mapping = aes(x= Product_type))
VLbytp <- DataFour[,c(1, 12)]
library(dplyr)
ptvl <- (VLbytp %>% 
  group_by(Product_type) %>% 
 summarise(Volume = sum(Volume)))


library(plotly)

p <- plot_ly(
  x = c("PC", "Smartphone","Laptop", "Netbook"),
  y = c(1578,2896,4249, 618),
  name = "Volume",
  type = "bar"
)


datass <- DataFour[DataFour$X5Stars!=0,]
str(datass)
ggplot(data = datass)+
  geom_point(aes(x=X5Stars, y=Volume, color = Product_type))
ggplot(data = DataFour)+
  geom_point(aes(x=X4Stars, y=Volume, color = Competitors))
withoutStars <- DataFour[DataFour$X5Stars == 0 & DataFour$X4Stars == 0
                         &DataFour$Positive_service_review == 0 & DataFour$Negative_service_review == 0, ]
ggplot(data = withoutStars)+
  geom_point(aes(x=Profit_margin, y=Volume, color = Product_type))
summary(ohne4and5)
class(ohne4and5)
withStars <- DataFour[DataFour$X5Stars != 0 | DataFour$X4Stars != 0
                      |DataFour$Positive_service_review != 0 | DataFour$Negative_service_review !=0 ,  ]
ggplot(data = withStars)+
  geom_point(aes(x=X5Stars, y=Volume, color = Product_type))

ggplot(data = withStars) + 
  geom_point(mapping = aes(x = Positive_service_review, y = Volume)) + 
  facet_grid(Competitors~ Product_type)
summary(withStars)
str(withStars)
str(withoutStars)
summary(DataFour)

set.seed(123)
trainfull_review <- createDataPartition(y=withStars$Volume,
                                 p=0.75,
                                 list =FALSE) 
train_rev <- withStars[trainfull_review,]
test_rev <- withStars[-trainfull_review,]
nrow(train_rev)
lm_rev <- lm(Volume~., train_rev)
summary(lm_rev)
lmss<-lm(Volume~ X5Stars + X4Stars + Product_type , datass)
summary(lmss)



ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rfsl_rev <- train(
  Volume~ .,
  data = train_rev,
  method = "rf",
  trControl = ctrl,
  importance = T
)
varImp(rfsl_rev)


#split dataset for variable selection####
set.seed(123)
trainfull <- createDataPartition(y=DataFour$Volume,
                                 p=0.75,
                                 list =FALSE) 
train_full <- DataFour[trainfull,]
test_full <-  DataFour[-trainfull,]
#variable selection based on RF####
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#259.7823  0.3241647
rfsl <- train(
  Volume~ .,
  data = train_full,
  method = "rf",
  trControl = ctrl,
  importance = T
)
plot(varImp(rfsl))

names(DataFour)
rfvar <- DataFour[,c(1,2,3,8,9,10,12)]
set.seed(123)
trainingrf <- createDataPartition(y=rfvar$Volume,
                                  p=0.75,
                                  list =FALSE) 
train_rf <-rfvar[trainingrf,]
test_rf<-rfvar[-trainingrf,]
#241.7822  0.3933
rf_fit1 <- train(Volume~., data= train_rf, method = "rf",
                 trControl=ctrl)

# 290.3491  0.4068862
xgbt_rf <-train(Volume~., data= train_rf, method = "xgbTree",
                trControl=ctrl)
#  263.4071  0.4887779
svmrf_fit1 <- train(Volume~., data= train_rf, method = "svmLinear",
                    preProcess ="scale", trControl= ctrl)
# 270.5010  0.2454013
svmrf_fitra <- train(Volume~., data= train_rf, method = "svmRadial",
                    preProcess ="scale", trControl= ctrl)

#199.23   0.5285
lm_rf <- lm(Volume~., data= train_rf)
summary(lm_rf)
RSS_rf <- c(crossprod(lm_rf$residuals))
MSE_rf<- RSS_rf / length(lm_rf$residuals)
RMSE_rf <- sqrt(MSE_rf)


#variable selection based on LM####
#  162.06       0.506
lmva <- lm(Volume~., train_full)
summary(lmva)


#209.38      0.52
lmva1  <- lm(Volume~X5Stars + Product_type + Positive_service_review, train_full)
summary(lmva1)

#184.12     0.57
lmva2 <- lm(Volume~X5Stars + Product_type + Positive_service_review + Competitors, train_full)
summary(lmva2)

#176.18  0.60
lmva3 <- lm(Volume~X5Stars + Product_type + Positive_service_review + Competitors + X4Stars, train_full)
summary(lmva3)
#172.5  0.60 , best variables to choose
lmva4 <- lm(Volume~X5Stars + Product_type + Positive_service_review + Competitors + X4Stars + Profit_margin, train_full)
summary(lmva4)

#171.5  0.60
lmva5 <- lm(Volume~X5Stars + Product_type + Positive_service_review +Negative_service_review+ Competitors + X4Stars + Profit_margin, train_full)
summary(lmva5)

#calculate RMSE in LM
RSS_full<- c(crossprod(lmva5$residuals))
MSE_full<- RSS_full/ length(lmva5$residuals)
RMSE_full <- sqrt(MSE_full)

#Datasubset based on LM
Data_corr <- DataFour[,c(1,3,4,8,11,12,13)]
set.seed(123)
trainingcorr <- createDataPartition(y=Data_corr$Volume,
                                    p=0.75,
                                    list =FALSE) 
train_corr <- Data_corr[trainingcorr,]
test_corr <-  Data_corr[-trainingcorr,]

#239.3, 0.47
svmrf_fit2 <- train(Volume~., data= train_corr, method = "svmLinear",
                    preProcess ="scale", trControl= ctrl)

#196.3445  0.5627255
rf_fit2 <- train(Volume~., data= train_corr, method = "rf",
                 trControl=ctrl)

# 289.0880  0.2764778 
xgbt_corr <- train(Volume~., data= train_corr, method = "xgbTree",
                   trControl=ctrl)

#306.1206  0.4584130 
xgbt_full <- train(Volume~., data= train_full, method = "xgbTree",
                   trControl=ctrl)

# 272.5965  0.4140228 
svmfull_fit1 <- train(Volume~., data= train_full, method = "svmLinear",
                      preProcess ="scale", trControl=ctrl)
#LM prediction####
lmPR <- predict(lmva4, test_corr)
lmc<- postResample(lmPR, test_corr$Volume)

#Residuals vs Fitted, subset outlinar rows.
autoplot(lmva4)

Data_corr_sel <- Data_corr[-c(15,34,39),]
Data_corr_sel<-Data_corr_sel[,-10]
#plot to compare the predicted values (95% CI) with actual values####
lmaf <- predict(lmva4, Data_corr_sel)
summary(lmaf)
err <- predict(lmva4, Data_corr_sel, se = TRUE)
Data_corr_sel$ucl <- err$fit + 1.96 * err$se.fit
Data_corr_sel$lcl <- err$fit - 1.96 * err$se.fit


ggplot(Data_corr_sel)+
  geom_point(aes(x=Volume, y = lmaf, color = Product_type), size = 2)+
    geom_smooth( aes(x=Volume, y=lmaf, ymin=lcl, ymax=ucl), size = 1.5, 
                     colour = "red", se = TRUE, stat = "smooth")

applypd <- predict(lmva4, neData)
neData$predict <- round(applypd)
neData$predict[neData$predict <0 ] <- 0
neData

ggplot(data=neData)+
  geom_boxplot(aes(y = neData$predict, x = neData$Product_type))


#Impact assessment####
head(Data_corr_sel)
PC <- Data_corr_sel[which(Data_corr_sel$Product_type %in% "PC"),]
Smartph <-  Data_corr_sel[which(Data_corr_sel$Product_type %in% "Smartphone"),]
Laptop <-  Data_corr_sel[which(Data_corr_sel$Product_type %in% "Laptop"),]
Netbook <- Data_corr_sel[which(Data_corr_sel$Product_type %in% "Netbook"),]

lmtypePC <- lm(Volume~ X5Stars + X4Stars+ Positive_service_review + Competitors 
               + Profit_margin, PC)
lmtypeSmartph <- lm(Volume~ X5Stars + X4Stars+ Positive_service_review + Competitors 
                    + Profit_margin, Smartph)

lmtypelapTop <- lm(Volume~ X5Stars + X4Stars+ Positive_service_review + Competitors 
                   + Profit_margin, Laptop)

ggplot(data = PC)+
  geom_point(aes(x= Positive_service_review, y = Volume, color = Competitors))

lmtypeNetbook <- lm(Volume~ X5Stars + X4Stars+ Positive_service_review + Competitors 
                    + Profit_margin, Netbook)

relPC <-calc.relimp(lmtypePC, type = c("lmg"), rela = TRUE)
relSmart <-calc.relimp(lmtypeSmartph, type = c("lmg"), rela = TRUE)
relLaptop <-calc.relimp(lmtypelapTop, type = c("lmg"), rela = TRUE)
relNetb <- calc.relimp(lmtypeNetbook, type = c("lmg"), rela = TRUE)

relImportance <- cbind(relPC$lmg,relSmart$lmg,relLaptop$lmg,relNetb$lmg)
colnames(relImportance) <- c("PC", "Smartphone", "Laptop","Netbook")



relImportance2 <- as.matrix(relImportance)
barplot(relImportance2,
        legend = rownames(relImportance2), xlab = "Product_Type")

relImportance<- as.data.frame(relImportance)
install.packages("reshape2")
library(reshape2)
relImportance$Variable <- rownames(relImportance)
dataTrans <- melt(relImportance, id.vars = c("Variable"),value.name = "proportion")
names(dataTrans)[2] <- paste("Product_Type")

ggplot(data = dataTrans) + 
  geom_bar(aes(y=proportion,
               x = Product_Type,
               fill = Variable),
           stat = "identity")