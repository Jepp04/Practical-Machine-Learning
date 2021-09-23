library(e1071)
library(forecast)
library(caret)
library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

# Set Environment
set.seed(1234)

# Load Data
Data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
TestCases <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

# Preprocessing
Data <- Data[,-c(1,3,4,5)]
Data <- Data[, colSums(is.na(Data)) == 0]
Data <- Data[,-nearZeroVar(Data)]

# Set Train Control for Cross validation
trainC <- trainControl(method = "cv", number = 5, allowParallel = T)

# Slicing Data
Partition <- createDataPartition(y = Data$classe,p = 0.80,list = FALSE)

Training <- data.frame( Data[Partition,])
Testing <- data.frame(Data[-Partition,])

# train and validation
## Linear Model
# trainSet<- Training[ ,c("total_accel_belt",
#                         "total_accel_arm",
#                         "total_accel_dumbbell",
#                         "total_accel_forearm",
#                         "classe")]
modFit_rf <- train(classe~.,method = "rf",data = Training, trControl = trainC)
modFit_gbm <- train(classe~.,method = "gbm" ,data = Training, trControl = trainC,verbose = FALSE)
modFit_lda <- train(classe~.,method = "lda" ,data = Training, trControl = trainC)

predict_rf <- predict(modFit_rf,Testing)
predict_gbm <- predict(modFit_gbm,Testing)
predict_lda <- predict(modFit_lda,Testing)

accuracy(predict_rf, Testing$classe)
accuracy(predict_gbm, Testing$classe)
accuracy(predict_lda, Testing$classe)

confusionMatrix(predict_rf,predict_gbm)

stopCluster(cluster)