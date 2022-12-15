#20 Seasons of mvp data

#################################################################
##########         Importing/Cleaning data          #############
#################################################################
setwd("C:/Users/jpsch/OneDrive/Desktop/R_Project/capstone_data")

mvp03 <- read.csv("sportsref_download3.csv")
mvp04 <- read.csv("sportsref_download4.csv")
mvp05 <- read.csv("sportsref_download5.csv")
mvp06 <- read.csv("sportsref_download6.csv")
mvp07 <- read.csv("sportsref_download7.csv")
mvp08 <- read.csv("sportsref_download8.csv")
mvp09 <- read.csv("sportsref_download9.csv")
mvp10 <- read.csv("sportsref_download10.csv")
mvp11 <- read.csv("sportsref_download11.csv")
mvp12 <- read.csv("sportsref_download12.csv")
mvp13 <- read.csv("sportsref_download13.csv")
mvp14 <- read.csv("sportsref_download14.csv")
mvp15 <- read.csv("sportsref_download15.csv")
mvp16 <- read.csv("sportsref_download16.csv")
mvp17 <- read.csv("sportsref_download17.csv")
mvp18 <- read.csv("sportsref_download18.csv")
mvp19 <- read.csv("sportsref_download_19.csv")
mvp20 <- read.csv("sportsref_download_20.csv")
mvp21 <- read.csv("sportsref_download_21.csv")
mvp22 <- read.csv("sportsref_download_22.csv")


#Removing NA line from code
mvp03 <- mvp03[1:13,]
mvp04 <- mvp04[1:16,]
mvp05 <- mvp05[1:16,]
mvp06 <- mvp06[1:11,]
mvp07 <- mvp07[1:17,]
mvp08 <- mvp08[1:17,]
mvp09 <- mvp09[1:12,]
mvp10 <- mvp10[1:15,]
mvp11 <- mvp11[1:13,]
mvp12 <- mvp12[1:15,]
mvp13 <- mvp13[1:16,]
mvp14 <- mvp14[1:17,]
mvp15 <- mvp15[1:12,]
mvp16 <- mvp16[1:10,]
mvp17 <- mvp17[1:11,]
mvp18 <- mvp18[1:13,]
mvp19 <- mvp19[1:12,]
mvp20 <- mvp20[1:12,]
mvp21 <- mvp21[1:15,]
mvp22 <- mvp22[1:12,]

rankdata <- read.csv("mvptotal.csv")


#combining into one df
mvptotal <- rbind(mvp03, mvp04, mvp05, mvp06, mvp07, mvp08, mvp09, 
      mvp10, mvp11, mvp12, mvp13, mvp14, mvp15, mvp16,
      mvp17, mvp18, mvp19, mvp20, mvp21, mvp22)

mvptotalr <- cbind(mvptotal, rankdata$Rank)

#View(mvptotalr)

library(neuralnet)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Randomizes the data
set.seed(13579)
mvpRand= mvptotalr[sample(1:nrow(mvptotalr)), ]


#Removes any non-numerical vectors from df
mvpRandPlyr <- cbind(mvpRand[,3], mvpRand[,5:22])
colnames(mvpRandPlyr)
names(mvpRandPlyr)[names(mvpRandPlyr) == "mvpRand[, 3]"] <- "Age"
names(mvpRandPlyr)[names(mvpRandPlyr) == "rankdata$Rank"] <- "Rank"


#View(mvpRandPlyr)

#str(mvpRandPlyr)

mvpRandPlyr[is.na(mvpRandPlyr)] = 0

#Normalizes df
mvp_norm <- as.data.frame(lapply(mvpRandPlyr, normalize))

#Partition
sample_size = floor(0.7*nrow(mvp_norm))
mvp_train <- mvp_norm[1:sample_size, ]
mvp_test <- mvp_norm[sample_size:nrow(mvp_norm), ]

#View(mvpRandPlyr)
#View(mvp_train)





#################################################################
##############         Neural Network          ##################
#################################################################

#Neural Net Model
set.seed(67676)
mvp_model <- neuralnet(Share ~ Age + G + MP + PTS + TRB + AST + STL + BLK +
                         FG. + X3P. + FT. + WS + WS.48 + TmWin, data = mvp_train)

set.seed(67676)

mvp_model <- neuralnet(Share ~ Age + G + MP + PTS + TRB + AST + STL + BLK +
                         FG. + X3P. + FT. + WS + WS.48 + TmWin, data = mvp_train,
                       hidden =1, threshold = 0.02, stepmax = 1e+05, rep = 2, 'sag')

olden(mvp_model)
garson(mvp_model)
#plot(mvp_model)

##Training Results
mvp_trainresults <- compute(mvp_model, mvp_train)
predicted_train_mvp <- mvp_trainresults$net.result
cor(predicted_train_mvp, mvp_train$Share)
### 0.8123

mvp_testresults <- compute(mvp_model, mvp_test)
predicted_test_mvp <- mvp_testresults$net.result
cor(predicted_test_mvp, mvp_test$Share)
### 0.7828


#################################################################
##############               KNN               ##################
#################################################################


#Install class package
#install.packages('class')
# Load class package
library(class)

mvp2<-mvpRandPlyr
for (x in 1:275){
  if (mvp2[x,19]>1){
    mvp2[x,19]<-0 
  }
}
mvp2$Rank<-factor(mvp1$Rank,levels=c(1,0))

#Partition
sample_size = floor(0.7*nrow(mvp2))
mvp_train <- mvp2[1:sample_size, ]
mvp_test <- mvp2[sample_size:nrow(mvp2), ]

mvp_train_knn <- mvp_train[,c(1,6:17)]
mvp_train_knn

mvp_test_knn <- mvp_test[,c(1,6:17)]
mvp_test_knn

knn <- knn(train=mvp_train_knn, test=mvp_test_knn, cl=mvp_train$Rank, k = 5)
knntrain <- knn(train=mvp_train_knn, test=mvp_train_knn, cl=mvp_train$Rank, k = 5)

#Calculate the proportion of correct classification for k = 26, 27
accuracytrain <- sum(mvp_train$Rank == knn)/NROW(mvp_train$Rank)
accuracy <- sum(mvp_test$Rank == knn)/NROW(mvp_test$Rank)

accuracy
accuracytrain

x <- c(2,3,4,5,6,7,8,9,10)

for(i in range(x)){
  knn_k <- knn(train=mvp_train_knn, test=mvp_test_knn, cl=mvp_train$Rank, k=i)
  acc <- 100 * sum(mvp_test$Rank == knn_k)/NROW(mvp_test$Rank)
  print(i)
  print(acc)
}


#################################################################
##############               SVM               ##################
#################################################################


#library(pROC)
require(e1071)
#svm model (linear)
set.seed(67676)
svmlin <-svm(Share~Age + G + MP + PTS + TRB + AST + STL + BLK +
            FG. + X3P. + FT. + WS + WS.48 + TmWin ,data=mvp_train, kernel="radial", gamma = 1/55, cost = 1.1, epsilon = .065)

#training results
pred_train_svmlin<-predict(svmlin,newdata=mvp_train)
cor(pred_train_svmlin,mvp_train$Share)
#0.74
#test results
pred_test_svmlin<-predict(svmlin,newdata=mvp_test)
cor(pred_test_svmlin,mvp_test$Share)
#0.66

##svm model (polynomial)
svmpoly <-svm(Share~Age + G + MP + PTS + TRB + AST + STL + BLK +
               FG. + X3P. + FT. + WS + WS.48 + TmWin ,data=mvp_train, kernel = "polynomial", degree = 3)

#training results
pred_train_svmpoly<-predict(svmpoly,newdata=mvp_train)
cor(pred_train_svmpoly,mvp_train$Share)
#0.8717
#test results
pred_test_svmpoly<-predict(svmpoly,newdata=mvp_test)
cor(pred_test_svmpoly,mvp_test$Share)
#0.8012

##svm model (radial)
svmrad <-svm(Share~Age + G + MP + PTS + TRB + AST + STL + BLK +
               FG. + X3P. + FT. + WS + WS.48 + TmWin ,data=mvp_train, kernel = "radial")

#training results
pred_train_svmrad<-predict(svmrad,newdata=mvp_train)
cor(pred_train_svmrad,mvp_train$Share)
#0.89
#test results
pred_test_svmrad<-predict(svmrad,newdata=mvp_test)
cor(pred_test_svmrad,mvp_test$Share)
#0.71


##svm model sigmoid
##svm model (radial)
svmsig <-svm(Share~Age + G + MP + PTS + TRB + AST + STL + BLK +
               FG. + X3P. + FT. + WS + WS.48 + TmWin ,data=mvp_train, kernel = "sigmoid")

#training results
pred_train_svmsig<-predict(svmsig,newdata=mvp_train)
cor(pred_train_svmsig,mvp_train$Share)
#0.89
#test results
pred_test_svmsig<-predict(svmsig,newdata=mvp_test)
cor(pred_test_svmrad,mvp_test$Share)
#0.71


#################################################################
############          Linear Regression          ################
#################################################################


lm<-lm(Share~Age + G + MP + PTS + TRB + AST + STL + BLK +
         FG. + X3P. + FT. + WS + WS.48 + TmWin , data = mvp_train)
lm<-lm(Share~PTS + AST + FT. + WS + WS.48 + TmWin , data = mvp_train)
summary(lm)

#training results
lm_predict_train<-predict(lm,mvp_train[,-2])
cor(lm_predict_train,mvp_train$Share)
#.7222

#testing results
lm_predict_test<-predict(lm,mvp_test[,-2])
cor(lm_predict_test,mvp_test$Share)
#.7118


#################################################################
##############          Decision Tree          ##################
#################################################################


#decision tree model
library(C50)
#use mvp rank as classification
mvp1<-mvpRandPlyr
for (x in 1:275){
  if (mvp1[x,19]>1){
    mvp1[x,19]<-0 
  }
}
mvp1$Rank<-factor(mvp1$Rank,levels=c(1,0))
#Partition
sample_size = floor(0.7*nrow(mvp1))
mvp_train <- mvp1[1:sample_size, ]
mvp_test <- mvp1[sample_size:nrow(mvp1), ]

mvp_train <- mvp_train[,c(-2,-3,-4,-5)]
mvp_test <- mvp_test[,c(-2,-3,-4,-5)]

mvp_train$Rank<-factor(mvp_train$Rank,levels=c(1,0))
cart_model<-C5.0(mvp_train[,-15],mvp_train$Rank,trials=10)
cart_model

cart_pred_train<-predict(cart_model,mvp_train)
cart_pred_train
table(predict=cart_pred_train,true=mvp_train$Rank)
#error rate on training

cor(as.numeric(cart_pred_train), as.numeric(mvp_train$Rank))
accuracy<-sum(cart_pred_train==mvp_train$Rank)/nrow(mvp_train)
errors=1-signif(accuracy,2)
errors
#.0

cart_pred<-predict(cart_model,mvp_test)
table(predict=cart_pred,true=mvp_test$Rank)
#error rate on testing
accuracy<-sum(cart_pred==mvp_test$Rank)/nrow(mvp_test)
errors=1-signif(accuracy,2)
errors
#.07

cbind(cart_pred, mvp_test$Rank)
cart_pred
mvp_test$Rank


#################################################################
#######           Prediction for Current Season          ########
#################################################################

setwd("C:/Users/jpsch/OneDrive/Desktop/R_Project/capstone_data")

mvp_new <- read.csv("pred_set.csv")
View(mvp_new)
mvp_new <- mvp_new[,-2]
mvp_new <- mvp_new[,-3]

View(mvp_new)

set.seed(13579)
mvpnew= mvp_new#[sample(1:nrow(mvp_new)), ]


#Normalizes df
mvpnew_norm <- as.data.frame(lapply(mvpnew, normalize))


########### Neural Net ###############
mvp_testresults <- predict(mvp_model, mvpnew_norm)
mvp_testresults
round(mvp_testresults)
predicted_test_mvp <- mvp_testresults$net.result
predicted_test_mvp
cor(predicted_test_mvp, mvp_test$Share)
### 0.7828

pred_train_svmlin<-predict(svmlin,newdata=mvpnew_norm)
pred_train_svmlin

cart_pred_train<-predict(cart_model,mvpnew[,-1])
mvpnew
cart_pred_train
table(predict=cart_pred,true=mvp_test$Rank)


knn <- knn(train=mvp_train_knn, test=mvpnew[,c(-1,-15)], cl=mvp_train$Rank, k = 3)
knn
