rm(list = ls())
getwd()
setwd("/Users/hxu3/Desktop/RIMES/UTMB/COVID.diagnosis.train.imp.cart.df.csv~1/")


FILE.TRAIN <- "COVID.diagnosis.train.imp.cart.df.csv"
FILE.TEST  <- "COVID.diagnosis.test.imp.cart.df.csv"

dat.train <- read.csv(file = FILE.TRAIN, header = T, sep = ",")
dat.test  <- read.csv(file = FILE.TEST, header = T, sep = ",")

dat.train <- dat.train[ ,-1]
dat.test <-  dat.test[ , -1]


top20ICDcodes <- c(which(colnames(dat.train)=="Z20.828"), which(colnames(dat.train)=="R05"), which(colnames(dat.train)=="R50.9"), 
                   which(colnames(dat.train)=="I10"), which(colnames(dat.train)=="R07.9"), which(colnames(dat.train)=="J18.9"),
                   which(colnames(dat.train)=="J06.9"), which(colnames(dat.train)=="R06.00"),which(colnames(dat.train)=="R09.02"), 
                   which(colnames(dat.train)=="Z11.59"), which(colnames(dat.train)=="B34.9"), which(colnames(dat.train)=="N17.9"),
                   which(colnames(dat.train)=="J02.9"), which(colnames(dat.train)=="R19.7"), which(colnames(dat.train)=="R07.89"),
                   which(colnames(dat.train)=="R53.1"), which(colnames(dat.train)=="R52"), which(colnames(dat.train)=="E11.9"),
                   which(colnames(dat.train)=="I50.9"), which(colnames(dat.train)=="R06.02"))

dat.train <- dat.train[,c(1:39,top20ICDcodes)]
dat.test  <- dat.test[ ,c(1:39,top20ICDcodes)]

dat.train$Z20.828 <- as.factor(dat.train$Z20.828 )
dat.train$R05 <- as.factor(dat.train$R05)
dat.train$R50.9 <- as.factor(dat.train$R50.9)
dat.train$I10 <- as.factor(dat.train$I10)
dat.train$R07.9 <- as.factor(dat.train$R07.9)
dat.train$J18.9 <- as.factor(dat.train$J18.9)
dat.train$J06.9 <- as.factor(dat.train$J06.9)
dat.train$R06.00 <- as.factor(dat.train$R06.00)
dat.train$R09.02 <- as.factor(dat.train$R09.02)
dat.train$Z11.59 <- as.factor(dat.train$Z11.59)
dat.train$B34.9 <- as.factor(dat.train$B34.9)
dat.train$N17.9 <- as.factor(dat.train$N17.9)
dat.train$J02.9 <- as.factor(dat.train$J02.9)
dat.train$R19.7 <- as.factor(dat.train$R19.7)
dat.train$R07.89 <- as.factor(dat.train$R07.89)
dat.train$R53.1 <- as.factor(dat.train$R53.1)
dat.train$R52 <- as.factor(dat.train$R52)
dat.train$E11.9 <- as.factor(dat.train$E11.9)
dat.train$I50.9 <- as.factor(dat.train$I50.9)
dat.train$R06.02 <- as.factor(dat.train$R06.02)


dat.test$Z20.828 <- as.factor(dat.test$Z20.828 )
dat.test$R05 <- as.factor(dat.test$R05)
dat.test$R50.9 <- as.factor(dat.test$R50.9)
dat.test$I10 <- as.factor(dat.test$I10)
dat.test$R07.9 <- as.factor(dat.test$R07.9)
dat.test$J18.9 <- as.factor(dat.test$J18.9)
dat.test$J06.9 <- as.factor(dat.test$J06.9)
dat.test$R06.00 <- as.factor(dat.test$R06.00)
dat.test$R09.02 <- as.factor(dat.test$R09.02)
dat.test$Z11.59 <- as.factor(dat.test$Z11.59)
dat.test$B34.9 <- as.factor(dat.test$B34.9)
dat.test$N17.9 <- as.factor(dat.test$N17.9)
dat.test$J02.9 <- as.factor(dat.test$J02.9)
dat.test$R19.7 <- as.factor(dat.test$R19.7)
dat.test$R07.89 <- as.factor(dat.test$R07.89)
dat.test$R53.1 <- as.factor(dat.test$R53.1)
dat.test$R52 <- as.factor(dat.test$R52)
dat.test$E11.9 <- as.factor(dat.test$E11.9)
dat.test$I50.9 <- as.factor(dat.test$I50.9)
dat.test$R06.02 <- as.factor(dat.test$R06.02)


scaled.dat.train <- data.frame( apply(dat.train[,-c(1:2,4,40:59)], 2, scale, center=T, scale=T) )
scaled.dat.test  <- data.frame( apply(dat.test[, -c(1:2,4,40:59)], 2, scale, center=T, scale=T) )

dataNEW.train <- cbind.data.frame(dat.train$SEX, dat.train$ETHNICITY, scaled.dat.train, dat.train$Z20.828, dat.train$R05, dat.train$R50.9, 
                                  dat.train$I10, dat.train$R07.9, dat.train$J18.9,dat.train$J06.9, dat.train$R06.00, dat.train$R09.02, 
                                  dat.train$Z11.59, dat.train$B34.9, dat.train$N17.9,dat.train$J02.9, dat.train$R19.7, dat.train$R07.89, 
                                  dat.train$R53.1, dat.train$R52, dat.train$E11.9, dat.train$I50.9, dat.train$R06.02, dat.train$PCR)

colnames(dataNEW.train) <-  c(colnames(dat.test)[-4], "PCR")

dataNEW.test <- cbind.data.frame(dat.test$SEX, dat.test$ETHNICITY, scaled.dat.test, dat.test$Z20.828, dat.test$R05, dat.test$R50.9, 
                                 dat.test$I10, dat.test$R07.9, dat.test$J18.9,dat.test$J06.9, dat.test$R06.00, dat.test$R09.02, 
                                 dat.test$Z11.59, dat.test$B34.9, dat.test$N17.9,dat.test$J02.9, dat.test$R19.7, dat.test$R07.89, 
                                 dat.test$R53.1, dat.test$R52, dat.test$E11.9,dat.test$I50.9, dat.test$R06.02, dat.test$PCR)

colnames(dataNEW.test) <- colnames(dataNEW.train)
dat.train <- dataNEW.train
dat.test <- dataNEW.test

dat.train$PCR = ifelse(dat.train$PCR == "Positive", "Positive", "NotDetected")
dat.test$PCR  = ifelse(dat.test$PCR == "Positive", "Positive", "NotDetected")

dat.train$PCR = factor(dat.train$PCR, levels =  c("NotDetected", "Positive"), labels = c("NotDetected", "Positive"))
dat.test$PCR  = factor(dat.test$PCR,  levels =  c("NotDetected", "Positive"), labels = c("NotDetected", "Positive"))

dat.test.label <- ifelse(dat.test$PCR == "Positive",1,0)


library(verification)
library(randomForest)
library(caret)
library(cvAUC)
library(pROC)

set.seed(23)
NTREE <- sample(100:500, 20, replace=F)
k.rf <- 0*c()
for (i in 1:length(NTREE)) {
  rf.model <- randomForest(PCR~., data = dat.train, ntree = NTREE[i], mtry = floor(sqrt(ncol(dat.train))), importance = T)
  rf.pred  <- predict(rf.model, dat.test)
  k.rf[i]  <- unlist(confusionMatrix(dat.test$PCR, rf.pred)$overall[1])
}

MaxIndex <- which(k.rf == max(k.rf))
nval <- NTREE[MaxIndex[1]]
rf.modeln <- randomForest(PCR~., data = dat.train, ntree = nval, mtry = floor(sqrt(ncol(dat.train))), importance = T)
rf.predn  <- predict(rf.modeln, dat.test)
rf.predn.prob  <- predict(rf.modeln, dat.test, type = "prob")
confusionMatrix(dat.test$PCR, rf.predn, positive = "Positive")

# ===========================
# ROC CURVE AND AUC VALUE
# ===========================

(a.ROC <- roc.area(obs = dat.test.label, pred = rf.predn.prob[,2])$A)
mod.rf <- verify(obs=dat.test.label, pred = rf.predn.prob[,2])
rfroc <- roc.plot(mod.rf, plot.thres = NULL,main ="ROC Curve from Random Forest")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)
rflmdat <- data.frame(rfroc$plot.data)

library(KernSmooth)
fit <- locpoly(rflmdat$X3, rflmdat$X2, bandwidth = 0.05)
plot(rflmdat$X3, rflmdat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fit)

xpred <- 0.2
R1 <- fit$y[which.min(abs(fit$x - xpred))]

#############################FULL DATA  #######################################
###############################################################################
##############################Bagging model ###################################

k.bag <- 0*c()
for (i in 1:length(NTREE)) {
  bag.model <- randomForest(PCR~., data = dat.train, ntree = NTREE[i], mtry = ncol(dat.train)-1, importance = T)
  bag.pred  <- predict(bag.model, dat.test)
  k.bag[i] <- unlist(confusionMatrix(dat.test$PCR, bag.pred)$overall[1])
}

MaxIndex <- which(k.bag == max(k.bag))
nval <- NTREE[MaxIndex[1]]

bag.modeln <- randomForest(PCR~., data = dat.train, ntree = nval, mtry = ncol(dat.train)-1, importance = T)
bag.modeln
bag.predn <- predict(bag.modeln, dat.test)
bag.predn.prob <- predict(bag.modeln, dat.test, type = "prob")
confusionMatrix(dat.test$PCR,  bag.predn, positive = "Positive")

# ===========================
# ROC CURVE AND AUC VALUE
# ===========================

(a.ROCbag <- roc.area(obs = dat.test.label, pred = bag.predn.prob[,2])$A)
mod.bagg <- verify(obs=dat.test.label, pred = bag.predn.prob[,2])
bagroc <- roc.plot(mod.bagg, plot.thres = NULL,main ="ROC Curve from BAGGING")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROCbag, digits=4), 
                         sep=" "), col="blue", cex=1)

baglmdat <- data.frame(bagroc$plot.data)
fitbag <- locpoly(baglmdat$X3, baglmdat$X2, bandwidth = 0.05)
plot(baglmdat$X3, baglmdat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitbag)

xpred <- 0.2
R2 <- fitbag$y[which.min(abs(fitbag$x - xpred))]

####################################################################
##############SUPPORT VECTOR MACHINE###############################
###################################################################

dat.train$PCR = ifelse(dat.train$PCR == "Positive", "Positive", "NotDetected")
dat.test$PCR  = ifelse(dat.test$PCR == "Positive", "Positive", "NotDetected")

dat.train$PCR = factor(dat.train$PCR, levels =  c("NotDetected", "Positive"), labels = c("NotDetected", "Positive"))
dat.test$PCR  = factor(dat.test$PCR,  levels =  c("NotDetected", "Positive"), labels = c("NotDetected", "Positive"))

dat.test.label <- ifelse(dat.test$PCR == "Positive",1,0)

# ########################################################
# AN EXAMPLE OF SVM CLASSIFICATION WITH R PACKAGE caret
# ########################################################
library(caret)
library(kernlab)
library(verification)

# SVM I: LINEAR
# ----------------

trctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3)

set.seed(3233)
svm_Linear <- train(PCR~., data = dat.train, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
svm_Linear$finalModel

# PREDICTION
test_pred <- predict(svm_Linear, newdata = dat.test)
confusionMatrix(test_pred, dat.test$PCR, positive = "Positive")

# Selecting the OPTIMAL C value(Cost) in Linear SVM classifier
bestgrid <- expand.grid(C = c(0.01) )

set.seed(111)
svm_Linear_Grid <- train(PCR~., data = dat.train, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = bestgrid, tuneLength = 10) 
svm_Linear_Grid
svm_Linear_Grid$bestTune
svm_Linear_Grid$finalModel
plot(svm_Linear_Grid)
# PREDICTION
test_pred_grid <- predict(svm_Linear_Grid, newdata = dat.test)
confusionMatrix(test_pred_grid, dat.test$PCR , positive = "Positive")


# OBTAIN ROC CURVE AND AUC FOR SVM
trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)
fit.LinearSVM.tuned <- train(PCR~., data = dat.train, method = "svmLinear",
                             trControl=trctr2, preProcess = c("center", "scale"),
                             tuneGrid = bestgrid, tuneLength = 10, 
                             metric = "ROC") 
max(fit.LinearSVM.tuned$results[,"ROC"])  # MAXIMUM AUC FROM TRAINING DATA

pred.prob <- predict(fit.LinearSVM.tuned, newdata = dat.test, type = "prob")
(linear.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.lin <- verify(obs=dat.test.label, pred = pred.prob[,2])
svml_roc <- roc.plot(mod.lin, plot.thres = NULL,main ="ROC Curve from SVM LINEAR")
abline(v = 0.2, col="red", lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(linear.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)



library(KernSmooth)
mod.lindat <- data.frame(svml_roc$plot.data)
fitlinsvm <- locpoly(mod.lindat$X3, mod.lindat$X2, bandwidth = 0.05)
plot(mod.lindat$X3, mod.lindat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitlinsvm)

xpred <- 0.2
R3 <- fitlinsvm$y[which.min(abs(fitlinsvm$x - xpred))]

# SVM II: Radial Basis Kernel
# ---------------------------

set.seed(3233)
svm_Radial <- train(PCR~., data = dat.train, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial$bestTune
svm_Radial$finalModel
svm_Radial
plot(svm_Radial)

# PREDICTION
test_pred_Radial <- predict(svm_Radial, newdata = dat.test)
confusionMatrix(test_pred_Radial, dat.test$PCR, positive = "Positive")

#grid_radial <- expand.grid(sigma = c(0.001, 0.01, 0.1, 1, 10), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2))
bestgrid_radial <- expand.grid(sigma = c(0.01), C = c(0.5) )

set.seed(321)
svm_Radial_Grid <- train(PCR~., data = dat.train, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = bestgrid_radial,
                         tuneLength = 10)
svm_Radial_Grid$bestTune
svm_Radial_Grid$finalModel
svm_Radial_Grid
plot(svm_Radial_Grid)

# PREDICTION
test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = dat.test)
confusionMatrix(test_pred_Radial_Grid, dat.test$PCR, positive = "Positive")

# OBTAIN ROC CURVE AND AUC FOR SVM
trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)

fit.RadialSVM.tuned <- train(PCR~., data = dat.train, method = "svmRadial",
                             trControl=trctr2, preProcess = c("center", "scale"),
                             tuneGrid = bestgrid_radial,  tuneLength = 3, 
                             metric = "ROC") 
max(fit.RadialSVM.tuned$results[,"ROC"])  # MAXIMUM AUC FROM TRAINING DATA

# PREDICTION WITH TESTING DATA
pred.prob <- predict(fit.RadialSVM.tuned, newdata = dat.test, type = "prob")
(rbf.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.rbf <- verify(obs=dat.test.label, pred = pred.prob[,2])
svmrad_roc <- roc.plot(mod.rbf, plot.thres = NULL,main ="ROC Curve from SVM RADIAL BASIS")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(rbf.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)



mod.rbfdat <- data.frame(svmrad_roc$plot.data)
fitrbfsvm <- locpoly(mod.rbfdat$X3, mod.rbfdat$X2, bandwidth = 0.05)
plot(mod.rbfdat$X3, mod.rbfdat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitrbfsvm)

xpred <- 0.2
R4 <- fitrbfsvm$y[which.min(abs(fitrbfsvm$x - xpred))]

# SVM III: POLYNOMIAL KERNEL
# ---------------------------

trctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3)
set.seed(3239)
svm_Poly <- train(PCR~., data = dat.train, method = "svmPoly",
                  trControl=trctrl,
                  preProcess = c("center", "scale"))
svm_Poly
svm_Poly$bestTune #bestparameters
svm_Poly$finalModel#Training missclassification error

# PREDICTION
test_pred <- predict(svm_Poly, newdata = dat.test)
confusionMatrix(test_pred, dat.test$PCR, positive = "Positive")

bestgrid_Poly <- expand.grid(degree = c(2), scale = c(0.01), C = c(0.5) )

set.seed(1191)
svm_Poly_Grid <- train(PCR~., data = dat.train, method = "svmPoly",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneGrid = bestgrid_Poly) 
svm_Poly_Grid
svm_Poly_Grid$bestTune
svm_Poly_Grid$finalModel #Training missclassification error
plot(svm_Poly_Grid)

# PREDICTION
test_pred_grid <- predict(svm_Poly_Grid, newdata = dat.test)
confusionMatrix(test_pred_grid, dat.test$PCR )


# OBTAIN ROC CURVE AND AUC FOR SVM
trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)
fit.PolySVM.tuned <- train(PCR~., data = dat.train, method = "svmPoly",
                           trControl=trctr2, preProcess = c("center", "scale"),
                           tuneGrid = bestgrid_Poly, metric = "ROC") 
max(fit.PolySVM.tuned$results[,"ROC"])  # MAXIMUM AUC FROM TRAINING DATA

# PREDICTION WITH TESTING DATA
pred.prob <- predict(fit.PolySVM.tuned, newdata = dat.test, type = "prob")

(poly.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.poly <- verify(obs = dat.test.label, pred = pred.prob[,2])
svmpoly_roc <- roc.plot(mod.poly, plot.thres = NULL,main ="ROC Curve from SVM POLYNOMIAL")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(poly.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)



mod.polydat <- data.frame(svmpoly_roc$plot.data)
fitpolysvm <- locpoly(mod.polydat$X3, mod.polydat$X2, bandwidth = 0.05)
plot(mod.polydat$X3, mod.polydat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitpolysvm)

xpred <- 0.2
R5 <- fitpolysvm$y[which.min(abs(fitpolysvm$x - xpred))]
