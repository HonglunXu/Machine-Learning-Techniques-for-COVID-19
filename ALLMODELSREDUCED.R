rm(list = ls())
getwd()
setwd("/Users/hxu3/Desktop/RIMES/UTMB/COVID.diagnosis.train.imp.cart.df.csv~1/")
#list.files()

FILE.TRAIN <- "train.reduced.data.1.csv"
FILE.TEST  <- "test.reduced.data.1.csv"

training <- read.csv(file = FILE.TRAIN, header = T, sep = ",")
testing  <- read.csv(file = FILE.TEST, header = T,  sep = ",")


training <- training[ ,-1]
testing <-  testing[ , -1]

dim(testing)
dim(training)

anyNA(training)
anyNA(testing)

table(training$PCR)

table(testing$PCR)



training$PCR = factor(training$PCR, levels = c(0L, 1L), labels = c("NotDetected", "Positive"))
testing$PCR  = factor(testing$PCR,  levels = c(0L, 1L), labels = c("NotDetected", "Positive"))

training$ETHNICITY = factor(training$ETHNICITY, levels = c(0L, 1L), labels = c("NOTHISPANIC", "HISPANIC"))
testing$ETHNICITY  = factor(testing$ETHNICITY,  levels = c(0L, 1L), labels = c("NOTHISPANIC", "HISPANIC"))


training$I10 = factor(training$I10 , levels = c(0L, 1L), labels = c("FALSE", "TRUE"))
testing$I10  = factor(testing$I10 ,  levels = c(0L, 1L), labels = c("FALSE", "TRUE"))


dat.test.label <- ifelse(testing$PCR == "Positive",1,0)

library(randomForest)
library(caret)
library(verification)
library(ipred)

#trctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3)

#set.seed(323)
#mtry = floor(sqrt(ncol(training)))
#tunegrid <- expand.grid(.mtry = c((mtry-2): (mtry+2) ))

#NTREE <- sample(100:500, 10, replace=F)
#NTREE <- 700
#tunegrid <- expand.grid(.mtry = mtry)
#Rf <- train(PCR~., data = training, method = "rf",
#                    trControl=trctrl,
#                   preProcess = c("center", "scale"),
#                    tuneLength = 10, tuneGrid = tunegrid, ntree = NTREE)
#print(Rf)
#Rf$bestTune
#Rf$finalModel
#plot(Rf)




# PREDICTION
#test_pred <- predict(Rf, newdata = testing)
#confusionMatrix(test_pred, testing$PCR)



#pred.prob <- predict(Rf, newdata=testing, type = "prob")

#(Rf.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
#mod.rf <- verify(obs=dat.test.label, pred = pred.prob[,2])
#roc.plot(mod.rf, plot.thres = NULL)
#text(x=0.7, y=0.2, paste("Area under ROC =", round(Rf.ROC, digits=3), 
#                         sep=" "), col="blue", cex=2.0)


###############################################################################
##############################Scale data  ###################################
scaled.dat.train <- data.frame( apply(training[,c(2:7)], 2, scale, center=T, scale=T) )
scaled.dat.test  <- data.frame( apply(testing[,c(2:7)],  2, scale, center=T, scale=T) )

finaltraining <- cbind.data.frame(training$ETHNICITY, scaled.dat.train, training$I10, training$PCR)
finaltesting  <- cbind.data.frame(testing$ETHNICITY,  scaled.dat.test,  testing$I10,   testing$PCR)

colnames(finaltraining) <- colnames(training)
colnames(finaltesting)  <- colnames(training)

###############################################################################
##############################R A N D O M F O R E S T##########################
#############################REDUCED DATA. STANDARDIZED########################

set.seed(21)
NTREE <- sample(100:500, 10, replace=F)
k.rf <- 0*c()
for (i in 1:length(NTREE)) {
  rf.model <- randomForest(PCR~., data = finaltraining, ntree = NTREE[i], mtry = floor(sqrt(ncol(finaltraining))), importance = T)
  rf.pred  <- predict(rf.model, finaltesting)
  k.rf[i]  <- unlist(confusionMatrix(finaltesting$PCR, rf.pred)$overall[1])
}

max(k.rf)
MaxIndex <- which(k.rf == max(k.rf))
nval <- NTREE[MaxIndex[1]]

set.seed(20)
rf.model <- randomForest(PCR~., data = finaltraining, ntree = nval, mtry = floor(sqrt(ncol(finaltraining))), importance = T)
print(rf.model)

rf.pred  <- predict(rf.model, finaltesting)
rf.predn.prob  <- predict(rf.model, finaltesting, type = "prob")
confusionMatrix(finaltesting$PCR, rf.pred, positive = "Positive", mode = "everything")



#(a.ROCaa <- roc(dat.test$PCR, rf.predn1[,1], plot = T, grid = T)$auc)
(a.ROC <- roc.area(obs = dat.test.label, pred = rf.predn.prob[,2])$A)
mod.rf <- verify(obs=dat.test.label, pred = rf.predn.prob[,2])#,frcst.type = "prob",obs.type = "binary"
#par(mfrow=c(1,2))
rfroc <- roc.plot(mod.rf, plot.thres = NULL,main ="ROC Curve from Random Forest")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)
write.csv(rf.predn.prob[,2],'Reduced_RF_Y_predict_full.csv')


#rfroc$plot.data
rflmdat <- data.frame(rfroc$plot.data)
#linearmod <- lm(X2 ~ X3, data = rflmdat)
#p <- as.data.frame(0.2)
#colnames(p) <- "X3"
#predvalrf <-predict(linearmod, newdata = p)

library(KernSmooth)
fit <- locpoly(rflmdat$X3, rflmdat$X2, bandwidth = 0.05)
plot(rflmdat$X3, rflmdat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fit)

xpred <- 0.2
R1 <- fit$y[which.min(abs(fit$x - xpred))]


##############################B A G G I N G ###################################
#############################REDUCED DATA. STANDARDIZED########################

k.bag <- 0*c()
for (i in 1:length(NTREE)) {
  bag.model <- randomForest(PCR~., data = finaltraining, ntree = NTREE[i], mtry = ncol(finaltraining)-1, importance = T)
  bag.pred  <- predict(bag.model, finaltesting)
  k.bag[i] <- unlist(confusionMatrix(finaltesting$PCR, bag.pred)$overall[1])
}

max(k.bag)

MaxIndex <- which(k.bag == max(k.bag))
nval <- NTREE[MaxIndex[1]]

#bag.model
set.seed(241)
bag.modeln <- randomForest(PCR~., data = finaltraining, ntree = nval, mtry = ncol(finaltraining)-1, importance = T)
#randomForest::varImpPlot(bag.modeln)
print(bag.modeln)

bag.predn <- predict(bag.modeln, finaltesting)
bag.predn.prob <- predict(bag.modeln, finaltesting, type = "prob")
confusionMatrix(finaltesting$PCR,  bag.predn, positive = "Positive", mode = "everything")


# ===========================
# ROC CURVE AND AUC VALUE
# ===========================


#a.ROCaa <- roc(dat.test$Value, rf.predn1[,2], plot = T, grid = T)
(a.ROCbag <- roc.area(obs = dat.test.label, pred = bag.predn.prob[,2])$A)
mod.bagg <- verify(obs=dat.test.label, pred = bag.predn.prob[,2])
bagroc <- roc.plot(mod.bagg, plot.thres = NULL,main ="ROC Curve from BAGGING")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROCbag, digits=4), 
                         sep=" "), col="blue", cex=1)

write.csv(bag.predn.prob[,2],'Reduced_Bagging_Y_predict_full.csv')

baglmdat <- data.frame(bagroc$plot.data)
fitbag <- locpoly(baglmdat$X3, baglmdat$X2, bandwidth = 0.05)
plot(baglmdat$X3, baglmdat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitbag)

xpred <- 0.2
R2 <- fitbag$y[which.min(abs(fitbag$x - xpred))]


#bagroc$plot.data

# ===========================
# OR USING THIS
# ===========================
#gbag <- bagging(PCR~., data = finaltraining, coob =TRUE, nbagg = 25)
#print(gbag)


#bag.pred  <- predict(gbag, finaltesting)


#bag.pred.prob  <- predict(gbag, finaltesting, type = "prob")
#confusionMatrix(finaltesting$PCR, bag.pred)



#(a.ROCaa <- roc(dat.test$PCR, rf.predn1[,1], plot = T, grid = T)$auc)
#(a.ROC <- roc.area(obs = dat.test.label, pred = bag.pred.prob[,2])$A)
#mod.bag <- verify(obs=dat.test.label, pred = bag.pred.prob[,2])
#roc.plot(mod.bag, plot.thres = NULL)
#text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROC, digits=3), 
#                         sep=" "), col="blue", cex=1)


#####################################
##SUPPORT VECTOR MACHINE ############
#####################################

getwd()
setwd("/Users/hxu3/Desktop/RIMES/UTMB/COVID.diagnosis.train.imp.cart.df.csv~1/")
#list.files()

FILE.TRAIN <- "train.reduced.data.csv"
FILE.TEST  <- "test.reduced.data.csv"

training <- read.csv(file = FILE.TRAIN, header = T, sep = ",")
testing  <- read.csv(file = FILE.TEST, header = T,  sep = ",")


training <- training[ ,-1]
testing <-  testing[ , -1]

dim(testing)
dim(training)

anyNA(training)
anyNA(testing)

table(training$PCR)

table(testing$PCR)



training$PCR = factor(training$PCR, levels = c(0L, 1L), labels = c("NotDetected", "Positive"))
testing$PCR  = factor(testing$PCR,  levels = c(0L, 1L), labels = c("NotDetected", "Positive"))

training$ETHNICITY = factor(training$ETHNICITY, levels = c(0L, 1L), labels = c("NOTHISPANIC", "HISPANIC"))
testing$ETHNICITY  = factor(testing$ETHNICITY,  levels = c(0L, 1L), labels = c("NOTHISPANIC", "HISPANIC"))


training$I10 = factor(training$I10 , levels = c(0L, 1L), labels = c("FALSE", "TRUE"))
testing$I10  = factor(testing$I10 ,  levels = c(0L, 1L), labels = c("FALSE", "TRUE"))


dat.test.label <- ifelse(testing$PCR == "Positive",1,0)

library(caret)
library(kernlab)
library(verification)

# SVM I: LINEAR
# ----------------

trctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3)

set.seed(3233)
svm_Linear <- train(PCR~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
svm_Linear$finalModel

# PREDICTION
test_pred <- predict(svm_Linear, newdata = testing)
confusionMatrix(test_pred, testing$PCR)

# Selecting the OPTIMAL C value(Cost) in Linear SVM classifier
grid <- expand.grid(C =  c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2))#0.1
#bestgrid <- expand.grid(C = c(0.1))


set.seed(111)
svm_Linear_Grid <- train(PCR~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid, tuneLength = 10) 
svm_Linear_Grid$bestTune
svm_Linear_Grid$finalModel
svm_Linear_Grid
plot(svm_Linear_Grid)
# PREDICTION
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
confusionMatrix(test_pred_grid, testing$PCR )


# OBTAIN ROC CURVE AND AUC FOR SVM
trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)
fit.LinearSVM.tuned <- train(PCR~., data = training, method = "svmLinear",
                             trControl=trctr2, preProcess = c("center", "scale"),
                             tuneGrid = grid , tuneLength = 10, 
                             metric = "ROC") 

max(fit.LinearSVM.tuned$results[,"ROC"])  # MAXIMUM AUC FROM TRAINING DATA

# PREDICTION WITH TESTING DATA
pred.prob <- predict(fit.LinearSVM.tuned, newdata=testing, type = "prob")
#require(pROC)  # USING PACKAGE pROC
#ROC <- roc(response=testing$PCR, predictor = pred.prob[, "NotDetected"])
#plot(ROC, col="brown")
#ROC$auc  # AUC
#text(x=0.4, y=0.25, paste("Area Under Curve = ", round(ROC$auc, digits=4), sep=""), col="blue", cex=1.2) 




(linear.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.lin <- verify(obs=dat.test.label, pred = pred.prob[,2])
svml_roc <- roc.plot(mod.lin, plot.thres = NULL,main ="ROC Curve from SVM LINEAR")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(linear.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)
write.csv(pred.prob[,2],'Reduced_Linear_Y_predict_full.csv')

library(KernSmooth)
mod.lindat <- data.frame(svml_roc$plot.data)
fitlinsvm <- locpoly(mod.lindat$X3, mod.lindat$X2, bandwidth = 0.05)
plot(mod.lindat$X3, mod.lindat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitlinsvm)

xpred <- 0.2
R3 <- fitlinsvm$y[which.min(abs(fitlinsvm$x - xpred))]


# SVM II: RBF
# ---------------------------

set.seed(3233)
svm_Radial <- train(PCR~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial$bestTune
svm_Radial$finalModel
svm_Radial
#plot(svm_Radial)

# PREDICTION
test_pred_Radial <- predict(svm_Radial, newdata = testing)
confusionMatrix(test_pred_Radial, testing$PCR)

# SELECTING OPTIMAL SIGMA AND C (COST)
#grid_radial <- expand.grid(sigma = c(0.01, 0.02, 0.025, 0.03, 0.04,
#                                    0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
#                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2))

#grid_radial <- expand.grid(sigma =10^(-6:1), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2))

grid_radial <- expand.grid(sigma = c(0.001, 0.01, 0.1, 1, 10), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2))

#bestgrid_radial <- expand.grid(sigma = c(0.1), C = c(0.1) )
set.seed(321)
svm_Radial_Grid <- train(PCR~., data = training, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)
svm_Radial_Grid$bestTune
svm_Radial_Grid$finalModel
svm_Radial_Grid
plot(svm_Radial_Grid)

# PREDICTION
test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing)
confusionMatrix(test_pred_Radial_Grid, testing$PCR)

#


# OBTAIN ROC CURVE AND AUC FOR SVM
trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)

fit.RadialSVM.tuned <- train(PCR~., data = training, method = "svmRadial",
                             trControl=trctr2, preProcess = c("center", "scale"),
                             tuneGrid = grid_radial,  tuneLength = 3, 
                             metric = "ROC") 
max(fit.RadialSVM.tuned$results[,"ROC"])  # MAXIMUM AUC FROM TRAINING DATA

# PREDICTION WITH TESTING DATA
pred.prob <- predict(fit.RadialSVM.tuned, newdata=testing, type = "prob")
#require(pROC)  # USING PACKAGE pROC
#ROC <- roc(response=testing$PCR, predictor = pred.prob[, "NotDetected"])
#plot(ROC, col="brown")
#ROC$auc  # AUC
#text(x=0.4, y=0.25, paste("Area Under Curve = ", round(ROC$auc, digits=4), sep=""), col="blue", cex=1.2) 



(rbf.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.rbf <- verify(obs=dat.test.label, pred = pred.prob[,2])
svmrad_roc <- roc.plot(mod.rbf, plot.thres = NULL,main ="ROC Curve from SVM RADIAL BASIS")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(rbf.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)
write.csv(pred.prob[,2],'Reduced_Radial_Y_predict_full.csv')


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
svm_Poly <- train(PCR~., data = training, method = "svmPoly",
                  trControl=trctrl,
                  preProcess = c("center", "scale"))
svm_Poly
svm_Poly$bestTune #bestparameters
svm_Poly$finalModel#Training missclassification error

# PREDICTION
test_pred <- predict(svm_Poly, newdata = testing)
confusionMatrix(test_pred, testing$PCR)

# Selecting the OPTIMAL C value(Cost) in Linear SVM classifier
#grid_Poly <- expand.grid(degree = c(2,3,4), scale = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1),
#                         C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2) )

grid_Poly <- expand.grid(degree = c(2,3), scale = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2) )
bestgrid_Poly <- expand.grid(degree = 2, scale = 0.01, C = 1 )


set.seed(1191)
svm_Poly_Grid <- train(PCR~., data = training, method = "svmPoly",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneGrid = bestgrid_Poly) 
svm_Poly_Grid
svm_Poly_Grid$bestTune
svm_Poly_Grid$finalModel #Training missclassification error
plot(svm_Poly_Grid)

# PREDICTION
test_pred_grid <- predict(svm_Poly_Grid, newdata = testing)
confusionMatrix(test_pred_grid, testing$PCR )


# OBTAIN ROC CURVE AND AUC FOR SVM
trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)

fit.PolySVM.tuned <- train(PCR~., data = training, method = "svmPoly",
                           trControl=trctr2, preProcess = c("center", "scale"),
                           tuneGrid = bestgrid_Poly, metric = "ROC") 
max(fit.PolySVM.tuned$results[,"ROC"])  # MAXIMUM AUC FROM TRAINING DATA

# PREDICTION WITH TESTING DATA
pred.prob <- predict(fit.PolySVM.tuned, newdata=testing, type = "prob")
#require(pROC)  # USING PACKAGE pROC
#ROC <- roc(response=testing$PCR, predictor = pred.prob[, "NotDetected"])
#plot(ROC, col="brown")
#ROC$auc  # AUC
#text(x=0.4, y=0.25, paste("Area Under Curve = ", round(ROC$auc, digits=4), sep=""), col="blue", cex=1.2) 



#dat.test.label <- ifelse(testing$PCR == "Positive",1,0)
(poly.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.poly <- verify(obs=dat.test.label, pred = pred.prob[,2])
svmpoly_roc <- roc.plot(mod.poly, plot.thres = NULL,main ="ROC Curve from SVM POLYNOMIAL")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(poly.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)
write.csv(pred.prob[,2],'Reduced_poly_Y_predict_full.csv')


mod.polydat <- data.frame(svmpoly_roc$plot.data)
fitpolysvm <- locpoly(mod.polydat$X3, mod.polydat$X2, bandwidth = 0.05)
plot(mod.polydat$X3, mod.polydat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fitpolysvm)

xpred <- 0.2
R5 <- fitpolysvm$y[which.min(abs(fitpolysvm$x - xpred))]




mulroc <- cbind(matrix( c(mod.rf$pred, mod.bagg$pred, mod.lin$pred, mod.rbf$pred, mod.poly$pred), nrow = nrow(testing) ) )
allroc <- roc.plot(dat.test.label, pred =mulroc, plot.thres = FALSE, show.thres = FALSE)

#legend = T,   leg.text = c("RF", "Bagging", "SVM Linear", "SVM RBF", "SVM Poly"), col = c(1,2,3,4,5) )

abline(v = 0.2, col="red", lwd=2)

#create a vector to contain the auc values to be pasted.
aucvals <- c(a.ROC, a.ROCbag, linear.ROC, rbf.ROC, poly.ROC)

leg.txt = c( paste("RF", round(aucvals[1], 4), sep = "  "),  paste("Bagging", round(aucvals[2],4), sep = " "), paste("SVM Linear", round(aucvals[3],4), sep = " "), 
             paste("SVM RBF", round(aucvals[4],4), sep = " "), paste("SVM Poly", round(aucvals[5],4), sep = " ") )

legend( "bottomright",  leg.txt, col = c(1,2,3,4,5), lwd = 4, lty = "dotted", cex = 0.6)

FIXED80 <- c(R1, R2, R3, R4, R5 )
