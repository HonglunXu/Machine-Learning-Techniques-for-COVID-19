####################################################################################################
# Copyright (C) 2023 Honglun Xu and Andrews T. Anum                                                             #
# All Methods For Reduced Data                                                                                                 #
# Utilizing Machine Learning Techniques for COVID-19 Screening Based on Clinical Data                           #                                                                                              #
####################################################################################################


## ！！！
# Set working directory
rm(list = ls())
getwd()
setwd(path) # Replace with actual directory path!

## Data Preprocessing
## Split data into training and test
FILE.TRAIN <- "train.reduced.data.csv"
FILE.TEST  <- "test.reduced.data.csv"

training <- read.csv(file = FILE.TRAIN, header = T, sep = ",")
testing  <- read.csv(file = FILE.TEST, header = T,  sep = ",")


training <- training[ ,-1]
testing <-  testing[ , -1]

anyNA(training)
anyNA(testing)

table(training$PCR)
table(testing$PCR)


library(randomForest)
library(caret)
library(verification)
library(ipred)

dat.test.label <- testing$PCR


###############################################################################
##############################Scale data  ###################################
scaled.dat.train.1 <- data.frame( apply(training[,c(1:7)], 2, scale, center=T, scale=T) )
scaled.dat.train.2 <- data.frame( apply(training[,c(9:13)], 2, scale, center=T, scale=T) )
scaled.dat.test.1  <- data.frame( apply(testing[,c(1:7)],  2, scale, center=T, scale=T) )
scaled.dat.test.2  <- data.frame( apply(testing[,c(9:13)],  2, scale, center=T, scale=T) )

finaltraining <- cbind.data.frame(scaled.dat.train.1,training$ETHNICITY, scaled.dat.train.2, training$PCR)
finaltesting  <- cbind.data.frame(scaled.dat.test.1,testing$ETHNICITY,  scaled.dat.test.2, testing$PCR)

colnames(finaltraining) <- colnames(training)
colnames(finaltesting)  <- colnames(training)

# ======================================
# 1. FITTING THE FULL LOGISTIC MODEL 
# =======================================

fit.full <- glm(PCR ~ ., family="binomial", data=finaltraining)
summary(fit.full)
names(summary(fit.full))
BIC(fit.full)


pred <- predict(fit.full, newdata=finaltesting, type="response", se.fit=TRUE)
yhat <- pred$fit
write.csv(yhat,'Reduced_Logistic_Regression_Y_predict_full.csv')

a<-(yhat>=0.5)+0
confusionMatrix(factor(as.numeric(as.character(a))), factor(as.numeric(as.character(dat.test.label))))

(a.ROClg <- roc.area(obs = dat.test.label, pred = yhat)$A)
mod.lg <- verify(obs=dat.test.label, pred = yhat)
lgroc <- roc.plot(mod.lg, plot.thres = NULL,main ="ROC Curve from Logistic Regression")
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROClg, digits=4), 
                         sep=" "), col="blue", cex=1)


# ####################################################
# 2. TRY OUT CART
# ####################################################

library(rpart)
control0 <- rpart.control(minsplit=10, minbucket=3, maxdepth=15,
                          cp=0, maxcompete=4, 
                          maxsurrogate=5, usesurrogate=2, surrogatestyle=0,  		# SURROGATE SPLITS FOR MISSING DATA
                          xval=10)									# SET THE VALUE V FOR V-FOLD CROSS VALIDATION
tre0 <- rpart(PCR ~ ., data=finaltraining, method='class', control=control0,
              parms=list(split='information'))
plot(tre0)
plotcp(tre0)
dev.print(postscript, 'spam-fig1.ps', paper='special', height=6, width=10)
printcp(tre0)

btre <- prune(tre0, cp=.0028)
plot(btre, uniform=T, compress=T, margin=.05)
text(btre, use.n=T)
dev.print(postscript, 'spam-fig2.ps', paper='special', height=8.5, width=11)

print(btre, cp=.05)


# TEST ERROR
btre.test.class <- predict(btre, type='class', newdata=finaltesting)
btre.test.class.prob <- predict(btre, newdata=finaltesting,type = "prob")

confusionMatrix(factor(as.numeric(as.character(testing$PCR))), factor(as.numeric(as.character(btre.test.class))))


(a.ROCcart <- roc.area(obs = dat.test.label, pred = btre.test.class.prob[,2])$A)
mod.cart <- verify(obs=dat.test.label, pred = btre.test.class.prob[,2])
cartroc <- roc.plot(mod.cart, plot.thres = NULL,main ="ROC Curve from CART")
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROCcart, digits=4), 
                         sep=" "), col="blue", cex=1)


write.csv(btre.test.class.prob[,2],'Reduced_CART_Y_predict_full.csv')


# ####################################################
# 3. TRY OUT ANN
# ####################################################
library(neuralnet) 
options(digits=4)
net1 <- neuralnet(PCR ~ ., data=finaltraining, hidden=3, rep=5, 
                  act.fct='logistic', err.fct="ce", linear.output=F, likelihood=TRUE)

ypred <- compute(net1, covariate=finaltesting[,-14], rep=5)$net.result
yobs <- finaltesting$PCR

library(caret)
ypred.binary <- (ypred>=0.5)+0
confusionMatrix(factor(ypred.binary), factor(yobs))


(a.ROCann <- roc.area(obs = dat.test.label, pred = ypred)$A)
mod.ann <- verify(obs=dat.test.label, pred = ypred)
annroc <- roc.plot(mod.ann, plot.thres = NULL,main ="ROC Curve from ANN")
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROCann, digits=4), 
                         sep=" "), col="blue", cex=1)

write.csv(ypred,'Reduced_ANN_Y_predict_full.csv')



###############################################################################
## 4. Andrews##########################
#############################################################################

getwd()
setwd("D:/desktop/RIMES/UTMB/COVID.diagnosis.train.imp.cart.df.csv~1/")
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

###############################################################################
##############################Scale data  ###################################
scaled.dat.train.1 <- data.frame( apply(training[,c(1:7)], 2, scale, center=T, scale=T) )
scaled.dat.train.2 <- data.frame( apply(training[,c(9:13)], 2, scale, center=T, scale=T) )
scaled.dat.test.1  <- data.frame( apply(testing[,c(1:7)],  2, scale, center=T, scale=T) )
scaled.dat.test.2  <- data.frame( apply(testing[,c(9:13)],  2, scale, center=T, scale=T) )

finaltraining <- cbind.data.frame(scaled.dat.train.1,training$ETHNICITY, scaled.dat.train.2, training$PCR)
finaltesting  <- cbind.data.frame(scaled.dat.test.1,testing$ETHNICITY,  scaled.dat.test.2, testing$PCR)

colnames(finaltraining) <- colnames(training)
colnames(finaltesting)  <- colnames(training)

dat.test.label <- ifelse(testing$PCR == "Positive",1,0)

library(caret)
library(kernlab)
library(verification)

###############################################################################
## 4. R A N D O M F O R E S T##########################
## REDUCED DATA. STANDARDIZED########################

set.seed(22)
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

set.seed(25)
rf.model <- randomForest(PCR~., data = finaltraining, ntree =50, mtry = floor(sqrt(ncol(finaltraining))), importance = T)
print(rf.model)

rf.pred  <- predict(rf.model, finaltesting)
rf.predn.prob  <- predict(rf.model, finaltesting, type = "prob")
confusionMatrix(finaltesting$PCR, rf.pred, positive = "Positive", mode = "everything")

(a.ROC <- roc.area(obs = dat.test.label, pred = rf.predn.prob[,2])$A)
mod.rf <- verify(obs=dat.test.label, pred = rf.predn.prob[,2])#,frcst.type = "prob",obs.type = "binary"
rfroc <- roc.plot(mod.rf, plot.thres = NULL,main ="ROC Curve from Random Forest")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(a.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)

write.csv(rf.predn.prob[,2],'Reduced_RF_Y_predict_full.csv')



rflmdat <- data.frame(rfroc$plot.data)
library(KernSmooth)
fit <- locpoly(rflmdat$X3, rflmdat$X2, bandwidth = 0.05)
plot(rflmdat$X3, rflmdat$X2, xlab = "false alarm rate", ylab = "hit rate")
lines(fit)

xpred <- 0.2
R1 <- fit$y[which.min(abs(fit$x - xpred))]
R1

############################## 5. B A G G I N G ###################################
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
print(bag.modeln)

bag.predn <- predict(bag.modeln, finaltesting)
bag.predn.prob <- predict(bag.modeln, finaltesting, type = "prob")
confusionMatrix(finaltesting$PCR,  bag.predn, positive = "Positive", mode = "everything")


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


#####################################
##SUPPORT VECTOR MACHINE ############
#####################################

# 6. SVM I: LINEAR
# ----------------

set.seed(3233)

grid <- expand.grid(C =  c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2))#0.1

trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)
fit.LinearSVM.tuned <- train(PCR~., data = finaltraining, method = "svmLinear",
                             trControl=trctr2, preProcess = c("center", "scale"),
                             tuneGrid = grid , tuneLength = 10, 
                             metric = "ROC") 


pred.prob <- predict(fit.LinearSVM.tuned, newdata=finaltesting)
confusionMatrix(finaltesting$PCR,  pred.prob, positive = "Positive", mode = "everything")

pred.prob <- predict(fit.LinearSVM.tuned, newdata=finaltesting, type = "prob")
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


# 7.SVM II: RBF
# ---------------------------

grid_radial <- expand.grid(sigma = c(0.001, 0.01, 0.1, 1, 10), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2))

set.seed(3211)

trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)

fit.RadialSVM.tuned <- train(PCR~., data = finaltraining, method = "svmRadial",
                             trControl=trctr2, preProcess = c("center", "scale"),
                             tuneGrid = grid_radial,  tuneLength = 3, 
                             metric = "ROC") 

pred.prob <- predict(fit.RadialSVM.tuned, newdata=finaltesting)
confusionMatrix(finaltesting$PCR,  pred.prob, positive = "Positive", mode = "everything")


pred.prob <- predict(fit.RadialSVM.tuned, newdata=finaltesting, type = "prob")
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


# 8. SVM III: POLYNOMIAL KERNEL
# ---------------------------

set.seed(12456)

bestgrid_Poly <- expand.grid(degree = 2, scale = 0.01, C = 1 )

trctr2 <- trainControl(method="repeatedcv", number = 5, repeats = 3,
                       classProbs=TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)

fit.PolySVM.tuned <- train(PCR~., data = finaltraining, method = "svmPoly",
                           trControl=trctr2, preProcess = c("center", "scale"),
                           tuneGrid = bestgrid_Poly, metric = "ROC") 

pred.prob <- predict(fit.PolySVM.tuned, newdata=finaltesting)
confusionMatrix(finaltesting$PCR,  pred.prob, positive = "Positive", mode = "everything")

pred.prob <- predict(fit.PolySVM.tuned, newdata=finaltesting, type = "prob")
(poly.ROC <- roc.area(obs = dat.test.label, pred = pred.prob[, 2])$A)
mod.poly <- verify(obs=dat.test.label, pred = pred.prob[,2])
svmpoly_roc <- roc.plot(mod.poly, plot.thres = NULL,main ="ROC Curve from SVM POLYNOMIAL")
abline(v = 0.2, col="red",lwd=2)
text(x=0.7, y=0.2, paste("Area under ROC =", round(poly.ROC, digits=4), 
                         sep=" "), col="blue", cex=1)
