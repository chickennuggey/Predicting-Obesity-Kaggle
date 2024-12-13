

# Load in libraries

library(ggplot2)
library(reshape2)
library(mice)
library(Hmisc)
library(glmnet)
library(tree)
library(randomForest)
library(e1071)

#########################################################################################################################################################

# Load in Data 

origTrain <- read.csv("/Users/briannanguyen/Desktop/Kaggle/ObesityTrain.csv", stringsAsFactors = TRUE) # original data
Xtest <- read.csv("/Users/briannanguyen/Desktop/Kaggle/ObesityTest.csv", stringsAsFactors = TRUE)

cat("Dimensions of Training Data:", dim(origTrain), "\n")
cat("Dimensions of Testing Data:", dim(Xtest))

str(origTrain)
summary(origTrain)

#########################################################################################################################################################

# Data Cleaning

# Barchart of Missing Training and Testing Data

# Using ggplot
origTrain <- origTrain[, -30]
propMissingTrain <- apply(origTrain, 2, function(x) sum(is.na(x)) / nrow(origTrain))
propMissingTrain <- round(sort(propMissingTrain, TRUE), 3)

propMissingTest <- apply(Xtest, 2, function(x) sum(is.na(x)) / nrow(Xtest))
propMissingTest <- round(sort(propMissingTest, TRUE), 3)

missingData <- data.frame("Feature" = rep(names(propMissingTrain), 2),
        "PropMissing" = c(propMissingTrain, propMissingTest), 
        "Data" = c(rep("Train", length(propMissingTrain)), rep("Test", length(propMissingTest))))

ggplot(missingData, aes(x=Feature, y=PropMissing, fill=Data)) +
geom_bar(stat="identity", position=position_dodge()) + coord_flip() + labs(title = "Proportion of Missing Data")

# using VIM
library(VIM)

missing_plot <- aggr(origTrain[, 1:14], col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, labels=names(origTrain[1:14]), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

# Data Imputation Using Mice

traindataTemp <- mice(Xtrain, m = 5, maxit = 5)
XtrainNew <- complete(traindataTemp,1)

testdataTemp <- mice(Xtest, m = 5, maxit = 5)
XtestNew <- complete(testdataTemp, 1)

#write.csv(XtrainNew, file = "/Users/briannanguyen/Desktop/Xtrain_mice1")
#write.csv(XtestNew, file = "/Users/briannanguyen/Desktop/Xest_mice1")

# My imputed data 
#XtrainNew <- read.csv("/Users/briannanguyen/Desktop/Xtrain_mice1", stringsAsFactors = TRUE)
#XtrainNew <- XtrainNew[, -1]
#train <- cbind(XtrainNew, "ObStatus" = Ytrain)

#XtestNew <- read.csv("/Users/briannanguyen/Desktop/Xest_mice1", stringsAsFactors = TRUE) # saved data
#XtestNew <- XtestNew[, -1]

# Kevin's imputed data
train <- read.csv("/Users/briannanguyen/Desktop/Kaggle/train_mice_prop.csv", stringsAsFactors = TRUE) 

Xtrain <- train[, -30]
Ytrain <- train[, 30]

head(Xtrain, 10)

#########################################################################################################################################################

# EDA

## Correlation Matrix 

categorical <- names(XtrainNew)[sapply(XtrainNew, is.character)]
numeric <- names(XtrainNew)[sapply(XtrainNew, is.numeric)]

corr <- round(cor(XtrainNew[numeric], ), 2)
melted_corr_mat <- melt(corr)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(axis.text.x = element_text(size = 5.5), axis.text.y = element_text(size = 7)) +
  labs(title = "Correlation Matrix")

## Histogram

library(Hmisc)
hist.data.frame(XtrainNew[numeric]) 
mtext("Distribution for Numeric Variables", side = 3, outer = TRUE, line = -2, cex = 1)

## Density Plots

library(ggpubr)

data <- train

p1 <- ggplot(data, aes(x=Age, color=ObStatus)) + geom_density()
p2 <- ggplot(data, aes(x=Height, color=ObStatus)) + geom_density()
p3 <- ggplot(data, aes(x=NCP, color=ObStatus)) + geom_density()
p4 <- ggplot(data, aes(x=CH2O, color=ObStatus)) + geom_density()
p5 <- ggplot(data, aes(x=FAF, color=ObStatus)) + geom_density()
p6 <- ggplot(data, aes(x=TUE, color=ObStatus)) + geom_density()
p7 <- ggplot(data, aes(x=RestingBP, color=ObStatus)) + geom_density()
p8 <- ggplot(data, aes(x=Cholesterol, color=ObStatus)) + geom_density()
p9 <- ggplot(data, aes(x=MaxHR, color=ObStatus)) + geom_density()
p10 <- ggplot(data, aes(x=avg_glucose_level, color=ObStatus)) + geom_density()

# all variables
figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2, nrow = 5)
figure <- annotate_figure(figure, top = text_grob("Density Plots", face = "bold", size = 15))
figure

# important variables
figure <- ggarrange(p1, p2, p4, p5, p6, ncol = 2, nrow = 3)
figure <- annotate_figure(figure, top = text_grob("Density Plots", face = "bold", size = 15))
figure

## Boxplots

p1 <- ggplot(data, aes(y=Age, x=ObStatus)) + geom_boxplot()
p2 <- ggplot(data, aes(y=Height, x=ObStatus)) + geom_boxplot()
p3 <- ggplot(data, aes(y=NCP, x=ObStatus)) + geom_boxplot()
p4 <- ggplot(data, aes(y=CH2O, x=ObStatus)) + geom_boxplot()
p5 <- ggplot(data, aes(y=FAF, x=ObStatus)) + geom_boxplot()
p6 <- ggplot(data, aes(y=TUE, x=ObStatus)) + geom_boxplot()
p7 <- ggplot(data, aes(y=RestingBP, x=ObStatus)) + geom_boxplot()
p8 <- ggplot(data, aes(y=Cholesterol, x=ObStatus)) + geom_boxplot()
p9 <- ggplot(data, aes(y=MaxHR, x=ObStatus)) + geom_boxplot()
p10 <- ggplot(data, aes(y=avg_glucose_level, x=ObStatus)) + geom_boxplot()

figure <- ggarrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)
figure <- annotate_figure(figure, top = text_grob("Boxplots", face = "bold", size = 15))
figure

figure <- ggarrange(p6, p7, p8, p9, p10, ncol = 3, nrow = 2)
figure <- annotate_figure(figure, top = text_grob("Boxplots", face = "bold", size = 15))
figure

## Barcharts 

p11 <- ggplot(data, aes(x = Gender, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p12 <- ggplot(data, aes(x = family_history_with_overweight, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p13 <- ggplot(data, aes(x = FAVC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p14 <- ggplot(data, aes(x = CAEC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p15 <- ggplot(data, aes(x = SMOKE, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p16 <- ggplot(data, aes(x = SCC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p17 <- ggplot(data, aes(x = CALC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p18 <- ggplot(data, aes(x = MTRANS, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p19 <- ggplot(data, aes(x = Race, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p20 <- ggplot(data, aes(x = FastingBS, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p21 <- ggplot(data, aes(x = RestingECG, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p22 <- ggplot(data, aes(x = ExerciseAngina, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p23 <- ggplot(data, aes(x = HeartDisease, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p24 <- ggplot(data, aes(x = hypertension, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p25 <- ggplot(data, aes(x = ever_married, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p26 <- ggplot(data, aes(x = work_type, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p27 <- ggplot(data, aes(x = Residence_type, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")
p28 <- ggplot(data, aes(x = stroke, fill = ObStatus))+ geom_bar(stat = "count", position = "stack")

figure1 <- ggarrange(p11, p12, p13, p14, p15, p16, p17, p18, p19, ncol = 3, nrow = 3)
figure2 <- ggarrange(p20, p21, p22, p23, p24, p25, p26, p27, p28, ncol = 3, nrow = 3)
figure1
figure2

#########################################################################################################################################################

# Models

## Logistic 

# Logsitic Performance Function
logisticMisclass <- function(log_model){
  glm.pred <- predict(log_model, newdata = XtrainNew, type = "response")
  glm.pred <- ifelse(glm.pred >= 0.5, "Obese", "Not Obese")
  tab <- table(glm.pred, Ytrain)
  cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n")
}


log1 <- glm(as.factor(ObStatus) ~., family = binomial(), train)
summary(log1)
logisticMisclass(log1) # Full Logistic - 0.7429874 

log2 <- glm(as.factor(ObStatus) ~ Gender + Height + family_history_with_overweight + FAVC + FCVC + NCP + CAEC + CH2O + SMOKE + SCC + FAF + MTRANS + Race + Cholesterol + FastingBS + avg_glucose_level + stroke, family = binomial(), train)
logisticMisclass(log2) # Reduced Logistic - 0.7363341 

### Stepwise Logistic Regression

BIC_model <- step(log1, direction = "both", k = log(nrow(train)), trace = 1)
AIC_model <- step(log1, direction = "both", trace = 1)

summary(BIC_model)
summary(AIC_model)

BIClog <- glm(formula = as.factor(ObStatus) ~ Gender + Height + family_history_with_overweight + 
    FAVC + FCVC + NCP + CAEC + CH2O + SCC + FAF + CALC + MTRANS + 
    Race + Cholesterol + FastingBS + avg_glucose_level, family = binomial(), 
    data = train)

logisticMisclass(BIClog) # BIC - 0.741082

AIClog <- glm(formula = as.factor(ObStatus) ~ Gender + Height + family_history_with_overweight + 
    FAVC + FCVC + NCP + CAEC + CH2O + SCC + FAF + CALC + MTRANS + 
    Race + Cholesterol + FastingBS + RestingECG + ExerciseAngina + 
    work_type + avg_glucose_level + stroke, family = binomial(), 
    data = train)

logisticMisclass(AIClog) # AIC - 0.742925 

### Regularization 

library(glmnet)
x <- model.matrix(ObStatus ~ ., scalex)  
y <- train$ObStatus    

lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")  # LASSO - 0.7432061 

m1cTest.pred <- predict(lasso_model,s=lasso_model$lambda.min, newx = x, type = "response") 
glm.pred <- ifelse(m1cTest.pred >= 0.5, "Obese", "Not Obese")
tab <- table(glm.pred, y)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n")

ridge_model <- cv.glmnet(x, y, alpha = 0, family = "binomial")  # RIDGE - 0.7440182 

m1cTest.pred <- predict(ridge_model,s=ridge_model$lambda.min, newx = x, type = "response") 
glm.pred <- ifelse(m1cTest.pred >= 0.5, "Obese", "Not Obese")
tab <- table(glm.pred, y)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n")

## Decision Trees

full <- tree(as.factor(ObStatus)~.,data=train) # Full tree - 0.793809 

plot(full) 
text(full,pretty=3)
summary(full)

pred.full <- predict(full,newdata=train,type="class")
tab <- table(pred.full,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n")


cvtree <- cv.tree(full,FUN=prune.misclass) # Pruned tree - 0.7727244

plot(cvtree$size,cvtree$dev)
lines(cvtree$size, cvtree$dev)
pruned <- prune.misclass(full, best = 5)
plot(pruned)
text(pruned,pretty=0)
summary(pruned)

pred.pr <- predict(pruned,newdata=train,type="class")
tab <- table(pred.pr,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

## Random Forests

rm1 <- randomForest(as.factor(ObStatus)~.,data=train,importance=TRUE) # Forest 0.9843818
pred.rm <- predict(rm1,data=train)
tab <- table(train$ObStatus,pred.rm)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))
summary(rm1)
plot(rm1)

rm2 <- randomForest(as.factor(ObStatus)~.,data=train,importance=TRUE, ntree = 30) # Forest 0.9761979
pred.rm=predict(rm2,data=train)
tab <- table(train$ObStatus,pred.rm)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))


rm3 <- randomForest(as.factor(ObStatus)~.,data=train,importance=TRUE, ntree = 25) # Forest 0.9734804
pred.rm <- predict(rm3,data=train)
tab <- table(train$ObStatus,pred.rm)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

## Support Vector Machines

library(e1071)

train$ObStatus <- as.factor(train$ObStatus)
test <- read.csv("/Users/briannanguyen/Desktop/test_mice_prop.csv")

# Radial Kernel

svmfit <- svm(as.factor(ObStatus)~.,data=train,kernel="radial") # Radial kernel SVM - 0.8670269

preds <- predict(svmfit,newdata=test, type = "response")
radialResults <- data.frame("ID" = Ytest$ID, "ObStatus" = preds)
write.csv(radialResults, file = "/Users/briannanguyen/Desktop/radialResults.csv", row.names = FALSE, quote = FALSE)

tab <- table(train$ObStatus,preds)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

# Linear Kernel

svmfit2=svm(ObStatus~.,data=train,kernel="linear",gamma=1,degree = 1) # Linear kernel SVM - 0.7500469

preds.tr=predict(svmfit2,newdata=test, type = "response")
linearResults <- data.frame("ID" = Ytest$ID, "ObStatus" = preds.tr)
write.csv(linearResults, file = "/Users/briannanguyen/Desktop/linearResults.csv", row.names = FALSE, quote = FALSE)

tab <- table(preds.tr,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

# Polynomial Kernel

svmfit3=svm(ObStatus~.,data=train,kernel="polynomial", degree = 3) # Polynomial kernel SVM - 0.88

preds.tr=predict(svmfit3,newdata=test, type = "response")
polyResults <- data.frame("ID" = Ytest$ID, "ObStatus" = preds.tr)
write.csv(polyResults, file = "/Users/briannanguyen/Desktop/polyResults.csv", row.names = FALSE, quote = FALSE)

tab <- table(preds.tr,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

# Sigmoid Kernel

svmfit4=svm(ObStatus~.,data=train,kernel="sigmoid") 

preds.tr=predict(svmfit4,data=train)
tab <- table(preds.tr,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))



