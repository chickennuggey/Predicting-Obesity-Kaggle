
# Load in libraries

library(ggplot2)
library(reshape2)
library(mice)
library(Hmisc)
library(glmnet)
library(tree)
library(randomForest)
library(e1071)

############################################################################################################################################

# Load in Data 

origTrain <- read.csv("/Users/briannanguyen/Desktop/Kaggle/ObesityTrain.csv", stringsAsFactors = TRUE) # original data
Xtest <- read.csv("/Users/briannanguyen/Desktop/Kaggle/ObesityTest.csv", stringsAsFactors = TRUE)

cat("Dimensions of Training Data:", dim(origTrain), "\n")
cat("Dimensions of Testing Data:", dim(Xtest))

# Data Structure 
str(origTrain)
summary(origTrain)

# Proportion Response Variable
tab <- table(origTrain$ObStatus)
prop.table(tab)

############################################################################################################################################
########################################################### Data Cleaning ##################################################################
############################################################################################################################################

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
geom_bar(stat="identity", position=position_dodge()) + coord_flip() + labs(title = "Proportion of Missing Data") + scale_fill_manual(values = c("Train" = "lightsteelblue3", "Test" = "steelblue4"), name = "Data Type") + theme_minimal()

# using VIM
library(VIM)

missing_plot <- aggr(origTrain[, 1:14], col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, labels=names(origTrain[1:14]), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

############################################################################################################################################

# Data Imputation Using Mice

#traindataTemp <- mice(Xtrain, m = 5, maxit = 5)
#XtrainNew <- complete(traindataTemp,1)
#testdataTemp <- mice(Xtest, m = 5, maxit = 5)
#XtestNew <- complete(testdataTemp, 1)

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
test <- read.csv("/Users/briannanguyen/Desktop/Kaggle/test_mice_prop.csv", stringsAsFactors = TRUE) 

XtrainNew <- train[, -30]
YtrainNew <- train[, 30]

head(XtrainNew, 10)

############################################################################################################################################
################################################### Exploratory Data Analysis ##############################################################
############################################################################################################################################

## Correlation Matrix 

categorical <- names(XtrainNew)[sapply(XtrainNew, is.character)]
numeric <- names(XtrainNew)[sapply(XtrainNew, is.numeric)]

corr <- round(cor(XtrainNew[numeric], ), 2)
melted_corr_mat <- melt(corr)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +  theme_minimal() +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(axis.text.x = element_text(size = 8, angle = 30, hjust = 1, vjust = 1), axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Matrix") + xlab("") + ylab("") 

############################################################################################################################################

## Histogram

library(Hmisc)
hist.data.frame(XtrainNew[numeric]) 
mtext("Distribution for Numeric Variables", side = 3, outer = TRUE, line = -2, cex = 1)

############################################################################################################################################

## Density Plots

library(ggpubr)

data <- train

p1 <- ggplot(data, aes(x=Age, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p2 <- ggplot(data, aes(x=Height, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p3 <- ggplot(data, aes(x=NCP, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p4 <- ggplot(data, aes(x=CH2O, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p5 <- ggplot(data, aes(x=FAF, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p6 <- ggplot(data, aes(x=TUE, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p7 <- ggplot(data, aes(x=RestingBP, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p8 <- ggplot(data, aes(x=Cholesterol, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p9 <- ggplot(data, aes(x=MaxHR, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")
p10 <- ggplot(data, aes(x=avg_glucose_level, color=ObStatus)) + geom_density() + theme_minimal() + theme(legend.position = "none")

# all variables
figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 5, nrow = 2, common.legend = TRUE, legend = "bottom")
figure <- annotate_figure(figure, top = text_grob("Density of Numeric Variables", size = 15))
figure

# important variables
figure <- ggarrange(p1, p2, p4, p5, p6, ncol = 2, nrow = 3)
figure <- annotate_figure(figure, top = text_grob("Density Plots", face = "bold", size = 15))
figure

############################################################################################################################################

## Boxplots

p1 <- ggplot(data, aes(y=Age, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p2 <- ggplot(data, aes(y=Height, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p3 <- ggplot(data, aes(y=NCP, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p4 <- ggplot(data, aes(y=CH2O, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p5 <- ggplot(data, aes(y=FAF, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p6 <- ggplot(data, aes(y=TUE, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p7 <- ggplot(data, aes(y=RestingBP, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p8 <- ggplot(data, aes(y=Cholesterol, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p9 <- ggplot(data, aes(y=MaxHR, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))
p10 <- ggplot(data, aes(y=avg_glucose_level, x=ObStatus)) + geom_boxplot() + theme_minimal() + theme(axis.text.x = element_text(size = 7))

figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 5, nrow = 2)
figure <- annotate_figure(figure, top = text_grob("Boxplots of Numeric Variables", size = 15))
figure

############################################################################################################################################

## Barcharts 

# Counts
p11 <- ggplot(data, aes(x = Gender, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p12 <- ggplot(data, aes(x = family_history_with_overweight, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p13 <- ggplot(data, aes(x = FAVC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p14 <- ggplot(data, aes(x = CAEC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) + scale_x_discrete(labels =c("Always", "Freq", "no", "Sometime"))
p15 <- ggplot(data, aes(x = SMOKE, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p16 <- ggplot(data, aes(x = SCC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p17 <- ggplot(data, aes(x = CALC, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) + scale_x_discrete(labels =c("Always", "Freq", "no", "Sometime"))
p18 <- ggplot(data, aes(x = MTRANS, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1, size = 8)) + scale_x_discrete(labels =c("auto", "bike", "motorbike", "public", "walk"))
p19 <- ggplot(data, aes(x = Race, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p20 <- ggplot(data, aes(x = FastingBS, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p21 <- ggplot(data, aes(x = RestingECG, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p22 <- ggplot(data, aes(x = ExerciseAngina, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p23 <- ggplot(data, aes(x = HeartDisease, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p24 <- ggplot(data, aes(x = hypertension, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p25 <- ggplot(data, aes(x = ever_married, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p26 <- ggplot(data, aes(x = work_type, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal() +
 scale_x_discrete(labels =c("youth", "govt", "never", "priv", "self"))
p27 <- ggplot(data, aes(x = Residence_type, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()
p28 <- ggplot(data, aes(x = stroke, fill = ObStatus))+ geom_bar(stat = "count", position = "stack") + theme_minimal()

figure <- ggarrange(p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, 
                    ncol = 6, nrow = 3, common.legend = TRUE, legend = "bottom") 
figure <- annotate_figure(figure, top = text_grob("Barcharts for Categorical Variables", size = 15))
figure

# Proportion
p11 <- ggplot(data) + aes(x = Gender, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p12 <- ggplot(data) + aes(x = family_history_with_overweight, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p13 <-ggplot(data) + aes(x = FAVC, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p14 <- ggplot(data) + aes(x = CAEC, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) + scale_x_discrete(labels =c("Always", "Freq", "no", "Sometime"))
p15 <- ggplot(data) + aes(x = SMOKE, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p16 <- ggplot(data) + aes(x = SCC, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p17 <- ggplot(data) + aes(x = CALC, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) + scale_x_discrete(labels =c("Always", "Freq", "no", "Sometime"))
p18 <- ggplot(data) + aes(x = MTRANS, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1, size = 8)) + scale_x_discrete(labels =c("auto", "bike", "motorbike", "public", "walk"))
p19 <- ggplot(data) + aes(x = Race, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p20 <- ggplot(data) + aes(x = FastingBS, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p21 <- ggplot(data) + aes(x = RestingECG, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p22 <- ggplot(data) + aes(x = ExerciseAngina, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p23 <- ggplot(data) + aes(x = HeartDisease, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p24 <- ggplot(data) + aes(x = hypertension, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p25 <- ggplot(data) + aes(x = ever_married, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p26 <- ggplot(data) + aes(x = work_type, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal() +
 scale_x_discrete(labels =c("youth", "govt", "never", "priv", "self"))
p27 <- ggplot(data) + aes(x = Residence_type, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()
p28 <- ggplot(data) + aes(x = stroke, fill = ObStatus) + geom_bar(position = "fill") + ylab("Freq") + theme_minimal()

figure <- ggarrange(p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, ncol = 6, nrow = 3, common.legend = TRUE, legend = "bottom") 
figure <- annotate_figure(figure, top = text_grob("Barcharts for Categorical Variables", size = 15))
figure

############################################################################################################################################
########################################################### Predictive Models ##############################################################
############################################################################################################################################

## Logistic 

# Logsitic Performance Function
logisticMisclass <- function(log_model){
  glm.pred <- predict(log_model, newdata = XtrainNew, type = "response")
  glm.pred <- ifelse(glm.pred >= 0.5, "Obese", "Not Obese")
  tab <- table(glm.pred, Ytrain)
  cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n", "Confusion Matrix:", tab, "\n")
}

# Full Model
log1 <- glm(as.factor(ObStatus) ~., family = binomial(), train)
summary(log1)
logisticMisclass(log1) 

log1_preds <- predict(log1,newdata=test, type = "response") 
log1_preds <- ifelse(log1_preds >= 0.5, "Obese", "Not Obese")
fullLogResults <- data.frame("ID" = seq(1, nrow(test)), "ObStatus" = log1_preds)
write.csv(fullLogResults, file = "/Users/briannanguyen/Desktop/Kaggle/fullLogResults.csv", row.names = FALSE, quote = FALSE)

# Reduced Model (based on significant predictors from full model summary)
log2 <- glm(as.factor(ObStatus) ~ Gender + Height + family_history_with_overweight + FAVC + FCVC + NCP + CAEC + CH2O + SMOKE + SCC + FAF + MTRANS + Race + Cholesterol + FastingBS + avg_glucose_level + stroke, family = binomial(), train)
logisticMisclass(log2) 

log2_preds <- predict(log2,newdata=test, type = "response")
log2_preds <- ifelse(log2_preds >= 0.5, "Obese", "Not Obese")
reducedLogResults <- data.frame("ID" = seq(1, nrow(test)), "ObStatus" = log2_preds)
write.csv(reducedLogResults, file = "/Users/briannanguyen/Desktop/Kaggle/reducedLogResults.csv", row.names = FALSE, quote = FALSE)

### Stepwise Logistic Regression

# BIC 
BIC_model <- step(log1, direction = "both", k = log(nrow(train)), trace = 1)
summary(BIC_model)

BIClog <- glm(formula = as.factor(ObStatus) ~ Gender + Height + family_history_with_overweight + 
    FAVC + FCVC + NCP + CAEC + CH2O + SCC + FAF + CALC + MTRANS + 
    Race + Cholesterol + FastingBS + avg_glucose_level, family = binomial(), 
    data = train)

logisticMisclass(BIClog) # BIC - 0.741082

BIC_preds <- predict(BIClog,newdata=test, type = "response")
BIC_preds <- ifelse(BIC_preds >= 0.5, "Obese", "Not Obese")
BICLogResults <- data.frame("ID" = seq(1, nrow(test)), "ObStatus" = BIC_preds)
write.csv(BICLogResults, file = "/Users/briannanguyen/Desktop/Kaggle/BICLogResults.csv", row.names = FALSE, quote = FALSE)

# AIC
AIC_model <- step(log1, direction = "both", trace = 1)
summary(AIC_model)

AIClog <- glm(formula = as.factor(ObStatus) ~ Gender + Height + family_history_with_overweight + 
    FAVC + FCVC + NCP + CAEC + CH2O + SCC + FAF + CALC + MTRANS + 
    Race + Cholesterol + FastingBS + RestingECG + ExerciseAngina + 
    work_type + avg_glucose_level + stroke, family = binomial(), 
    data = train)

logisticMisclass(AIClog) # AIC - 0.742925 

AIC_preds <- predict(AIClog,newdata=test, type = "response")
AIC_preds <- ifelse(AIC_preds >= 0.5, "Obese", "Not Obese")
AICLogResults <- data.frame("ID" = seq(1, nrow(test)), "ObStatus" = AIC_preds)
write.csv(AICLogResults, file = "/Users/briannanguyen/Desktop/Kaggle/AICLogResults.csv", row.names = FALSE, quote = FALSE)

### Regularization 

library(glmnet)
x <- model.matrix(ObStatus ~ ., train)[,-1]
y <- train$ObStatus 
regtest <- model.matrix(~., test)[, -1]

# Lasso
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")  

lambda.min <- 0.0005
m1cTest.pred <- predict(lasso_model,s=0.005, newx = x, type = "response") 
glm.pred <- ifelse(m1cTest.pred >= 0.5, "Obese", "Not Obese")
tab <- table(glm.pred, y)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n", "Confusion Matrix: ", tab)

lasso_preds <- predict(lasso_model,s=lasso_model$lambda.min, newx=regtest, type = "response")
lasso_preds <- ifelse(lasso_preds >= 0.5, "Obese", "Not Obese")
lassoResults <- data.frame("ID" = seq(1, nrow(test)), "ObStatus" = lasso_preds)
write.csv(lassoResults, file = "/Users/briannanguyen/Desktop/Kaggle/lassoResults.csv", row.names = FALSE, quote = FALSE)

# Lasso Coefficient
coef_lasso <- coefficients(lasso_model, s = lambda.min)
coef_df <- data.frame(Variable = rownames(coef_lasso),Coefficient = as.vector(coef_lasso))
coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]

ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Lasso Coefficients", x = "Variable", y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 7))

# Ridge

ridge_model <- cv.glmnet(x, y, alpha = 0, family = "binomial")  

m1cTest.pred <- predict(ridge_model,s=ridge_model$lambda.min, newx = x, type = "response") 
glm.pred <- ifelse(m1cTest.pred >= 0.5, "Obese", "Not Obese")
tab <- table(glm.pred, y)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n", "Confusion Matrix: ", tab)

ridge_preds <- predict(ridge_model,s=ridge_model$lambda.min, newx=regtest, type = "response")
ridge_preds <- ifelse(ridge_preds >= 0.5, "Obese", "Not Obese")
ridgeResults <- data.frame("ID" = seq(1, nrow(test)), "ObStatus" = ridge_preds)
write.csv(ridgeResults, file = "/Users/briannanguyen/Desktop/Kaggle/ridgeResults.csv", row.names = FALSE, quote = FALSE)

############################################################################################################################################

## Decision Tree

# Full tree
full <- tree(as.factor(ObStatus)~.,data=train) 

plot(full) 
text(full,pretty=3)
summary(full)

pred.full <- predict(full,newdata=train,type="class")
tab <- table(pred.full,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)), "\n")

# Pruned tree
cvtree <- cv.tree(full,FUN=prune.misclass) 
plot(cvtree$size,cvtree$dev)
lines(cvtree$size, cvtree$dev)

pruned <- prune.misclass(full, best = 5)

plot(pruned)
text(pruned,pretty=0)
summary(pruned)

pred.pr <- predict(pruned,newdata=train,type="class")
tab <- table(pred.pr,train$ObStatus)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

############################################################################################################################################

## Random Forests

# Full random forest 
rm1 <- randomForest(as.factor(ObStatus)~.,data=train,importance=TRUE) # Forest 0.9843818
pred.rm <- predict(rm1,data=train)
tab <- table(train$ObStatus,pred.rm)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))
summary(rm1)
plot(rm1)

# Reduced random forest (ntree = 30)
rm2 <- randomForest(as.factor(ObStatus)~.,data=train,importance=TRUE, ntree = 30) # Forest 0.9761979
pred.rm=predict(rm2,data=train)
tab <- table(train$ObStatus,pred.rm)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

# Reduced random forest (ntree = 25)
rm3 <- randomForest(as.factor(ObStatus)~.,data=train,importance=TRUE, ntree = 25) # Forest 0.9734804
pred.rm <- predict(rm3,data=train)
tab <- table(train$ObStatus,pred.rm)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))

############################################################################################################################################

## Support Vector Machines

library(e1071)

train$ObStatus <- as.factor(train$ObStatus)

# Radial Kernel

svmfit <- svm(as.factor(ObStatus)~.,data=train,kernel="radial") # Radial kernel SVM - 0.8670269
preds <- predict(svmfit,newdata=Xtrain, type = "response")
tab <- table(train$ObStatus,preds)
cat("Accuracy:", sum(as.numeric(tab)[c(1, 4)]) / sum(as.numeric(tab)))


preds <- predict(svmfit,newdata=test, type = "response")
radialResults <- data.frame("ID" = Ytest$ID, "ObStatus" = preds)
write.csv(radialResults, file = "/Users/briannanguyen/Desktop/radialResults.csv", row.names = FALSE, quote = FALSE)

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

############################################################################################################################################

# Barcharts for Logistic and SVM Models

# Logistic Models
log_df <- data.frame(
  "Model" = rep(c("Full", "Reduced", "Ridge", "Lasso", "AIC Step", "BIC Step"),2),
  "Number of p" = rep(c(29, 17, 29, 26, 16, 16),6),
  "Accuracy" = c(74.64, 74.04, 74.78, 74.63, 74.54, 74.54, 74.72, 73.93, 74.71, 74.67, 74.57, 74.43),
  "Data" = c(rep("Training", 6),rep("Testing", 6)))

ggplot(log_df, aes(x = Model, y = Accuracy, fill = Data)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Testing" = "steelblue4", "Training" = "lightsteelblue3"), name = "Data Type") + 
  geom_text(aes(label = Accuracy), position = position_dodge(width = 0.6), vjust = -0.5, size = 2.5) +
  geom_line(data = log_df, aes(x = Model, y = Number.of.p * (60 / max(Number.of.p))), group = 1, color = "orangered", size = 0.75, inherit.aes = FALSE) +
  scale_y_continuous(name = "Accuracy (%)", limits = c(0, 100),  sec.axis = sec_axis(~ . * (max(log_df$Number.of.p) / 60), name = "Number of predictors")) + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.title.y.right = element_text(color = "orangered", size = 12), axis.text.y.right = element_text(color =  "orangered", size = 10)) + 
  labs(title = "Comparison of Training and Testing Accuracy by Logistic Model") 

# SVM Models
svm_df <- data.frame(
  "Kernel" = rep(c("Linear", "Polynomial", "Radial"), 2),
  "Accuracy" = c(75.60, 86.32, 87.70, 62.35, 85.56, 86.71),
    "Data" = c(rep("Training", 3),rep("Testing", 3))
)

ggplot(svm_df, aes(x = Kernel, y = Accuracy, fill = Data)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Training" = "lightsteelblue3", "Testing" = "steelblue4"), name = "Data Type") + 
  geom_text(aes(label = round(Accuracy, 2)), position = position_dodge(width = 0.6), vjust = -0.5, size = 3) +
  scale_y_continuous(name = "Accuracy (%)", limits = c(0, 100)) + theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(title = "Comparison of Training and Testing Accuracy by Kernel") 



