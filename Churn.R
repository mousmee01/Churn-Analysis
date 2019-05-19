install.packages('pacman')
require('pacman')

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

churn <- read.csv('C:/Users/moshm/Desktop/Edinburgh_study/Predictive Analysis & Modelling of Data/Project_Matthias/Edinburgh_train_sample.csv')

str(churn)

sapply(churn, function(x) sum(is.na(x)))

churn <- churn[complete.cases(churn), ]

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

churn$customerID <- NULL
churn$tenure <- NULL

numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

churn$TotalCharges <- NULL

set.seed(2017)

ratio = sample(1:nrow(churn), size = 0.25*nrow(churn))
Test = churn[ratio,] #Test dataset 25% of total
Training = churn[-ratio,] #Train dataset 75% of total

#intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)

#training<- churn[intrain,]
#testing<- churn[-intrain,]

dim(Test); dim(Training)

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, Training)
plot(tree, type='simple')

pred_tree <- predict(tree, Test)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = Test$Churn)

p1 <- predict(tree, Training)
tab1 <- table(Predicted = p1, Actual = Training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = Test$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))