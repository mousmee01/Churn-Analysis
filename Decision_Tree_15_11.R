rm(list=ls())
setwd('C:/Users/moshm/Desktop/Edinburgh_study/Predictive Analysis & Modelling of Data/Project_Matthias/')

#library packages
install.packages('pacman')
require('pacman')
p_load("plyr", "corrplot", "ggplot2", "gridExtra", "ggthemes","data.table", "caret", "MASS", "randomForest", "party","plyr",'imputeMissings')


set.seed(11)
#import data
classes<-sapply(data,class)
data<-read.csv('Edinburgh_train_sample.csv',header=TRUE,sep=',',colClasses=classes)
str(data)
head(data)
names(data)
dim(data)
# summary statisics
summary(data)
lapply(data, unique)

#remove customerID 
data$customerID<-NULL

#Transofrom blank values into NA
data[data==""]=NA

#data transformation: one-hot encodeing 
data$MultipleLines <- as.factor(mapvalues(data$MultipleLines, 
                                          from=c("No phone service","No","Yes"),
                                          to=c("0","0","1")))
data$OnlineSecurity <- as.factor(mapvalues(data$OnlineSecurity, 
                                           from=c("No internet service","No","Yes"),
                                           to=c("0","0","1")))
data$OnlineBackup <- as.factor(mapvalues(data$OnlineBackup, 
                                         from=c("No internet service","No","Yes"),
                                         to=c("0","0","1")))
data$DeviceProtection <- as.factor(mapvalues(data$DeviceProtection, 
                                             from=c("No internet service","No","Yes"),
                                             to=c("0","0","1")))
data$TechSupport<- as.factor(mapvalues(data$TechSupport, 
                                       from=c("No internet service","No","Yes"),
                                       to=c("0","0","1")))
data$StreamingTV<- as.factor(mapvalues(data$StreamingTV, 
                                       from=c("No internet service","No","Yes"),
                                       to=c("0","0","1")))
data$StreamingMovies<- as.factor(mapvalues(data$StreamingMovies, 
                                           from=c("No internet service","No","Yes"),
                                           to=c("0","0","1")))
data$InternationalPlan <- as.factor(mapvalues(data$InternationalPlan,
                                              from=c("No","Yes"),
                                              to=c("0","1")))
data$VoiceMailPlan<- as.factor(mapvalues(data$VoiceMailPlan,
                                         from=c("No","Yes"),
                                         to=c("0","1")))
data$PaperlessBilling<- as.factor(mapvalues(data$PaperlessBilling,
                                            from=c("No","Yes"),
                                            to=c("0","1")))
data$PhoneService<- as.factor(mapvalues(data$PhoneService,
                                        from=c("No","Yes"),
                                        to=c("0","1")))
# Female: 0, Male: 1
data$gender<- as.factor(mapvalues(data$gender,
                                  from=c("Female","Male"),
                                  to=c("0","1")))
data$Partner<- as.factor(mapvalues(data$Partner,
                                   from=c("No","Yes"),
                                   to=c("0","1")))
data$Dependents<- as.factor(mapvalues(data$Dependents,
                                      from=c("No","Yes"),
                                      to=c("0","1")))
data$Churn<- as.factor(mapvalues(data$Churn,
                                 from=c("No","Yes"),
                                 to=c("0","1")))



#data transformation 2: tenure
min(data$tenure); max(data$tenure)
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
data$tenure_group <- sapply(data$tenure,group_tenure)
data$tenure_group <- as.factor(data$tenure_group)
data$tenure <- NULL

#missing value        
table(is.na(data))
colSums(is.na(data))

# Method1: impute Median or mode
library(imputeMissings)
data_imp <- impute(data = data, flag = FALSE)
colSums(is.na(data_imp))
data<-data_imp
table(is.na(data))

#correlation

library(reshape)

ndata<-data[, sapply(data, is.numeric)]
a <- cor(ndata)
a[a == 1] <- NA #drop perfect
a[abs(a) < 0.5] <- NA # drop less than abs(0.5)
a <- na.omit(melt(a)) 
a[order(abs(a$value),decreasing = TRUE),] 

###plot
numeric.var <- sapply(data, is.numeric) ## Find numerical variables
corr.matrix <- cor(data[,numeric.var])  ## Calculate the correlation matrix
corrplot::corrplot(corr.matrix)

# remove Variables with high correlated 
data$TotalCharges <- NULL
data$TotalCall <- NULL

#split into Train and test data
ind <-sample(x =1:nrow(data), size = nrow(data),replace = FALSE)
trainind <- ind[1:round(length(ind)*.70)]
testind <- ind[(round(length(ind)*.70)+1):length(ind)] 

#Test whether there are no intersects
intersect(trainind, testind)

#Create the sets and separate the response
train <- data[trainind,]
y_train <- train$Churn
train$Churn <- NULL

test <- data[testind,]
y_test <- test$Churn
test$Churn <- NULL

#### 3. Decision tree ####
##########################

if (!require('rpart')) { 
  install.packages('rpart',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('rpart') 
}

#Build the model and evaluate performance
library("AUC", lib.loc="~/R/win-library/3.5")
(DT <- rpart(y_train ~., train))
predDT <- predict(object = DT,newdata = test, type = 'prob')[,2]
(auc_dt <- AUC::auc(roc(predDT,y_test)))


par(xpd = TRUE)
#plot.rpart
plot(DT, compress = TRUE, minbranch = 1)
text(DT, use.n = TRUE)

#Pretty plot, easy syntax
library(rpart.plot)
rpart.plot(DT)

#Now tune the the complexity parameter
candidates <- seq(0.00,0.1,by=0.001)
aucstore <- numeric(length(candidates))
j <- 0
for (i in candidates) {
  j <- j + 1
  tree <- rpart(y_train ~ .,control=rpart.control(cp = i),
                data = train)
  predTree <- predict(tree,test)[,2]
  aucstore[j] <- AUC::auc(roc(predTree,y_test))
  if (j %% 20==0) cat(j/length(candidates)*100,"% finished\n")
}

plot(aucstore)
#Select the pest cp parameter
which.max(aucstore) 

#Re-model and evaluate on the test set
tree <- rpart(y_train ~ .,
              control=rpart.control(cp = candidates[which.max(aucstore)])
              ,train)
predTree <- predict(tree,test)[,2]
AUC::auc(roc(predTree,y_test))

#The Gini function looks like this:
plot(seq(0,1,0.1),seq(0,1,0.1)-seq(0,1,0.1)^2,type="l",
     ylab="Gini index",xlab="Proportion of ones")