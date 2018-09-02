
#--------------------------------Logistic Regression Modelling by Shashank Tanwar--------------------------------#
#-----------------------------Customer Churn Prediction----------------------------

#Problem Statement: 


#To predict which customers are more probable to churn (Y=1), based on the attributes to the customer 

#------------------------------Preparing the environment for Logistic Regression---------------------------------------#

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"C:/Users/Stark/Documents/ivy files/R/IVY--R PREDICTIVE ANALYTICS -- RESOURCES-20180618T082011Z-001/IVY--R PREDICTIVE ANALYTICS -- RESOURCES/01DATASETS"

setwd(Path)
getwd()


data<-read.csv("Fn-UseC_-Telco-Customer-Churn.csv",header = TRUE)
data1=data#To create a backup of original data
head(data1)

#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)
data1$SeniorCitizen<-as.factor(data1$SeniorCitizen)
str(data1)


#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
data.frame(colSums(is.na(data1)))


#---->Substituting missing values with mean

data1[is.na(data1$TotalCharges),19]=mean(data1$TotalCharges,na.rm=T)

data.frame(colSums(is.na(data1)))



#--------------------------------Information Value Calculation (A variable reduction technique)----------------------------------#

#-----------> Creating two data sets for numeric and categorical values

## Data set with numeric variable
num <- data1[,-c(1:4,6:17)]#Numerical Data Frame
cat <- data1[,c(1:4,6:17,20)]#Categorical Data Frame
head(cat)
head(num)
str(num)
str(cat)


#---------------------------------------IV for numeric data-------------------------------------------------------#


IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCal("tenure","Churn",num,groups=10)
a2<- IVCal("MonthlyCharges","Churn",num,groups=10)
a3<- IVCal("TotalCharges","Churn",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3))
IV_num





 #-------------------------------------Information Value for categorical data----------------------------------------------------------#

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
A<- CA("Churn","gender",cat)
B<- CA("Churn","SeniorCitizen",cat)
C<- CA("Churn","Partner",cat)
D<- CA("Churn","Dependents",cat)
E<- CA("Churn","PhoneService",cat)
F<- CA("Churn","MultipleLines",cat)

G<- CA("Churn","InternetService",cat)
H<- CA("Churn","OnlineSecurity",cat)
I<- CA("Churn","OnlineBackup",cat)
J<- CA("Churn","DeviceProtection",cat)
K<- CA("Churn","TechSupport",cat)
L<- CA("Churn","StreamingTV",cat)
M<- CA("Churn","StreamingMovies",cat)
N<- CA("Churn","Contract",cat)
O<- CA("Churn","PaperlessBilling",cat)
P<- CA("Churn","PaymentMethod",cat)


IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

write.csv(Final_IV,"Final_IV.csv")



#--------------------------Splitting the data into training and test data set------------------------#


set.seed(144)

spl = sample.split(data1$Churn, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)


#-------------------------------------Logistic Regression Model Building------------------------------------------#


model <- glm(Churn~., data=data.train, family=binomial())
summary(model)




## Remove the insignificant variable
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService + MultipleLines
             + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV 
             + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges 
             + TotalCharges, data=data.train, family=binomial())
summary(model)



##
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove Partner, PhoneService, Dependents, gender 
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove  I(OnlineBackup=="Yes"), I(DeviceProtection=="Yes") , I(OnlineSecurity=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService   +  I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)

## Remove   I(TechSupport=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + I(PaymentMethod=="Credit card (automatic)")
             + I(PaymentMethod=="Electronic check")+MonthlyCharges +	TotalCharges , data=data.train, family=binomial())
summary(model)

#-----------------------------------------Final Model---------------------------------------#
## Remove  I(PaymentMethod == "Credit card (automatic)")
model1 <- glm(Churn~ 	SeniorCitizen  
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
             + I(PaymentMethod=="Electronic check")+ TotalCharges+MonthlyCharges+tenure , data=data.train, family=binomial())
summary(model1)


vif(model1)

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



## Remove   MonthlyCharges, tenure
model <- glm(Churn~ 	SeniorCitizen  
             + I(MultipleLines=="Yes")+ InternetService  +tenure  + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
             + I(PaymentMethod=="Electronic check"), data=data.train, family=binomial())
summary(model)

vif(model)


#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(model), Sigma= vcov(model), Terms=1:12)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)


# Difference betweene null deviance and deviance
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf

# 
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(model)

# Hosmer and Lemeshow given by the McFadden 6R square
R2.hl<-modelChi/model$null.deviance
R2.hl


# Cox and Snell R Square (the last number; 

R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data.train))
R.cs

# Max rescaled R square (Nagelkarke) 

R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data.train))))))
R.n



#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

#########Larger p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies


# Hosmer and Lemeshow test 


hl <- hoslem.test(as.integer(data.train$Churn), fitted(model), g=10)
hl


#####################################################################################################################
# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)


# Variable Importance of the model
varImp(model)

# Predicted Probabilities
prediction <- predict(model,newdata = data.train,type="response")
prediction

write.csv(prediction,"pred.csv")

data.train$Churn <- as.factor(data.train$Churn)

rocCurve   <- roc(response = data.train$Churn, predictor = prediction, 
                  levels = rev(levels(data.train$Churn)))


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Churn)

AccuracyRate <- sum(diag(Confusion))/sum(Confusion)

Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Churn)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # should lie between 0.4 - 0.7

############################################################################################################


###################### Residual Analysis ################################################################################


logistic_data <- data.train

logistic_data$predicted.probabilities<-fitted(model)

logistic_data$standardized.residuals<-rstandard(model)
logistic_data$studentized.residuals<-rstudent(model)
logistic_data$dfbeta<-dfbeta(model)
logistic_data$dffit<-dffits(model)
logistic_data$leverage<-hatvalues(model)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, "Res.csv")




###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################




# Logistic Regression on full data


modelt <- glm(Churn~ 	SeniorCitizen  
              + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.test, family=binomial())
summary(modelt)

modelt <- glm(Churn~ I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.test, family=binomial())
summary(modelt)



vif(modelt)


library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt$null.deviance - modelt$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt$df.null - modelt$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/modelt$null.deviance
R2.hl


# Cox and Snell R Square (the last number; here is 2000 should be total no. of ovservation)

R.cs <- 1 - exp ((modelt$deviance - modelt$null.deviance) /2000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(modelt$null.deviance/2000))))
R.n



######### Lackfit Deviance ######################################################
residuals(modelt) # deviance residuals
residuals(modelt, "pearson") # pearson residuals

sum(residuals(modelt, type = "pearson")^2)
deviance(modelt)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt), df.residual(modelt))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## High p value incidates the model fits well

hosmerlem(y = data.test$Churn, yhat = fitted(modelt))
################################################################################################################
# Hosmer and Lemeshow test in a different way
## High p value incidates the model fits well

library(ResourceSelection)
hl <- hoslem.test(data.test$Churn, fitted(modelt), g=10)
hl
#####################################################################################################################
# Coefficients (Odds)
modelt$coefficients
# Coefficients (Odds Ratio)
exp(modelt$coefficients)

# Predicted Probabilities
prediction <- predict(modelt,newdata = data.test,type="response")
prediction

#write.csv(prediction, file = "C://Users//Subhojit//Desktop//Logistic Regression//Prepared by me//pred.csv")

data.test$Churn <- as.factor(data.test$Churn)

rocCurve   <- roc(response = data.test$Churn, predictor = prediction, 
                  levels = rev(levels(data.test$Churn)))


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.test$m1.yhat <- predict(modelt, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$Churn)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################

#########################################################################################################################
###################### Residual Analysis ################################################################################


logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(modelt)
logistic_data$standardized.residuals<-rstandard(modelt)
logistic_data$studentized.residuals<-rstudent(modelt)
logistic_data$dfbeta<-dfbeta(modelt)
logistic_data$dffit<-dffits(modelt)
logistic_data$leverage<-hatvalues(modelt)


#######################################################################################################

##########################################

