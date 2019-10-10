
############### KDD Methodology ########################
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse","dplyr", "corrplot","ggcorrplot","Boruta","caret","e1071","randomForest","ROSE","class")

################ Reading the target datasets #########################

###########Step 01 - Selection of the target datasets##############
# Loading The Dataset -----------------------------------------------------
creditfraud <- read.csv("C:/Users/admin/Desktop/Data Analytics/Project/Creditfraud.csv", header=TRUE)
creditdefault <- read.csv("C:/Users/admin/Desktop/Data Analytics/Project/creditdefault.csv", header=TRUE)
loandefault <- read.csv("C:/Users/admin/Desktop/Data Analytics/Project/loandefault.csv", header=TRUE)

###### Step 02 - Pre-processing of dataset ################
fraud_df<-creditcard
creddt_df <- creditdefault
loandt_df <- loandefault

##### Header of the datasets ###############
head(fraud_df)  
head(creddt_df) 
head(loandt_df)  

#### Structure of the datasets ########
str(fraud_df)  
str(creddt_df)  
str(loandt_df) 

### Ientifying missingvalues in each dataset ########
sapply(fraud_df, FUN=function(x) {sum(is.na(x))})
sapply(creddt_df, FUN=function(x) {sum(is.na(x))})
sapply(loandt_df, FUN=function(x) {sum(is.na(x))})

####### Exploring Relationshop among variables #############
####### correlation plot on predictive variables ############
numeric <- names(select_if(fraud_df, is.numeric))
numeric1 <- names(select_if(creddt_df, is.numeric))
numeric2 <- names(select_if(loandt_df, is.numeric))

####### Correlation matrix #######
cor_df <- as.matrix(fraud_df[numeric])
cor_mat <- cor(as.matrix(cor_df))

cor_df1 <- as.matrix(creddt_df[numeric1])
cor_mat1 <- cor(as.matrix(cor_df1))

cor_df2 <- as.matrix(loandt_df[numeric2])
cor_mat2 <- cor(as.matrix(cor_df2))


#### Correlation plot for each dataset ####
corrplot(cor_mat, method = "circle", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(cor_mat1, method = "circle", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(cor_mat2, method = "circle", order = "hclust", tl.col = "black", tl.srt = 45)


########## Outlier Function ################
outlierAM <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
# From the correlation plot we could see that "recoveries" feature is posses highest correlation with -
# target variable "loan_condition_cat" so, lets find the outliers in the "recoveries" feature,
outlierAM(fraud_df, V11)
outlierAM(creddt_df, PAY_0)
outlierAM(loandt_df, recoveries)

#########Step03 Transformation of datasets########
#### Credit Card Fraud #######
fraud_df$Class <- as.factor(fraud_df$Class)
str(fraud_df$Class)

######## Credit Card default #########
creddt_df$SEX <- as.factor(creddt_df$SEX)
str(creddt_df$SEX)
creddt_df$EDUCATION <- as.factor(creddt_df$EDUCATION)
str(creddt_df$EDUCATION)
creddt_df$MARRIAGE <- as.factor(creddt_df$MARRIAGE)
str(creddt_df$MARRIAGE)
creddt_df$PAY_0 <- as.factor(creddt_df$PAY_0)
str(creddt_df$PAY_0)
creddt_df$PAY_2 <- as.factor(creddt_df$PAY_2)
str(creddt_df$PAY_2)
creddt_df$PAY_3 <- as.factor(creddt_df$PAY_3)
str(creddt_df$PAY_3)
creddt_df$PAY_4 <- as.factor(creddt_df$PAY_4)
str(creddt_df$PAY_4)
creddt_df$PAY_5 <- as.factor(creddt_df$PAY_5)
str(creddt_df$PAY_5)
creddt_df$PAY_6 <- as.factor(creddt_df$PAY_6)
str(creddt_df$PAY_6)
creddt_df$default.payment.next.month <- as.factor(creddt_df$default.payment.next.month)
str(creddt_df$default.payment.next.month)

###### P2P loan default ###########
loandt_df$home_ownership_cat <- as.factor(loandt_df$home_ownership_cat)
str(loandt_df$home_ownership_cat)
loandt_df$income_cat <- as.factor(loandt_df$income_cat)
str(loandt_df$income_cat)
loandt_df$term_cat <- as.factor(loandt_df$term_cat)
str(loandt_df$term_cat)
loandt_df$application_type_cat <- as.factor(loandt_df$application_type_cat)
str(loandt_df$application_type_cat)
loandt_df$purpose_cat <- as.factor(loandt_df$purpose_cat)
str(loandt_df$purpose_cat)
loandt_df$interest_payment_cat <- as.factor(loandt_df$interest_payment_cat)
str(loandt_df$interest_payment_cat)
loandt_df$loan_condition_cat <- as.factor(loandt_df$loan_condition_cat)
str(loandt_df$loan_condition_cat)
loandt_df$grade_cat <- as.factor(loandt_df$grade_cat)
str(loandt_df$grade_cat)

############### Numeric Data Quality Report #################
#############################################################
dataQualityNum <- function(df) {
  # filter numerics
  n <- sapply(df, function(x) {is.numeric(x)})
  df_numerics1 <- df[, n]
  # count number of rows
  instances <- sapply(df_numerics1, FUN=function(x) {length(x)})
  # count number of missing values
  missing <- sapply(df_numerics1, FUN=function(x) {sum(is.na(x))})
  missing <- missing / instances * 100
  # determine the length of the vector of unique values
  unique <- sapply(df_numerics1, FUN=function(x) {length(unique(x))})
  # calculate the quantiles
  quantiles <- t(sapply(df_numerics1, FUN=function(x) {quantile(x)}))
  # calculate the mean
  means <- sapply(df_numerics1, FUN=function(x) {mean(x)})
  # calculate the standard deviation
  sds <- sapply(df_numerics1, FUN=function(x) {sd(x)})
  # sapply is a loop operator that works on the COLUMNS of a data.frame
  # build a dataframe of all components of the DQR
  df_numeric <- data.frame(Feature=names(df_numerics1),
                           Instances=instances,
                           Missing=missing,
                           Cardinality=unique,
                           Min=quantiles[,1],
                           FirstQuartile=quantiles[,2],
                           Median=quantiles[,3],
                           ThirdQuartile=quantiles[,4],
                           Max=quantiles[,5],
                           Mean=means,
                           Stdev=sds)
  #  remove rownames -- they have no meaning here
  rownames(df_numeric) <- NULL
  return(df_numeric)
}
#Now to call:
df_numerics1 <- dataQualityNum(fraud_df)
View(df_numerics1)
df_numerics2 <- dataQualityNum(creddt_df)
View(df_numerics2)
df_numerics3 <- dataQualityNum(loandt_df)
View(df_numerics3)

#And view:
View(df_numerics1)
View(df_numerics2)
View(df_numerics3)



###########Categorical Data Quality Report ###########
dataQualityCat <- function(df) {
  # Filteration of categorical data from dataset 2 without outliers
  n <- sapply(df, function(x) {is.numeric(x)})
  df_categoricals <- df[, !n]
  # Number of categorical rows in each feature
  instances <- sapply(df_categoricals, FUN=function(x) {length(x)})
  # Number of missing values (It must be zero for all numeric features as we are generating DQR for transformed data)
  missing <- sapply(df_categoricals, FUN=function(x) {sum(is.na(x))})
  missing <- missing / instances * 100
  # Length of the vector of unique values
  unique <- sapply(df_categoricals, FUN=function(x) {length(unique(x))})
  # Finding the most frequent categorical level
  modeFreqs <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    return(modeFreq)
  })
  # For all modes, get their frequency
  modes <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    mode <- names(t)[t==modeFreq]
    return(mode)
  })
  # Now throw away the mode and repeat for the second mode
  modeFreqs2 <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    mode <- names(t)[t==modeFreq]
    # we remove the 1st mode here
    x <- x[x != mode]
    t <- table(x)
    mode2Freq <- max(t)
    return(mode2Freq)
  })
  modes2 <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    mode <- names(t)[t==modeFreq]
    # we remove the 1st mode here
    x <- x[x != mode]
    t <- table(x)
    mode2Freq <- max(t)
    mode2 <- names(t)[t==mode2Freq]
    return(mode2)
  })
  # Build data.frame as before, but also derive the mode frequenies
  df_categorical <- data.frame(Feature=names(df_categoricals),
                               Inst=instances,
                               Miss=missing,
                               Card=unique,
                               FstMod=modes,
                               FstModFrq=modeFreqs,
                               Feature=names(df_categoricals),
                               FstModPnt=modeFreqs/instances*100,
                               SndMod=modes2,
                               SndModFrq=modeFreqs2,
                               SndModPnt=modeFreqs2/instances*100)
  #To fit the table on the page, the above columns were slightly renamed.
  rownames(df_categorical) <- NULL
  return(df_categorical)
}
# Now to call:
df_categorical1 <- dataQualityCat(fraud_df)
df_categorical2 <- dataQualityCat(creddt_df)
df_categorical3 <- dataQualityCat(loandt_df)

# And view:
View(df_categorical1)
View(df_categorical2)
View(df_categorical3)


#######################################################################



############ Model Building #################################
set.seed(123)
DA_df1  <- fraud_df
DA_df2  <- creddt_df
DA_df3  <- loandt_df
numeric <- names(select_if(loandt_df, is.numeric))
DA_df3a <- (loandt_df[numeric])

################# Sampling the Datasets ##########################
########## Credit card fraud ############
DA_df1$Class <- as.factor(DA_df1$Class)
DA_df1 <- sample_n(DA_df1,20000) #Due to long run time we are considering only 20000 rows
index1 <- createDataPartition(DA_df1$Class, p=.70, list=FALSE, times=1)
train1 <- DA_df1[index1, ]
test1 <- DA_df1[-index1, ]

########## Credit card Default ############
DA_df2$default.payment.next.month <- as.factor(DA_df2$default.payment.next.month)
index2 <- createDataPartition(DA_df2$default.payment.next.month, p=.70, list=FALSE, times=1)
train2 <- DA_df2[index2, ]
test2 <- DA_df2[-index2, ]

########## P2P lending loan default ############
DA_df3$loan_condition_cat <- as.factor(DA_df3$loan_condition_cat)
DA_df3 <- sample_n(DA_df3,20000) #Due to long run time we are considering only 20000 rows
index3 <- createDataPartition(DA_df3$loan_condition_cat, p=.70, list=FALSE, times=1)
train3 <- DA_df3[index3, ]
test3 <- DA_df3[-index3, ]
test3$issue_d <- NULL
train3$issue_d <- NULL

########## P2P lending loan default numeric ############
DA_df3a$loan_condition_cat <- as.factor(DA_df3a$loan_condition_cat)
DA_df3a <- sample_n(DA_df3a,20000) #Due to long run time we are considering only 20000 rows
index3 <- createDataPartition(DA_df3a$loan_condition_cat, p=.70, list=FALSE, times=1)
train3a <- DA_df3a[index3, ]
test3a <- DA_df3a[-index3, ]
test3$issue_d <- NULL
train3$issue_d <- NULL

########### Data Mining ###########################

########Model 01 - Logistic regression ################

########Credit Card Fraud ##############
lg_model1 <- glm(Class ~ ., data = train1,  family=binomial)
summary(lg_model1)
#### Prediction ##########
pred1 <- as.factor(ifelse(predict(lg_model1,newdata = test1) >0.5, 1,0))
confusionMatrix(test1$Class,pred1, positive = "1")
###########

###Credit card default ########
lg_model2 <- glm(default.payment.next.month ~ ., data = train2,  family=binomial)
summary(lg_model2)
#### Prediction ##########
#pred2 <- as.factor(predict(lg_model2,newdata = test2))
pred2 <- as.factor(ifelse(predict(lg_model2,newdata = test2) >0.5, 1,0))
confusionMatrix(test2$default.payment.next.month,pred2, positive = "1")
####

###P2P loan default ########
lg_model3 <- glm(loan_condition_cat ~ ., data = subset(train3,select=c(-id,-final_d)),  family=binomial)
summary(lg_model3)
#### Prediction ##########
pred3 <- as.factor(ifelse(predict(lg_model3,newdata = test3) >0.5, 0,1))
confusionMatrix(test3$loan_condition_cat,pred3, positive = "1")

###### Model 02 - SVM model ##################
###### Credit card fraud ######################
library(kernlab)
svm1 <- ksvm(Class ~ ., data = train1)
pred1 <- predict(svm1, newdata = test1, type = 'response')
# confusion matrix 
confusionMatrix(pred1, test1$Class)

###### Credit card default ######################
svm2 <- ksvm(default.payment.next.month ~ ., data = train2)
pred2 <- predict(svm2, newdata = test2, type = 'response')
# confusion matrix 
confusionMatrix(pred2, test2$default.payment.next.month)

###### P2P loan default ######################
svm3 <- ksvm(loan_condition_cat ~ ., data = train3)
pred3 <- predict(svm3, newdata = test3, type = 'response')
# confusion matrix 
confusionMatrix(pred3, test3$loan_condition_cat)

# Model 03 - Random Forest #####################
###### Credit card Fraud ######################
rf_model1 <- randomForest(Class ~ ., data = train1)
summary(rf_model1)
varImpPlot(rf_model1)

pred1 <- (predict(rf_model1,newdata = test1))
confusionMatrix(test1$Class,pred1, positive = "1")
######

###### Credit Card default ##########
rf_model2 <- randomForest(default.payment.next.month ~ ., data = train2)
summary(rf_model2)
varImpPlot(rf_model2)

pred2 <- (predict(rf_model2,newdata = test2))
confusionMatrix(test2$default.payment.next.month,pred2, positive = "1")
######

####### P2P Loan Default #################

rf_model3 <- randomForest(loan_condition_cat ~ ., data = train3)
summary(rf_model3)
varImpPlot(rf_model3)

pred3 <- (predict(rf_model3,newdata = test3))
confusionMatrix(test3$loan_condition_cat,pred3, positive = "1")
######

######## Model 04 - KNN #####################
###### Credit card Fraud ######################
knn1 <- knn(train = train1, test = test1, cl = train1$Class, k = 5)
confusionMatrix(knn1, test1$Class, positive = "1")

###### Credit card Default ######################
knn2 <- knn(train = train2, test = test2, cl = train2$default.payment.next.month, k = 5)
confusionMatrix(knn2, test2$default.payment.next.month, positive = "1")

###### P2P Loan Default ######################
knn3a <- knn(train = train3a, test = test3a, cl = train3a$loan_condition_cat, k = 3)
confusionMatrix(knn3a, test3a$loan_condition_cat, positive = "1")
###############That's All Folks ####################