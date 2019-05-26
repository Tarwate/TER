library(dplyr) ## data manipulation
library(ggplot2) ## data viz
library(readr)  ## reading data
library(rmarkdown) ## report markdown
library(gmodels) ## logistic regression
library(rpart) ## decision trees
library(pROC)
library(tidyverse)    
library(rpart.plot)

##Load the table
loan_data<-readRDS("C:\\Users\\Benja\\Downloads\\loan_data_ch1.rds")
## Structure of the table
str(loan_data)
summary(loan_data)
head(loan_data)
##Default information is stored in variable loan_status : 1=default, 0=non-default, we will check what the proportion of defaults is in a loan dataset. 
##you should examine the relationship between loan_status and certain factor variables. 
##We would expect that the proportion of defaults in the group of customers with grade G (worst credit rating score) is substantially higher than the proportion of defaults in the grade A group (best credit rating score).
CrossTable(loan_data$loan_status)
CrossTable(loan_data$grade,loan_data$loan_status,prop.r=TRUE , prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)

                                               # Histogram 


### Create histogram of loan_amnt: hist_1
hist_1<-hist(loan_data$loan_amnt)

# Print locations of the breaks in hist_1
hist_1$breaks

# Change number of breaks and add labels: hist_2
hist_2 <- hist(loan_data$loan_amnt, breaks = 200, xlab = "Loan amount", 
               main = "Histogram of the loan amount")

                                              ## Outliers 

# Make bivariate scatterplot of age and annual income : we can see a outlier : age>140 & annual_inc >6000000
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual Income")

# remove the outlier
loan_data <- loan_data[-which(loan_data$age>140), ]

# Outlier remove
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual Income")

                                            ### REPLACING MISSING VALUE 



                                    ## REPLACING MISSING VALUE BY MEDIAN

## As we can see, there is NA values for int_rate and emp_grade 
summary(loan_data)

## get index of NA values
##na_index_int_rate <- which(is.na(loan_data$int_rate))
##na_index_emp_length <- which(is.na(loan_data$emp_length))

## get median for int_rate and emp_length
##median_int_rate<-median(loan_data$int_rate, na.rm=TRUE)
##median_emp_length<-median(loan_data$emp_length, na.rm=TRUE)

## replacing NA values of int_rate and emp_length by their median values
##loan_data$int_rate[na_index_int_rate] <- median_int_rate
##loan_data$emp_length[na_index_emp_length] <- median_emp_length

## check if there remain NA's Values
##summary(loan_data$int_rate)
##summary(loan_data$emp_length)


                            ## Replacements in the coarse classification in order to perform binomial logistic regression 


# In some situations, the fact that an input is missing is important information in itself. NAs can be kept in a separate "missing" category using coarse classification.
# Coarse classification allows you to simplify your data and improve the interpretability of your model. 
#Coarse classification requires that we bin our responses into groups that contain ranges of values. 

# Variable int_rate : new var classify by category : int_cat
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))
loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"
loan_data$ir_cat <- as.factor(loan_data$ir_cat)


#index<-which(is.na(loan_data$int_rate))
#index2<-which(is.na(loan_data$emp_length))
#both_col_na<-intersect(index,index2)
#loan_data<-loan_data[-both_col_na,]

# Variable emp_length : new var classify by category : emp_cat
loan_data$emp_cat <- rep(NA, length(loan_data$emp_length))
loan_data$emp_cat[which(loan_data$emp_length <= 15)] <- "0-15"
loan_data$emp_cat[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
loan_data$emp_cat[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
loan_data$emp_cat[which(loan_data$emp_length > 45)] <- "45+"
loan_data$emp_cat[which(is.na(loan_data$emp_length))] <- "Missing"
loan_data$emp_cat <- as.factor(loan_data$emp_cat)

# Look at your news variables using plot()
plot(loan_data$emp_cat, xlab="emp_cat", main="Category of em_lenght : emp length")
plot(loan_data$ir_cat, xlab="ir_cat", main="Category of interest rate : int rate")


# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train<-sample(1:nrow(loan_data),2/3*nrow(loan_data))

# Create training set: training_set
training_set <- loan_data[index_train, ]

# Create test set: test_set
test_set<-loan_data[-index_train,]

                                          # LOGISTIC REGRESSION


# Fit the logit, probit and cloglog-link logistic regression models
log_model_logit <- glm(loan_status ~ grade+annual_inc+age+emp_cat+ir_cat, family = binomial(link = logit), data = training_set)
summary(log_model_logit)
# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")

# Set a cut-off of 14% 
cutoff2 <- quantile(predictions_logit, 0.8)
# Make binary predictions-vector with cut off
class_pred_logit <- ifelse(predictions_logit > 0.16, 1, 0)


# Make a confusion matrix for the three models
conf_mat_logit <- table(test_set$loan_status,class_pred_logit)
conf<-table(class_pred_logit,test_set$loan_status)
# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
test<- sum(tab_class_logit[1,2],tab_class_logit[2,1])
ROC_logit <- roc(test_set$loan_status, predictions_logit)
plot(ROC_logit,col="red")
auc(ROC_logit)
                                               ### DECISION TREES

tree_prior <- rpart (loan_status ~ ., method = "class",
                     data = training_set,
                     parms= list(prior=c(0.7,0.3)),
                     control = rpart.control(cp=0.001))
# Plot the decision tree
plot(tree_prior,uniform=TRUE)
# Add labels to the decision tree
text(tree_prior)
# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[ , "xerror"])
# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]
# élaguer l'arbre de décision
ptree_prior<-prune(tree_prior,cp=tree_min)
# afficher l'arbre
prp(ptree_prior)
# Prédiction probabilité pour le test_set
pred_prior <- predict(ptree_prior, newdata=test_set)[,2]
# Matrice de confusion
conf_mat_prior <- table(test_set$loan_status, bin_pred_prior_80)

# Make predictions for the probability of default using the pruned tree and the test set.
prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]
# Obtain the cutoff for acceptance rate 80%
cutoff_prior <- quantile(prob_default_prior, 0.8)
# Obtain the binary predictions avec un taux d'acceptation de 80%
bin_pred_prior_80 <- ifelse(prob_default_prior > 0.3503, 1, 0)
#matrice de confusion 
table(test_set$loan_status, bin_pred_prior_80)
# Courbe ROc de l'arbre de décision et on ajour le ROC de la régression logistique
ROC_prior <- roc(test_set$loan_status,prob_default_prior)
plot(ROC_prior, main = "Régression logistique (blue) vs Arbre de décision (red)",col="red")
lines(ROC_logit,col="blue")

#calcul AUC pour L'arbre de décision et la régression logistique
auc(ROC_prior)
auc(ROC_logit)


              ## Strategy Curves 
strategy_bank<-function(prob_of_def){
  sensibility=rep(NA,21)
  sensitivity=rep(NA,21)
  bonne_prediction=rep(NA,21)
  cutoff=rep(NA,21)
  bad_rate=rep(NA,21)
  defaut<-test_set$loan_status
  accept_rate=seq(1,0,by=-0.05)
  for (i in 1:21){
    cutoff[i]=quantile(prob_of_def,accept_rate[i])
    pred_i=ifelse(prob_of_def>cutoff[i],1,0)
    pred_as_good=test_set$loan_status[pred_i==0]
    bad_rate[i]=sum(pred_as_good)/length(pred_as_good)
    tableau1<-as.data.frame(cbind(test_set$loan_status,pred_i))
    tableau1[,1]<-defaut
    bonne_prediction[i]=sum(tableau1 %>% filter (V1==1 & pred_i==1)%>% nrow(),tableau1 %>% filter(V1==0 & pred_i==0) %>% nrow())/ tableau1%>%nrow()
    sensibility[i]= tableau1 %>% filter(defaut==1 & pred_i==1)%>%nrow()/ sum(nrow(tableau1%>%filter(defaut==1 & pred_i==1)),nrow(tableau1%>%filter(defaut==1 & pred_i==0)))
    sensitivity[i]=tableau1 %>% filter(defaut==0 & pred_i==0) %>% nrow()/ sum(tableau1 %>% filter(defaut==0 & pred_i==0)%>%nrow(),tableau1 %>% filter(defaut==0 & pred_i==1)%>%nrow())
  }
  table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4),good_rate=round(bonne_prediction,4), sensibility=round(sensibility,4),specificity=round(sensitivity,4))
}

# LOAD & EXAMINE STRUCTURE OF THE TABLE

test<-strategy_bank(predictions_logit)
test
test2<-strategy_bank(pred_prior)
test2




