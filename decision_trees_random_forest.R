# Presentation: https://docs.google.com/presentation/d/1hYQwu-5PEJIB6IBX3WQYQrdZEtQhgMwqw6cGSATKdH8/edit#slide=id.g578a8ba5a0_0_25

### Libraries:
library(googledrive)
library(haven)
library(foreign)
library(ggplot2)
library(klaR)
library(caret)
library(Matrix)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(randomForest)


### Read files:
survey <- read.spss("/Volumes/GoogleDrive/My Drive/UCSF/17_0046_UCSF_CA_Residents_Clean.sav", to.data.frame=TRUE)
survey_edit <- survey

### Variable creation:
# Q17: "How would you describe your current opinion of UCSF? Is it very positive, positive, negative, very negative?" 
survey_edit$q17bucket <- 0
survey_edit$q17bucket[survey_edit$Q17 == "Very positive"] <- 1 
table(survey_edit$q17bucket)


### Stripping unneeded columns:
# FIPS (county code)
# Q44: "What was your household income in 2016?" (Similar to 4)
# QS4: "What is the highest level of education you have received?"
# Q38: "Have you earned a degree from any of the UC campuses?" 
# removed ETHNICITY and GENDER because sparse data
table(survey_edit$ETHNICITY, useNA = "ifany")
table(survey_edit$GENDER, useNA = "ifany")
q17_clusterbucket <- survey_edit[,c("FIPS", "Q44", "AGE_BUCKET", "REGION", "QS4", "Q38", "q17bucket")]

# Making sure correct variable type
q17_clusterbucket$FIPS <- as.factor(q17_clusterbucket$FIPS)
q17_clusterbucket$q17bucket <-factor(q17_clusterbucket$q17bucket, levels = c(0, 1))


### Machine Learning (Logistic regression and Tree):
set.seed(3000)
# Split into test and training set
spl_survey_q17bucket = createDataPartition(q17_clusterbucket$q17bucket, p = 0.75, list = FALSE)
train_survey_q17bucket = q17_clusterbucket[spl_survey_q17bucket,]
test_survey_q17bucket = q17_clusterbucket[-spl_survey_q17bucket,]

#rpart (Decision Tree)
tree.q17bucket <- rpart(q17bucket ~., data = train_survey_q17bucket)
prp(tree.q17bucket)
print(tree.q17bucket)

predict.tree.q17bucket <- predict(tree.q17bucket, newdata = test_survey_q17bucket, type = "class")
confMat <- table(test_survey_q17bucket$q17bucket,predict.tree.q17bucket)
sum(diag(confMat))/sum(confMat) #accuracy
#0.6115385
confusionMatrix(table(test_survey_q17bucket$q17bucket, predict.tree.q17bucket))$overall['Kappa']
#Kappa is a more robust metric to measuring correctness with unbalanced data
#Kappa 
#0.0975945


### Seeing if RandomForest can improve model
rf.model <- randomForest(q17bucket ~., data = train_survey_q17bucket)
pred.rf.model <- predict(rf.model, newdata = test_survey_q17bucket, type = "class")
confusionMatrix(table(test_survey_q17bucket$q17bucket, pred.rf.model))



### Other regressions:
# Logistic regression:
model_q17bucket <- glm(q17bucket ~.,family=binomial(link='logit'),data=train_survey_q17bucket)
summary(model_q17bucket)
fitted.results_q17bucket <- predict(model_q17bucket,newdata=subset(test_survey_q17bucket,select=c(1,2,3,4,5,6)),type='response')
fitted.results_q17bucket <- ifelse(fitted.results_q17bucket > 0.5,1,0)
misClasificError_q17bucket <- mean(fitted.results_q17bucket != test_survey_q17bucket$q17bucket)
print(paste('Accuracy',1-misClasificError_q17bucket))
#"Accuracy 0.661538461538462"

# K-modes:
cluster.results17bucket <-kmodes(q17_clusterbucket, 2, iter.max = 10, weighted = FALSE )
cluster.results17bucket$modes
cluster.results17bucket$size
