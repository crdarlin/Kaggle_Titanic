# Kaggle competition - Kaggle

library(dplyr)

test <- read.csv('test.csv')
train <- read.csv('train.csv')
gender_sub <- read.csv('gender_submission.csv')

PassengerId <- test$PassengerId

train <- train %>% select(PassengerId,Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)

train$Sex <- factor(train$Sex,
                    levels = c('male','female'),
                    labels = c(1,2))

train$Embarked <- factor(train$Embarked,
                         levels = c('C','Q','S'),
                         labels = c(1,2,3))

test <- test %>% select(PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)

test$Sex <- factor(test$Sex,
                    levels = c('male','female'),
                    labels = c(1,2))

test$Embarked <- factor(test$Embarked,
                         levels = c('C','Q','S'),
                         labels = c(1,2,3))

train$Sex <- as.numeric(train$Sex)
train$Embarked <- as.numeric(train$Embarked)
for( i in 3:ncol(train)) {train[is.na(train[,i]),i] = round(mean(train[,i], na.rm = TRUE))}

test$Sex <- as.numeric(test$Sex)
test$Embarked <- as.numeric(test$Embarked)

for( i in 2:ncol(test)) {test[is.na(test[,i]),i] = round(mean(test[,i], na.rm = TRUE))}

train <- train[,2:9]
test <- test[2:8]

train[,2:8] <- scale(train[,2:8])
test[,1:7] <- scale(test[,1:7])

### Logistic Regression
log_classifier = glm(formula = Survived ~ .,
                 family = binomial,
                 data = train)

# Predicting the Test set results
log_prob_pred <- predict(log_classifier, type = 'response', newdata = test)
log_Survived <- ifelse(prob_pred > .5, 1, 0)
Survived

log_model_submission <- tibble(PassengerId,log_Survived)

write.csv(log_model_submission,'Logistic_model_submission.csv', row.names = FALSE)

### Random Forest
library(randomForest)
