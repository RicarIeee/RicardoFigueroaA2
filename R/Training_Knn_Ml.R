# Libraries required for the project
library(conflicted)
library(tidyverse)
library(ggplot2)
library(lattice)
library(caret)
library(class)
library(gmodels)
library(psych)

folder <- dirname(rstudioapi :: getSourceEditorContext()$path)
parentFolder <- dirname (folder)
data <- read.csv(paste0(folder,"/diabetes_012.csv"))
data$Diabetes_012 <- ifelse(data$Diabetes_012 == 0, 0, 1)


## selección de 100 muestras de cada factor del dataset ##
set.seed(13)
stratified_data <- data %>% group_by(Diabetes_012) %>%
                                sample_n(1500, replace = TRUE) %>%
                                ungroup()
summary(stratified_data)
pairs.panels(stratified_data[c("Age", "BMI", "HighBP", "Education")],
             pch = 21,
             bg = c("blue", "red",)[unclass(stratified_data$Diabetes_012)])

sample.index <- sample(1:nrow(stratified_data)
                       ,nrow(stratified_data)*0.7
                       ,replace = F)

### KNN Models and Experiments to Find Diabetes #####################################################################

Predictions <- c("HighBP", "HighChol",
                 "CholCheck", "BMI", "Smoker", "Stroke",
                 "HeartDiseaseorAttack", "PhysActivity",
                 "Fruits", "Veggies",
                 "HvyAlcoholConsump", "AnyHealthcare",
                 "NoDocbcCost", "GenHlth", "MentHlth",
                 "PhysHlth", "DiffWalk", "Sex",
                 "Age", "Education", "Income")

# Original data
train.data <- stratified_data[sample.index, c(Predictions, "Diabetes_012"), drop = FALSE]
test.data <- stratified_data[-sample.index, c(Predictions, "Diabetes_012"), drop = FALSE]


train.data$Diabetes_012 <- factor(train.data$Diabetes_012)
test.data$Diabetes_012 <- factor(test.data$Diabetes_012)

# Train the k-NN model
Control <- trainControl(method = "cv", p = 0.7)
KnnTry <- train(Diabetes_012 ~ .
                , data = train.data
                , method = "knn", trControl = Control
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 20)

plot(KnnTry)

# Make Predictions
knnPredict <- predict(KnnTry, newdata = test.data)

# Creates the confusion matrix
confusionMatrix(data = knnPredict, reference = test.data$Diabetes_012)

CrossTable(x = test.data$Diabetes_012,  y = knnPredict,
           prop.chisq = F)


### second Model

Predictions_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Education", "Income")
train.data2 <- train.data[, !(names(train.data) %in% Predictions_remove)]
test.data2 <- test.data[, !(names(test.data) %in% Predictions_remove)]


Control <- trainControl(method = "cv", number = 5)
KnnTry_2 <- train(Diabetes_012 ~ .
                 , data = train.data2
                 , method = "knn", trControl = Control
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

plot(KnnTry_2)

# Make Predictions
knnPredict2 <- predict(KnnTry_2, newdata = test.data2)

# Creates the confusion matrix
confusionMatrix(data = knnPredict2, reference = test.data2$Diabetes_012)

CrossTable(x = test.data2$Diabetes_012, y = knnPredict2
           , prop.chisq = F)

### Third Model

Predictions_remove2 <- c("ChoclCheck", "MentHlth","PhysHlth", "Fruits", "Veggies")
train.data3 <- train.data2[, !(names(train.data2) %in% Predictions_remove2)]
test.data3 <- test.data2[, !(names(test.data2) %in% Predictions_remove2)]

Control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
KnnTry_3 <- train(Diabetes_012 ~ .
                 , data = train.data3
                 , method = "knn", trControl = Control2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

plot(KnnTry_3)

# Make Predictions
knnPredict3 <- predict(KnnTry_3, newdata = test.data3)

# Creates the confusion matrix
confusionMatrix(data = knnPredict3, reference = test.data3$Diabetes_012)

CrossTable(x = test.data3$Diabetes_012, y = knnPredict3
           , prop.chisq = F)


###KNN Models and Experiments to Find HeartDiseaseorAttack #####################################################################

## selection of 1500 samples of each factor of the dataset#
stratified_data <- data %>%
  group_by(HeartDiseaseorAttack) %>%
  sample_n(1500, replace = TRUE) %>%
  ungroup()

Predictions <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "Diabetes_012", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# Original data
train.data <- stratified_data[sample.index, c(Predictions, "HeartDiseaseorAttack"), drop = FALSE]
test.data <- stratified_data[-sample.index, c(Predictions, "HeartDiseaseorAttack"), drop = FALSE]

train.data$HeartDiseaseorAttack <- factor(train.data$HeartDiseaseorAttack)
test.data$HeartDiseaseorAttack <- factor(test.data$HeartDiseaseorAttack)

# Train the k-NN model
Control <- trainControl(method = "cv", p = 0.7)
KnnTry <- train(HeartDiseaseorAttack ~ .
                , data = train.data
                , method = "knn", trControl = Control
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 20)

plot(KnnTry)

# Make Predictions
knnPredict <- predict(KnnTry, newdata = test.data)

# Creates the confusion matrix
confusionMatrix(data = knnPredict, reference = test.data$HeartDiseaseorAttack)


CrossTable(x = test.data$HeartDiseaseorAttack,  y = knnPredict,
           prop.chisq = F)

### second model

Predictions_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Education", "Income")
train.data2 <- train.data[, !(names(train.data) %in% Predictions_remove)]
test.data2 <- test.data[, !(names(test.data) %in% Predictions_remove)]

# Train the k-NN model
Control <- trainControl(method = "cv", number = 5)
KnnTry_2 <- train(HeartDiseaseorAttack ~ .
                 , data = train.data2
                 , method = "knn", trControl = Control
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

plot(KnnTry_2)

# Make Predictions
knnPredict2 <- predict(KnnTry_2, newdata = test.data2)

# Creates the confusion matrix
confusionMatrix(data = knnPredict2, reference = test.data2$HeartDiseaseorAttack)


CrossTable(x = test.data2$HeartDiseaseorAttack, y = knnPredict2
           , prop.chisq = F)

### Third Model

Predictions_remove2 <- c("ChoclCheck", "MentHlth","HvyAlcoholConsump", "Fruits", "Veggies")
train.data3 <- train.data2[, !(names(train.data2) %in% Predictions_remove2)]
test.data3 <- test.data2[, !(names(test.data2) %in% Predictions_remove2)]

# Train the k-NN model
Control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
KnnTry_3 <- train(HeartDiseaseorAttack ~ .
                 , data = train.data3
                 , method = "knn", trControl = Control2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

plot(KnnTry_3)

# Make Predictions
knnPredict3 <- predict(KnnTry_3, newdata = test.data3)

# Creates the confusion matrix
confusionMatrix(data = knnPredict3, reference = test.data3$HeartDiseaseorAttack)


CrossTable(x = test.data3$HeartDiseaseorAttack, y = knnPredict3
           , prop.chisq = F)

###KNN Models and Experiments to Find Sex #####################################################################


## selection of 1500 samples of each factor of the dataset#
stratified_data <- data %>%
  group_by(Sex) %>%
  sample_n(1500, replace = TRUE) %>%
  ungroup()

Predictions <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack" ,"Diabetes_012", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Age", "Education", "Income")

# Original data
train.data <- stratified_data[sample.index, c(Predictions, "Sex"), drop = FALSE]
test.data <- stratified_data[-sample.index, c(Predictions, "Sex"), drop = FALSE]

train.data$Sex <- factor(train.data$Sex)
test.data$Sex <- factor(test.data$Sex)

# Train the k-NN model
Control <- trainControl(method = "cv", p = 0.7)
KnnTry <- train(Sex ~ .
                , data = train.data
                , method = "knn", trControl = Control
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 20)

plot(KnnTry)

# Make Predictions
knnPredict <- predict(KnnTry, newdata = test.data)

# Creates the confusion matrix
confusionMatrix(data = knnPredict, reference = test.data$Sex)

CrossTable(x = test.data$Sex,  y = knnPredict,
           prop.chisq = F)

# second model

Predictions_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Age", "PhysActivity")
train.data2 <- train.data[, !(names(train.data) %in% Predictions_remove)]
test.data2 <- test.data[, !(names(test.data) %in% Predictions_remove)]

# Train the k-NN model
Control <- trainControl(method = "cv", number = 5)
KnnTry_2 <- train(Sex ~ .
                 , data = train.data2
                 , method = "knn", trControl = Control
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

plot(KnnTry_2)

#Make Predictions
knnPredict2 <- predict(KnnTry_2, newdata = test.data2)

# Creates the confusion matrix
confusionMatrix(data = knnPredict2, reference = test.data2$Sex)

CrossTable(x = test.data2$HeartDiseaseorAttack, y = knnPredict2
           , prop.chisq = F)

### Third Model

Predictions_remove2 <- c("ChoclCheck", "MentHlth","HvyAlcoholConsump", "Fruits", "Veggies")
train.data3 <- train.data2[, !(names(train.data2) %in% Predictions_remove2)]
test.data3 <- test.data2[, !(names(test.data2) %in% Predictions_remove2)]

# Train the k-NN model
Control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
KnnTry_3 <- train(Sex ~ .
                 , data = train.data3
                 , method = "knn", trControl = Control2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

plot(KnnTry_3)

#Make Predictions
knnPredict3 <- predict(KnnTry_3, newdata = test.data3)

# Creates the confusion matrix
confusionMatrix(data = knnPredict3, reference = test.data3$Sex)

CrossTable(x = test.data3$Sex, y = knnPredict3
           , prop.chisq = F)

#### 3 point##################################################################################################################

### Linear regression model BMI #####################################################################################

set.seed(13)
stratified_data2 <- data[sample(nrow(data), 3000), ]

Predictions <- colnames(stratified_data2)[-5]
sample.index <- sample(1:nrow(stratified_data2),
                       nrow(stratified_data2) * 0.7,
                       replace = FALSE)


train.data <- stratified_data2[sample.index, c(Predictions, "BMI"), drop = FALSE]
test.data <- stratified_data2[-sample.index, c(Predictions, "BMI"), drop = FALSE]

ins_moderu <- lm(BMI ~ ., data = train.data)

summary(ins_moderu)


# Train the moderu
train.control <- trainControl(method = "cv", number = 10 )
moderu <- train(BMI ~ ., data = train.data, method = "lm",
               trControl = train.control)

# Summarize the results
print(moderu)

#### second

Predictions_remove <- c("AnyHealthcare", "CholCheck", "MentHlth", "Education", "Sex")

train.data2 <- train.data[, !(names(train.data) %in% Predictions_remove)]
test.data2 <- test.data[, !(names(test.data) %in% Predictions_remove)]

ins_moderu <- lm(BMI ~ ., data = train.data2)

summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "cv", number = 5)
moderu <- train(BMI ~ ., data = train.data2, method = "lm",
               trControl = train.control)

# Summarize the results
print(moderu)

#### Third
Predictions_remove <- c("Income", "Stroke", "NoDocbcCost", "Veggies", "HvyAlcoholConsump")

train.data3 <- train.data2[, !(names(train.data2) %in% Predictions_remove)]
test.data3 <- test.data2[, !(names(test.data2) %in% Predictions_remove)]

ins_moderu <- lm(BMI ~ ., data = train.data3)

summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
moderu <- train(BMI ~ ., data = train.data3, method = "lm",
               trControl = train.control)
# Summarize the results
print(moderu)


### Linear regression moderu MentHlth #####################################################################################

set.seed(13)
stratified_data2 <- data[sample(nrow(data), 3000), ]

Predictions <- colnames(stratified_data2)[-16]
sample.index <- sample(1:nrow(stratified_data2),
                       nrow(stratified_data2) * 0.7,
                       replace = FALSE)

### ENTRENAMIENTO
train.data <- stratified_data2[sample.index, c(Predictions, "MentHlth"), drop = FALSE]
test.data <- stratified_data2[-sample.index, c(Predictions, "MentHlth"), drop = FALSE]

ins_moderu <- lm(MentHlth ~ ., data = train.data)
summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "cv", number = 10 )
moderu <- train(MentHlth ~ ., data = train.data, method = "lm",
               trControl = train.control)

# Summarize the results
print(moderu)

###Second

Predictions_remove <- c("BMI", "HeartDiseaseorAttack", "Stroke", "PhysActivity", "CholCheck")

train.data2 <- train.data[, !(names(train.data) %in% Predictions_remove)]
test.data2 <- test.data[, !(names(test.data) %in% Predictions_remove)]

ins_moderu <- lm(MentHlth ~ ., data = train.data2)
summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "cv", number = 5)
moderu <- train(MentHlth ~ ., data = train.data2, method = "lm",
               trControl = train.control)

# Summarize the results
print(moderu)

#### Third
Predictions_remove <- c("Diabetes_012", "HighBP", "HighChol", "Veggies", "Education")

train.data3 <- train.data2[, !(names(train.data2) %in% Predictions_remove)]
test.data3 <- test.data2[, !(names(test.data2) %in% Predictions_remove)]

ins_moderu <- lm(MentHlth ~ ., data = train.data3)
summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
moderu <- train(MentHlth ~ ., data = train.data3, method = "lm",
               trControl = train.control)
# Summarize the results
print(moderu)

#### Linear regression moderu PhysHlth ###################################################################################

set.seed(13)
stratified_data3 <- data[sample(nrow(data), 3000), ]

Predictions <- colnames(stratified_data2)[-17]
sample.index <- sample(1:nrow(stratified_data3),
                       nrow(stratified_data3) * 0.7,
                       replace = FALSE)

train.data <- stratified_data2[sample.index, c(Predictions, "PhysHlth"), drop = FALSE]
test.data <- stratified_data2[-sample.index, c(Predictions, "PhysHlth"), drop = FALSE]

ins_moderu <- lm(PhysHlth ~ ., data = train.data)
summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "cv", number = 10 )
moderu <- train(MentHlth ~ ., data = train.data, method = "lm",
               trControl = train.control)
# Summarize the results
print(moderu)

###Second

Predictions_remove <- c("Sex", "Diabetes_012", "Education", "CholCheck", "Smoker")

train.data2 <- train.data[, !(names(train.data) %in% Predictions_remove)]
test.data2 <- test.data[, !(names(test.data) %in% Predictions_remove)]

ins_moderu <- lm(MentHlth ~ ., data = train.data2)
summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "cv", number = 5)
moderu <- train(MentHlth ~ ., data = train.data2, method = "lm",
               trControl = train.control)
# Summarize the results
print(moderu)

#### Third

Predictions_remove <- c("BMI", "HeartDiseaseorAttack", "PhysActivity", "Veggies", "Stroke")

train.data3 <- train.data2[, !(names(train.data2) %in% Predictions_remove)]
test.data3 <- test.data2[, !(names(test.data2) %in% Predictions_remove)]

ins_moderu <- lm(MentHlth ~ ., data = train.data3)
summary(ins_moderu)

# Train the moderu
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
moderu <- train(MentHlth ~ ., data = train.data3, method = "lm",
               trControl = train.control)
# Summarize the results
print(moderu)

